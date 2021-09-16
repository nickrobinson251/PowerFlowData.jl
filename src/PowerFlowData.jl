module PowerFlowData

using Parsers: Parsers, Options, xparse
using Parsers: codes, eof, invalid, newline, peekbyte
using Tables
using DocStringExtensions


export Network
export parse_network

# for easier local development
export getbytes, getstring
export parse_record
export CaseID, Bus, Load


###
### types
###

# TODO: should the various bits of free text / comments / timestamps be in this struct?
# Data can look like:
# 0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
#    SE SNAPSHOT 09-15-2021 PEAK CASE 18:00
#    FULL COPY OF ETC.
struct CaseID
    ic::Int
    sbase::Float64
end

CaseID() = CaseID(0, 100.0)

# So all tabular data records (bus, load, ...) can be handled the same.
abstract type Record end

# Store data in column table so conversion to DataFrame efficient.
Tables.istable(::Type{Record}) = true
Tables.columnaccess(::Type{Record}) = true
Tables.columns(x::Record) = x
Tables.getcolumn(x::Record, i::Int) = getfield(x, i)
Tables.columnnames(x::R) where {R <: Record} = fieldnames(R)
Tables.schema(x::R) where {R <: Record} = Tables.Schema(fieldnames(R), fieldtypes(R))

struct Bus <: Record
    i::Vector{Int}
    name::Vector{String}
    basekv::Vector{Float64}
    ide::Vector{Int}
    gl::Vector{Float64}
    bl::Vector{Float64}
    area::Vector{Int}
    zone::Vector{Int}
    vm::Vector{Float64}
    va::Vector{Float64}
    owner::Vector{Int}
end

function Bus(nrows)
    return Bus(
        Vector{Int}(undef, nrows),
        Vector{String}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
    )
end

"""
    Load <: Record

# Fields
$(TYPEDFIELDS)
"""
struct Load <: Record
    "Bus number, or extended bus name enclosed in single quotes."
    i::Vector{Int}
    "One- or two-character uppercase non blank alphanumeric load identifier used to distinguish among multiple loads at bus 'I'. It is recommended that, at buses for which a single load is present, the load be designated as having the load identifier '1'."
    id::Vector{String}
    "Initial load status of one for in-service and zero for out-of-service."
    status::Vector{Float64}
    "Area to which the load is assigned (1 through the maximum number of areas at the current size level)."
    area::Vector{Int}
    "Zone to which the load is assigned (1 through the maximum number of zones at the current size level)."
    zone::Vector{Float64}
    "Active power component of constant MVA load; entered in MW."
    pl::Vector{Float64}
    "Reactive power component of constant MVA load; entered in Mvar."
    ql::Vector{Float64}
    "Active power component of constant current load; entered in MW at one per unit voltage."
    ip::Vector{Float64}
    "Reactive power component of constant current load; entered in Mvar at one per unit voltage."
    iq::Vector{Float64}
    "Active power component of constant admittance load; entered in MW at one per unit voltage."
    yp::Vector{Float64}
    "Reactive power component of constant admittance load; entered in Mvar at one per unit voltage. YQ is a negative quantity for an inductive load and positive for a capacitive load."
    yq::Vector{Float64}
    "Owner to which the load is assigned (1 through the maximum number of owners at the current size level)."
    owner::Vector{Int}
end

function Load(nrows)
    return Load(
        Vector{Int}(undef, nrows),
        Vector{String}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
    )
end

struct Network
    caseid::CaseID
    bus::Bus
    load::Load
end

###
### parsing
###

getbytes(source::Vector{UInt8}) = source, 1, length(source)
getbytes(source::IOBuffer) = source.data, source.ptr, source.size
getbytes(source) = getbytes(read(source))

function parse_network(source; first_record=1)
    options = Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        # comment="0", # HACK2: alternative to using `peekbyte(...); next_line(...)`
        delim=',',
    )
    bytes, pos, len = getbytes(source)

    # TODO: figure out how to parse Case ID data...
    caseid = CaseID() # placeholder
    # HACK3: til we figure out how to parse CaseID and skip comments, just tell us where
    # data starts...
    for _ in 1:first_record-1
        pos = next_line(bytes, pos, len)
    end

    nrows = guess_nrows(bytes, pos, len, options)
    @debug "bus" nrows pos
    bus, pos = parse_record!(Bus(nrows), bytes, pos, len, options)

    nrows = guess_nrows(bytes, pos, len, options)
    @debug "load" nrows pos
    load, pos = parse_record!(Load(nrows), bytes, pos, len, options)

    return Network(caseid, bus, load)
end

function parse_record!(rec::Record, bytes, pos, len, options)
    # HACK1: avoid end-of-line comments, e.g. `/* comments */`, by setting "/" as delimiter
    # TODO: change Parsers.jl to handle rows having end-of-line comments
    eol_options = Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        # comment="0", # HACK2: alternative to using `peekbyte(...); next_line(...)`
        delim='/',
    )
    nrows = length(getfield(rec, 1))
    for row in 1:nrows
        pos, code = parse_row!(rec, row, bytes, pos, len, options, eol_options)

        # HACK1 continued: Because we're introducing a delimiter to workaround EOL comments,
        # rows with comments won't have hit the newline character yet, and
        # we need to run `xparse` again to get to the newline
        if !newline(code)
            pos = next_line(bytes, pos, len)
        end
    end

    # Data input is terminated by specifying a bus number of zero.
    # @assert peekbyte(bytes, pos) === UInt8('0')
    peekbyte(bytes, pos) === UInt8('0') || @warn "not at end of card"
    pos = next_line(bytes, pos, len)
    return rec, pos
end

# Taken from `CSV.detectdelimandguessrows`
# TODO: move to Parsers.jl?
# This is actually "count_nrow" right now..
# TODO: Figure out how to make this a _guess_ rather actually parsing
# all lines til we find a line starting with `0`
function guess_nrows(buf, pos, len, options)
    nbytes = 0
    nlines = 0
    parsedanylines = false
    lastbytenewline = false
    firstbyte = peekbyte(buf, pos)

    oq = options.oq
    eq = options.e
    cq = options.cq

    while pos <= len #= && nlines < 10 =# && firstbyte !== UInt8('0')
        if lastbytenewline
            firstbyte = peekbyte(buf, pos)
        end
        parsedanylines = true
        @inbounds b = buf[pos]
        pos += 1
        nbytes += 1
        if b == oq
            while pos <= len
                @inbounds b = buf[pos]
                pos += 1
                nbytes += 1
                if b == eq
                    if pos > len
                        break
                    elseif eq == cq && buf[pos] != cq
                        break
                    end
                    @inbounds b = buf[pos]
                    pos += 1
                    nbytes += 1
                elseif b == cq
                    break
                end
            end
        elseif b == UInt8('\n')
            nlines += 1
            lastbytenewline = true
        elseif b == UInt8('\r')
            if pos <= len && buf[pos] == UInt8('\n')
                pos += 1
            end
            nlines += 1
            lastbytenewline = true
        else
            lastbytenewline = false
        end
    end
    # TODO: why does this line from Parsers.jl introduce an off-by-one error here?
    # nlines += parsedanylines && !lastbytenewline
    return nlines
    # TODO: make it possible to guess nrows from just parsing a few
    # `len` here is going to give a bad estimate because different records have
    # different number of rows, and this would anyway be an estimate for the whole file
    # whereas we want to estimate the number of rows _in this record_.
    # @show guess = (len - pos) / (nbytes / nlines)
    # return isfinite(guess) ? ceil(Int, guess) : 0
end

# Taken from `Parsers.checkcmtemptylines`
# TODO: move to Parsers.jl?
function next_line(bytes, pos, len)
    b = peekbyte(bytes, pos)
    while b !== UInt8('\n') && b !== UInt8('\r')
        pos += 1
        Parsers.incr!(bytes)  # TODO: not needed if not in Parsers.jl as got Vec{UInt8}?
        eof(bytes, pos, len) && break
        b = peekbyte(bytes, pos)
    end
    # Move forward to be at the `\r` or `\n` byte.
    pos += 1
    Parsers.incr!(bytes)
    # if line ends `\r\n` and we're at `\r`, move forward.
    if b === UInt8('\r') && !eof(bytes, pos, len) && peekbyte(bytes, pos) === UInt8('\n')
        pos += 1
        Parsers.incr!(bytes)
    end
    return pos
end

function parse_row!(rec::Record, row::Int, bytes, pos, len, options, eol_options)
    ncols = nfields(rec)
    local code::Parsers.ReturnCode
    for col in 1:ncols
        opts = col == ncols ? eol_options : options  # see HACK1
        eltyp = eltype(fieldtype(typeof(rec), col))
        res = xparse(eltyp, bytes, pos, len, opts)

        invalid(res.code) && @warn "invalid" codes(res.code) pos col
        if eltyp <: AbstractString
            @inbounds getfield(rec, col)[row] = Parsers.getstring(bytes, res.val, options.e)
        else
            @inbounds getfield(rec, col)[row] = res.val
        end

        pos += res.tlen
        code = res.code

        @debug codes(code) row col pos newline=newline(code)
    end
    return pos, code
end

end  # module

# # Usage:
# network = Network("filename.raw")
# df = DataFrame(network.bus)
