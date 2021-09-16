module PowerFlowData

using Parsers
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

function parse_network(source)
    options = Parsers.Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        # comment="0", # HACK2: alternative to using `peekbyte(...); consume_line(...)`
        delim=',',
    )
    bytes, pos, len = getbytes(source)

    # TODO: figure out how to parse Case ID data...
    caseid = CaseID() # placeholder

    # TODO: figure out how to estimate number of rows in the card
    # nrows = estimate_nrows(bytes, pos, len, options)
    nrows = 3
    bus, pos = parse_record!(Bus(nrows), bytes, pos, len, options)

    nrows = 2
    load, pos = parse_record!(Load(nrows), bytes, pos, len, options)
    # load = Load(0)

    return Network(caseid, bus, load)
end

function parse_record!(rec::Record, bytes, pos, len, options)
    # HACK1: avoid end-of-line comments, e.g. `/* comments */`, by setting "/" as delimiter
    # TODO: change Parsers.jl to handle rows having end-of-line comments
    eol_options = Parsers.Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        # comment="0", # HACK2: alternative to using `peekbyte(...); consume_line(...)`
        delim='/',
    )
    nrows = length(getfield(rec, 1))
    for row in 1:nrows
        pos, code = parse_row!(rec, row, bytes, pos, len, options, eol_options)

        # HACK1 continued: Because we're introducing a delimiter to workaround EOL comments,
        # rows with comments won't have hit the newline character yet, and
        # we need to run `xparse` again to get to the newline
        if !Parsers.newline(code)
            pos += consume_line(bytes, pos, len, options)
        end
    end

    # Data input is terminated by specifying a bus number of zero.
    @assert Parsers.peekbyte(bytes, pos) === UInt8('0')
    pos += consume_line(bytes, pos, len, options)
    return rec, pos
end

function consume_line(bytes, pos, len, options)
    res = Parsers.xparse(String, bytes, pos, len, options)

    @debug Parsers.codes(res.code) newline=Parsers.newline(res.code) string=Parsers.getstring(bytes, res.val, options.e)
    return res.tlen
end

function parse_row!(rec::Record, row::Int, bytes, pos, len, options, eol_options)
    ncols = nfields(rec)
    local code::Parsers.ReturnCode
    for col in 1:ncols
        opts = col == ncols ? eol_options : options  # see HACK1
        eltyp = eltype(fieldtype(typeof(rec), col))
        res = Parsers.xparse(eltyp, bytes, pos, len, opts)

        Parsers.invalid(res.code) && @warn "invalid" Parsers.codes(res.code) pos col
        if eltyp <: AbstractString
            @inbounds getfield(rec, col)[row] = Parsers.getstring(bytes, res.val, options.e)
        else
            @inbounds getfield(rec, col)[row] = res.val
        end

        pos += res.tlen
        code = res.code

        @debug Parsers.codes(code) row col pos newline=Parsers.newline(code)
    end
    return pos, code
end

end  # module

# # Usage:
# network = Network("filename.raw")
# df = DataFrame(network.bus)
