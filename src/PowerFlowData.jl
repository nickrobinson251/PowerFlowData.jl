module PowerFlowData

using Parsers: Parsers, Options, xparse
using Parsers: codes, eof, invalid, invaliddelimiter, newline, peekbyte
using Tables
using DocStringExtensions


export Network
export parse_network

# for easier local development
export getbytes, getstring
export parse_records
export CaseID, Buses, Loads


###
### types
###

# TODO: should the various bits of free text / comments / timestamps be in this struct?
# Data can look like:
# 0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
#    SE SNAPSHOT 09-15-2021 PEAK CASE 18:00
#    FULL COPY OF ETC.
"""
    CaseID

# Fields
$TYPEDFIELDS
"""
struct CaseID
    """
    IC Change code:
    0 - base case (i.e., clear the working case before adding data to it)
    1 - add data to the working case
    """
    ic::Int
    "System base MVA."
    sbase::Float64
end

CaseID() = CaseID(0, 100.0)

# So all tabular data records (buses, loads, ...) can be handled the same.
abstract type Records end

# Store data in column table so conversion to DataFrame efficient.
Tables.istable(::Type{Records}) = true
Tables.columnaccess(::Type{Records}) = true
Tables.columns(x::Records) = x
Tables.getcolumn(x::Records, i::Int) = getfield(x, i)
Tables.columnnames(x::R) where {R <: Records} = fieldnames(R)
Tables.schema(x::R) where {R <: Records} = Tables.Schema(fieldnames(R), fieldtypes(R))

"""
    Buses <: Records

Each network bus to be represented in PSS/E is introduced through a bus data record.
Each bus data record includes not only data for the basic bus properties but also includes
information on an optionally connected shunt admittance to ground. That admittance can
represent a shunt capacitor or a shunt reactor (both with or without a real component) or a
shunt resistor. It must not represent line connected admittance, loads, line charging or
transformer magnetizing impedance, all of which are entered in other data categories.

# Fields
$TYPEDFIELDS
"""
struct Buses <: Records
    "Bus number (1 to 999997)."
    i::Vector{Int}
    """
    Alphanumeric identifier assigned to bus "I".
    The name may be up to twelve characters and must be enclosed in single quotes.
    NAME may contain any combination of blanks, uppercase letters, numbers and special characters, but the first character must not be a minus sign.
    """
    name::Vector{String}
    "Bus base voltage; entered in kV."
    basekv::Vector{Float64}
    """
    Bus type code:
    1 - load bus or other bus without any generator boundary condition.
    2 - generator or plant bus either regulating voltage or with a fixed reactive power (Mvar).
    A generator that reaches its reactive power limit will no longer control voltage but rather hold reactive power at its limit.
    3 - swing bus or slack bus.
    It has no power or reactive limits and regulates voltage at a fixed reference angle.
    4 - disconnected or isolated bus.
    """
    ide::Vector{Int}
    """
    Active component of shunt admittance to ground; entered in MW at one per unit voltage.
    GL should not include any resistive admittance load, which is entered as part of load data.
    """
    gl::Vector{Float64}
    """
    Reactive component of shunt admittance to ground; entered in Mvar at one per unit voltage.
    BL should not include any reactive impedance load, which is entered as part of load data;
    line charging and line connected shunts, which are entered as part of non-transformer branch data;
    or transformer magnetizing admittance, which is entered as part of transformer data.
    BL is positive for a capacitor, and negative for a reactor or an inductive load.
    """
    bl::Vector{Float64}
    "Area number. 1 through the maximum number of areas at the current size level."
    area::Vector{Int}
    "Zone number. 1 through the maximum number of zones at the current size level."
    zone::Vector{Int}
    "Bus voltage magnitude; entered in pu."
    vm::Vector{Float64}
    "Bus voltage phase angle; entered in degrees."
    va::Vector{Float64}
    "Owner number. 1 through the maximum number of owners at the current size level."
    owner::Vector{Int}
end

function Buses(nrows)
    return Buses(
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
    Loads <: Records

Each network bus at which a load is to be represented must be specified in at least one load
data record. If multiple loads are to be represented at a bus, they must be individually
identified in a load data record for the bus with a different load identifier.
Each load at a bus can be a mixture of loads with different characteristics.

# Fields
$TYPEDFIELDS
"""
struct Loads <: Records
    "Buses number, or extended buses name enclosed in single quotes."
    i::Vector{Int}
    """
    One- or two-character uppercase non blank alphanumeric load identifier used to distinguish among multiple loads at bus "I".
    It is recommended that, at buses for which a single load is present, the load be designated as having the load identifier '1'.
    """
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
    """
    Reactive power component of constant admittance load; entered in Mvar at one per unit voltage.
    YQ is a negative quantity for an inductive load and positive for a capacitive load.
    """
    yq::Vector{Float64}
    "Owner to which the load is assigned (1 through the maximum number of owners at the current size level)."
    owner::Vector{Int}
end

function Loads(nrows)
    return Loads(
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

"""
    Generators <: Records

Each network bus to be represented as a generator or plant bus in PSS/E must be specified
in a generator data record. In particular, each bus specified in the bus data input with a
type code of two (2) or three (3) must have a generator data record entered for it.

# Fields
$TYPEDFIELDS
"""
struct Generators <: Records
    i::Vector{Int}
    id::Vector{String}
    pg::Vector{Float64}
    qg::Vector{Float64}
    qt::Vector{Float64}
    qb::Vector{Float64}
    vs::Vector{Float64}
    ireg::Vector{Int}
    mbase::Vector{Float64}
    zr::Vector{Float64}
    zx::Vector{Float64}
    rt::Vector{Float64}
    xt::Vector{Float64}
    gtap::Vector{Float64}
    stat::Vector{Int}
    rmpct::Vector{Float64}
    pt::Vector{Float64}
    pb::Vector{Float64}
    oi::Vector{Int}
    fi::Vector{Float64}
end

function Generators(nrows)
    return Generators(
        Vector{Int}(undef, nrows),
        Vector{String}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Float64}(undef, nrows),
        Vector{Int}(undef, nrows),
        Vector{Float64}(undef, nrows),
    )
end

"""
    Network

Representation of power networks in PSS/E comprises 16 data categories of network and
equipment elements, each of which requires a particular type of data:
1. [`CaseID`](@ref)
1. [`Buses`](@ref)
1. [`Loads`](@ref)
"""
struct Network
    caseid::CaseID
    buses::Buses
    loads::Loads
    generators::Generators
end

###
### parsing
###

getbytes(source::Vector{UInt8}) = source, 1, length(source)
getbytes(source::IOBuffer) = source.data, source.ptr, source.size
getbytes(source) = getbytes(read(source))

function parse_network(source)
    options = Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        delim=',',
    )
    bytes, pos, len = getbytes(source)

    caseid, pos = parse_caseid(bytes, pos, len, options)
    @debug "caseid" pos

    # Skip the 2 lines of comments
    # TODO: confirm it is always only and exactly 2 lines of comments
    pos = next_line(bytes, pos, len)
    pos = next_line(bytes, pos, len)
    @debug "comments" pos

    nrows = count_nrow(bytes, pos, len, options)
    @debug "buses" nrows pos
    buses, pos = parse_records!(Buses(nrows), bytes, pos, len, options)

    nrows = count_nrow(bytes, pos, len, options)
    @debug "loads" nrows pos
    loads, pos = parse_records!(Loads(nrows), bytes, pos, len, options)

    nrows = count_nrow(bytes, pos, len, options)
    @debug "gens" nrows pos
    gens, pos = parse_records!(Generators(nrows), bytes, pos, len, options)

    return Network(caseid, buses, loads, gens)
end

function parse_caseid(bytes, pos, len, options)
    ic, pos, code = parse_value(Int, bytes, pos, len, options)
    @debug codes(code) pos newline=newline(code)

    # Support files that have first row like:
    # 0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
    # and those with first row like:
    # 0,100.0,30 / PSS(tm)E-30 RAW created      Wed, Sep 15 2021 21:04
    # TODO: avoid needing to extract `sbase` value from String?
    sbase = Parsers.tryparse(Float64, bytes, options, pos, len)
    if sbase === nothing
        str, pos, code = parse_value(String, bytes, pos, len, options)
        @debug codes(code) pos newline=newline(code)
        sbase = parse(Float64, first(split(str, '/')))
    end

    # if delimiter after `sbase` value, then won't have reached end of line.
    if !newline(code)
        pos = next_line(bytes, pos, len)
    end
    @debug codes(code) pos newline=newline(code)

    return CaseID(ic, sbase), pos
end

function parse_records!(rec::Records, bytes, pos, len, options)
    nrows = length(getfield(rec, 1))
    for row in 1:nrows
        pos, code = parse_row!(rec, row, bytes, pos, len, options)

        # Because we're working around end-of-line comments,
        # rows with comments won't have hit the newline character yet
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

function count_nrow(buf, pos, len, options)
    nlines = 0
    while true
        res = xparse(String, buf, pos, len, options)
        pos += res.tlen
        if newline(res.code)
            nlines += 1
            eof(buf, pos, len) && break
            peekbyte(buf, pos) == UInt8('0') && break
        end
    end
    return nlines
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
    # Move forward to be past the `\r` or `\n` byte.
    pos += 1
    Parsers.incr!(bytes)
    # if line ends `\r\n`, then we're at `\n`and need to move forward again.
    if b === UInt8('\r') && !eof(bytes, pos, len) && peekbyte(bytes, pos) === UInt8('\n')
        pos += 1
        Parsers.incr!(bytes)
    end
    return pos
end

function parse_row!(rec::Records, row::Int, bytes, pos, len, options)
    ncols = nfields(rec)
    local code::Parsers.ReturnCode
    for col in 1:ncols
        eltyp = eltype(fieldtype(typeof(rec), col))
        eol = col == ncols
        val, pos, code = parse_value(eltyp, bytes, pos, len, options, eol)
        @inbounds getfield(rec, col)[row] = val

        @debug codes(code) row col pos newline=newline(code)
    end
    return pos, code
end

function parse_value(T, bytes, pos, len, options, eol=false)
    res = xparse(T, bytes, pos, len, options)

    if invalid(res.code)
        # for the last column, there might be end-of-line comments;
        # so if last column and hit invaliddelimiter, that is fine.
        # !!! warning: this won't work if last column is a StringType
        if !eol && !invaliddelimiter(res.code)
            @warn codes(res.code) pos col
        end
    end

    pos += res.tlen
    code = res.code

    val = if T <: AbstractString
        Parsers.getstring(bytes, res.val, options.e)
    else
        res.val
    end
    return val, pos, code
end

end  # module

# # Usage:
# network = Network("filename.raw")
# df = DataFrame(network.buses)
