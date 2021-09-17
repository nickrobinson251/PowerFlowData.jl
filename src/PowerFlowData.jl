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
export CaseID, Buses, Loads, Generators


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
Tables.istable(::Type{<:Records}) = true
Tables.columnaccess(::Type{<:Records}) = true
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
    "Bus number, or extended bus name enclosed in single quotes."
    i::Vector{Int}
    """
    One- or two-character uppercase non blank alphanumeric machine identifier used to distinguish among multiple machines at bus "I".
    It is recommended that, at buses for which a single machine is present, the machine be designated as having the machine identifier ’1’.
    ID = ’1’ by default.
    """
    id::Vector{String}
    "Generator active power output; entered in MW. PG = 0.0 by default."
    pg::Vector{Float64}
    """
    Generator reactive power output; entered in Mvar.
    QG needs to be entered only if the case, as read in, is to be treated as a solved case.
    QG = 0.0 by default.
    """
    qg::Vector{Float64}
    """
    Maximum generator reactive power output; entered in Mvar.
    For fixed output gen- erators (i.e., nonregulating), QT must be equal to the fixed Mvar output.
    QT = 9999.0 by default.
    """
    qt::Vector{Float64}
    """
    Minimum generator reactive power output; entered in Mvar.
    For fixed output generators, QB must be equal to the fixed Mvar output.
    QB = -9999.0 by default.
    """
    qb::Vector{Float64}
    "Regulated voltage setpoint; entered in pu. VS = 1.0 by default."
    vs::Vector{Float64}
    """
    Bus number, or extended bus name enclosed in single quotes,
    of a remote type 1 or 2 bus whose voltage is to be regulated by this plant to the value specified by VS.
    If bus IREG is other than a type 1 or 2 bus, bus "I" regulates its own voltage to the value specified by VS.
    IREG is entered as zero if the plant is to regulate its own voltage and must be zero for a type three (swing) bus.
    IREG = 0 by default.
    """
    ireg::Vector{Int}
    """
    Total MVA base of the units represented by this machine; entered in MVA.
    This quantity is not needed in normal power flow and equivalent construction work,
    but is required for switching studies, fault analysis, and dynamic simulation.
    MBASE = system base MVA by default.
    """
    mbase::Vector{Float64}
    """
    Complex machine impedance, ZSORCE; entered in pu on MBASE base.
    This data is not needed in normal power flow and equivalent construction work,
    but is required for switching studies, fault analysis, and dynamic simulation.
    For dynamic simulation, this impedance must be set equal to the unsaturated subtransient impedance for those generators to be modeled by subtransient level machine models,
    and to unsaturated transient impedance for those to be modeled by classical or transient level models.
    For short-circuit studies, the saturated subtransient or transient impedance should be used.
    ZR = 0.0 by default.
    """
    zr::Vector{Float64}
    "See `zr`. ZX = 1.0 by default."
    zx::Vector{Float64}
    """
    Step-up transformer impedance, XTRAN; entered in pu on MBASE base.
    XTRAN should be entered as zero if the step-up transformer is explicitly modeled as a network branch and bus "I" is the terminal bus.
    RT+jXT = 0.0 by default.
    """
    rt::Vector{Float64}
    "See `rt`. RT+jXT = 0.0 by default."
    xt::Vector{Float64}
    """
    Step-up transformer off-nominal turns ratio; entered in pu.
    GTAP is used only if XTRAN is nonzero.
    GTAP = 1.0 by default.
    """
    gtap::Vector{Float64}
    """
    Initial machine status of one for in-service and zero for out-of-service.
    STAT = 1 by default.
    """
    stat::Vector{Int}
    """
    Percent of the total Mvar required to hold the voltage at the bus controlled by this bus "I" that are to be contributed by the generation at bus "I";
    RMPCT must be positive.
    RMPCT is needed if IREG specifies a valid remote bus and there is more than one local or remote voltage controlling device
    (plant, switched shunt, FACTS device shunt element, or VSC dc line converter) controlling the voltage at bus IREG to a setpoint.
    RMPCT is needed also if bus "I" itself is being controlled locally or remotely by one or more other setpoint mode voltage controlling devices.
    RMPCT = 100.0 by default.
    """
    rmpct::Vector{Float64}
    "Maximum generator active power output; entered in MW. PT = 9999.0 by default."
    pt::Vector{Float64}
    "Minimum generator active power output; entered in MW. PB = -9999.0 by default."
    pb::Vector{Float64}
    """
    Owner number; (1 through the maximum number of owners at the current size level).
    Each machine may have up to four owners.
    By default, O1 is the owner to which bus "I" is assigned and O2, O3, and O4 are zero.
    """
    oi::Vector{Int}
    """
    Fraction of total ownership assigned to owner Oi; each Fi must be positive.
    The Fi values are normalized such that they sum to 1.0 before they are placed in the working case.
    By default, each Fi is 1.0.
    """
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
    nrows == 0 && return rec, pos
    for row in 1:nrows
        pos, code = parse_row!(rec, row, bytes, pos, len, options)

        # Because we're working around end-of-line comments,
        # rows with comments won't have hit the newline character yet
        if !newline(code)
            pos = next_line(bytes, pos, len)
        end
    end

    # Data input is terminated by specifying a bus number of zero.
    # @assert peekbyte(bytes, pos) == UInt8('0')
    if !(eof(bytes, pos, len) || peekbyte(bytes, pos) == UInt8('0'))
        @warn "Not at end of $(typeof(rec)) records"
    end
    pos = next_line(bytes, pos, len)
    return rec, pos
end

function count_nrow(buf, pos, len, options)
    nlines = 0
    if eof(buf, pos, len) || peekbyte(buf, pos) == UInt8('0')
        return nlines
    end
    while true
        res = xparse(String, buf, pos, len, options)
        pos += res.tlen
        if newline(res.code)
            nlines += 1
            if eof(buf, pos, len) || peekbyte(buf, pos) == UInt8('0')
                break
            end
        end
    end
    return nlines
end

# Taken from `Parsers.checkcmtemptylines`
# TODO: move to Parsers.jl?
function next_line(bytes, pos, len)
    eof(bytes, pos, len) && return pos
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
    opts = !eol ? options : Options(
        sentinel=missing,
        openquotechar='\'',
        closequotechar='\'',
        delim='/',  # change delimiter as way to handle end-of-line comments
    )
    res = xparse(T, bytes, pos, len, opts)

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
        Parsers.getstring(bytes, res.val, opts.e)
    else
        res.val
    end
    return val, pos, code
end

end  # module

# # Usage:
# network = Network("filename.raw")
# df = DataFrame(network.buses)
