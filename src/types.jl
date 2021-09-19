###
### types
###

# TODO: should the various bits of free text / comments / timestamps be in this struct?
# Data can look like:
# 0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
#    SE SNAPSHOT 09-15-2021 PEAK CASE 18:00
#    FULL COPY OF ETC.
"""
    $TYPEDEF

# Fields
$TYPEDFIELDS
"""
struct CaseID <: Tables.AbstractRow
    """
    IC Change code:
    0 - base case (i.e., clear the working case before adding data to it).
    1 - add data to the working case.
    """
    ic::Int
    "System base MVA."
    sbase::Float64
end

CaseID() = CaseID(0, 100.0)

Tables.columnnames(::CaseID) = fieldnames(CaseID)
Tables.getcolumn(cid::CaseID, i::Int) = getfield(cid, i)
Tables.getcolumn(cid::CaseID, nm::Symbol) = getfield(cid, nm)

# So all tabular data records (buses, loads, ...) can be handled the same.
abstract type Records end

# Create a instance of a `Records` subtype, with all fields (assumed to be Vector)
# containing `nrow` elements (initialised to undefined values, as they'll be overwritten).
(::Type{R})(nrow) where {R <: Records} = R(map(T -> T(undef, nrow), fieldtypes(R))...)

# Store data in column table so conversion to DataFrame efficient.
Tables.istable(::Type{<:Records}) = true
Tables.columnaccess(::Type{<:Records}) = true
Tables.columns(x::Records) = x
Tables.getcolumn(x::Records, i::Int) = getfield(x, i)
Tables.columnnames(x::R) where {R <: Records} = fieldnames(R)
Tables.schema(x::R) where {R <: Records} = Tables.Schema(fieldnames(R), fieldtypes(R))

"""
    $TYPEDEF

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
    name::Vector{InlineString15}
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

"""
    $TYPEDEF

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
    id::Vector{InlineString3}  # TODO: confirm 3 is enough in practice, when whitespace can be present
    "Initial load status of one for in-service and zero for out-of-service."
    status::Vector{Bool}
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


"""
    $TYPEDEF

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
    id::Vector{InlineString3}  # TODO: confirm 3 is enough in practice, when whitespace can be present
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
    stat::Vector{Bool}
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


"""
    Network

Representation of a power network.

The PSS/E data format comprises 16 data categories of network and equipment
elements, each of which requires a particular type of data.

Similarly, a `Network` stores the data from each category in its own dedicated structure.

Currently supported are:
1. [`CaseID`](@ref)
1. [`Buses`](@ref)
1. [`Loads`](@ref)
1. [`Generators`](@ref)

`CaseID` data is a single row (in the Tables.jl-sense).
You can access it like `network.caseid` and interact with it like a `NamedTuple`,
or even convert it to a `NamedTuple` with `NamedTuple(caseid)`.

All other records (buses, loads, etc.) can be accessed also via the fields, for example
`network.buses`, and each is returned as lightweight table structure (again, in the Tables.jl-sense).
That is, all structures implement the Tables.jl interface, so can be passed to any valid
sink, such as a `DataFrame` like `DataFrame(network.buses)`.

For more info on working with tables see [Tables.jl](https://tables.juliadata.org/), and for
common table operations see [TableOperations.jl](https://github.com/JuliaData/TableOperations.jl).

# Fields
$TYPEDFIELDS
"""
struct Network
    "Case identification data."
    caseid::CaseID
    "Bus records."
    buses::Buses
    "Load records."
    loads::Loads
    "Generator records."
    generators::Generators
end
