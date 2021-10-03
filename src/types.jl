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

CaseID(; ic=0, sbase=100.0) = CaseID(ic, sbase)

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
Tables.columnnames(R::Type{<:Records}) = fieldnames(R)
Tables.schema(x::R) where {R <: Records} = Tables.Schema(fieldnames(R), fieldtypes(R))
Tables.rowcount(x::Records) = length(x)  # faster than going via `columnnames`
Base.length(x::Records) = length(getfield(x, 1)::Vector)
Base.size(x::R) where {R <: Records} = (length(x), fieldcount(R))

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
    $TYPEDEF

In PSS/E, the basic transmission line model is an Equivalent Pi connected between network buses.

Data for shunt equipment units, such as reactors, which are connected to and switched with the line,
are entered in the same data record.

!!! note "Shunts connected to buses"
    To represent shunts connected to buses, that shunt data should be entered in the bus data record.

!!! note "Transformers"
    Branches to be modeled as transformers are not specified in this data category;
    rather, they are specified in the [`Transformers`](@ref) data category.

# Fields
$TYPEDFIELDS
"""
struct Branches <: Records
    "Branch \"from bus\" number, or extended bus name enclosed in single quotes."
    i::Vector{Int}
    """
    Branch "to bus" number, or extended bus name enclosed in single quotes.
    "J" is entered as a negative number, or with a minus sign before the first character of the extended bus name,
    to designate it as the metered end; otherwise, bus "I" is assumed to be the metered end.
    """
    j::Vector{Int}
    """
    One- or two-character uppercase nonblank alphanumeric branch circuit identifier;
    the first character of CKT must not be an ampersand "&".
    It is recommended that single circuit branches be designated as having the circuit identifier '1'.
    CKT = '1' by default.
    """
    ckt::Vector{InlineString3}
    "Branch resistance; entered in pu. A value of R must be entered for each branch."
    r::Vector{Float64}
    "Branch reactance; entered in pu. A nonzero value of X must be entered for each branch."
    x::Vector{Float64}
    "Total branch charging susceptance; entered in pu. B = 0.0 by default."
    b::Vector{Float64}
    """
    First loading rating; entered in MVA.
    If RATEA is set to 0.0, the default value, this branch will not be included in any examination of circuit loading.

    Ratings are entered as:
    ``MVA_{rated} = sqrt(3) × E_{base} × I_{rated} × 10^{-6}`` where:
    - ``E_{base}`` is the base line-to-line voltage in volts of the buses to which the terminal of the branch is connected.
    - ``I_{rated}`` is the branch rated phase current in amperes.
    """
    rate_a::Vector{Float64}
    "Second loading rating; entered in MVA. RATEB = 0.0 by default."
    rate_b::Vector{Float64}
    "Third loading rating; entered in MVA. RATEC = 0.0 by default."
    rate_c::Vector{Float64}
    """
    Complex admittance of the line shunt at the bus "I" end of the branch; entered in pu.
    BI is negative for a line connected reactor and positive for line connected capacitor.
    GI + jBI = 0.0 by default.
    """
    gi::Vector{Float64}
    """
    Complex admittance of the line shunt at the bus "I" end of the branch; entered in pu.
    BI is negative for a line connected reactor and positive for line connected capacitor.
    GI + jBI = 0.0 by default.
    """
    bi::Vector{Float64}
    """
    Complex admittance of the line shunt at the bus "J" end of the branch; entered in pu.
    BJ is negative for a line connected reactor and positive for line connected capacitor.
    GJ + jBJ = 0.0 by default.
    """
    gj::Vector{Float64}
    """
    Complex admittance of the line shunt at the bus "J" end of the branch; entered in pu.
    BJ is negative for a line connected reactor and positive for line connected capacitor.
    GJ + jBJ = 0.0 by default.
    """
    bj::Vector{Float64}
    """
    Initial branch status where 1 designates in-service and 0 designates out-of-service.
    ST = 1 by default.
    """
    st::Vector{Bool}
    "Line length; entered in user-selected units. LEN = 0.0 by default."
    len::Vector{Float64}
    """
    Owner number; 1 through the maximum number of owners at the current size level.
    Each branch may have up to four owners.
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

###
### Transformers
###

"""
    $TYPEDEF

Each AC transformer to be represented in PSS/E is introduced through transformer data records
that specify all the data required to model transformers in power flow calculations, with
one exception.

That exception is a set of ancillary data, comprising transformer impedance correction tables,
which define the manner in which transformer impedance changes as off-nominal turns ratio or
phase shift angle is adjusted. Those data records are described in Transformer Impedance
Correction Tables.

Both two-winding and three-winding transformers are specified in the transformer data records.
The data records for the two-winding transformer are common to the three-winding transformer;
the data block for two-winding transformers is a subset of the data required for three-winding
transformers.

# Fields
$TYPEDFIELDS
"""
struct Transformers <: Records
    # first row
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus to which the
    first winding is connected. The transformer’s magnetizing admittance is modeled on winding one.
    The first winding is the only winding of a two-winding transformer whose tap ratio or
    phase shift angle may be adjusted by the power flow solution activities;
    any winding(s) of a three-winding transformer may be adjusted. No default is allowed.
    """
    i::Vector{Int}
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus to which the
    second winding is connected. This winding may have a fixed, off-nominal tap ratio
    assigned to it. No default is allowed.
    """
    j::Vector{Int}
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus to which the
    third winding is connected. Zero is used to indicate that no third winding is present.
    _Always equal to zero for a two-winding transformer._
    """
    k::Vector{Int}
    """
    One- or two-character uppercase nonblank alphanumeric transformer circuit identifier;
    the first character of `ckt` must not be an ampersand ('&').
    """
    ckt::Vector{InlineString3}
    """
    The winding data I/O code which defines the units in which the turns ratios `windv1` and
    `windv2` are specified (the units of `rma1` and `rmi1` are also governed by `cw` when
    `|cod1|` is 1 or 2):
    * 1 for off-nominal turns ratio in pu of winding bus base voltage;
    * 2 for winding voltage in kV.
    `cw` = 1 by default.
    """
    cw::Vector{Int} # 1 or 2
    """
    The impedance data I/O code that defines the units in which the winding impedances
    `r1_2` and `x1_2` are specified:
    * 1 for resistance and reactance in pu on system base quantities;
    * 2 for resistance and reactance in pu on a specified base MVA and winding bus base voltage;
    * 3 for transformer load loss in watts and impedance magnitude in pu on a specified base MVA
      and winding bus base voltage.
    `cz` = 1 by default.
    """
    cz::Vector{Int} # 1, 2 or 3
    """
    The magnetizing admittance I/O code that defines the units in which `mag1` and `mag2` are specified:
    * 1 for complex admittance in pu on system base quantities;
    * 2 for no load loss in watts and exciting current in pu on winding one to two base MVA
      and nominal voltage.
    `cm` = 1 by default.
    """
    cm::Vector{Int} # 1 or 2
    """
    When `cm` is 1, `mag1` is the magnetizing conductance in pu on system base quantities;
    when `cm` is 2, `mag1` is the no load loss in watts.
    `mag1` = 0.0 by default.
    """
    mag1::Vector{Float64}
    """
    When `cm` is 1, `mag2` is the magnetizing susceptance in pu on system base quantities;
    when `cm` is 2, `mag2` is the exciting current in pu on winding one to two base MVA (`sbase1_2`)
    and nominal voltage (`nomv1`).
    `mag2` = 0.0 by default.
    """
    mag2::Vector{Float64}
    """
    The nonmetered end code of either:
    * 1 (for the winding one bus), or
    * 2 (for the winding two bus).
    `nmetr` = 2 by default.
    """
    nmetr::Vector{Int} # 1 or 2
    """
    An alphanumeric identifier assigned to the transformer. The name may be up to twelve characters.
    `name` may contain any combination of blanks, uppercase letters, numbers and special characters.
    `name` is twelve blanks by default.
    """
    name::Vector{InlineString15}
    """
    The initial transformer status, where 1 designates in-service and 0 designates out-of-service.
    `stat` = 1 by default.
    """
    stat::Vector{Bool}
    """
    An owner number; (1 through the maximum number of owners at the current size level).
    Each transformer may have up to four owners. By default, O1 is the owner to which bus "I" is assigned
    """
    oi::Vector{Int}
    """
    The fraction of total ownership assigned to owner `Oi`; each Fi must be positive.
    The Fi values are normalized such that they sum to 1.0 before they are placed in the working case.
    By default, each `fi` is 1.0.
    """
    fi::Vector{Float64}
    # second row
    """
    The measured impedance of the transformer between the buses to which its first and second
    windings are connected (see also `x1_2`).
    * When `cz` is 1, `r1_2` is the resistance in pu on system base quantities;
    * when `cz` is 2, `r1_2` is the resistance in pu on winding one to two base MVA (`sbase1_2`) and winding one bus base voltage;
    * when `cz` is 3, `r1_2` is the load loss in watts.
    `r1_2` = 0.0  by default.
    """
    r1_2::Vector{Float64}
    """
    The measured impedance of the transformer between the buses to which its first and second
    windings are connected (see also `r1_2`).
    * When `cz` is 1, `x1_2` is the reactance in pu on system base quantities;
    * when `cz` is 2, `x1_2` is the reactance in pu on winding one to two base MVA (`sbase1_2`) and winding one bus base voltage;
    * when `cz` is 3, `x1_2` is the impedance magnitude in pu on winding one to two base MVA (`sbase1_2`) and winding one bus base voltage.
    `x1_2` has no default.
    """
    x1_2::Vector{Float64}
    """
    The winding one to two base MVA of the transformer.
    `sbase1_2` = `sbase` (the system base MVA) by default.
    """
    sbase1_2::Vector{Float64}
    # second row, but 3-winding transformers only
    """
    The measured impedance of a three-winding transformer between the buses to which its
    second and third windings are connected (see also `x2_3`).
    * When `cz` is 1, `r2_3` is the resistance in pu on system base quantities;
    * when `cz` is 2, `r2_3` is the resistance in pu on winding two to three base MVA (`sbase2_3`) and winding two bus base voltage;
    * when `cz` is 3, `r2_3` is the load loss in watts
    `r2_3` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    r2_3::Vector{Union{Float64, Missing}}
    """
    The measured impedance of a three-winding transformer between the buses to which its
    second and third windings are connected (see also `x2_3`).
    * When `cz` is 1, `x2_3` is the reactance in pu on system base quantities;
    * when `cz` is 2, `x2_3` is the reactance in pu on winding one to two base MVA (`sbas2_3`) and winding one bus base voltage;
    * when `cz` is 3, `x2_3` is the impedance magnitude in pu on winding two to three base MVA (`sbase2_3`) and winding two bus base voltage.
    `x2_3` has no default.
    _Ignored for a two-winding transformer._
    """
    x2_3::Vector{Union{Float64, Missing}}
    """
    The winding two to three base MVA of a three-winding transformer; ignored for a two-winding
    transformer.
    `sbase2_3` = `sbase` (the system base MVA) by default.
    _Ignored for a two-winding transformer._
    """
    sbase2_3::Vector{Union{Float64, Missing}}
    """
    The measured impedance of a three-winding transformer between the buses to which its
    third and first windings are connected (see also `x3_1`).
    * When `cz` is 1, `r3_1` is the resistance in pu on system base quantities;
    * when `cz` is 2, `r3_1` is the resistance in pu on winding three to one base MVA (`sbase3_1`) and winding three bus base voltage;
    * when `cz` is 3, `r3_1` is the load loss in watts
    `r3_1` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    r3_1::Vector{Union{Float64, Missing}}
    """
    The measured impedance of a three-winding transformer between the buses to which its
    third and first windings are connected (see also `x3_1`).
    * When `cz` is 1, `x3_1` is the reactance in pu on system base quantities;
    * when `cz` is 2, `x3_1` is the reactance in pu on winding three to one base MVA (`sbas3_1`) and winding three bus base voltage;
    * when `cz` is 3, `x3_1` is the impedance magnitude in pu on winding three to one base MVA (`sbase3_1`) and winding three bus base voltage.
    `x3_1` has no default.
    _Ignored for a two-winding transformer._
    """
    x3_1::Vector{Union{Float64, Missing}}
    """
    The winding three to one base MVA of a three-winding transformer.
    `sbase3_1` = `sbase` (the system base MVA) by default.
    _Ignored for a two-winding transformer._
    """
    sbase3_1::Vector{Union{Float64, Missing}}
    """
    The voltage magnitude at the hidden star point bus; entered in pu.
    `vmstar` = 1.0 by default.
    _Ignored for a two-winding transformer._
    """
    vmstar::Vector{Union{Float64, Missing}}
    """
    The bus voltage phase angle at the hidden star point bus; entered in degrees.
    `anstar` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    anstar::Vector{Union{Float64, Missing}}
    # third row
    """
    When `cw` is 1, `windv1` is the winding one off-nominal turns ratio in pu of winding one bus base voltage,
    and windv1 = 1.0 by default.
    When `cw` is 2, `windv1` is the actual winding one voltage in kV,
    and `windv1` is equal to the base voltage of bus "I" by default.
    """
    windv1::Vector{Float64}
    """
    The nominal (rated) winding one voltage in kV, or zero to indicate that nominal winding
    one voltage is to be taken as the base voltage of bus "I".
    `nomv1` is used only in converting magnetizing data between per unit admittance values
    and physical units when `cm` is 2.
    `nomv1` = 0.0 by default.
    """
    nomv1::Vector{Float64}
    """
    The winding one phase shift angle in degrees.
    `ang1` is positive for a positive phase shift from the winding one side to the winding two side (for a two-winding transformer).
    `ang1` must be greater than -180.0 and less than or equal to +180.0.
    `ang1` = 0.0 by default.
    """
    ang1::Vector{Float64}
    """
    The first winding’s first rating entered in MVA (not current expressed in MVA).
    """
    rata1::Vector{Float64}
    """
    The first winding’s second rating entered in MVA (not current expressed in MVA).
    """
    ratb1::Vector{Float64}
    """
    The first winding’s third rating entered in MVA (not current expressed in MVA).
    """
    ratc1::Vector{Float64}
    """
    The transformer control mode for automatic adjustments of the winding one tap or
    phase shift angle during power flow solutions:
    * 0 for no control (fixed tap and phase shift);
    * ±1 for voltage control;
    * ±2 for reactive power flow control;
    * ±3 for active power flow control;
    * ±4 for control of a DC line quantity.
    If the control mode is entered as a positive number, automatic adjustment of this transformer
    winding is enabled when the corresponding adjustment is activated during power flow solutions;
    a negative control mode suppresses the automatic adjustment of this transformer winding.
    `cod1` = 0 by default.
    """
    cod1::Vector{Int}
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus whose voltage
    is to be controlled by the transformer turns ratio adjustment option of the power flow
    solution activities when `cod1` is 1.

    `cont1` should be non-zero only for voltage controlling transformer windings.
    `cont1` may specify a bus other than "I", "J", or "K"; in this case, the sign of `cont1`
    defines the location of the controlled bus relative to the transformer winding.

    If `cont1` is entered as a positive number, the ratio is adjusted as if bus `cont1` is on the winding two side of the transformer;
    if `cont1` is entered as a negative number, the ratio is adjusted as if bus `|cont1|` is on the winding one side of the transformer.
    `cont1` = 0 by default.
    """
    cont1::Vector{Int}
    """
    `rma1` is the upper limit (and `rmi1` the lower limit) of either:
    * Off-nominal turns ratio in pu of winding one bus base voltage when `|cod1|` is 1 or 2 and `cw` is 1;
      `rma1` = 1.1 and `rmi1` = 0.9 by default.
    * Actual winding one voltage in kV when `|cod1|` is 1 or 2 and `cw` is 2. No default is allowed.
    * Phase shift angl e in degrees when `|cod1|` is 3. No default is allowed.
    * Not used when `|cod1|` is 0 or 4;
    `rma1` = 1.1 and `rmi1` = 0.9 by default.
    """
    rma1::Vector{Float64}
    "The lower limit to `rma1`'s upper limit. See `rma1` for details."
    rmi1::Vector{Float64}
    """
    `vma1` is the upper limit (and `vmi1` the lower limit) of either:
    * Voltage at the controlled bus (bus `|cont1|`) in pu when `|cod1|` is 1. `vma1` = 1.1 and `vmi1` = 0.9 by default.
    * Reactive power flow into the transformer at the winding one bus end in Mvar when `|cod1|` is 2. no default is allowed.
    * Active power flow into the transformer at the winding one bus end in MW when `|cod1|` is 3. no default is allowed.
    * Not used when `|cod1|` is 0 or 4; `vma1` = 1.1 and `vmi1` = 0.9 by default.
    """
    vma1::Vector{Float64}
    "The lower limit to `vma1`'s upper limit. See `vma1` for details."
    vmi1::Vector{Float64}
    """
    The number of tap positions available; used when `cod1` is 1 or 2.
    `ntp1` must be between 2 and 9999.
    `ntp1` = 33 by default.
    """
    ntp1::Vector{Int}
    """
    The number of a transformer impedance correction table if this transformer winding’s
    impedance is to be a function of either off-nominal turns ratio or phase shift angle,
    or 0 if no transformer impedance correction is to be applied to this transformer winding.
    `tab1` = 0 by default.
    """
    tab1::Vector{Int}
    """
    The load drop compensation impedance for voltage controlling transformers entered in pu
    on system base quantities; used when `cod1` is 1.
    `cr1` + j`cx1` = 0.0 by default.
    """
    cr1::Vector{Float64}
    "See `cr1` for details."
    cx1::Vector{Float64}
    # fourth row
    """
    When `cw` is 1, `windv2` is the winding two off-nominal turns ratio in pu of winding two bus base voltage,
    and `windv2` = 1.0 by default.
    When `cw` is 2, `windv2` is the actual winding two voltage in kV,
    and `windv2` is equal to the base voltage of bus `j` by default.
    """
    windv2::Vector{Float64}
    """
    The nominal (rated) winding two voltage in kV, or zero to indicate that nominal winding
    two voltage is to be taken as the base voltage of bus `j`.
    `nomv2` is present for information purposes only; it is not used in any of the calculations
    for modeling the transformer.
    `nomv2` = 0.0 by default.
    """
    nomv2::Vector{Float64}
    # fourth row, but 3-winding transformers only
    """
    The winding two phase shift angle in degrees.
    `ang2` is positive for a positive phase shift from the winding two side to the "T"
    (or star) point bus.
    `ang2` must be greater than -180.0 and less than or equal to +180.0.
    `ang2` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    ang2::Vector{Union{Float64, Missing}}
    """
    The second winding’s first rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    rata2::Vector{Union{Float64, Missing}}
    """
    The second winding’s second rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    ratb2::Vector{Union{Float64, Missing}}
    """
    The second winding’s third rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    ratc2::Vector{Union{Float64, Missing}}
    """
    The transformer control mode for automatic adjustments of the winding two tap or phase
    shift angle during power flow solutions:
    * 0 for no control (fixed tap and phase shift);
    * ±1 for voltage control;
    * ±2 for reactive power flow control;
    * ±3 for active power flow control.
    If the control mode is entered as a positive number, automatic adjustment of this transformer
    winding is enabled when the corresponding adjustment is activated during power flow solutions;
    a negative control mode suppresses the automatic adjustment of this transformer winding.
    `cod2` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    cod2::Vector{Union{Int, Missing}} # one of: -3, -2, -1, 0, 1, 2, 3
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus whose voltage
    is to be controlled by the transformer turns ratio adjustment option of the power flow
    solution activities when `cod2` is 1.
    `cont2` should be nonzero only for voltage controlling transformer windings.

    `cont2` may specify a bus other than `i`, `j`, or `k`; in this case, the sign of
    `cont2` defines the location of the controlled bus relative to the transformer winding.
    If `cont2` is entered as a positive number, or a quoted extended bus name, the ratio is
    adjusted as if bus `cont2` is on the winding one or winding three side of the transformer;
    if `cont2` is entered as a negative number, or a quoted extended bus name with a minus sign
    preceding the first character, the ratio is adjusted as if bus `|cont2|` is on the winding
    two side of the transformer. `cont2` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    cont2::Vector{Union{Int, Missing}}
    """
    `rma2` is the upper limit (and `rmi2` the lower limit) of either:
    * Off-nominal turns ratio in pu of winding two bus base voltage when `|cod2|` is 1 or 2 and `cw` is 1;
        `rma2` = 1.1 and `rmi2` = 0.9 by default.
    * Actual winding one voltage in kV when `|cod2|` is 1 or 2 and `cw` is 2. No default is allowed.
    * Phase shift angle in degrees when `|cod2|` is 3. No default is allowed.
    * Not used when `|cod2|` is 0;
    `rma2` = 1.1 and `rmi2` = 0.9 by default.
    _Ignored for a two-winding transformer._
    """
    rma2::Vector{Union{Float64, Missing}}
    """
    The lower limit to `rma2`'s upper limit. See `rma2` for details.
    _Ignored for a two-winding transformer._
    """
    rmi2::Vector{Union{Float64, Missing}}
    """
    `vma2` is the upper limit (and `vmi2` the lower limit) of either:
    * Voltage at the controlled bus (bus `|cont2|`) in pu when `|cod2|` is 1.
        `vma2` = 1.1 and `vmi2` = 0.9 by default.
    * Reactive power flow into the transformer at the winding two bus end in Mvar when `|cod2|` is 2.
        No default is allowed.
    * Active power flow into the transformer at the winding two bus end in MW when `|cod2|` is 3.
        No default is allowed.
    * Not used when `|cod2|` is 0; `vma2` = 1.1 and `vmi2` = 0.9 by default.
    _Ignored for a two-winding transformer._
    """
    vma2::Vector{Union{Float64, Missing}}
    """
    The lower limit to `vma1`'s upper limit. See `vma1` for details.
    _Ignored for a two-winding transformer._
    """
    vmi2::Vector{Union{Float64, Missing}}
    """
    The number of tap positions available; used when `cod2` is 1 or 2.
    `ntp2` must be between 2 and 9999.
    `ntp2` = 33 by default.
    _Ignored for a two-winding transformer._
    """
    ntp2::Vector{Union{Int, Missing}}
    """
    The number of a transformer impedance correction table if this transformer winding’s
    impedance is to be a function of either off-nominal turns ratio or phase shift angle,
    or 0 if no transformer impedance correction is to be applied to this transformer winding.
    `tab2` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    tab2::Vector{Union{Int, Missing}}
    """
    The load drop compensation impedance for voltage controlling transformers entered in pu
    on system base quantities; used when `cod2` is 1.
    `cr2` + j`cx2` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    cr2::Vector{Union{Float64, Missing}}
    """
    See `cr2` for details.
    _Ignored for a two-winding transformer._
    """
    cx2::Vector{Union{Float64, Missing}}
    # fifth row, only 3-winding transformers
    """
    When `cw` is 1, `windv3` is the winding three off-nominal turns ratio in pu of winding three bus base voltage,
    and windv3 = 1.0 by default.
    When `cw` is 2, `windv3` is the actual winding three voltage in kV,
    and `windv3` is equal to the base voltage of bus `k` by default.
    _Ignored for a two-winding transformer._
    """
    windv3::Vector{Union{Float64, Missing}}
    """
    The nominal (rated) winding three voltage in kV, or zero to indicate that nominal winding
    two voltage is to be taken as the base voltage of bus `j`.
    `nomv3` is present for information purposes only; it is not used in any of the calculations
    for modeling the transformer.
    `nomv3` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    nomv3::Vector{Union{Float64, Missing}}
    """
    The winding three phase shift angle in degrees.
    `ang3` is positive for a positive phase shift from the winding two side to the "T"
    (or star) point bus.
    `ang3` must be greater than -180.0 and less than or equal to +180.0.
    `ang3` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    ang3::Vector{Union{Float64, Missing}}
    """
    The third winding’s first rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    rata3::Vector{Union{Float64, Missing}}
    """
    The third winding’s second rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    ratb3::Vector{Union{Float64, Missing}}
    """
    The third winding’s third rating entered in MVA (not current expressed in MVA).
    _Ignored for a two-winding transformer._
    """
    ratc3::Vector{Union{Float64, Missing}}
    """
    The transformer control mode for automatic adjustments of the winding three tap or phase
    shift angle during power flow solutions:
    * 0 for no control (fixed tap and phase shift);
    * ±1 for voltage control;
    * ±2 for reactive power flow control;
    * ±3 for active power flow control.
    If the control mode is entered as a positive number, automatic adjustment of this transformer
    winding is enabled when the corresponding adjustment is activated during power flow solutions;
    a negative control mode suppresses the automatic adjustment of this transformer winding.
    `cod3` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    cod3::Vector{Union{Int, Missing}}
    """
    The bus number, or extended bus name enclosed in single quotes, of the bus whose voltage
    is to be controlled by the transformer turns ratio adjustment option of the power flow
    solution activities when `cod3` is 1.
    `cont3` should be nonzero only for voltage controlling transformer windings.

    `cont3` may specify a bus other than `i`, `j`, or `k`; in this case, the sign of
    `cont3` defines the location of the controlled bus relative to the transformer winding.
    If `cont3` is entered as a positive number, or a quoted extended bus name, the ratio is
    adjusted as if bus `cont3` is on the winding one or winding two side of the transformer;
    if `cont3` is entered as a negative number, or a quoted extended bus name with a minus sign
    preceding the first character, the ratio is adjusted as if bus `|cont3|` is on the winding
    three side of the transformer. `cont3` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    cont3::Vector{Union{Int, Missing}}
    """
    `rma3` is the upper limit (and `rmi3` the lower limit) of either:
    * Off-nominal turns ratio in pu of winding three bus base voltage when `|cod3|` is 1 or 2 and `cw` is 1;
      `rma3` = 1.1 and `rmi3` = 0.9 by default.
    * Actual winding one voltage in kV when `|cod3|` is 1 or 2 and `cw` is 2. No default is allowed.
    * Phase shift angle in degrees when `|cod3|` is 3. No default is allowed.
    * Not used when `|cod3|` is 0;
    `rma3` = 1.1 and `rmi3` = 0.9 by default.
    _Ignored for a two-winding transformer._
    """
    rma3::Vector{Union{Float64, Missing}}
    """
    The lower limit to `rma3`'s upper limit. See `rma3` for details.
    _Ignored for a two-winding transformer._
    """
    rmi3::Vector{Union{Float64, Missing}}
    """
    `vma3` is the upper limit (and `vmi3` the lower limit) of either:
    * Voltage at the controlled bus (bus `|cont3|`) in pu when `|cod3|` is 1.
      `vma3` = 1.1 and `vmi3` = 0.9 by default.
    * Reactive power flow into the transformer at the winding three bus end in Mvar when `|cod3|` is 2.
      No default is allowed.
    * Active power flow into the transformer at the winding two bus end in MW when `|cod3|` is 3.
      No default is allowed.
    * Not used when `|cod3|` is 0; `vma3` = 1.1 and `vmi3` = 0.9 by default.
    _Ignored for a two-winding transformer._
    """
    vma3::Vector{Union{Float64, Missing}}
    """
    The lower limit to `vma3`'s upper limit. See `vma3` for details.
    _Ignored for a two-winding transformer._
    """
    vmi3::Vector{Union{Float64, Missing}}
    """
    The number of tap positions available; used when `cod3` is 1 or 2.
    `ntp3` must be between 2 and 9999.
    `ntp3` = 33 by default.
    _Ignored for a two-winding transformer._
    """
    ntp3::Vector{Union{Int, Missing}}
    """
    The number of a transformer impedance correction table if this transformer winding’s
    impedance is to be a function of either off-nominal turns ratio or phase shift angle,
    or 0 if no transformer impedance correction is to be applied to this transformer winding.
    `tab3` = 0 by default.
    _Ignored for a two-winding transformer._
    """
    tab3::Vector{Union{Int, Missing}}
    """
    The load drop compensation impedance for voltage controlling transformers entered in pu
    on system base quantities; used when `cod3` is 1.
    `cr3` + j`cx3` = 0.0 by default.
    _Ignored for a two-winding transformer._
    """
    cr3::Vector{Union{Float64, Missing}}
    """
    See `cr3` for details.
    _Ignored for a two-winding transformer._
    """
    cx3::Vector{Union{Float64, Missing}}
end

# Constants for Transformers data.
#
# Transformers data is a bit special, as records have 2 possible schemas
# see https://github.com/nickrobinson251/PowerFlowData.jl/issues/17
#
# Each two-winding transformer ("T2") is 4 lines with (14, 3, 16, 2) columns each, and
# each three-winding transformer ("T3") is 5 lines with (14, 11, 16, 16, 16) columns each.
# `TX_COLS[i]` is the (expected) number of columns on line i of X-winding transformer data.
const T2_COLS = (14,  3, 16,  2)
const T3_COLS = (14, 11, 16, 16, 16)

# The "columns" (fields) which come at the end of a line in the data.
const EOL_COLS = cumsum(T3_COLS)

# The fields of the struct that contain data for two-winding transformers.
const T2_FIELDS = (
    1:T2_COLS[1]...,
    (EOL_COLS[1] + 1):(EOL_COLS[1] + T2_COLS[2])...,
    (EOL_COLS[2] + 1):(EOL_COLS[2] + T2_COLS[3])...,
    (EOL_COLS[3] + 1):(EOL_COLS[3] + T2_COLS[4])...,
)

_is_t2(x::Transformers) = all(ismissing, x.cx3)

# Since 2-winding data is a subset of 3-winding data, check at runtime if we have any
# 3-winding data and if not just return the subset of columns required for 2-winding data.
function Tables.schema(x::R) where {R <: Transformers}
    return if _is_t2(x)
        Tables.Schema(fieldname.(R, T2_FIELDS), fieldtype.(R, T2_FIELDS))
    else
        Tables.Schema(fieldnames(R), fieldtypes(R))
    end
end

# `DataFrame` just calls `columns`, so we need that to return something that respects the
# schema (which for `Transformers` data depends on the values).
Tables.columns(x::Transformers) = Tables.columntable(Tables.schema(x), x)

# Again, respect the schema.
# TODO: Can `schema` or `columnnames` just be defined using the other?
function Tables.columnnames(x::R) where {R <: Transformers}
    if _is_t2(x)
        fieldname.(R, T2_FIELDS)
    else
        fieldnames(R)
    end
end

# Again, respect the schema.
Base.size(x::Transformers) = (length(x), length(Tables.columnnames(x)))

###
### Network
###

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
1. [`Branches`](@ref)
1. [`Transformers`](@ref)

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
    "Non-transformer Branch records."
    branches::Branches
    "Transformer records."
    transformers::Transformers
end

###
### show
###

function Base.show(io::IO, mime::MIME"text/plain", network::T) where {T <: Network}
    show(io, mime, T)
    get(io, :compact, false)::Bool && return nothing
    nfields = fieldcount(T)
    print(io, " with $nfields data categories:\n ")
    show(io, mime, network.caseid)
    io_compact = IOContext(io, :compact => true)
    foreach(2:nfields) do i
        print(io, "\n ")
        show(io_compact, mime, getfield(network, i))
    end
    return nothing
end

Base.show(io::IO, x::CaseID) = print(io, "CaseID", NamedTuple(x))  # parseable repr
Base.show(io::IO, ::MIME"text/plain", x::CaseID) = print(io, "CaseID: ", NamedTuple(x))

Base.summary(io::IO, x::R) where {R <: Records} = print(io, "$R with $(length(x)) records")

function Base.show(io::IO, mime::MIME"text/plain", x::R) where {R <: Records}
    if get(io, :compact, false)::Bool
        Base.summary(io, x)
    else
        printstyled(io, R; bold=true)
        print(io, " with $(length(x)) records,")
        print(io, " $(length(Tables.columnnames(x))) columns:\n")
        pretty_table(
            io, x;
            compact_printing=true,
            crop=:both,
            header=collect(Symbol, Tables.columnnames(x)),
            newline_at_end=false,
            vcrop_mode=:middle,  # show first and last rows
            vlines=Int[],  # no vertical lines
        )
    end
    return nothing
end
