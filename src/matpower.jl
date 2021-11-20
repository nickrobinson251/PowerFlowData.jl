###
### parsing
###

# OPTIONS_M = Parsers.Options(
#     sentinel=missing,
#     quoted=true,
#     openquotechar='\'',
#     closequotechar='\'',
#     delim=' ',
#     ignorerepeated=true,
#     comment="%",
#     wh1='\t',
# )

# function parse_matpower(source)
#     OPTIONS_M = Parsers.Options(
#         sentinel=missing,
#         quoted=true,
#         openquotechar='\'',
#         closequotechar='\'',
#         delim='\t',
#         ignorerepeated=true,
#         comment="%",
#         wh2=' ',
#     )
#     @debug 1 "source = $source"
#     bytes, pos, len = getbytes(source)
#     buses, pos = parse_mat!(BusesM2(), bytes, pos, len, OPTIONS_M)
#     return buses
# end

# function parse_mat!(rec::R, bytes, pos, len, options)::Tuple{R, Int} where {R <: Records}
#     # Records terminated by specifying a bus number of zero or `Q`.
#     while !(
#         eof(bytes, pos, len) ||
#         peekbyte(bytes, pos) == UInt8(']')
#     )
#         _, pos = parse_row!(rec, bytes, pos, len, options)
#     end
#     pos = next_line(bytes, pos, len)  # Move past a "closing bracket" line.
#     @debug 1 "Parsed $R: nrows = $(length(rec)), pos = $pos"
#     return rec, pos
# end

###
### types
###

"""
    $TYPEDFIELDS

# Fields
$TYPEDFIELDS
"""
struct BusesM2 <: Records
    "Bus number (positive integer)"
    i::Vector{BusNum}
    "Bus type (1 = PQ, 2 = PV, 3 = ref, 4 = isolated)"
    type::Vector{Int8}
    "Real power demand (MW)"
    pd::Vector{Float64}
    "Reactive power demand (MVAr)"
    qd::Vector{Float64}
    "Shunt conductance (MW demanded at V = 1.0 p.u.)"
    gs::Vector{Float64}
    "Shunt susceptance (MVAr injected at V = 1.0 p.u.)"
    bs::Vector{Float64}
    "Area number (positive integer)"
    area::Vector{AreaNum}
    "Voltage magnitude (p.u.)"
    vm::Vector{Float64}
    "Voltage angle (degrees)"
    va::Vector{Float64}
    "base voltage (kV)"
    basekv::Vector{Float64}
    "Loss zone (positive integer)"
    zone::Vector{ZoneNum}
    "Maximum voltage magnitude (p.u.)"
    vmax::Vector{Float64}
    "Minimum voltage magnitude (p.u.)"
    vmin::Vector{Float64}
    # optional
    "Lagrange multiplier on real power mismatch (u /MW)"
    lam_p::Vector{Float64}
    "Lagrange multiplier on reactive power mismatch (u /MVAr)"
    lam_q::Vector{Float64}
    "Kuhn-Tucker multiplier on upper voltage limit (u /p.u.)"
    mu_vmax::Vector{Float64}
    "Kuhn-Tucker multiplier on lower voltage limit (u /p.u.)"
    mu_vmin::Vector{Float64}
end

"""
    $TYPEDFIELDS

# Fields
$TYPEDFIELDS
"""
struct GeneratorsM2 <: Records
    "Bus number"
    bus::Vector{BusNum}
    "Real power output (MW)"
    pg::Vector{Float64}
    "Reactive power output (MVAr)"
    qg::Vector{Float64}
    "Maximum reactive power output (MVAr)"
    qmax::Vector{Float64}
    "Minimum reactive power output (MVAr)"
    qmin::Vector{Float64}
    """
    Voltage magnitude setpoint (p.u.)

    Used to determine voltage setpoint for optimal power flow only if `opf.use_vg` option is non-zero
    (0 by default). Otherwise generator voltage range is determined by limits set for corresponding
    bus in bus matrix ([`BusesM2`](@ref)).
    """
    vg::Vector{Float64}
    "Total MVA base of machine, defaults to baseMVA"
    mbase::Vector{Float64}
    """
    Machine status
    * > 0 = machine in-service
    * ≤ 0 = machine out-of-service
    """
    status::Vector{Bool}
    "Maximum real power output (MW)"
    pmax::Vector{Float64}
    "Minimum real power output (MW)"
    pmin::Vector{Float64}
    # Not in M1
    "Lower real power output of PQ capability curve (MW)"
    pc1::Vector{Float64}
    "Upper real power output of PQ capability curve (MW)"
    pc2::Vector{Float64}
    "Minimum reactive power output at PC1 (MVAr)"
    qc1min::Vector{Float64}
    "Maximum reactive power output at PC1 (MVAr)"
    qc1max::Vector{Float64}
    "Minimum reactive power output at PC2 (MVAr)"
    qc2min::Vector{Float64}
    "Maximum reactive power output at PC2 (MVAr)"
    qc2max::Vector{Float64}
    "Ramp rate for load following/AGC (MW/min)"
    ramp_agc::Vector{Float64}
    "Ramp rate for 10 minute reserves (MW)"
    ramp_10::Vector{Float64}
    "Ramp rate for 30 minute reserves (MW)"
    ramp_30::Vector{Float64}
    "Ramp rate for reactive power (2 sec timescale) (MVAr/min)"
    ramp_q::Vector{Float64}
    "Area participation factor"
    apf::Vector{Float64}
    # optional
    "Kuhn-Tucker multiplier on upper Pg limit (u /MW)"
    mu_pmax::Vector{Float64}
    "Kuhn-Tucker multiplier on lower Pg limit (u /MW)"
    mu_pmin::Vector{Float64}
    "Kuhn-Tucker multiplier on upper Qg limit (u /MVAr)"
    mu_qmax::Vector{Float64}
    "Kuhn-Tucker multiplier on lower Qg limit (u /MVAr)"
    mu_qmin::Vector{Float64}
end

"""
    $TYPEDFIELDS

# Fields
$TYPEDFIELDS
"""
struct BranchesM2 <: Records
    "\"From\" bus number"
    from::Vector{BusNum}
    "\"To\" bus number"
    to::Vector{BusNum}
    "Resistance (p.u.)"
    br_r::Vector{Float64}
    "Reactance (p.u.)"
    br_x::Vector{Float64}
    "Total line charging susceptance (p.u.)"
    br_b::Vector{Float64}
    "MVA rating A (long term rating), set to 0 for unlimited"
    rate_a::Vector{Float64}
    "MVA rating B (short term rating), set to 0 for unlimited"
    rate_b::Vector{Float64}
    "MVA rating C (emergency rating), set to 0 for unlimited"
    rate_c::Vector{Float64}
    """
    Transformer oﬀ nominal turns ratio, if non-zero
    (taps at "from" bus, impedance at "to" bus,
    i.e. if ``r = x = b = 0``, ``tap = |V_f| / |V_t|``;
    `tap` = 0 used to indicate transmission line rather than transformer,
    i.e. mathematically equivalent to transformer with `tap` = 1)
    """
    tap::Vector{Float64}
    "Transformer phase shift angle (degrees), positive => delay"
    shift::Vector{Float64}
    """
    Initial branch status
    * 1 = in-service
    * 0 = out-of-service
    """
    status::Vector{Bool}
    # Not in M1
    "Minimum angle diﬀerence, ``\theta_f − \theta_t`` (degrees)"
    angmin::Vector{Float64}
    "Maximum angle diﬀerence, ``\theta_f − \theta_t`` (degrees)"
    angmax::Vector{Float64}
    # optional
    "Real power injected at \"from\" bus end (MW)"
    pf::Vector{Float64}
    "Reactive power injected at “from” bus end (MVAr)"
    qf::Vector{Float64}
    "Real power injected at \"to\" bus end (MW)"
    pt::Vector{Float64}
    "Reactive power injected at \"to\" bus end (MVAr)"
    qt::Vector{Float64}
    "Kuhn-Tucker multiplier on MVA limit at \"from\" bus (u /MVA)"
    mu_sf::Vector{Float64}
    "Kuhn-Tucker multiplier on MVA limit at \"to\" bus (u /MVA)"
    mu_st::Vector{Float64}
    "Kuhn-Tucker multiplier lower angle diﬀerence limit (u /degree)"
    mu_angmin::Vector{Float64}
    "Kuhn-Tucker multiplier upper angle diﬀerence limit (u /degree)"
    mu_angmax::Vector{Float64}
end

"""
    $TYPEDFIELDS

If gen has ``n_g`` rows, then the ﬁrst ``n_g`` rows of [`GeneratorCostsM2`](@ref) contain
the costs for active power produced by the corresponding generators.
If gencost has ``2n_g`` rows, then rows ``n_g + 1`` through ``2n_g`` contain
the reactive power costs in the same format.

# Fields
$TYPEDFIELDS
"""
struct GeneratorCostsM2 <: Records
    """
    Cost model
    * 1 = piecewise linear
    * 2 = polynomial
    """
    model::Vector{Int8}
    "Startup cost in US dollars"
    startup::Vector{Float64}
    "Shutdown cost in US dollars"
    shutdown::Vector{Float64}
    """
    Number ``N = n + 1`` of data points defining an ``n``-segment piecewise linear cost function,
    or of coefficients defining an ``n``-th order polynomial cost function
    """
    ncosts::Vector{Int8}
    """
    Parameters defining total cost function ``f(p)`` begin in this column,
    units of ``f`` and ``p`` are ``$/hr`` and MW (or MVAr), respectively
    * (MODEL = 1) => ``p_1, f_1, p_2, f_2, ..., p_N, f_N``
    where  ``p_1 < p_2 < ... < p_N`` and the cost ``f(p)`` is defined by the coordinates
    ``(p_1, f_1), (p_2, f_2), ..., (p_N, f_N)``
    of the end/break-points of the piecewise linear cost.
    * (MODEL = 2) => ``c_n, ..., c_1, c_0``
      ``N`` coefficients of ``n``-th order polynomial cost function, starting with highest order,
      where cost is ``f(p) = c_n p_n + ... + c_1 p + c0``
    """
    costs::Vector{Vector{Float64}} # N vectors
end

###
### constructors
###

for R in (:BusesM2, :GeneratorsM2)
    @eval $R(sizehint::Integer=0) = $R(map(T -> sizehint!(T(), sizehint), fieldtypes($R))...)::$R
end
