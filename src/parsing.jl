###
### parsing
###

# We currently support comma-delimited and space-delimited files,
# so just always create these two options, rather than having to
# create `Options` anew each time we parse a file.
const OPTIONS_COMMA = Parsers.Options(
    sentinel=missing,
    quoted=true,
    openquotechar='\'',
    closequotechar='\'',
    stripquoted=true,
    delim=',',
)
const OPTIONS_SPACE = Parsers.Options(
    sentinel=missing,
    quoted=true,
    openquotechar='\'',
    closequotechar='\'',
    stripquoted=true,
    delim=' ',
    ignorerepeated=true,
    wh1=0x00,
)

@inline getoptions(delim::Char) = ifelse(delim === ',', OPTIONS_COMMA, OPTIONS_SPACE)

getbytes(source::Vector{UInt8}) = source, 1, length(source)
getbytes(source::IOBuffer) = source.data, source.ptr, source.size
getbytes(source) = getbytes(read(source))

# `delim` can either be comma `','` or space `' '`
# The documented PSSE format specifies comma... but some files somehow use spaces instead.
# We look for commas in the first line and if found assume comma is the delim,
# but if none found we assume space is the delim.
function detectdelim(bytes, pos, len)
    eof(bytes, pos, len) && return ',' # doesn't matter which we return
    b = peekbyte(bytes, pos)
    while b !== UInt8('\n') && b !== UInt8('\r')
        b == UInt8(',') && return ','
        pos += 1
        eof(bytes, pos, len) && return ','
        b = peekbyte(bytes, pos)
    end
    return ' '
end

"""
    parse_network(source) -> Network

Read a PSS/E-format `.raw` Power Flow Data file and return a [`Network`](@ref) object.

The version of the PSS/E format can be specified with the `v` keyword, like `v=33`,
or else it will be automatically detected when parsing the file.

The delimiter can be specified with the `delim` keyword, like `delim=' '`,
or else it will be automatically detected when parsing the file.
"""
function parse_network(source; v::Union{Integer,Nothing}=nothing, delim::Union{Nothing,Char}=nothing)
    @debug 1 "source = $source, v = $v"
    bytes, pos, len = getbytes(source)
    d = delim === nothing ? detectdelim(bytes, pos, len) : delim
    options = getoptions(d)
    pos = checkdelim!(bytes, pos, len, options)
    caseid, pos = parse_idrow(CaseID, bytes, pos, len, options)
    @debug 1 "Parsed CaseID: rev = $(caseid.rev), pos = $pos"
    # when `v` not given, if `caseid.rev` missing we assume it is because data is v30 format
    version = something(v, coalesce(caseid.rev, 30))
    is_v33 = version == 33
    @debug 1 "Set version = $version"

    # Skip the 2 lines of comments
    pos = next_line(bytes, pos, len)
    pos = next_line(bytes, pos, len)
    @debug 1 "Parsed comments: pos = $pos"
    return if is_v33
        parse_network33(source, version, caseid, bytes, pos, len, options)
    else
        parse_network30(source, version, caseid, bytes, pos, len, options)
    end
end

function parse_network33(source, version, caseid, bytes, pos, len, options)
    buses, pos = parse_records!(Buses33(len÷1000), bytes, pos, len, options)
    nbuses = length(buses)
    loads, pos = parse_records!(Loads(nbuses), bytes, pos, len, options)
    fixed_shunts, pos = parse_records!(FixedShunts(), bytes, pos, len, options)
    gens, pos = parse_records!(Generators(nbuses÷10), bytes, pos, len, options)
    ngens = length(gens)
    branches, pos = parse_records!(Branches33(nbuses), bytes, pos, len, options)
    transformers, pos = parse_records!(Transformers(ngens*2), bytes, pos, len, options)
    interchanges, pos = parse_records!(AreaInterchanges(), bytes, pos, len, options)
    two_terminal_dc, pos = parse_records!(TwoTerminalDCLines33(), bytes, pos, len, options)
    vsc_dc, pos = parse_records!(VSCDCLines(), bytes, pos, len, options)
    impedance_corrections, pos = parse_records!(ImpedanceCorrections(), bytes, pos, len, options)
    multi_terminal_dc, pos = parse_records!(MultiTerminalDCLines{DCLineID33}(), bytes, pos, len, options)
    multi_section_lines, pos = parse_records!(MultiSectionLineGroups33(), bytes, pos, len, options)
    zones, pos = parse_records!(Zones(), bytes, pos, len, options)
    area_transfers, pos = parse_records!(InterAreaTransfers(), bytes, pos, len, options)
    owners, pos = parse_records!(Owners(), bytes, pos, len, options)
    facts, pos = parse_records!(FACTSDevices33(), bytes, pos, len, options)
    switched_shunts, pos = parse_records!(SwitchedShunts33(nbuses÷11), bytes, pos, len, options)
    return Network(
        version,
        caseid,
        buses,
        loads,
        fixed_shunts,
        gens,
        branches,
        transformers,
        interchanges,
        two_terminal_dc,
        vsc_dc,
        switched_shunts,
        impedance_corrections,
        multi_terminal_dc,
        multi_section_lines,
        zones,
        area_transfers,
        owners,
        facts,
    )
end

function parse_network30(source, version, caseid, bytes, pos, len, options)
    buses, pos = parse_records!(Buses30(len÷1000), bytes, pos, len, options)
    nbuses = length(buses)
    loads, pos = parse_records!(Loads(nbuses), bytes, pos, len, options)
    fixed_shunts = nothing
    gens, pos = parse_records!(Generators(nbuses÷10), bytes, pos, len, options)
    ngens = length(gens)
    branches, pos = parse_records!(Branches30(nbuses), bytes, pos, len, options)
    transformers, pos = parse_records!(Transformers(ngens*2), bytes, pos, len, options)
    interchanges, pos = parse_records!(AreaInterchanges(), bytes, pos, len, options)
    two_terminal_dc, pos = parse_records!(TwoTerminalDCLines30(), bytes, pos, len, options)
    vsc_dc, pos = parse_records!(VSCDCLines(), bytes, pos, len, options)
    switched_shunts, pos = parse_records!(SwitchedShunts30(nbuses÷11), bytes, pos, len, options)
    impedance_corrections, pos = parse_records!(ImpedanceCorrections(), bytes, pos, len, options)
    multi_terminal_dc, pos = parse_records!(MultiTerminalDCLines{DCLineID30}(), bytes, pos, len, options)
    multi_section_lines, pos = parse_records!(MultiSectionLineGroups30(), bytes, pos, len, options)
    zones, pos = parse_records!(Zones(), bytes, pos, len, options)
    area_transfers, pos = parse_records!(InterAreaTransfers(), bytes, pos, len, options)
    owners, pos = parse_records!(Owners(), bytes, pos, len, options)
    facts, pos = parse_records!(FACTSDevices30(), bytes, pos, len, options)
    return Network(
        version,
        caseid,
        buses,
        loads,
        fixed_shunts,
        gens,
        branches,
        transformers,
        interchanges,
        two_terminal_dc,
        vsc_dc,
        switched_shunts,
        impedance_corrections,
        multi_terminal_dc,
        multi_section_lines,
        zones,
        area_transfers,
        owners,
        facts,
    )
end

# identify `0` or `'0'`
@inline function _iszero(bytes, pos, len)
    peekbyte(bytes, pos) == UInt8('0') ||
    peekbyte(bytes, pos) == UInt8('\'') && !eof(bytes, pos+1, len) && peekbyte(bytes, pos+1) == UInt8('0')
end

function parse_records!(rec::R, bytes, pos, len, options)::Tuple{R, Int} where {R <: Records}
    # Records terminated by specifying a bus number of zero or `Q`.
    while !(
        eof(bytes, pos, len) ||
        _iszero(bytes, pos, len) ||
        peekbyte(bytes, pos) == UInt8(' ') && !eof(bytes, pos+1, len) && _iszero(bytes, pos+1, len) ||
        peekbyte(bytes, pos) == UInt8('Q')
    )
        _, pos = parse_row!(rec, bytes, pos, len, options)
    end
    pos = next_line(bytes, pos, len)  # Move past a "0 bus" line.
    @debug 1 "Parsed $R: nrows = $(length(rec)), pos = $pos"
    return rec, pos
end

# Taken from `Parsers.checkcmtemptylines`
# TODO: move to Parsers.jl?
function next_line(bytes, pos, len)
    eof(bytes, pos, len) && return pos
    b = peekbyte(bytes, pos)
    while b !== UInt8('\n') && b !== UInt8('\r')
        pos += 1
        eof(bytes, pos, len) && break
        b = peekbyte(bytes, pos)
    end
    # Move forward to be past the `\r` or `\n` byte.
    pos += 1
    # if line ends `\r\n`, then we're at `\n`and need to move forward again.
    if b === UInt8('\r') && !eof(bytes, pos, len) && peekbyte(bytes, pos) === UInt8('\n')
        pos += 1
    end
    return pos
end

function parse_value(::Type{T}, bytes, pos, len, options) where {T}
    res = xparse(T, bytes, pos, len, options)
    code = res.code
    if invalid(code)
        if !(newline(code) && invaliddelimiter(code))  # not due to end-of-line comments
            @warn codes(res.code) pos
        end
    end
    pos += res.tlen
    return res.val, pos, res.code
end

function parse_value!(rec, col::Int, ::Type{T}, bytes, pos, len, options) where {T}
    val, pos, code = parse_value(nonmissingtype(T), bytes, pos, len, options)
    push!(getfield(rec, col)::Vector{T}, val)
    return rec, pos, code
end

@generated function parse_row!(rec::R, bytes, pos, len, options) where {R <: Records}
    block = Expr(:block)
    push!(block.args, quote
        pos = checkdelim!(bytes, pos, len, options)
    end)
    for col in 1:fieldcount(R)
        T = eltype(fieldtype(R, col))
        push!(block.args, quote
            rec, pos, code = parse_value!(rec, $col, $T, bytes, pos, len, options)
        end)
    end
    # @show block
    return block
end

###
### transformers
###

function _setmissing(a::Int, b::Int)
    exprs = Expr[]
    for col in a:b
        push!(exprs, :(push!(getfield(rec, $col), missing)))
    end
    return exprs
end

function _parse_values(::Type{R}, a::Int, b::Int) where {R <: Records}
    exprs = Expr[]
    for col in a:b
        T = eltype(fieldtype(R, col))
        push!(exprs, :((rec, pos, code) = parse_value!(rec, $col, $T, bytes, pos, len, options)))
    end
    return exprs
end

function _parse_maybemissing(R, col)
    T = eltype(fieldtype(R, col))
    return quote
        if newline(code)
            push!(getfield(rec, $col), missing)
        else
            (rec, pos, code) = parse_value!(rec, $col, $T, bytes, pos, len, options)
        end
    end
end

function _parse_maybemissing(R, col1, col2)
    T1 = eltype(fieldtype(R, col1))
    T2 = eltype(fieldtype(R, col2))
    return quote
        if newline(code)
            push!(getfield(rec, $col1), missing)
            push!(getfield(rec, $col2), missing)
        else
            (rec, pos, code) = parse_value!(rec, $col1, $T1, bytes, pos, len, options)
            (rec, pos, code) = parse_value!(rec, $col2, $T2, bytes, pos, len, options)
        end
    end
end

function _parse_maybezero(R, col1, col2)
    T1 = eltype(fieldtype(R, col1))
    T2 = eltype(fieldtype(R, col2))
    return quote
        if newline(code)
            push!(getfield(rec, $col1), zero($T1))
            push!(getfield(rec, $col2), zero($T2))
        else
            (rec, pos, code) = parse_value!(rec, $col1, $T1, bytes, pos, len, options)
            (rec, pos, code) = parse_value!(rec, $col2, $T2, bytes, pos, len, options)
        end
    end
end

function _parse_t2()
    block = Expr(:block)
    append!(block.args, _setmissing(EOL_COLS[1]+1+T2_COLS[2], EOL_COLS[2]))
    append!(block.args, _parse_values(Transformers, EOL_COLS[2]+1, EOL_COLS[3]-1))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[3]))
    append!(block.args, _parse_values(Transformers, EOL_COLS[3]+1, EOL_COLS[3]+T2_COLS[4]))
    append!(block.args, _setmissing(EOL_COLS[3]+1+T2_COLS[4], EOL_COLS[5]))
    return block
end

function _parse_t3()
    block = Expr(:block)
    append!(block.args, _parse_values(Transformers, EOL_COLS[1]+1+T2_COLS[2], EOL_COLS[3]-1))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[3]))
    append!(block.args, _parse_values(Transformers, EOL_COLS[3]+1, EOL_COLS[4]-1))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[4]))
    append!(block.args, _parse_values(Transformers, EOL_COLS[4]+1, EOL_COLS[5]-1))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[5]))
    return block
end

# 2-winding transformers (T2) have a subset of the data for 3-winding transformers (T3).
# T2 has data over 4 lines, T3 has data for 5 lines:
# - Line 1 is the same for both
# - Line 2 has only 3 entries for T2 data, and more for T3
# - Line 3 is the same for both
# - Line 4 has only 2 entries for T2 data, and more for T3 (similar to line 2)
# - Line 5 only exists for T3 data
# We determine data is T2 if there is a newline after 3 entries of line 2, else it's T3.
# This means T2 data with a comment after the last entry on line 2 will fool us.
@generated function parse_row!(rec::R, bytes, pos, len, options) where {R <: Transformers}
    block = Expr(:block)
    push!(block.args, quote
        pos = checkdelim!(bytes, pos, len, options)
    end)
    # parse row 1
    append!(block.args, _parse_values(R, 1, EOL_COLS[1]-7))
    # parse possibly missing o2,f2,o3,f3,o4,f4
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[1]-6, EOL_COLS[1]-5))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[1]-4, EOL_COLS[1]-3))
    push!(block.args, _parse_maybemissing(Transformers, EOL_COLS[1]-2, EOL_COLS[1]-1))
    push!(block.args, _parse_maybemissing(R, EOL_COLS[1]))  # last col only in v33 data (not v30)
    # parse first part of row 2
    append!(block.args, _parse_values(R, EOL_COLS[1]+1, EOL_COLS[1]+T2_COLS[2]))
    # now we can detect if it is two-winding or three-winding data
    push!(block.args, :(newline(code) ? $(_parse_t2()) : $(_parse_t3())))
    push!(block.args, :(return rec, pos))
    # @show block
    return block
end

###
### SwitchedShunts, ImpedanceCorrections
###

const N_SPECIAL = IdDict(
    # SwitchedShunts can have anywhere between 1 - 8 `N` and `B` values in the data itself,
    # if n2, b2, ..., n8, b8 are not present, we set them to zero.
    # i.e. the last 14 = 7(n) + 7(b) columns reqire special handling.
    SwitchedShunts30 => 14,
    SwitchedShunts33 => 14,
    # ImpedanceCorrections can have anywhere between 2 - 11 `T` and `F` values in the data itself,
    # if t3, f3, ..., t11, f11 are not present, we set them to zero.
    # i.e. the last 18 = 9(t) + 9(f) columns reqire special handling.
    ImpedanceCorrections => 18,
    # MultiSectionLineGroups can have between 1 - 9 `DUM_i` columns
    MultiSectionLineGroups30 => 8,
    MultiSectionLineGroups33 => 8,
    # Loads have 2 extra columns in v33 compared to v30
    Loads => 2,
    # Generators have 1 - 4 `Oi`, `Fi` values, plus 2 extra columns in v33 compared to v30
    Generators => 8, # 3*2 + 2
    # Branches have 1 - 4 `Oi`, `Fi` values
    Branches30 => 6, # 3*2
    Branches33 => 6, # 3*2
)

@generated function parse_row!(rec::R, bytes, pos, len, options) where {R <: Union{SwitchedShunts, ImpedanceCorrections, Branches}}
    block = Expr(:block)
    push!(block.args, quote
        pos = checkdelim!(bytes, pos, len, options)
    end)
    N = fieldcount(R) - N_SPECIAL[R]
    append!(block.args, _parse_values(R, 1, N))
    coln = N + 1
    colb = N + 2
    for _ in 1:(N_SPECIAL[R] ÷ 2)
        push!(block.args, _parse_maybezero(R, coln, colb))
        coln += 2
        colb += 2
    end
    push!(block.args, :(return rec, pos))
    # @show block
    return block
end

###
### Loads, Generators, MultiSectionLineGroups
###

@generated function parse_row!(
    rec::R, bytes, pos, len, options
) where {R <: Union{Loads,Generators,MultiSectionLineGroups}}
    block = Expr(:block)
    push!(block.args, quote
        pos = checkdelim!(bytes, pos, len, options)
    end)
    N = fieldcount(R) - N_SPECIAL[R]
    append!(block.args, _parse_values(R, 1, N))
    for col in (N + 1):fieldcount(R)
        push!(block.args, _parse_maybemissing(R, col))
    end
    push!(block.args, :(return rec, pos))
    # @show block
    return block
end

###
### MultiTerminalDCLines
###

function parse_row!(rec::R, bytes, pos, len, options) where {I, R <: MultiTerminalDCLines{I}}
    pos = checkdelim!(bytes, pos, len, options)
    line_id, pos = parse_idrow(I, bytes, pos, len, options)

    nconv = line_id.nconv
    converters = ACConverters(nconv)
    for _ in 1:nconv
        converters, pos = parse_row!(converters, bytes, pos, len, options)
    end

    ndcbs = line_id.ndcbs
    dc_buses = DCBuses(ndcbs)
    for _ in 1:ndcbs
        dc_buses, pos = parse_row!(dc_buses, bytes, pos, len, options)
    end

    ndcln = line_id.ndcln
    dc_links = DCLinks(ndcln)
    for _ in 1:ndcln
        dc_links, pos = parse_row!(dc_links, bytes, pos, len, options)
    end
    line = MultiTerminalDCLine(line_id, converters, dc_buses, dc_links)
    push!(rec.lines, line)
    return rec, pos
end

###
### IDRow
###

function parse_value!(args, ::Type{T}, bytes, pos, len, options) where {T}
    res = xparse(T, bytes, pos, len, options)
    code = res.code
    val = ifelse(valueok(code), res.val, missing)
    push!(args, val)
    pos += res.tlen
    return pos, code
end

@generated function parse_idrow(::Type{R}, bytes, pos, len, options) where {R <: IDRow}
    block = Expr(:block)
    nfields = fieldcount(R)
    T = nonmissingtype(fieldtype(R, 1))
    push!(block.args, quote
        args = Any[]
        (pos, code) = parse_value!(args, $T, bytes, pos, len, options)
    end)
    for i in 2:nfields
        T = nonmissingtype(fieldtype(R, i))
        push!(block.args, quote
            if invalid(code) || newline(code)
                push!(args, missing)
            else
                (pos, code) = parse_value!(args, $T, bytes, pos, len, options)
            end
        end)
    end
    push!(block.args, quote
        if !newline(code)
            pos = next_line(bytes, pos, len)
        end
        return R(args...), pos
    end)
    # @show block
    return block
end
