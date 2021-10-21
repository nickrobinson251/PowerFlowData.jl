###
### parsing
###

const OPTIONS = Parsers.Options(
    sentinel=missing,
    quoted=true,
    openquotechar='\'',
    closequotechar='\'',
    delim=',',
)

getbytes(source::Vector{UInt8}) = source, 1, length(source)
getbytes(source::IOBuffer) = source.data, source.ptr, source.size
getbytes(source) = getbytes(read(source))

"""
    parse_network(source) -> Network

Read a PSS/E-format `.raw` Power Flow Data file and return a [`Network`](@ref) object.
"""
function parse_network(source)
    bytes, pos, len = getbytes(source)

    caseid, pos = parse_caseid(bytes, pos, len, OPTIONS)
    @debug 1 "Parsed CaseID: pos = $pos"

    # Skip the 2 lines of comments
    # TODO: confirm it is always only and exactly 2 lines of comments
    pos = next_line(bytes, pos, len)
    pos = next_line(bytes, pos, len)
    @debug 1 "Parsed comments: pos = $pos"

    buses, pos = parse_records!(Buses(len÷1000), bytes, pos, len, OPTIONS)
    nbuses = length(buses)
    @debug 1 "Parsed Buses: nrows = $nbuses, pos = $pos"

    loads, pos = parse_records!(Loads(nbuses), bytes, pos, len, OPTIONS)
    @debug 1 "Parsed Loads: nrows = $(length(loads)), pos = $pos"

    gens, pos = parse_records!(Generators(nbuses÷10), bytes, pos, len, OPTIONS)
    ngens = length(gens)
    @debug 1 "Parsed Generators: nrows = $ngens, pos = $pos"

    branches, pos = parse_records!(Branches(nbuses), bytes, pos, len, OPTIONS)
    @debug 1 "Parsed Branches: nrows = $(length(branches)), pos = $pos"

    transformers, pos = parse_records!(Transformers(ngens*2), bytes, pos, len, OPTIONS)
    @debug 1 "Parsed Transformers: nrows = $(length(transformers)), pos = $pos"

    interchanges, pos = parse_records!(AreaInterchanges(), bytes, pos, len, OPTIONS)
    @debug 1 "Parsed AreaInterchanges: nrows = $(length(interchanges)), pos = $pos"

    return Network(caseid, buses, loads, gens, branches, transformers, interchanges)
end

function parse_caseid(bytes, pos, len, options)
    ic, pos, code = parse_value(Int, bytes, pos, len, options)
    @debug 2 "$(codes(code)) pos = $pos, newline = $(newline(code))"

    # Support files that have first row like:
    # 0,   100.00          / PSS/E-30.3    WED, SEP 15 2021  21:04
    # and those with first row like:
    # 0,100.0,30 / PSS(tm)E-30 RAW created      Wed, Sep 15 2021 21:04
    # TODO: avoid needing to extract `sbase` value from String?
    sbase = Parsers.tryparse(Float64, bytes, options, pos, len)
    if sbase === nothing
        val, pos, code = parse_value(String, bytes, pos, len, options)
        str = Parsers.getstring(bytes, val, options.e)
        @debug 2 "$(codes(code)) pos = $pos, newline = $(newline(code))"
        sbase = parse(Float64, first(split(str, '/')))
    end

    # if delimiter after `sbase` value, then won't have reached end of line.
    if !newline(code)
        pos = next_line(bytes, pos, len)
    end
    return CaseID(ic, sbase), pos
end

function parse_records!(rec::R, bytes, pos, len, options)::Tuple{R, Int} where {R <: Records}
    # Records terminated by specifying a bus number of zero.
    while !(eof(bytes, pos, len) || peekbyte(bytes, pos) == UInt8('0'))
        _, pos = parse_row!(rec, bytes, pos, len, options)
    end
    pos = next_line(bytes, pos, len)  # Move past a "0 bus" line.
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

function _parse_values(a::Int, b::Int)
    exprs = Expr[]
    for col in a:b
        T = eltype(fieldtype(Transformers, col))
        push!(exprs, :((rec, pos, code) = parse_value!(rec, $col, $T, bytes, pos, len, options)))
    end
    return exprs
end

function _parse_t2()
    block = Expr(:block)
    append!(block.args, _setmissing(EOL_COLS[1]+1+T2_COLS[2], EOL_COLS[2]))
    append!(block.args, _parse_values(EOL_COLS[2]+1, EOL_COLS[3]+T2_COLS[4]))
    append!(block.args, _setmissing(EOL_COLS[3]+1+T2_COLS[4], EOL_COLS[5]))
    return block
end

function _parse_t3()
    block = Expr(:block)
    append!(block.args, _parse_values(EOL_COLS[1]+1+T2_COLS[2], EOL_COLS[5]))
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
    append!(block.args, _parse_values(1, EOL_COLS[1]+T2_COLS[2]))
    push!(block.args, :(newline(code) ? $(_parse_t2()) : $(_parse_t3())))
    push!(block.args, :(return rec, pos))
    # @show block
    return block
end
