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
    @debug "caseid" pos

    # Skip the 2 lines of comments
    # TODO: confirm it is always only and exactly 2 lines of comments
    pos = next_line(bytes, pos, len)
    pos = next_line(bytes, pos, len)
    @debug "comments" pos

    nrows = count_nrow(bytes, pos, len, OPTIONS)
    @debug "buses" nrows pos
    buses, pos = parse_records!(Buses(nrows), bytes, pos, len, OPTIONS)

    nrows = count_nrow(bytes, pos, len, OPTIONS)
    @debug "loads" nrows pos
    loads, pos = parse_records!(Loads(nrows), bytes, pos, len, OPTIONS)

    nrows = count_nrow(bytes, pos, len, OPTIONS)
    @debug "gens" nrows pos
    gens, pos = parse_records!(Generators(nrows), bytes, pos, len, OPTIONS)

    nrows = count_nrow(bytes, pos, len, OPTIONS)
    @debug "branches" nrows pos
    branches, pos = parse_records!(Branches(nrows), bytes, pos, len, OPTIONS)

    # 2-winding Transformers data is 4 lines each... so this will be correct when all
    # transformers as 2-winding, and become incorrect once there are multiple 3-winding.
    # TODO: ditch counting of rows and use `push!`
    # https://github.com/nickrobinson251/PowerFlowData.jl/issues/5
    nrows = count_nrow(bytes, pos, len, OPTIONS) ÷ 4
    @debug "2-winding transformers" nrows pos
    transformers, pos = parse_records!(Transformers(nrows), bytes, pos, len, OPTIONS)
    return Network(caseid, buses, loads, gens, branches, transformers)
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

function parse_records!(rec::R, bytes, pos, len, options)::Tuple{R, Int} where {R <: Records}
    nrows = length(getfield(rec, 1))
    nrows == 0 && return rec, pos
    for row in 1:nrows
        pos, code = parse_row!(rec, row, bytes, pos, len, options)
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
        if newline(res.code) || eof(res.code)
            nlines += 1
            if eof(buf, pos, len) || (
                !eof(buf, pos, len) && peekbyte(buf, pos) == UInt8('0') &&
                !eof(buf, pos+1, len) && peekbyte(buf, pos+1) == UInt8(' ')
            )
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

function parse_row!(rec::R, row::Int, bytes, pos, len, options) where {R <: Records}
    ncols = fieldcount(R)
    local code::Parsers.ReturnCode
    for col in 1:ncols
        eltyp = eltype(fieldtype(typeof(rec), col))
        # TODO: come up with a way to avoid type instability/dynamic dispatch
        # in this call to parse_value (this will effect performance a lot!)
        val, pos, code = parse_value(eltyp, bytes, pos, len, options)
        @inbounds getfield(rec, col)[row] = val

        @debug codes(code) row col pos newline=newline(code)
    end
    return pos, code
end


###
### transformers
###

# To hold "three-winding" data we need `sum((14, 11, 16, 16, 16)) == 73` columns, and
# column 14+3=17 and column 14+11+16+2=43 are "special" in that they may or may not be at
# the end of a line.
function parse_row!(rec::Transformers, row::Int, bytes, pos, len, options)
    ncols = fieldcount(Transformers)
    @assert ncols == last(EOL_COLS)

    local code::Parsers.ReturnCode
    col = 1
    is_t2 = false
    while col ≤ ncols
        eltyp = nonmissingtype(eltype(fieldtype(typeof(rec), col)))
        val, pos, code = parse_value(eltyp, bytes, pos, len, options)
        @inbounds getfield(rec, col)[row] = val

        @debug codes(code) row col pos newline=newline(code)

        # TODO: handle 2-winding data with end-of-line comments on row 2.
        if col == (EOL_COLS[1] + T2_COLS[2]) && newline(code)
            is_t2 = true  # it's two-winding data
            while col < EOL_COLS[2]  # the rest of line 2 is missing
                col += 1
                @inbounds getfield(rec, col)[row] = missing
            end
        end

        if is_t2 && col == EOL_COLS[3] + T2_COLS[4]
            # TODO: handle end-of-line comments on row 4 of 2-winding data.
            while col < EOL_COLS[5]  # the rest of line 4 and all of line 5 is missing
                col += 1
                @inbounds getfield(rec, col)[row] = missing
            end
        end

        col += 1
    end
    return pos, code
end

function parse_value(T, bytes, pos, len, options)
    res = xparse(T, bytes, pos, len, options)

    code = res.code
    if invalid(code)
        if !(newline(code) && invaliddelimiter(code))  # not due to end-of-line comments
            @warn codes(res.code) pos
        end
    end

    pos += res.tlen
    code = res.code

    val = if T === String
        Parsers.getstring(bytes, res.val, options.e)
    else
        res.val
    end
    return val, pos, code
end
