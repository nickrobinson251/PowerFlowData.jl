# This is a simple `@debug` macro that we can use in the code
# without it slowing the code down, unlike `Base.@debug`.

const DEBUG_LEVEL = Ref(0)

function setdebug!(level::Int)
    DEBUG_LEVEL[] = level
    return nothing
end

"""
    withdebug(level::Int) do
        func()
    end
"""
function withdebug(f, level)
    lvl = DEBUG_LEVEL[]
    try
        setdebug!(level)
        f()
    finally
        setdebug!(lvl)
    end
end

"""
    @debug 1 "msg"
"""
macro debug(level, msg)
    esc(quote
        if DEBUG_LEVEL[] >= $level
            println(string("DEBUG: ", $(QuoteNode(__source__.file)), ":", $(QuoteNode(__source__.line)), " ", $msg))
        end
    end)
end
