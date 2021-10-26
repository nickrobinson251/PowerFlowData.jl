macro test_inferred(ex...)
    inferred_call = Expr(:macrocall, GlobalRef(Test, Symbol("@inferred")), __source__, ex...)
    return :(try
        val = $(esc(inferred_call))
        @test true
        return val
    catch err
        err_txt = sprint(showerror, err)
        m = match(r"return type (\w+\s+) does not match inferred return type (\w+)", err_txt)
        m === nothing && rethrow(err)
        inferred_type = m[1]
        return_type = m[2]
        @test inferred_type == return_type
    end)
end
