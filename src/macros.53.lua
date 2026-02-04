local ret = dofile("src/macros.common.lua")

function ret.block.lua(block, wat)
    if wat == "5.3" then
        return block
    else
        return ""
    end
end

function ret.arg.btest(a,b)
    return ("(((%s) & (%s)) ~= 0)"):format(a,b)
end

function ret.arg.band(a,b)
    return ("((%s) & (%s))"):format(a,b)
end

function ret.arg.bor(...)
    local t = {...}
    for n=1,#t do t[n] = "("..t[n]..")" end
    return "(" .. table.concat(t,"|") .. ")"
end

function ret.arg.bxor(a,b)
    return ("((%s) ~ (%s))"):format(a,b)
end

function ret.arg.bnot(a)
    if a:match("^[0-9]+$") then
        return tostring(math.tointeger(a)~0xFFFFFFFF)
    else
        return ("((%s) ~ 0xFFFFFFFF)"):format(a)
    end
end

function ret.arg.lshift(a,b)
    return ("(((%s) << (%s)) & 0xFFFFFFFF)"):format(a,b)
end

function ret.arg.rshift(a,b)
    return ("((%s) >> (%s))"):format(a,b)
end

function ret.arg.arshift(a,b)
    return ("((((%s) >> (%s)) | ($btest(%s,0x80000000) and (0xFFFFFFFF << (32-(%s))) or 0)) & 0xFFFFFFFF)"):format(a,b,a,b)
end

return ret
