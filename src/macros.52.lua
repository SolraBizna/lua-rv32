local ret = dofile("src/macros.common.lua")

function ret.block.lua(block, wat)
    if wat == "5.2" then
        return block
    else
        return ""
    end
end

function ret.arg.btest(a,b)
    return ("bit32.btest(%s,%s)"):format(a,b)
end

function ret.arg.band(a,b)
    return ("bit32.band(%s,%s)"):format(a,b)
end

function ret.arg.bor(...)
    return ("bit32.bor(%s)"):format(table.concat({...},","))
end

function ret.arg.bxor(a,b)
    return ("bit32.bxor(%s,%s)"):format(a,b)
end

function ret.arg.bnot(a)
    if a:match("^[0-9]+$") then
        return tostring(math.tointeger(a)~0xFFFFFFFF)
    else
        return ("bit32.bnot(%s)"):format(a)
    end
end

function ret.arg.lshift(a,b)
    return ("bit32.lshift(%s,%s)"):format(a,b)
end

function ret.arg.rshift(a,b)
    return ("bit32.rshift(%s,%s)"):format(a,b)
end

function ret.arg.arshift(a,b)
    return ("bit32.arshift(%s,%s)"):format(a,b)
end

return ret
