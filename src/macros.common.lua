local ret = {
    block={},
    arg={}
}

local function splitup(wat, t, coverage)
    if #t == 1 then
        if coverage[t[1].number-1] then
            return t[1].code
        else
            return ("if %s == %i then\n  %s\nend"):format(wat, t[1].number, t[1].code)
        end
    end
    local split_point = #t//2
    local pret = {}
    local postt = {}
    for n=1,split_point do
        pret[n] = t[n]
    end
    for n=split_point+1,#t do
        postt[#postt+1] = t[n]
    end
    return ("if %s <= %i then\n  %s\nelse\n  %s\nend"):format(wat, t[split_point].number, splitup(wat, pret, coverage):gsub("\n","\n  "), splitup(wat, postt, coverage):gsub("\n","\n  "))
end

function ret.block.select(block, wat)
    local coverage = {}
    local t = {}
    block = block:gsub("([0-9]+) => (%b{})", function(number, code)
        number = math.tointeger(number)
        assert(not coverage[number], "duplicate %select number")
        t[#t+1] = {number=number,code=code:sub(2,-2)}
        coverage[number] = true
        return ""
    end)
    assert(not block:gsub("%-%-[^\n]*\n", "\n"):match("[^ \t\n]"), "extraneous code in select")
    table.sort(t, function(a,b) return a.number < b.number end)
    return "--begin machine generated code (sorry)\n"..splitup(wat, t, coverage).."\n--end machine generated code\n"
end

function ret.block.trace(block)
    return ""
end

function ret.arg.assemble(...)
    local ret = {}
    for _,el in ipairs{...} do
        local expr,shift = el:match("^(.*) *%-> *(.*)$")
        if expr then
            ret[#ret+1] = ("$lshift(%s,%s)"):format(expr,shift)
        else
            ret[#ret+1] = el
        end
    end
    return "$bor("..table.concat(ret,",")..")"
end

function ret.arg.disassemble(what, ...)
    local ret = {}
    for _,el in ipairs{...} do
        local extend,top,bot,final = el:match("^(~?)([0-9]+)%.%.([0-9]+)%->([0-9]+)$")
        assert(extend, ("invalid arg to disassemble: %q"):format(el))
        local top = math.tointeger(top)
        local bot = math.tointeger(bot)
        local final = math.tointeger(final)
        if extend == "~" then
            ret[#ret+1] = ("$btest(%s,%i) and 0x%08X or 0"):format(what, 1<<top, (0xFFFFFFFF<<final+(top-bot))&0xFFFFFFFF)
        end
        if final == 0 then
            ret[#ret+1] = ("$extract(%s,%i,%i)"):format(what, bot, top-bot+1)
        else
            ret[#ret+1] = ("$lshift($extract(%s,%i,%i),%i)"):format(what, bot, top-bot+1, final)
        end
    end
    return "$bor("..table.concat(ret,",")..")"
end

function ret.arg.extract(value, shift, bits)
    return ("$band($rshift(%s,%s),%i)"):format(value, shift, (1<<math.tointeger(bits))-1)
end

function ret.arg.funct3(instruction)
    return ("$extract(%s,12,3)"):format(instruction)
end

function ret.arg.funct5(instruction)
    return ("$extract(%s,27,5)"):format(instruction)
end

function ret.arg.funct7(instruction)
    return ("$extract(%s,25,7)"):format(instruction)
end

function ret.arg.rd(instruction)
    return ("$extract(%s,7,5)"):format(instruction)
end

function ret.arg.rs1(instruction)
    return ("$extract(%s,15,5)"):format(instruction)
end

function ret.arg.rs2(instruction)
    return ("$extract(%s,20,5)"):format(instruction)
end

function ret.arg.immI(instruction)
    return ("$arshift(%s,20)"):format(instruction)
end

function ret.arg.immS(instruction)
    return ("$bor($band($arshift(%s,20), 0xFFFFFFE0),$extract(%s,7,5))"):format(instruction, instruction)
end

function ret.arg.immB(instruction)
    -- oh jeez
    return ("$bor($band($arshift(%s,19),0xFFFFF000),$lshift($extract(%s,7,1),11),$lshift($extract(%s,25,6),5),$lshift($extract(%s,8,4),1))"):format(instruction,instruction, instruction, instruction)
end

function ret.arg.immJ(instruction)
    -- oh boy here we go
    return ("$bor($band($arshift(%s,11),0xFFF00000),$band(%s,0xFF000),$lshift($extract(%s,20,1),11),$lshift($extract(%s,21,10),1))"):format(instruction, instruction, instruction, instruction)
end

function ret.arg.sex8(wat)
    return ("$bor(%s,$btest(%s,0x80) and 0xFFFFFF00 or 0)"):format(wat, wat)
end

function ret.arg.sex16(wat)
    return ("$bor(%s,$btest(%s,0x8000) and 0xFFFF0000 or 0)"):format(wat, wat)
end

function ret.arg.sex32(wat)
    return ("((%s) - ($btest(%s,0x80000000) and 0x100000000 or 0))"):format(wat, wat)
end

return ret
