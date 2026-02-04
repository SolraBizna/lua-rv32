local ret = dofile("src/macros.53.lua")

function ret.block.trace(block)
    return block
end

function ret.block.notrace(block)
    return ""
end

return ret

