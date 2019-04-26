--- Lua evaluation plugin.
-- This module is intended to be loaded as a plugin.
--
-- extensions:
-- * path: "glirc-lua.bundle"
--   args: [ "extension.lua", "Eval" ]
--
-- @module Eval

-- Penlight
local tablex = require 'pl.tablex'

local Plugin = require 'Plugin'
local M = Plugin 'Eval'

function M.commands.eval(src)
    local chunk, err = load(src, '=(eval)', 't')
    if chunk then
        local results = {chunk()}
        if #results > 0 then
            tablex.transform(tostring, results)
            print(table.unpack(results))
        end
    else
        glirc.error(err)
    end
end

return M
