--- Plugin loader extension.
-- This module is intended to be loaded directly by the glirc
-- Lua extension.
--
-- extensions:
-- * path: "glirc-lua.bundle"
--   args: [ "extension.lua", "Plugin1", "Plugin1" ]
--
-- @module extension

-- Penlight
local app       = require 'pl.app'
local strict    = require 'pl.strict'
local stringx   = require 'pl.stringx'
local tablex    = require 'pl.tablex'

stringx.import()
app.require_here()

local plugins = {}
for _, name in ipairs(arg) do
    local plugin = require(name)
    table.insert(plugins, plugin)
end

-- ----------------------------------------------------------------------
-- Beginning of extension module definition
-- ----------------------------------------------------------------------

-- glirc callback module
local extension = strict.module('extension')

--- Handle an incoming IRC message.
-- @tparam table self Extension table
-- @tparam table msg IRC message table
-- @treturn boolean Should drop messages
function extension:process_message(msg) -- luacheck: ignore self

    if msg.tags.batch then
        return
    end

    for _, plugin in ipairs(plugins) do
        local k = plugin.messages[msg.command]
        if k then
            local drop = k(msg.network, msg.prefix, table.unpack(msg.params))
            if drop then
                return drop
            end
        end
    end
end

--- Handle a local client command.
-- These are passed in via `/extension Lua ...`
-- @tparam table self Extension table
-- @tparam table c Command table
function extension:process_command(c) -- luacheck: ignore self
    local action, args = string.match(c.command, '(%S+)%s?(.*)')

    if not action then

        local keys = {}
        for _, plugin in ipairs(plugins) do
            keys = tablex.merge(keys, tablex.keys(plugin.commands), true)
        end

        table.sort(keys)
        glirc.print("Administrator Commands available: " .. table.concat(keys, ', '))
        return
    end

    -- try a plugin command
    for _, plugin in ipairs(plugins) do
        local k = plugin.commands[action]
        if k then
            k(args)
            return
        end
    end

    -- try one of the remote administrator actions
    glirc.error(string.format('Unknown command: %s', action))
end

--- Extension destructor.
-- Serialize the banned words list when the module is unloaded.
-- @tparam table self Extension table
function extension:stop() -- luacheck: ignore self
    for _, plugin in ipairs(plugins) do
        plugin:stop()
    end
end

return extension
