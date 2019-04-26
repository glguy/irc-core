local class = require 'pl.class'

local Plugin = class()
Plugin._name = 'Plugin'

--- Plugin constructor
-- @function Plugin
function Plugin:_init(name)
    self.messages = {}
    self.commands = {}
    self.name = name
end

function Plugin:stop() -- luacheck: ignore self
end

return Plugin
