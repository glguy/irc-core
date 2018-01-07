local extension = {}

------------------------------------------------------------------------
-- Message handlers
------------------------------------------------------------------------

local playbacks = {}

-- The script can be reloaded at runtime with networks already connected
for _,network in ipairs(glirc.list_networks()) do
        playbacks[network] = {}
end

------------------------------------------------------------------------
-- Message handlers
------------------------------------------------------------------------

local messages = {}

-- When joining the network, request full playback
messages['001'] = function(network)
                playbacks[network] = {}
                glirc.send_message
                   { network = network
                   , command = "ZNC"
                   , params  = { '*playback' , 'play' ,'*', '0' }
                   }
end

-- Detect ZNC's playback module BATCH and mark channels as seen afterward
function messages.BATCH(network, _, reftag, batchtype, channel)

        local isStart = '+' == reftag:sub(1,1)
        reftag = reftag:sub(2)

        if isStart then
                if batchtype == 'znc.in/playback' then
                        playbacks[network][reftag] = channel
                        glirc.clear_window(network, channel)
                end
        else
                local channel = playbacks[network][reftag]
                if channel then
                        playbacks[network][reftag] = nil
                        glirc.mark_seen(network, channel)
                end
        end
end

messages['422'] = function ()
        return true -- ignore nomotd message
end

-- Dispatch message to the appropriate handlers
function extension:process_message(msg)
        local k = messages[msg.command]
        if k then
                return k(msg.network, msg.prefix, table.unpack(msg.params))
        end
end

------------------------------------------------------------------------
-- Command handlers
------------------------------------------------------------------------

local commands = {}

function commands.compare(x,y)
    glirc.print('Comparing : ' .. tostring(glirc.identifier_cmp(x,y)))
end

function commands.list_networks()
    glirc.print(table.concat(glirc.list_networks(), ' '))
end

function commands.list_channels(network)
    glirc.print(table.concat(glirc.list_channels(network), ' '))
end

function commands.list_channel_users(network, channel)
    glirc.print(table.concat(glirc.list_channel_users(network,channel), ' '))
end

function commands.my_nick(network)
    glirc.print(glirc.my_nick(network))
end

function extension:process_command(cmd)

    local params = {}
    for w in cmd.command:gmatch("%S+") do table.insert(params, w) end

    local k = commands[params[1]]
    if k then
        k(table.unpack(params, 2))
    else
        local cmds = {}
        for k,v in pairs(commands) do table.insert(cmds,k) end
        glirc.error('Available commands: ' .. table.concat(cmds, ', '))
    end
end

function extension:stop()
end

-- return the extension module to the client
return extension
