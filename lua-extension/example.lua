local extension = {}

glirc.print 'glirc.lua startup'

function extension:process_message(msg)

        if msg.command == '001' then
                glirc.send_message
                   { network = msg.network
                   , command = "ZNC"
                   , params  = { '*playback' , 'play' ,'*', '0' } }
        end

end

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

function extension:process_command(cmd, ...)

    local k = commands[cmd]
    if k then
        k(...)
    else
        local cmds = {}
        for k,v in pairs(commands) do table.insert(cmds,k) end
        glirc.error('Available commands: ' .. table.concat(cmds, ', '))
    end
end

function extension:stop()
        glirc.print('glirc.lua shutdown')
end

return extension
