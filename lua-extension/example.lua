local extension = {}

glirc.print 'glirc.lua startup'

glirc.print ('initial networks: ' .. table.concat(glirc.list_networks(), ', '))


extension.file = io.open('output.txt','w')
extension.file:write('--START--', tostring(glirc.version.major), '.',
                                  tostring(glirc.version.minor), '--\n')

function extension:process_message(msg)

        if msg.command == '001' then
                glirc.send_message
                   { network = msg.network
                   , command = "ZNC"
                   , params  = { '*playback' , 'play' ,'*', '0' } }
        end

        self.file:write(msg.prefix.nick, ' ',
                        msg.command, ' ',
                        table.concat(msg.params, ' '),'\n')

        self.file:flush()
end

function extension:process_command(params)

        local x,y = params[1], params[2]
        glirc.print('Comparing x and y: ' .. tostring(glirc.identifier_cmp(x,y)))

end

function extension:stop()
        self.file:write('--END--')
        self.file:close()
        glirc.print('glirc.lua shutdown')
end

return extension
