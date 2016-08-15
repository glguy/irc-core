local extension = {}

glirc.print 'glirc.lua startup'

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

        self.file:write(msg.prefix, ' ', msg.command, ' ', table.concat(msg.params, ' '),'\n')
        self.file:flush()
end

function extension:process_command(params)

        glirc.error('Bad command: ' .. table.concat(params,' '))

end

function extension:stop()
        self.file:write('--END--')
        self.file:close()
        glirc.print('glirc.lua shutdown')
end

return extension
