local extension = {}

glirc.print 'Starting extension'
glirc.error 'Checking error print'

extension.file = io.open('output.txt','w')
extension.file:write('--START--', tostring(glirc.version.major), '.',
                                  tostring(glirc.version.minor), '\n')

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

function extension:stop()
        self.file:write('--END--')
        self.file:close()
end

return extension
