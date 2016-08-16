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

  local cmd = params[1]
  if cmd == 'compare' then
    glirc.print('Comparing : ' .. tostring(glirc.identifier_cmp(params[2],params[3])))
  elseif cmd == 'list_networks' then
    glirc.print(table.concat(glirc.list_networks(), ' '))
  elseif cmd == 'list_channels' then
    glirc.print(table.concat(glirc.list_channels(params[2]), ' '))
  elseif cmd == 'list_channel_users' then
    glirc.print(table.concat(glirc.list_channel_users(params[2],params[3]), ' '))
  elseif cmd == 'my_nick' then
    glirc.print(glirc.my_nick(params[2]))
  else
    glirc.error('Unknown command')
  end

end

function extension:stop()
        self.file:write('--END--')
        self.file:close()
        glirc.print('glirc.lua shutdown')
end

return extension
