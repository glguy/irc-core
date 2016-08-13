local extension = {}

extension.file = io.open('output.txt','w')

function extension:process_message(msg)
        self.file:write(msg.prefix, ' ', msg.command, ' ', table.concat(msg.params, ' '),'\n')
        self.file:flush()
end

function extension:stop()
        self.file:write('--END--')
        self.file:close()
end

return extension
