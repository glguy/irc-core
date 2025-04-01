use glirc_plugin::{declare_glirc_plugin, Chat, Command, Glirc, GlircPlugin, MessageCode};

struct ExamplePlugin {
    glirc: Glirc,
}

impl GlircPlugin for ExamplePlugin {
    const MAJOR: u8 = 1;
    const MINOR: u8 = 0;

    fn start_plugin(glirc: Glirc, _path: &str, _args: &[&str]) -> Box<Self> {
        glirc.write_message(MessageCode::Normal, "started");
        Box::new(ExamplePlugin{glirc})
    }

    fn process_command(&mut self, command: Command) {
        self.glirc.write_message(MessageCode::Normal, "I got a command:");
        self.glirc.write_message(MessageCode::Normal, command.command);
    }

    fn process_chat(&mut self, chat: Chat) -> bool {
        self.glirc.write_message(MessageCode::Normal, "I got some chat:");
        self.glirc.write_message(MessageCode::Normal, chat.message);
        false
    }
}

declare_glirc_plugin!{"example", ExamplePlugin}
