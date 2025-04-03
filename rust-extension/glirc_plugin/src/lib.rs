#![allow(
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    unsafe_op_in_unsafe_fn
)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::cmp::Ordering;
use std::ffi::CStr;
use std::mem::MaybeUninit;
use std::os::raw::{c_char, c_void};
use std::ptr;
use std::slice;
use std::str;

#[derive(Copy, Clone, Debug)]
pub enum Error {
    NotFound,
}

#[derive(Copy, Clone)]
pub struct Glirc {
    token: *mut glirc,
}

#[derive(Copy, Clone, Debug)]
pub enum MessageCode {
    Normal,
    Error,
}

#[derive(Copy, Clone, Debug)]
pub struct Focus<'a> {
    network: &'a str,
    target: &'a str,
}

#[derive(Copy, Clone, Debug)]
pub struct Command<'a> {
    pub command: &'a str,
}

#[derive(Copy, Clone, Debug)]
pub struct Message<'a> {
    pub network: &'a str,
    pub prefix_nick: &'a str,
    pub prefix_user: &'a str,
    pub prefix_host: &'a str,
    pub command: &'a str,
    pub params: &'a [&'a str],
    pub tags: &'a [(&'a str, &'a str)],
}

#[derive(Copy, Clone, Debug)]
pub struct Chat<'a> {
    pub network: &'a str,
    pub target: &'a str,
    pub message: &'a str,
}

impl MessageCode {
    fn as_message_code(self) -> message_code {
        match self {
            MessageCode::Normal => message_code_NORMAL_MESSAGE,
            MessageCode::Error => message_code_ERROR_MESSAGE,
        }
    }
}

impl Glirc {
    /// Writes a message to the client with the specified message code and content.
    ///
    /// # Arguments
    /// * `code` - The type of message (e.g., normal or error).
    /// * `msg` - The message content to display.
    pub fn write_message(self, code: MessageCode, msg: &str) {
        unsafe {
            glirc_print(
                self.token,
                code.as_message_code(),
                msg.as_ptr() as *const i8,
                msg.len(),
            );
        }
    }

    /// Injects a chat message into the client for the specified focus.
    ///
    /// # Arguments
    /// * `focus` - The network and target where the message will be sent.
    /// * `src` - The source of the message (e.g., a nickname).
    /// * `msg` - The message content to inject.
    ///
    /// # Returns
    /// `true` if the injection was successful, `false` otherwise.
    pub fn inject_chat(self, focus: Focus, src: &str, msg: &str) -> bool {
        unsafe {
            glirc_inject_chat(
                self.token,
                focus.network.as_ptr() as _,
                focus.network.len(),
                src.as_ptr() as _,
                src.len(),
                focus.target.as_ptr() as _,
                focus.target.len(),
                msg.as_ptr() as _,
                msg.len(),
            ) != 0
        }
    }

    /// Retrieves a list of all networks currently connected to the client.
    ///
    /// # Returns
    /// The connected network names.
    pub fn list_networks(self) -> Vec<String> {
        unsafe { import_strings(glirc_list_networks(self.token)) }
    }

    /// Retrieves a list of channels for the specified network.
    ///
    /// # Arguments
    /// * `net` - The name of the network.
    ///
    /// # Returns
    /// The connected channel names if network is active.
    pub fn list_channels(self, net: &str) -> Result<Vec<String>, Error> {
        unsafe {
            let ptr = glirc_list_channels(self.token, net.as_ptr() as *const i8, net.len());
            if ptr.is_null() {
                Err(Error::NotFound)
            } else {
                Ok(import_strings(ptr))
            }
        }
    }

    /// Sends an IRC command to the specified network.
    ///
    /// # Arguments
    /// * `net` - The name of the network.
    /// * `cmd` - The IRC command to send.
    /// * `args` - The arguments for the command.
    pub fn irc_command(self, net: &str, cmd: &str, args: &[&str]) -> Result<(), Error> {
        let v: Vec<glirc_string> = args.into_iter().map(|x| export_string(x)).collect();

        let gmsg = glirc_message {
            network: export_string(net),
            command: export_string(cmd),
            params: v.as_ptr(),
            params_n: v.len(),
            ..Default::default()
        };

        unsafe {
            if 0 == glirc_send_message(self.token, &gmsg) {
                Ok(())
            } else {
                Err(Error::NotFound)
            }
        }
    }

    /// Retrieves a list of users in a specific channel.
    ///
    /// # Arguments
    /// * `focus` - The network and channel to query.
    ///
    /// # Returns
    /// A vector of usernames as strings.
    pub fn list_channel_users(self, focus: Focus) -> Result<Vec<String>, Error> {
        unsafe {
            let ptr = glirc_list_channel_users(
                self.token,
                focus.network.as_ptr() as *const i8,
                focus.network.len(),
                focus.target.as_ptr() as *const i8,
                focus.target.len(),
            );
            if ptr.is_null() {
                Err(Error::NotFound)
            } else {
                Ok(import_strings(ptr))
            }
        }
    }

    /// Retrieves the current focus of the client.
    ///
    /// # Returns
    /// The current network and target.
    pub fn current_focus(self) -> (String, String) {
        unsafe {
            let mut net = MaybeUninit::uninit();
            let mut net_len = MaybeUninit::uninit();
            let mut tgt = MaybeUninit::uninit();
            let mut tgt_len = MaybeUninit::uninit();
            glirc_current_focus(
                self.token,
                net.as_mut_ptr(),
                net_len.as_mut_ptr(),
                tgt.as_mut_ptr(),
                tgt_len.as_mut_ptr(),
            );
            let net = net.assume_init();
            let net_len = net_len.assume_init();
            let tgt = tgt.assume_init();
            let tgt_len = tgt_len.assume_init();

            let result = (
                str::from_utf8_unchecked(slice::from_raw_parts(
                    tgt as _,
                    tgt_len,
                ))
                .to_string(),
                str::from_utf8_unchecked(slice::from_raw_parts(
                    net as _,
                    net_len,
                ))
                .to_string(),
            );
            glirc_free_string(net);
            glirc_free_string(tgt);
            result
        }
    }

    /// Sets the focus of the client to the specified network and target.
    ///
    /// # Arguments
    /// * `focus` - The network and target to set as the focus.
    pub fn set_focus(self, focus: Focus) {
        unsafe {
            glirc_set_focus(
                self.token,
                focus.network.as_ptr() as _,
                focus.network.len(),
                focus.target.as_ptr() as _,
                focus.target.len(),
            );
        }
    }

    /// Clears the window for the specified focus.
    ///
    /// # Arguments
    /// * `focus` - The network and target whose window will be cleared.
    pub fn clear_window(self, focus: Focus) {
        unsafe {
            glirc_clear_window(
                self.token,
                focus.network.as_ptr() as _,
                focus.network.len(),
                focus.target.as_ptr() as _,
                focus.target.len(),
            );
        }
    }

    /// Retrieves the client's nickname for the specified network.
    ///
    /// # Arguments
    /// * `net` - The name of the network.
    ///
    /// # Returns
    /// An `Option` containing the nickname as a string, or `None` if unavailable.
    pub fn my_nick(self, net: &str) -> Result<String, Error> {
        unsafe {
            let ptr = glirc_my_nick(self.token, net.as_ptr() as *const i8, net.len());
            if ptr == ptr::null_mut() {
                Err(Error::NotFound)
            } else {
                let result = CStr::from_ptr(ptr).to_string_lossy().into_owned();
                glirc_free_string(ptr);
                Ok(result)
            }
        }
    }
}

/*
 * Marshaling of strings
 */

unsafe fn import_string<'a>(gstr: &glirc_string) -> &'a str {
    let slc = slice::from_raw_parts(gstr.str_ as *const u8, gstr.len);
    str::from_utf8_unchecked(slc)
}

fn export_string(s: &str) -> glirc_string {
    glirc_string {
        str_: s.as_ptr() as *const i8,
        len: s.len(),
    }
}

unsafe fn import_strings(p: *mut *mut c_char) -> Vec<String> {
    let mut v = Vec::new();

    for i in 0.. {
        let e = *p.offset(i);
        if e.is_null() {
            break;
        }
        let s = CStr::from_ptr(e).to_string_lossy().into_owned();
        v.push(s);
    }

    glirc_free_strings(p);

    v
}

/*
 * Wrappers for client API
 */

pub fn identifier_cmp(x: &str, y: &str) -> Ordering {
    unsafe {
        glirc_identifier_cmp(
            x.as_ptr() as *const i8,
            x.len(),
            y.as_ptr() as *const i8,
            y.len(),
        )
        .cmp(&0)
    }
}

/*
 * Extension entry points
 */

pub unsafe extern "C" fn start_entry<T: GlircPlugin>(
    token: *mut glirc,
    path: *const c_char,
    args: *const glirc_string,
    args_len: usize,
) -> *mut c_void {
    let glirc = Glirc { token };
    let p = CStr::from_ptr(path).to_str().unwrap();
    let args: Vec<&str> = slice::from_raw_parts(args, args_len)
        .into_iter()
        .map(|s| import_string(s))
        .collect();
    let plugin = T::start_plugin(glirc, p, &args);
    Box::into_raw(plugin) as *mut c_void
}

pub unsafe extern "C" fn stop_entry<T: GlircPlugin>(sptr: *mut c_void) {
    let _ = Box::from_raw(sptr as *mut T);
}

pub unsafe extern "C" fn process_command_entry<T: GlircPlugin>(
    sptr: *mut c_void,
    raw: *const glirc_command,
) {
    let s = &mut *(sptr as *mut T);
    let raw = &*raw;
    let command = import_string(&raw.command);
    s.process_command(Command { command })
}

pub unsafe extern "C" fn process_chat_entry<T: GlircPlugin>(
    sptr: *mut c_void,
    raw: *const glirc_chat,
) -> u32 {
    let s = &mut *(sptr as *mut T);
    let raw = &*raw;
    s.process_chat(Chat {
        network: import_string(&raw.network),
        target: import_string(&raw.target),
        message: import_string(&raw.message),
    }) as u32
}

pub unsafe extern "C" fn process_message_entry<T: GlircPlugin>(
    sptr: *mut c_void,
    raw: *const glirc_message,
) -> u32 {
    let s = &mut *(sptr as *mut T);
    let raw = &*raw;
    let params: Vec<&str> = slice::from_raw_parts(raw.params, raw.params_n)
        .into_iter()
        .map(|s| import_string(s))
        .collect();
    let mut tags = Vec::with_capacity(raw.tags_n);
    for i in 0..raw.params_n {
        tags.push((
            import_string(&*raw.tagkeys.offset(i as isize)),
            import_string(&*raw.tagvals.offset(i as isize)),
        ));
    }
    s.process_message(Message {
        network: import_string(&raw.network),
        prefix_nick: import_string(&raw.prefix_nick),
        prefix_user: import_string(&raw.prefix_user),
        prefix_host: import_string(&raw.prefix_host),
        command: import_string(&raw.command),
        params: &params,
        tags: &tags,
    });

    0
}

pub trait GlircPlugin {
    const MAJOR: u8;
    const MINOR: u8;

    fn start_plugin(glirc: Glirc, path: &str, args: &[&str]) -> Box<Self>;

    #[allow(unused_variables)]
    fn process_command(&mut self, command: Command) {}

    #[allow(unused_variables)]
    fn process_chat(&mut self, chat: Chat) -> bool {
        false
    }

    #[allow(unused_variables)]
    fn process_message(&mut self, message: Message) -> bool {
        false
    }
}

/*
 * Extension metadata
 */

#[macro_export]
macro_rules! declare_glirc_plugin {
    ($name: expr, $T: ty) => {
        #[unsafe(export_name = "extension")]
        pub static mut GLIRC_EXTENSION: $crate::glirc_extension = $crate::glirc_extension {
            name: concat!($name, "\0").as_ptr() as _,
            major_version: <$T>::MAJOR as _,
            minor_version: <$T>::MINOR as _,
            start: Some($crate::start_entry::<$T>),
            stop: Some($crate::stop_entry::<$T>),
            process_message: Some($crate::process_message_entry::<$T>),
            process_chat: Some($crate::process_chat_entry::<$T>),
            process_command: Some($crate::process_command_entry::<$T>),
        };
    };
}
