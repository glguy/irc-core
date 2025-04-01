#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::cmp::Ordering;
use std::ffi::CStr;
use std::marker::PhantomData;
use std::os::raw::c_char;
use std::os::raw::c_void;
use std::panic;
use std::ptr;
use std::slice;
use std::str;

#[derive(Copy, Clone)]
pub struct Glirc<'a> {
    token: *mut glirc,
    _lifetime: PhantomData<&'a ()>,
}

#[derive(Copy, Clone)]
enum MessageCode {
    Normal,
    Error,
}

impl MessageCode {
    fn as_MESSAGE_CODE(self) -> message_code {
        match self {
            MessageCode::Normal => message_code_NORMAL_MESSAGE,
            MessageCode::Error => message_code_ERROR_MESSAGE,
        }
    }
}

impl<'a> Glirc<'a> {
    pub fn write_message(&self, code: MessageCode, msg: &str) {
        unsafe {
            glirc_print(self.token, code.as_MESSAGE_CODE(), msg.as_ptr() as *const i8, msg.len());
        }
    }

    #[allow(dead_code)]
    fn list_networks(&mut self) -> Vec<String> {
        unsafe { import_strings(glirc_list_networks(self.token)) }
    }

    #[allow(dead_code)]
    fn list_channels(&mut self, net: &str) -> Vec<String> {
        unsafe {
            import_strings(glirc_list_channels(
                self.token,
                net.as_ptr() as *const i8,
                net.len(),
            ))
        }
    }

    #[allow(dead_code)]
    fn irc_command(&self, net: &str, cmd: &str, args: &[&str]) {
        let v: Vec<glirc_string> = args.iter().map(|&x| export_string(x)).collect();

        let gmsg = glirc_message {
            network: export_string(net),
            command: export_string(cmd),
            params: v.as_ptr(),
            params_n: v.len(),
            ..Default::default()
        };

        unsafe {
            glirc_send_message(self.token, &gmsg);
        }
    }

    #[allow(dead_code)]
    fn list_channel_users(&self, net: &str, chan: &str) -> Vec<String> {
        unsafe {
            import_strings(glirc_list_channel_users(
                self.token,
                net.as_ptr() as *const i8,
                net.len(),
                chan.as_ptr() as *const i8,
                chan.len(),
            ))
        }
    }

    #[allow(dead_code)]
    fn my_nick(&self, net: &str) -> Option<String> {
        unsafe {
            let ptr = glirc_my_nick(self.token, net.as_ptr() as *const i8, net.len());
            if ptr == ptr::null_mut() {
                None
            } else {
                Some(CStr::from_ptr(ptr).to_string_lossy().into_owned())
            }
        }
    }
}

/*
 * Marshaling of structs
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
    let mut i = p;

    while *i != ptr::null_mut() {
        let s = CStr::from_ptr(*i).to_string_lossy().into_owned();
        v.push(s);
        i = i.offset(1);
    }

    glirc_free_strings(p);

    v
}

/*
 * Wrappers for client API
 */

#[allow(dead_code)]
fn identifier_cmp(x: &str, y: &str) -> Ordering {
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

unsafe extern "C" fn start_entry<T: GlircPlugin>(
    token: *mut glirc,
    path: *const c_char,
    args: *const glirc_string,
    args_len: usize,
) -> *mut c_void {
    let G = Glirc { token, _lifetime: PhantomData::default() };
    let p = CStr::from_ptr(path).to_str().unwrap();
    let mut a = Vec::with_capacity(args_len);
    for i in 0..args_len {
        a.push(import_string(&*args.offset(i as isize)));
    }
    handle_panics(G, ||
        Box::into_raw(T::start_plugin(G, p, &a)) as *mut c_void
        , std::ptr::null_mut())
}

unsafe extern "C" fn stop_entry<T: GlircPlugin>(sptr: *mut c_void) {
    Box::from_raw(sptr as *mut T);
}

unsafe extern "C" fn process_command_entry<T: GlircPlugin>(
    sptr: *mut c_void,
    rawcmd: *const glirc_command,
) {
    let s = unsafe { &mut *(sptr as *mut T) };
    let cmdstr = import_string(& (*rawcmd).command);
    s.process_command(cmdstr);
}

fn handle_panics<F: FnOnce() -> R + panic::UnwindSafe, R>(G: Glirc, f: F, def: R) -> R {
    match panic::catch_unwind(f()) {
        Ok(x) => x,
        Err(e) => {
            let msg = e
                .downcast_ref::<String>()
                .map(|x| x as &str)
                .unwrap_or("unknown");
            let msg1 = format!("Panic in rust extension: {}", msg);
            G.write_message(MessageCode::Error, &msg1);
            def
        }
    }
}

trait GlircPlugin {
    fn start_plugin(G: Glirc, path: &str, args: &[&str]) -> Box<Self>;
    fn process_command(&mut self, command: &str);
}

/*
 * Extension metadata
 */

macro_rules! declare_glirc_plugin {
    ($T: ty) => {
        #[no_mangle]
        pub static mut glirc_extension = glirc_extension {
            name: "rust\0" as *const str as *const c_char,
            major_version: 1,
            minor_version: 0,
            start: Some(start_entry::<$T>),
            stop: Some(stop_entry::<$T>),
            process_message: None,
            process_chat: None,
            process_command: Some(process_command_entry),
        };
        
    };
}
