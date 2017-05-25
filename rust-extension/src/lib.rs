#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::ffi::CStr;
use std::os::raw::c_char;
use std::os::raw::c_void;
use std::slice;
use std::ptr;
use std::str;
use std::cmp::Ordering;
use std::mem;

// Example of some state
struct my_state {
    a_string: String,
}

/*
 * Marshaling of structs
 */

unsafe fn import_string<'a>(_: &'a glirc, gstr: &glirc_string) -> &'a str {
    let slc = slice::from_raw_parts(gstr.str as *const u8, gstr.len);
    str::from_utf8_unchecked(slc)
}

fn export_string(s: &str) -> glirc_string {
    glirc_string { str: s.as_ptr() as *const i8, len: s.len() }
}

unsafe fn import_command<'a>(G: &'a glirc, cmd: *const glirc_command) -> Vec<&'a str> {
    let ref cmdref = *cmd;
    let mut v = Vec::with_capacity(cmdref.params_n);
    for x in slice::from_raw_parts(cmdref.params, cmdref.params_n) {
        v.push(import_string(G, x))
    }
    v
}

unsafe fn close_session(sptr: *mut c_void) -> my_state {
    *Box::from_raw(sptr as *mut my_state)
}

unsafe fn use_session<'a>(_: &'a glirc, sptr: *mut c_void) -> &'a mut my_state {
    &mut *(sptr as *mut my_state)
}

fn export_session(s: my_state) -> *mut c_void {
    Box::into_raw(Box::new(s)) as *mut c_void
}

unsafe fn import_strings(p: *mut *mut c_char) -> Vec<String>{
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

fn write_message (G: &glirc, code: message_code, msg: &str) {
    unsafe { glirc_print(mem::transmute(G), code, export_string(msg)); }
}

#[allow(dead_code)]
fn irc_command(G: &glirc, net: &str, cmd: &str, args: &[&str]) {

    let v: Vec<glirc_string> = args.iter().map(|&x| export_string(x)).collect();

    let gmsg = glirc_message {
            network: export_string(net),
            command: export_string(cmd),
            params: v.as_ptr(),
            params_n: v.len(),
            ..Default::default() };

    unsafe { glirc_send_message(mem::transmute(G), &gmsg); }
}

#[allow(dead_code)]
fn list_networks(G: &glirc) -> Vec<String> {
    unsafe { import_strings(glirc_list_networks(mem::transmute(G))) }
}

#[allow(dead_code)]
fn list_channels(G: &glirc, net: &str) -> Vec<String> {
    unsafe { import_strings(glirc_list_channels(mem::transmute(G), export_string(net))) }
}

#[allow(dead_code)]
fn list_channel_users(G: &glirc, net: &str, chan: &str) -> Vec<String> {
    unsafe { import_strings(glirc_list_channel_users(mem::transmute(G), export_string(net), export_string(chan))) }
}

#[allow(dead_code)]
fn my_nick (G: &glirc, net: &str) -> Option<String> {
    unsafe {
        let ptr = glirc_my_nick(mem::transmute(G), export_string(net));
        if ptr == ptr::null_mut() {
            None
        } else {
            Some (CStr::from_ptr(ptr).to_string_lossy().into_owned())
        }
    }
}

#[allow(dead_code)]
fn identifier_cmp(x: &str, y: &str) -> Ordering {
    unsafe {glirc_identifier_cmp(export_string(x), export_string(y)).cmp(&0)}
}

/*
 * Entry points from client
 */

fn my_start(G: &glirc, path: &str) -> my_state {

    let msg = format!("Rust extension started: {}", path);
    write_message(G, message_code::NORMAL_MESSAGE, &msg);

    my_state { a_string: String::from("some_string") }
}

fn my_stop(G: &glirc, session: my_state) {
    let txt = format!("Rust extension stopped; {}", session.a_string);
    write_message(G, message_code::NORMAL_MESSAGE, &txt);
}

fn my_process_command(G: &glirc, session: &my_state, params: Vec<&str>) {

    if params.len() > 0 {
        match params[0] {
          "networks" =>
              for x in list_networks(G) {
                  write_message(G, message_code::NORMAL_MESSAGE, &format!("Network: {}", x))
              },
          "state" =>
              write_message(G, message_code::NORMAL_MESSAGE, &session.a_string),
          "nick" =>
              if params.len() > 1 {
                  if let Some(nick) = my_nick(G, params[1]) {
                      write_message(G, message_code::NORMAL_MESSAGE, &nick)
                  }
              },
          _ => write_message(G, message_code::ERROR_MESSAGE, "Unknown command"),
        }
    } else {
        write_message(G, message_code::ERROR_MESSAGE, "Missing argument")
    }
}

/*
 * Extension entry points
 */

unsafe extern "C" fn start_entry(G: *mut glirc, path: *const c_char) -> *mut c_void {
    let g = &*G;
    let p = CStr::from_ptr(path).to_str().unwrap();
    let st = my_start(g, p);
    export_session(st)
}

unsafe extern "C" fn stop_entry(G: *mut glirc, sptr: *mut c_void) {
    let g = &*G;
    let st = close_session(sptr);
    my_stop(g, st)
}

unsafe extern "C" fn process_command_entry
  (G: *mut glirc, sptr: *mut c_void, rawcmd: *const glirc_command) {
      let g = &*G;
      let session = use_session(g, sptr);
      let params = import_command(g, rawcmd);
      my_process_command(g, session, params)
}

/*
 * Extension metadata
 */

#[no_mangle]
pub static mut extension: glirc_extension = glirc_extension {
    name: "rust\0" as *const str as *const [c_char] as *const c_char,
    major_version: 1,
    minor_version: 0,
    start: Some(start_entry),
    stop: Some(stop_entry),
    process_message: None,
    process_command: Some(process_command_entry),
};
