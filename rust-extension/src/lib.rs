#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::os::raw::c_char;
use std::os::raw::c_void;
use std::slice;
use std::str;
use std::mem;

// Example of some state
struct my_state {
    a_string: String,
}

/*
 * Marshaling of structs
 */

fn import_string<'a>(gstr: &'a glirc_string) -> &'a str {
    unsafe {
        let slc = slice::from_raw_parts(gstr.str as *const u8, gstr.len);
        str::from_utf8_unchecked(slc)
    }
}

fn export_string(s: &str) -> glirc_string {
    glirc_string { str: s.as_ptr() as *const i8, len: s.len() }
}

fn import_command<'a>(cmd: *const glirc_command) -> Vec<&'a str> {
    unsafe{
        slice::from_raw_parts((*cmd).params, (*cmd).params_n)
              .iter().map(import_string).collect()
    }
}


fn import_session(sptr: *mut c_void) -> Box<my_state> {
    unsafe { Box::from_raw(sptr as *mut my_state) }
}

fn export_session(s: Box<my_state>) -> *mut c_void {
    Box::into_raw(s) as *mut c_void
}

/*
 * Wrappers for client API
 */

fn write_message (glirc: *mut c_void, code: message_code, msg: &str) {
    unsafe { glirc_print(glirc, code, export_string(msg)); }
}

/*
 * Entry points from client
 */

pub extern "C" fn my_start(glirc: *mut c_void, _path: *const c_char) -> *mut c_void {

    write_message(glirc, message_code::NORMAL_MESSAGE, "Rust extension started");
    let st = my_state { a_string: String::from("some_string") };
    export_session(Box::new(st))
}

pub extern "C" fn my_stop(glirc: *mut c_void, sptr: *mut c_void) {
    let session = import_session(sptr);

    let txt = format!("Rust extension stopped; {}", session.a_string);
    write_message(glirc, message_code::NORMAL_MESSAGE, txt.as_str());
}

pub extern "C" fn my_process_command
  (glirc: *mut c_void, sptr: *mut c_void, rawcmd: *const glirc_command) {

    let session = import_session(sptr);
    let params = import_command(rawcmd);

    if params.len() > 0 {
        write_message(glirc, message_code::NORMAL_MESSAGE, params[0])
    } else {
        write_message(glirc, message_code::ERROR_MESSAGE, "Bad arguments");
    }

    // Wait until the stop callback to deallocate
    mem::forget(session)
}

/*
 * Extension metadata
 */

#[no_mangle]
pub static mut extension: glirc_extension = glirc_extension {
    name: "rust\0" as *const str as *const [c_char] as *const c_char,
    major_version: 1,
    minor_version: 0,
    start: Some(my_start),
    stop: Some(my_stop),
    process_message: None,
    process_command: Some(my_process_command),
};
