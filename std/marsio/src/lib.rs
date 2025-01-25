use std::ffi::{CStr, CString};
use std::thread::sleep;
use std::time::{Duration, SystemTime, UNIX_EPOCH};


fn ptr_to_str<'a>(ptr: *mut i8) -> &'a str {
    assert!(!ptr.is_null());
    
    unsafe  {
        let cstr = CStr::from_ptr(ptr);
        let rust_str = cstr.to_str().expect("Invalid UTF-8 in C-string");
        rust_str
    }
}

fn str_to_ptr<'a>(s: &str) -> *mut i8 {
    let joined_cstr = CString::new(s).expect("Failed to create CString");
    joined_cstr.into_raw()
}

#[no_mangle]
pub extern "C" fn join_str(data: *mut Vec<*mut i8>) -> *mut i8 {
    assert!(!data.is_null());
    
    unsafe {
        let vec = &*data;
        
        let mut rust_strings = Vec::new();
        for &cstr_ptr in vec.iter() {
            if !cstr_ptr.is_null() {
                rust_strings.push(ptr_to_str(cstr_ptr));
            }
        }
        
        let joined = rust_strings.join("");
        str_to_ptr(joined.as_str())
    }
}


#[no_mangle]
pub extern "C" fn println(s: *mut i8) {
    let s = ptr_to_str(s);
    println!("{}", s);
}

#[no_mangle]
pub extern "C" fn print(s: *mut i8) {
    let s = ptr_to_str(s);
    print!("{}", s);
}

#[no_mangle]
pub extern "C" fn print_i64(value: i64) {
    print!("{}", value);
}

#[no_mangle]
pub extern "C" fn print_f64(value: f64) {
    print!("{}", value);
}

#[no_mangle]
pub extern "C" fn sleep_thread(duration: i64) {
    sleep(Duration::from_millis(duration as u64));
}

#[no_mangle]
pub extern "C" fn now() -> i64 {
    let since_epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap();
    since_epoch.as_millis() as i64
}