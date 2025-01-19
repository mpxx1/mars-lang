use marsc_driver;
use std::ffi::{CStr};

fn main() {
    marsc_driver::main()
}

#[cfg(test)]
mod tests {

}


#[no_mangle] // Отключаем изменение имени функции компилятором
pub extern "C" fn print_message(format: *const u8) {
    unsafe {
        if format.is_null() {
            return;
        }

        let c_str = CStr::from_ptr(format as *const i8);
        if let Ok(format_str) = c_str.to_str() {
            println!("{}", format_str);
        }
    }
}