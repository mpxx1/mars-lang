use std::process::Command;

fn main() {
    let mars_std_dir = "/users/nzaguta/mars/std";

    println!("STD: {mars_std_dir}");
    
    let status = Command::new("clang")
        .args(["-shared", "src/clib/io.c"])
        .arg("-o")
        .arg(format!("{}/libio.so", mars_std_dir))
        .status()
        .expect("Failed to invoke C compiler and build shared library for external C functions!");

    if !status.success() {
        panic!("Compilation of C add-on libraries failed!");
    }

    println!("cargo:rerun-if-changed=src/clib/io.c");
    println!("cargo:rustc-link-search=native={mars_std_dir}");
    println!("cargo:rustc-link-lib=dylib=io");
}