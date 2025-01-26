use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR is not set");
    let out_dir_path = Path::new(&out_dir);

    let home_dir = env::var("HOME").expect("Cannot find HOME dir");

    let target_dir = Path::new("/users/nzaguta/mars/std");

    if !target_dir.exists() {
        fs::create_dir_all(&target_dir).expect("Failed to create output directory");
    }

    for entry in fs::read_dir(out_dir_path).expect("Failed to read OUT_DIR") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "dylib" {
                    let file_name = path.file_name().unwrap();
                    let destination = target_dir.join(file_name);

                    fs::copy(&path, &destination).expect("Failed to copy dylib file");
                    println!("cargo:rerun-if-changed={}", path.display());
                }
            }
        }
    }
}