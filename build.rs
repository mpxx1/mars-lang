use std::env;
use std::fs;
use std::path::Path;

fn main() {
    // Получаем путь к выходной директории (OUT_DIR)
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR is not set");
    let out_dir_path = Path::new(&out_dir);
    println!("OURDIR: {:?}", out_dir_path);

    // Путь к корневой директории проекта
    let home_dir = env::var("HOME").expect("Cannot find HOME dir");

    // Целевая директория, куда нужно копировать файлы
    let target_dir = Path::new("/users/nzaguta/mars/std");

    // Убедимся, что целевая директория существует
    if !target_dir.exists() {
        fs::create_dir_all(&target_dir).expect("Failed to create output directory");
    }

    // Копируем только .dylib файлы из OUT_DIR
    for entry in fs::read_dir(out_dir_path).expect("Failed to read OUT_DIR") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // Проверяем, что это файл с расширением .dylib
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