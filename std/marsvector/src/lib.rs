// i64

#[no_mangle]
pub extern "C" fn vector_new_i64(capacity: i64) -> *mut Vec<i64> {
    vector_new(capacity)
}

#[no_mangle]
pub extern "C" fn vector_push_i64(vec_ptr: *mut Vec<i64>, value: i64) {
    vector_push(vec_ptr, value)
}

#[no_mangle]
pub extern "C" fn vector_get_i64(vec_ptr: *mut Vec<i64>, index: i64) -> i64 {
    vector_get(vec_ptr, index)
}

#[no_mangle]
pub extern "C" fn vector_set_i64(vec_ptr: *mut Vec<i64>, index: i64, value: i64)  {
    vector_set(vec_ptr, index, value)
}

#[no_mangle]
pub extern "C" fn vector_free_i64(vec_ptr: *mut Vec<i64>) {
    vector_free(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_len_i64(vec_ptr: *mut Vec<i64>) -> i64 {
    vector_len(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_pop_i64(vec_ptr: *mut Vec<i64>) -> i64 {
    vector_pop(vec_ptr)
}

// f64

#[no_mangle]
pub extern "C" fn vector_new_f64(capacity: i64) -> *mut Vec<i64> {
    vector_new(capacity)
}

#[no_mangle]
pub extern "C" fn vector_push_f64(vec_ptr: *mut Vec<f64>, value: f64) {
    vector_push(vec_ptr, value)
}

#[no_mangle]
pub extern "C" fn vector_get_f64(vec_ptr: *mut Vec<f64>, index: i64) -> f64 {
    vector_get(vec_ptr, index)
}

#[no_mangle]
pub extern "C" fn vector_set_f64(vec_ptr: *mut Vec<f64>, index: i64, value: f64)  {
    vector_set(vec_ptr, index, value)
}

#[no_mangle]
pub extern "C" fn vector_free_f64(vec_ptr: *mut Vec<f64>) {
    vector_free(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_len_f64(vec_ptr: *mut Vec<f64>) -> i64 {
    vector_len(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_pop_f64(vec_ptr: *mut Vec<f64>) -> f64 {
    vector_pop(vec_ptr)
}

// str

#[no_mangle]
pub extern "C" fn vector_new_str(capacity: i64) -> *mut Vec<*mut i8> {
    vector_new(capacity)
}

#[no_mangle]
pub extern "C" fn vector_push_str(vec_ptr: *mut Vec<*mut i8>, value: *mut i8) {
    vector_push(vec_ptr, value)
}

#[no_mangle]
pub extern "C" fn vector_get_str(vec_ptr: *mut Vec<*mut i8>, index: i64) -> *mut i8 {
    vector_get(vec_ptr, index)
}

#[no_mangle]
pub extern "C" fn vector_set_str(vec_ptr: *mut Vec<*mut i8>, index: i64, value: *mut i8)  {
    vector_set(vec_ptr, index, value)
}

#[no_mangle]
pub extern "C" fn vector_free_str(vec_ptr: *mut Vec<*mut i8>) {
    vector_free(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_len_str(vec_ptr: *mut Vec<*mut i8>) -> i64 {
    vector_len(vec_ptr)
}

#[no_mangle]
pub extern "C" fn vector_pop_str(vec_ptr: *mut Vec<*mut i8>) -> *mut i8 {
    vector_pop(vec_ptr)
}

// generic

fn vector_new<T>(capacity: i64) -> *mut Vec<T> {
    let vec = Box::new(Vec::with_capacity(capacity as usize));
    Box::into_raw(vec)
}

fn vector_push<T>(vec_ptr: *mut Vec<T>, value: T) {
    assert!(!vec_ptr.is_null());

    unsafe {
        (*vec_ptr).push(value);
    }
}

fn vector_get<T: Sized + Copy>(vec_ptr: *mut Vec<T>, index: i64) -> T {
    assert!(!vec_ptr.is_null());

    unsafe {
        let vec = &*vec_ptr;
        if let Some(&value) = vec.get(index as usize) {
            value
        } else {
            panic!("Index out of bounds");
        }
    }
}

fn vector_set<T: Sized + Copy>(vec_ptr: *mut Vec<T>, index: i64, value: T) {
    assert!(!vec_ptr.is_null());

    unsafe {
        let vec = &mut *vec_ptr;
        
        if (index as usize) < vec.len() && index >= 0 {
            vec[index as usize] = value;
        } else {
            panic!("Index out of range")
        }
    }
}

fn vector_free<T: Sized>(vec_ptr: *mut Vec<T>) {
    if vec_ptr.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(vec_ptr);
    }
}

fn vector_len<T: Sized>(vec_ptr: *mut Vec<T>) -> i64 {
    assert!(!vec_ptr.is_null());

    unsafe {
        let vec = &*vec_ptr;
        vec.len() as i64
    }
}

fn vector_pop<T: Sized>(vec_ptr: *mut Vec<T>) -> T {
    assert!(!vec_ptr.is_null());

    unsafe {
        let vec = &mut *vec_ptr;
        vec.pop().unwrap_or_else(|| {
            panic!("Vector is empty");
        })
    }
}