use std::time::Instant;

// Быстрая сортировка
fn quick_sort(arr: &[i64]) -> Vec<i64> {
    if arr.len() <= 1 {
        return arr.to_vec();
    }

    let pivot = arr[0];
    let mut left = Vec::new();
    let mut right = Vec::new();

    for &item in &arr[1..] {
        if item < pivot {
            left.push(item);
        } else {
            right.push(item);
        }
    }

    let mut result = quick_sort(&left);
    result.push(pivot);
    result.extend(quick_sort(&right));

    result
}

fn main() {
    let start = Instant::now();

    // Исходный массив
    let arr = vec![3, 6, 8, 10, 1, 2, 0];

    // Сортируем
    let sorted = quick_sort(&arr);

    // Вывод результата
    for num in sorted {
        println!("{}", num);
    }

    // Вывод времени выполнения
    let duration = start.elapsed();
    println!("Elapsed: {} microseconds", duration.as_micros());
}