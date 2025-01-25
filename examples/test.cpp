#include <iostream>
#include <vector>
#include <chrono>

// Быстрая сортировка
std::vector<int64_t> quick_sort(const std::vector<int64_t>& arr) {
    if (arr.size() <= 1) {
        return arr;
    }

    int64_t pivot = arr[0];
    std::vector<int64_t> left;
    std::vector<int64_t> right;

    for (size_t i = 1; i < arr.size(); ++i) {
        if (arr[i] < pivot) {
            left.push_back(arr[i]);
        } else {
            right.push_back(arr[i]);
        }
    }

    std::vector<int64_t> result = quick_sort(left);
    result.push_back(pivot);
    std::vector<int64_t> rightRes = quick_sort(right);

    result.insert(result.end(), rightRes.begin(), rightRes.end());

    return result;
}

int main() {
    auto start = std::chrono::high_resolution_clock::now();

    // Исходный массив
    std::vector<int64_t> arr = {3, 6, 8, 10, 1, 2, 1};

    // Сортируем
    std::vector<int64_t> sorted = quick_sort(arr);

    // Вывод результата
    for (const auto& num : sorted) {
        std::cout << num << std::endl;
    }

    // Вывод времени выполнения
    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    std::cout << "Elapsed: " << duration << " microseconds" << std::endl;

    return 0;
}