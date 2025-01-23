; ModuleID = 'test_module'
source_filename = "test_module"

@global_str = global [1 x i8] zeroinitializer

declare void @print_i64(i64)

declare void @println(ptr)

declare void @print_f64(double)

declare void @print(ptr)

define i64 @main() {
entry:
    %a = alloca i64, align 8
    store i64 10, ptr %a, align 8

    %b = alloca ptr, align 8
    store ptr %a, ptr %b, align 8

    %c = alloca i64, align 8
      %b1 = load ptr, ptr %b, align 8
      %dereferenced_value = load i64, ptr %b1, align 4
      store i64 %dereferenced_value, ptr %c, align 4
      %c2 = load i64, ptr %c, align 4
      call void @print_i64(i64 %c2)
      call void @println(ptr @global_str)
      ret i64 0
}


;  %a = alloca i64, align 8                      ; Выделяем память для переменной a
;  store i64 10, ptr %a, align 8                 ; Сохраняем значение 10 в a

;  %b = alloca ptr, align 8                      ; Выделяем память для указателя b
;  %referenced = alloca ptr, align 8             ; Выделяем память для указателя referenced
;  store ptr %a, ptr %referenced, align 8        ; Сохраняем указатель на a в referenced
;  store ptr %referenced, ptr %b, align 8        ; Сохраняем указатель на referenced в b

;  %c = alloca i64, align 8                      ; Выделяем память для переменной c
;  %b1 = load ptr, ptr %b, align 8               ; Загружаем указатель на referenced из b
;  %referenced_ptr = load ptr, ptr %b1, align 8  ; Загружаем указатель на a из referenced
;  %dereferenced_value = load i64, ptr %referenced_ptr, align 8 ; Разыменовываем указатель на a
;  store i64 %dereferenced_value, ptr %c, align 8 ; Сохраняем разыменованное значение в c
