#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#include <stdio.h>
#include <unistd.h>
#include <time.h>

extern DLLEXPORT void println(char *str) {
    printf("%s\n", str);
}

extern DLLEXPORT void print(char *str) {
    printf("%s", str);
}

extern DLLEXPORT void print_i64(long value) {
    printf("%ld", value);
}

extern DLLEXPORT void print_f64(double value) {
    printf("%f", value);
}

extern DLLEXPORT void sleep_thread(long time) {
    sleep(time);
}

extern DLLEXPORT long now() {
    return clock();
}
