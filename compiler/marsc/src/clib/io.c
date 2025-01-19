#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

#include <stdio.h>

extern DLLEXPORT void println(char *str) {
    printf("%s\n", str);
}