#include <stdio.h>
#include <stdlib.h>

void print(long a) {
    printf("%lu\n", a);
}

long load_file() {
    const char* file_name = "day5data";
    long file_size = 0;

    FILE* file = fopen(file_name, "rb");

    fseek(file, 0, SEEK_END);
    file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)malloc(file_size);

    size_t bytesRead = fread(buffer, 1, file_size, file);

    fclose(file);

    printf("%s\n", buffer);

    return (long)buffer;
}

void print_divider() {
    printf("------------------\n");
}