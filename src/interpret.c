#include <stdio.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Length string
typedef struct LString {
    char *start;
    unsigned length;
} LString;

void runFile(LString fileName) {
    printf("run file '%.*s'\n", fileName.length, fileName.start);

}

// For processing command-line args
LString shiftArgs(int *argc, char ***argv) {
    if (*argc) {
        const char *result = **argv;
        *argc = *argc - 1;
        *argv = *argv + 1;
        return (LString) {
            .start = result,
            .length = strlen(result)
        };
    }
    else {
        // No result -> empty
        return (LString) {
            .start = NULL,
            .length = 0,
        };
    }
}

int main(int argc, char **argv) {
    LString program = shiftArgs(&argc, &argv);
    if (argc == 1) {
        LString fileName = shiftArgs(&argc, &argv);
        runFile(fileName);
    }
    else {
        printf("error: expected a file name\n");
        exit(1);
    }
}
