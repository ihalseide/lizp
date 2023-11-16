// Main REPL (read-eval-print loop) program

#define LIZP_IMPLEMENTATION
#include "lizp.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BUF_SZ (4*1024)


// Print value to a file
void valWriteToFile(FILE *f, const LizpVal *v, int readable) {
    char *s = valWriteToNewString(v, readable);
    fprintf(f, "%s", s);
    free(s);
}


// [print (v)...]
LizpVal *print_func(LizpVal *args) {
    int readable = 0;
    for (unsigned i = 0; i < args->list->length; i++) {
        valWriteToFile(stdout, *LizpArrayGet(args->list, i), readable);
    }
    return NULL;
}


// do one read-eval-print cycle on the string
void rep(const char *str, int len, LizpVal *env, bool writeFinalValue) {
    if (!str || !*str || len <= 0) {
        return; 
    }

    // debug print
    //printf("\n<str len=%d>%.*s</str>", len, len, str);

    LizpVal *expr = NULL;
    int n = valReadAllFromBuffer(str, len, &expr);
    if (valIsError(expr)) {
        putchar('\n');
        valWriteToFile(stdout, expr, 1);
        return;
    }
    if (!n) {
        return; 
    }

    // wrap multiple expressions in an implicit "do" form
    if (n > 1) {
        LizpArray *newList = LizpArrayMake(2);
        LizpArrayAppend(newList, valCreateSymbolStr("do"));
        LizpArrayAppend(newList, expr);
        expr = valCreateList(newList);
    }

    // expr is always a value that should be free'd at this point

    // debug print
    //putchar('\n');
    //valWriteToFile(stdout, expr, 1);

    // Eval
    LizpVal *val = evaluate(expr, env);

    // Print
    if (writeFinalValue) {
        putchar('\n');
        valWriteToFile(stdout, val, 1);
    }

    valFreeRec(expr);
    valFree(val);
}


// read, eval, print loop
void REPL(LizpVal *env) {
    char buffer[BUF_SZ];
    while (true) {
        // Read
        printf("\n>>> ");
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            break;
        }
        int len = strlen(buffer);
        if (len <= 0) {
            break;
        }
        else {
            rep(buffer, len, env, true);
        }
    }
    printf("end of input\n");
}


void readFileName(const char *fileName, /*new*/ char **newContentOut, int *lengthOut) {
    FILE *f = fopen(fileName, "rb");
    if (!f) {
        perror("fopen");
        return;
    }
    fseek(f, 0, SEEK_END);
    long fileSize = ftell(f);
    rewind(f);
    char *buffer = malloc(fileSize + 1);
    fread(buffer, fileSize, 1, f);
    fclose(f);
    buffer[fileSize] = 0;
    *newContentOut = buffer;
    *lengthOut = fileSize;
}


// Concatentate a list of zero-terminated char strings
char *concatStrings(unsigned number, const char *strings[]) {
    int totalLength = 0;
    int *lengths = malloc(number * sizeof(*lengths));
    {
        // Get string lengths
        unsigned i = 0;
        while (i < number) {
            const char *string = strings[i];
            int length;
            if (string == NULL) {
                length = 0;
            }
            else {
                length = strlen(string);
            }
            lengths[i] = length;
            totalLength += length;

            // Truncate string list if a NULL string is encountered
            if (string == NULL) {
                number = i;
                break;
            }
            i++;
        }
    }
    char *result = malloc(totalLength + 1);
    {
        // Copy strings into the result buffer
        unsigned i = 0;
        unsigned j = 0;
        while (i < number) {
            if (lengths[i]) {
                memcpy(&result[j], strings[i], lengths[i]);
                j += lengths[i];
            }
            i++;
        }
    }
    result[totalLength] = 0;
    return result;
}


void loadFile(const char *fileName, LizpVal *env) {
    // read file contents wrapped in a "do" block
    /*new*/ char *fileContents;
    int fileContentsLength;
    readFileName(fileName, &fileContents, &fileContentsLength);
    const char *strings[4] = { "[do\n", fileContents, "]\n", NULL };
    /*new*/ char *runMe = concatStrings(3, strings);
    int runMeLength = strlen(runMe);
    assert(runMeLength > fileContentsLength);
    rep(runMe, runMeLength, env, false);
    free(fileContents);
    free(runMe);
}

int main (int argc, char **argv) {
    // init environment
    LizpVal *env = valCreateList(LizpArrayMake(10));
    LizpArrayAppend(env->list, NULL);
    lizpRegisterCore(env);
    EnvSetFunc(env, "print", print_func);
    EnvSetSym(env, "#f", valCreateFalse());
    EnvSetSym(env, "#t", valCreateTrue());
    print_func(env);
    // load each file given on the command line
    const char *programName = argv[0];
    {
        int i = 1;
        while (i < argc) {
            printf("%s: loading \"%s\"\n", programName, argv[i]);
            loadFile(argv[i], env);
            i++;
        }
    }
    // read-eval-print loop
    printf("\nLIZP read-eval-print loop:");
    REPL(env);
    return 0;
}

