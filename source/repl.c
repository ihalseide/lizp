// Main REPL (read-eval-print loop) program

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIZP_IMPLEMENTATION
#define LIZP_CORE_FUNCTIONS
#include "lizp.h"

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

#define BUF_SZ (2*1024)

// do one read-eval-print cycle on the string
void rep(const char *str, int len, Val_t *env)
{
    if (!str || !*str || len <= 0)
    {
        return;
    }

    // debug print
    //printf("\n<str len=%d>%.*s</str>", len, len, str);

    Val_t *expr;
    int n = valReadAllFromBuffer(str, len, &expr);
    if (!n) { return; }

    // wrap multiple expressions in an implicit "do" form
    if (n > 1)
    {
        Val_t *doo = valCreateSymbolCopy("do", 2);
        expr = valCreateList(doo, expr);
    }

    // debug print
    //putchar('\n');
    //valWriteToFile(stdout, expr, 1);

    // Eval
    Val_t *val = evaluate(expr, env);

    // Print
    putchar('\n');
    valWriteToFile(stdout, val, 1);

    valFreeRec(expr);
    valFree(val);
}

// read, eval, print loop
void REPL(Val_t *env)
{
    char buffer[BUF_SZ];
    while (1)
    {
        // Read
        printf("\n>>> ");
        if (!fgets(buffer, sizeof(buffer), stdin))
        {
            break;
        }
        int len = strlen(buffer);
        if (len <= 0)
        {
            break;
        }
        rep(buffer, len, env);
    }
    printf("end of input\n");
}

void loadFile(const char *fname, Val_t *env)
{
    // read file contents wrapped in a "do" block
    FILE *f = fopen(fname, "rb");
    if (!f)
    {
        perror("fopen");
        return;
    }
    fseek(f, 0, SEEK_END);
    long file_sz = ftell(f);
    rewind(f);
    char prelude[] = "[do\n";
    long buffer_sz = sizeof(prelude) - 1 + file_sz + 2;
    char *buffer = malloc(buffer_sz);
    if (!buffer)
    {
        fclose(f);
        return;
    }
    fread(buffer + sizeof(prelude) - 1, file_sz, 1, f);
    fclose(f);
    memcpy(buffer, prelude, sizeof(prelude) - 1);
    buffer[buffer_sz - 2] = ']';
    buffer[buffer_sz - 1] = 0;
    rep(buffer, buffer_sz, env);
    free(buffer);
}

int main (int argc, char **argv)
{
    // init environment
    Val_t *env = valCreateList(NULL, NULL);
    lizpRegisterCore(env);
    EnvSetFunc(env, "print", print_func);
    EnvSetSym(env, "#f", valCreateFalse());
    EnvSetSym(env, "#t", valCreateTrue());
    // load each file given on the command line
    for (int i = 1; i < argc; i++)
    {
        printf("loading %s\n", argv[i]);
        loadFile(argv[i], env);
    }
    // read-eval-print loop
    printf("\nLIZP read-eval-print loop:");
    REPL(env);
    return 0;
}

