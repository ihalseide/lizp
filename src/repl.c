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

// Print value to a file
void PrintValFile(FILE *f, Val *v, int readable)
{
    char *s = PrintValStr(v, readable);
    fprintf(f, "%s", s);
    free(s);
}

// [print (v)...]
Val *Lprint(Val *args)
{
    int readable = 0;
    Val *p = args;
    while (p)
    {
        PrintValFile(stdout, p->first, readable);
        p = p->rest;
    }
    return NULL;
}

// do one read-eval-print cycle on the string
void rep(const char *str, int len, Val *env)
{
    if (!str || !*str || len <= 0)
    {
        return;
    }

    // debug print
    //printf("\n<str len=%d>%.*s</str>", len, len, str);

    Val *expr;
    int readlen = ReadVal(str, len, &expr);
    if (!readlen)
    {
        return;
    }

    // debug print
    //putchar('\n');
    //PrintValFile(stdout, expr, 1);

    // Eval
    Val *val = Eval(expr, env);

    // Print
    putchar('\n');
    PrintValFile(stdout, val, 1);

    FreeValRec(expr);
    FreeValRec(val);
}

// read, eval, print loop
void REPL(Val *env)
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

void LoadFile(const char *fname, Val *env)
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
    Val *env = MakeList(NULL, NULL);
    LizpRegisterCoreFuncs(env);
    EnvSetFunc(env, "print", Lprint);
    EnvSetSym(env, "#f", MakeFalse());
    EnvSetSym(env, "#t", MakeTrue());
    // load each file given on the command line
    for (int i = 1; i < argc; i++)
    {
        printf("loading %s\n", argv[i]);
        LoadFile(argv[i], env);
    }
    // read-eval-print loop
    printf("\nLIZP read-eval-print loop:");
    REPL(env);
    return 0;
}

