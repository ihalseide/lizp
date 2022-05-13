#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lizp.h"

#define BUF_SZ (2*1024)

int main (int argc, char **argv)
{
    Val *env = MakeSeq(NULL, NULL);

    printf("\nLIZP read-print loop:");
    while (1)
    {
        printf("\n>>> ");
        char buffer[BUF_SZ];
        if (!fgets(buffer, sizeof(buffer), stdin))
        {
            printf("end of input\n");
            break;
        }
        int len = strlen(buffer);
        if (len <= 1)
        {
            printf("end of input\n");
            break;
        }
        Val *expr = NULL;
        int readlen = ReadVal(buffer, len, &expr);
        if (!expr)
        {
            printf("[]\n");
            continue;
        }
        if (readlen != len)
        {
            printf("read error\n");
            continue;
        }

        //printf("expr: ");
        //PrintValFile(stdout, expr, 1);
        //putchar('\n');

        Val *val = Eval(expr, env);
        //printf("eval: ");
        putchar('\n');
        PrintValFile(stdout, val, 1);

        FreeValRec(expr);
        FreeValRec(val);
    }
    return 0;
}

