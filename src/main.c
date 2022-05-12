#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lizp.h"

#define BUF_SZ (2*1024)

int main (int argc, char **argv)
{
    printf("\nLIZP read-print loop:\n");
    while (1)
    {
        printf(">>> ");
        Val *v;
        char buffer[BUF_SZ];
        if (!fgets(buffer, sizeof(buffer), stdin))
        {
            break;
        }
        int len1 = strlen(buffer);
        if (len1 <= 1)
        {
            break;
        }
        int len2 = ReadVal(buffer, sizeof(buffer), &v);
        int len3 = PrintValBuf(v, buffer, sizeof(buffer), 1);
        buffer[len3] = '\0';
        printf("%s\n", buffer);
        Val *e = Eval(v, NULL);
        FreeValRec(v);
        int len4 = PrintValBuf(e, buffer, sizeof(buffer), 1);
        FreeValRec(e);
        buffer[len4] = '\0';
        printf("%s\n", buffer);
    }
    return 0;
}

