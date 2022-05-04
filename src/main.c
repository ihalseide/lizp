#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lizp.h"

#define BUF_SZ (2*1024)

int main (int argc, char **argv)
{
    PrinterSetBase(10);
    PrinterSetUpper(true);
    InitLizp();
    while (1)
    {
        printf("LIZP> ");
        char buffer[BUF_SZ];
        if(!fgets(buffer, sizeof(buffer), stdin))
        {
            putchar('\n');
            break;
        }
        rep(buffer, strlen(buffer), &env);
    }
    return 0;
}

