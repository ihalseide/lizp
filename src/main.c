#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "reader.h"
#include "printer.h"
#include "value.h"
#include "eval.h"
#include "lizp.h"

int main (int argc, char **argv)
{
    PrinterSetBase(10);
    PrinterSetUpper(true);
    Val *env = NULL;
    while (1)
    {
        printf("LIZP> ");
        char buffer[2 * 1024];
        if(!fgets(buffer, sizeof(buffer), stdin))
        {
            putchar('\n');
            break;
        }
        rep(buffer, strlen(buffer), &env);
        putchar('\n');
    }
    return 0;
}

