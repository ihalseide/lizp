#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"
#include "printer.h"
#include "value.h"
#include "eval.h"
#include "sequence.h"
#include "lizp.h"

int main (int argc, char **argv)
{
    // REPL
    PrinterSetBase(10);
    PrinterSetUpper(0);
    Seq *env = NULL;
    char buffer[2 * 1024];
    while (1)
    {
        printf("LIZP> ");
        if(!fgets(buffer, sizeof(buffer), stdin))
        {
            putchar('\n');
            break;
        }
        rep(buffer, strlen(buffer), &env);
        printf("values leaked: %d\n", getCount());
    }
    return 0;
}

