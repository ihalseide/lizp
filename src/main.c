#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "reader.h"
#include "printer.h"
#include "value.h"
#include "eval.h"
#include "lizp.h"

Val *InitEnvironment(void)
{
    Val *env = NULL;
    EnvSet(&env, ValMakeInt(BASE), ValMakeInt(PrinterGetBase()));
    EnvSet(&env, ValMakeInt(UPPER), ValMakeInt(PrinterGetUpper()));
    return env;
}

int main (int argc, char **argv)
{
    PrinterSetBase(10);
    PrinterSetUpper(true);
    Val *env = InitEnvironment();
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
    }
    return 0;
}

