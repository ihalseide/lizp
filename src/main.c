#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lizp.h"

#define BUF_SZ (2*1024)

Val *LizpInitEnv(void)
{
    Val *env = NULL;
    EnvSetName(&env, "I", 1, MakeInt(99));
    return env;
}

void rep(const char *s, int len)
{
    print(eval(read(s, len)), true);
    putchar('\n');
}

int main (int argc, char **argv)
{
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
        rep(buffer, strlen(buffer));
    }
    return 0;
}

