#ifndef _strndup_h_
#define _strndup_h_

char *strndup(const char *str, int len);

#endif

#ifdef STRNDUP_IMPL

#include <stdlib.h>

// Duplicate string with given length
char *strndup(const char *str, int len)
{
    if (!str || len < 0)
    {
        return NULL;
    }
    char *p = malloc(len + 1);
    if (p)
    {
        memcpy(p, str, len);
        p[len] = 0;
    }
    return p;
}

#endif
