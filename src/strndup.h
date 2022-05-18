// String functions for non-POSIX systems

#ifndef _strndup_h_
#define _strndup_h_

char *strndup(const char *str, int len);
char *strdup(const char *str);

#endif

#ifdef STRNDUP_IMPLEMENTATION

#include <stdlib.h>

// Duplicate string with given length
char *strndup(const char *str, int len)
{
    if (!str || len < 0)
    {
        return NULL;
    }
    int n = 0;
    while (str[n] && n < len)
    {
        n++;
    }
    char *p = malloc(n + 1);
    if (p)
    {
        memcpy(p, str, n);
        p[n] = 0;
    }
    return p;
}

// Duplicate string
char *strdup(const char *str)
{
    if (!str)
    {
        return NULL;
    }
    int n = 0;
    while (str[n])
    {
        n++;
    }
    char *p = malloc(n + 1);
    if (p)
    {
        memcpy(p, str, n);
        p[n] = 0;
    }
    return p;
}

#endif /* STRNDUP_IMPL */
