#include <assert.h>
#include <stdlib.h>
#include "value.h"

Val *ValAlloc(void)
{
    Val *new;
    new = malloc(sizeof(*new));
    return new;
}

void ValFree(Val *p)
{
    free(p);
}

// A NULL val is considered an empty sequence,
int ValIsSeq(Val *p)
{
    return !p || p->rest != p;
}

int ValIsInt(Val *p)
{
    return p && p->rest == p;
}

int ValIsStr(Val *p)
{
    return p && p->rest != p && ValIsSeq(p->first) && ValIsInt(p->first->first)
        && p->first->first->integer == STR;
}

Val *ValMakeInt(long n)
{
    Val *p = ValAlloc();
    p->integer = n;
    p->rest = p;
    return p;
}

Val *ValMakeSeq(Val *first, Val *rest)
{
    Val *p = ValAlloc();
    p->first = first;
    p->rest = rest;
    return p;
}

Val *ValMakeStr(const char *s, int len)
{
    Val *p = ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), NULL);
    Val *ps = p;
    for (int i = 0; i < len; i++)
    {
        ps->rest = ValMakeSeq(ValMakeInt(s[i]), NULL);
        ps = ps->rest;
    }
    return p;
}

int ValSeqLength(Val *p)
{
    int i = 0;
    while (p && ValIsSeq(p))
    {
        i++;
        p = p->rest;
    }
    return i;
}

// New copy, with no structure-sharing
Val *ValCopy(Val *p)
{
    if (p)
    {
        if (ValIsInt(p))
        {
            return ValMakeInt(p->integer);
        }
        // Seq
        Val *copy = ValMakeSeq(ValCopy(p->first), NULL);
        Val *pcopy = copy;
        p = p->rest;
        while (ValIsSeq(p) && p)
        {
            pcopy->rest = ValMakeSeq(ValCopy(p->first), NULL);
            pcopy = pcopy->rest;
            p = p->rest;
        }
        return copy;
    }
    return NULL;
}

