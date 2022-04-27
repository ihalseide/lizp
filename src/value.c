#include <assert.h>
#include <stdbool.h>
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
bool ValIsSeq(Val *p)
{
    return !p || p->rest != p;
}

bool ValIsInt(Val *p)
{
    return p && p->rest == p;
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

// String is a special type of Seq
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

bool ValEqual(Val *x, Val *y)
{
    if (x == NULL || y == NULL)
    {
        return x == y;
    }
    if (ValIsInt(x))
    {
        return ValIsInt(y) && x->integer == y->integer;
    }
    if (ValIsSeq(x))
    {
        Val *px = x, *py = y;
        while (px && ValIsSeq(px) && py && ValIsSeq(py))
        {
            if (!ValEqual(px->first, py->first))
            {
                break;
            }
            px = px->rest;
            py = py->rest;
        }
        return px == NULL && py == NULL;
    }
    return false;
}

bool ValIsTrue(Val *p)
{
    return ValIsInt(p) && p->integer;
}

// String is a special type of Seq
// form [[str] ...]
bool ValIsStr(Val *p)
{
    return p && p->rest != p && p->first && ValIsSeq(p->first) &&
        ValIsInt(p->first->first) && p->first->first->integer == STR;
}

// Lambda is a special type of Seq
// form [[lambda args] expr]
bool ValIsLambda(Val *p)
{
    return p && ValIsSeq(p) && ValIsSeq(p->first) && p->first &&
        ValIsInt(p->first->first) && p->first->first->integer == LAMBDA;
}

