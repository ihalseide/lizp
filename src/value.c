#include <stdlib.h>

#include "value.h"
#include "sequence.h"

Val *ValAlloc(void)
{
    Val *new;
    new = malloc(sizeof(*new));
    return new;
}

void ValFree(Val *p)
{
    if (p)
    {
        free(p);
    }
}

// A NULL Seq is considered an empty sequence,
// BUT a NULL Val is not!
int ValIsSeq(const Val *p)
{
    return p && p->kind == CK_SEQ;
}

int ValIsInt(const Val *p)
{
    return p && p->kind == CK_INT;
}

Val *ValMakeInt(int n)
{
    Val *p = ValAlloc();
    if (p)
    {
        p->kind = CK_INT;
        p->integer = n;
    }
    return p;
}

Val *ValMakeSeq(Seq *s)
{
    Val *p = ValAlloc();
    if (p)
    {
        p->kind = CK_SEQ;
        p->sequence = s;
    }
    return p;
}

