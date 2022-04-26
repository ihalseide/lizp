#include <assert.h>
#include <stdlib.h>
#include "value.h"
#include "sequence.h"

static int vcount = 0;
int getCount(void)
{
    return vcount;
}

Val *ValAlloc(void)
{
    Val *new;
    new = malloc(sizeof(*new));
    vcount++;
    return new;
}

void ValFree(Val *p)
{
    if (p)
    {
        free(p);
        vcount--;
    }
}

void ValFreeAll(Val *p)
{
    if (ValIsSeq(p))
    {
        // Free the values in the sequence
        Seq *s = p->sequence;
        while (s)
        {
            ValFree((Val*)s->first);
            s = s->rest;
        }
        // Free the sequence nodes
        SeqFreeAll(s);
    }
    ValFree(p);
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

// New copy, with no structure-sharing
Val *ValCopy(const Val *p)
{
    switch (p->kind)
    {
        case CK_INT:
            return ValMakeInt(p->integer);
        case CK_SEQ:
            {
                Seq *new = NULL;
                while (p && p->sequence)
                {
                    assert(ValIsSeq(p));
                    SeqAppend(&new, ValCopy((Val*)p->sequence->first));
                    p = (const Val*)p->sequence->rest;
                }
                return ValMakeSeq(new);
            }
        default:
            assert(0);
            break;
    }
}

