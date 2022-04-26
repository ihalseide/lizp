#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "sequence.h"

static Seq *SeqAlloc(void)
{
    Seq *new;
    new = malloc(sizeof(*new));
    return new;
}

void SeqFree(Seq *p)
{
    if (p)
    {
        free(p);
    }
}

// Free every Seq node in a sequence
void SeqFreeAll(Seq *p)
{
    while (p)
    {
        Seq *q = p->rest;
        SeqFree(p);
        p = q;
    }
}

Seq *SeqInit(void *first, Seq *rest)
{
    Seq *new = SeqAlloc();
    if (new)
    {
        new->first = first;
        new->rest = rest;
    }
    return new;
}

// In-place
void SeqReverse(Seq **p)
{
    assert(0 && "not implemented");
}

int SeqLength(const Seq *p)
{
    int len = 0;
    while (p)
    {
        len++;
        p = p->rest;
    }
    return len;
}

// Get N'th first slot
void **SeqNth(Seq *p, int n)
{
    if (!p)
    {
        return NULL;
    }
    while (n > 0)
    {
        p = p->rest;
        if (!p)
        {
            return NULL;
        }
        n--;
    }
    return &p->first;
}

void SeqSet(Seq *p, int i, void *e)
{
    void **slot = SeqNth(p, i);
    if (slot)
    {
        *slot = e;
    }
}

void *SeqGet(Seq *p, int i)
{
    void **slot = SeqNth(p, i);
    if (slot)
    {
        return *slot;
    }
    return NULL;
}

// Get the last slot in a sequence
Seq *SeqLast(Seq *p)
{
    if (!p)
    {
        return NULL;
    }
    while (p->rest)
    {
        p = p->rest;
        if (!p)
        {
            return NULL;
        }
    }
    return p;
}

int SeqIsEmpty(Seq *p)
{
    return !p;
}

void SeqPush(Seq **p, void *item)
{
    assert(p);
    *p = SeqInit(item, *p);
}

void SeqAppend(Seq **p, void *item)
{
    assert(p);
    Seq *newCell = SeqInit(item, NULL);
    assert(newCell);
    if (*p)
    {
        Seq *last = SeqLast(*p);
        assert(last);
        last->rest = newCell;
    }
    else
    {
        *p = newCell;
    }
}

void *SeqVal(Seq *p)
{
    if (p)
    {
        return p->first;
    }
    else
    {
        return NULL;
    }
}

Seq *SeqNext(Seq *p)
{
    if (p)
    {
        return p->rest;
    }
    else
    {
        return NULL;
    }
}

Seq *SeqCopy(const Seq* p)
{
    Seq *copy = NULL;
    while (p)
    {
        SeqAppend(&copy, p->first);
        p = p->rest;
    }
    return copy;
}

