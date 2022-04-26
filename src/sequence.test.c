#include <assert.h>
#include <stdlib.h>
#include "sequence.h"
#include "sequence.test.h"

void SeqAppendTest(void)
{
    Seq *s;
    void *p = SeqAppendTest;

    // Append to empty once
    s = NULL;
    SeqAppend(&s, p);
    assert(s);
    assert(SeqLength(s) == 1);
    assert(SeqGet(s, 0) == p);
    SeqFree(s);

    // Append to empty twice
    s = NULL;
    SeqAppend(&s, p);
    assert(s);
    assert(SeqLength(s) == 1);
    assert(SeqGet(s, 0) == p);
    SeqAppend(&s, p);
    assert(s);
    assert(SeqLength(s) == 2);
    assert(SeqGet(s, 0) == p);
    assert(SeqGet(s, 1) == p);
    SeqFree(s);

    // Append
    s = SeqInit(NULL, NULL);
    assert(s);
    SeqAppend(&s, p);
    assert(s);
    assert(SeqLength(s) == 2);
    assert(SeqGet(s, 0) == NULL);
    assert(SeqGet(s, 1) == p);
    SeqFree(s);
}

void SeqNthTest(void)
{
    int a = 'a', b = 'b', c = 'c';
    Seq *s = SeqInit(&a, SeqInit(&b, SeqInit(&c, NULL)));
    void **p;

    p = SeqNth(s, 0);
    assert(*p == &a);

    p = SeqNth(s, 1);
    assert(*p == &b);

    p = SeqNth(s, 2);
    assert(*p == &c);

    SeqFreeAll(s);
}

void SeqCopyTest(void)
{
    Seq *x, *y;

    // Empty sequence
    x = NULL;
    y = SeqCopy(x);
    assert(x == y);

    // Single-item sequence
    x = SeqInit(SeqCopyTest, NULL);
    y = SeqCopy(x);
    assert(x != y);
    assert(x->first == y->first);
    assert(x->rest == y->rest);
    SeqFreeAll(x);
    SeqFreeAll(y);

    // Many-item sequence
    x = SeqInit(SeqCopyTest, SeqInit(SeqNthTest, SeqInit(SeqAppendTest, NULL)));
    y = SeqCopy(x);
    assert(x != y);
    assert(x->first == y->first);
    assert(x->rest != y->rest);
    assert(x->rest->first == y->rest->first);
    assert(x->rest->rest != y->rest->rest);
    assert(x->rest->rest->first == y->rest->rest->first);
    // NULL terminator is the same
    assert(x->rest->rest->rest == y->rest->rest->rest);
    SeqFreeAll(x);
    SeqFreeAll(y);
}

void SequenceTest(void)
{
    SeqNthTest();
    SeqAppendTest();
    SeqCopyTest();
}

