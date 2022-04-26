#include <assert.h>
#include <stdlib.h>
#include "value.h"
#include "value.test.h"

void ValFreeAllTest(void)
{
    int startCount = getCount();

    Val *p;

    // Free number
    p = ValMakeInt(3);
    ValFreeAll(p);
    assert(getCount() == startCount);

    // Free empty list
    p = ValMakeSeq(NULL);
    ValFreeAll(p);
    assert(getCount() == startCount);

    // Free single-item list
    p = ValMakeSeq(SeqInit(ValMakeInt(1), NULL));
    ValFreeAll(p);
    assert(getCount() == startCount);

    // Free bigger list
    p = ValMakeSeq(SeqInit(ValMakeInt(5),
                   SeqInit(ValMakeInt(4),
                   SeqInit(ValMakeInt(3),
                   SeqInit(ValMakeInt(2),
                   SeqInit(ValMakeInt(1), NULL))))));
    ValFreeAll(p);
    assert(getCount() == startCount);
}

void ValCopyTest(void)
{
    int startCount = getCount();

    Val *x, *y;

    x = ValMakeInt(3);
    y = ValCopy(x);
    assert(x != y);
    assert(x->kind == y->kind);
    assert(x->integer == y->integer);
    ValFree(x);
    ValFree(y);

    x = ValMakeSeq(NULL);
    y = ValCopy(x);
    assert(x != y);
    assert(x->kind == y->kind);
    assert(x->sequence == y->sequence);
    ValFreeAll(x);
    ValFreeAll(y);

    Seq *s;
    s = SeqInit(ValMakeInt(3), NULL);
    x = ValMakeSeq(s);
    y = ValCopy(x);
    assert(x != y);
    assert(x->kind == y->kind);
    assert(x->sequence != y->sequence);
    assert(x->sequence->first != y->sequence->first);
    assert(((Val*)x->sequence->first)->integer == ((Val*)y->sequence->first)->integer);
    ValFreeAll(x);
    ValFreeAll(y);

    assert(getCount() == startCount);
}

void ValueTest(void)
{
    ValFreeAllTest();
    ValCopyTest();
}

