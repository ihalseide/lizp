#include <assert.h>
#include <stdlib.h>
#include "value.h"
#include "value.test.h"

void ValCopyTest(void)
{
    Val *x, *y;

    // #0
    x = ValMakeInt(0);
    y = ValCopy(x);
    assert(x != y);
    assert(y);
    assert(ValIsInt(y));
    assert(x->integer == y->integer);

    // #1
    x = ValMakeInt(1);
    y = ValCopy(x);
    assert(x != y);
    assert(y);
    assert(ValIsInt(y));
    assert(x->integer == y->integer);

    // []
    x = NULL;
    y = ValCopy(x);
    assert(y == NULL);

    // [#1]
    x = ValMakeSeq(ValMakeInt(1), NULL);
    y = ValCopy(x);
    assert(x != y);
    assert(y->first != x->first);
    assert(y);
    assert(ValIsSeq(y));
    assert(ValSeqLength(y) == 1);
    assert(ValIsInt(y->first));
    assert(y->first->integer == 1);
    assert(y->rest == NULL);

    // [#1 #2]
    x = ValMakeSeq(ValMakeInt(1), ValMakeSeq(ValMakeInt(2), NULL));
    y = ValCopy(x);
    assert(x != y);
    assert(y->first != x->first);
    assert(y);
    assert(ValIsSeq(y));
    assert(ValSeqLength(y) == 2);
    assert(ValIsInt(y->first));
    assert(y->first->integer == 1);
    assert(y->rest);
    assert(ValIsInt(y->rest->first));
    assert(y->rest->first->integer == 2);
    assert(y->rest->rest == NULL);

    // [[#1]]
    x = ValMakeSeq(ValMakeSeq(ValMakeInt(1), NULL), NULL);
    y = ValCopy(x);
    assert(x != y);
    assert(y);
    assert(y->first != x->first);
    assert(x->first->first->integer == y->first->first->integer);
}

void ValMakeIntTest(void)
{
    Val *x;

    x = ValMakeInt(0);
    assert(x);
    assert(ValIsInt(x));
    assert(!ValIsSeq(x));
    assert(x->integer == 0);

    x = ValMakeInt(1);
    assert(x);
    assert(ValIsInt(x));
    assert(!ValIsSeq(x));
    assert(x->integer == 1);

    x = ValMakeInt(-328931);
    assert(x);
    assert(ValIsInt(x));
    assert(!ValIsSeq(x));
    assert(x->integer == -328931);
}

void ValMakeSeqTest(void)
{
    Val *x;

    x = ValMakeSeq(NULL, NULL);
    assert(x);
    assert(ValIsSeq(x));
    assert(!ValIsInt(x));
    assert(x->first == NULL);
    assert(x->rest == NULL);

    x = ValMakeSeq(ValMakeSeq(NULL, NULL), NULL);
    assert(x);
    assert(ValIsSeq(x));
    assert(!ValIsInt(x));
    assert(x->first != NULL);
    assert(ValIsSeq(x->first));
    assert(x->first->first == NULL);
    assert(x->first->rest == NULL);
    assert(x->rest == NULL);
}

void ValIsIntTest(void)
{
    Val x;

    assert(!ValIsInt(NULL));

    x = (Val){ .integer=5, .rest=&x };
    assert(ValIsInt(&x));

    x = (Val){ .first=NULL, .rest=NULL };
    assert(!ValIsInt(&x));
}

void ValIsSeqTest(void)
{
    Val x;

    assert(ValIsSeq(NULL));

    x = (Val){ .integer=5, .rest=&x };
    assert(!ValIsSeq(&x));

    x = (Val){ .first=NULL, .rest=NULL };
    assert(ValIsSeq(&x));
}

void ValueTest(void)
{
    ValIsIntTest();
    ValIsSeqTest();
    ValMakeIntTest();
    ValMakeSeqTest();
    ValCopyTest();
}

