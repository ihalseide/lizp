#include <stdlib.h>
#include <assert.h>
#include "eval.h"
#include "eval.test.h"

#include "lizp.h"
#include "printer.h"

void EvalAstTest(void)
{
    Val *x, *y;
    Val *env = NULL;

    // []
    x = NULL;
    y = EvalAst(x, &env);
    assert(y == NULL);

    // #5
    x = ValMakeInt(5);
    y = EvalAst(x, &env);
    assert(ValIsInt(y));
    assert(y->integer == x->integer);

    // [add 7 8]
    x = ValMakeSeq(ValMakeInt(ADD),
          ValMakeSeq(ValMakeInt(7),
          ValMakeSeq(ValMakeInt(8), NULL)));
    y = EvalAst(x, &env);
    assert(ValIsInt(y));
    assert(y->integer == 15);

    // [list]
    x = ValMakeSeq(ValMakeInt(LIST), NULL);
    y = EvalAst(x, &env);
    assert(y == NULL);
    assert(ValIsSeq(y));

    // [list 1]
    x = ValMakeSeq(ValMakeInt(LIST), ValMakeSeq(ValMakeInt(1), NULL));
    y = EvalAst(x, &env);
    assert(y != NULL);
    assert(ValIsSeq(y));
    assert(ValSeqLength(y) == 1);
    assert(y->first);
    assert(ValIsInt(y->first));
    assert(y->first->integer == 1);
    assert(y->rest == NULL);

    // [list 1 2]
    x = ValMakeSeq(ValMakeInt(LIST),
          ValMakeSeq(ValMakeInt(1),
          ValMakeSeq(ValMakeInt(2), NULL)));
    y = EvalAst(x, &env);
    assert(y != NULL);
    assert(ValIsSeq(y));
    assert(ValSeqLength(y) == 2);
    assert(ValIsInt(y->first));
    assert(y->first->integer == 1);
    assert(ValIsInt(y->rest->first));
    assert(y->rest->first->integer == 2);
}

void EvalTest(void)
{
    EvalAstTest();
}

