#include <assert.h>
#include <string.h>
#include "lizp.h"
#include "lizp.test.h"

void ListTest(void)
{
    const char *s;
    Val *env = NULL;
    Val *v = NULL;

    s = "[]";
    v = rep(s, strlen(s), &env);
    assert(v == NULL);

    s = "[list]";
    v = rep(s, strlen(s), &env);
    assert(v == NULL);

    s = "[list #1]";
    v = rep(s, strlen(s), &env);
    assert(v != NULL);
    assert(ValIsSeq(v));
    assert(ValIsInt(v->first));
    assert(v->first->integer == 1);
    assert(v->rest == NULL);

    s = "[list #1 #2]";
    v = rep(s, strlen(s), &env);
    assert(v != NULL);
    assert(ValIsSeq(v));
    assert(ValIsInt(v->first));
    assert(v->first->integer == 1);
    assert(v->rest);
    assert(ValIsSeq(v->rest));
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == 2);
    assert(v->rest->rest == NULL);
}

void LizpTest(void)
{
    ListTest();
}

