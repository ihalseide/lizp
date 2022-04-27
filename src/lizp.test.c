#include <assert.h>
#include <string.h>
#include "lizp.h"
#include "lizp.test.h"

// Read & eval
static Val * re(const char *s, int len, Val **env)
{
    return eval(read(s, len), env);
}

static void ListTest(void)
{
    const char *s;
    Val *env = NULL;
    Val *v = NULL;

    s = "[]";
    v = re(s, strlen(s), &env);
    assert(v == NULL);

    s = "[list]";
    v = re(s, strlen(s), &env);
    assert(v == NULL);

    s = "[list #1]";
    v = re(s, strlen(s), &env);
    assert(v != NULL);
    assert(ValIsSeq(v));
    assert(ValIsInt(v->first));
    assert(v->first->integer == 1);
    assert(v->rest == NULL);

    s = "[list #1 #2]";
    v = re(s, strlen(s), &env);
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

