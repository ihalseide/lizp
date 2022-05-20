#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define LIZP_IMPLEMENTATION
#include "lizp.h"
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

// Note: does not free memory

static void TestEscapeStr1(void)
{
    char *s = "hi";
    int l = EscapeStr(s, 0);
    assert(l == 0);
    assert(strncmp("hi", s, 2) == 0);
}

static void TestEscapeStr2(void)
{
    int x = EscapeStr(NULL, 10);
    assert(x == 0);
}

static void TestEscapeStr3(void)
{
    char a[] = "\\\"string\\\"";
    int l = EscapeStr(a, strlen(a));
    assert(l == 8);
    assert(strcmp(a, "\"string\"") == 0);
}

static void TestEscapeStr4(void)
{
    // other char should keep char
    char a[] = "\\a";
    int l = EscapeStr(a, strlen(a));
    assert(l == 1);
    assert(strcmp(a, "a") == 0);
}

static void TestEscapeStr5(void)
{
    char a[] = "double\\\\slash";
    int l = EscapeStr(a, strlen(a));
    assert(l == 12);
    assert(strcmp(a, "double\\slash") == 0);
}

static void TestEscapeStr6(void)
{
    char a[] = "line1\\nline2\0";
    int alen = strlen(a);
    char b[] = "line1\nline2\0";
    int l = EscapeStr(a, strlen(a));
    assert(l == alen - 1);
    assert(strcmp(a, b) == 0);
}

static void TestEscapeStr(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestEscapeStr1();
    TestEscapeStr2();
    TestEscapeStr3();
    TestEscapeStr4();
    TestEscapeStr5();
    TestEscapeStr6();
}

static void TestVal1(void)
{
    // Check invalid arguments
    Val *x;
    x = MakeSymCopy("", 0);
    assert(!x);
    x = MakeSymCopy(NULL, 0);
    assert(!x);
    x = MakeSymCopy(NULL, 10);
    assert(!x);
}

static void TestVal2(void)
{
    // Check invalid arguments
    Val * x;
    x = MakeSymCopy("string", 0);
    assert(!x);
    x = MakeSymCopy(NULL, -10);
    assert(!x);
    x = MakeSymCopy("string", -10);
    assert(!x);
}

static void TestVal3(void)
{
    Val *v = MakeSymCopy("a", 1);
    assert(v);
    assert(IsSym(v));
    assert(!IsList(v));
    assert(v->symbol);
    assert(strcmp(v->symbol, "a") == 0);
}

static void TestVal4(void)
{
    Val *v = MakeSymCopy("\na", 2);
    assert(v);
    assert(IsSym(v));
    assert(!IsList(v));
    assert(v->symbol);
    assert(strcmp(v->symbol, "\na") == 0);
}

static void TestVal5(void)
{
    Val *v = MakeList(MakeSymCopy("a", 1), NULL);
    assert(v);
    assert(IsList(v));
    assert(!IsSym(v));
    assert(v->first);
    assert(IsSym(v->first));
    assert(!IsList(v->first));
    assert(v->first->symbol);
    assert(strcmp(v->first->symbol, "a") == 0);
    assert(!v->rest);
}

static void TestVal6(void)
{
    Val *v = MakeList(MakeSymCopy("a", 1), MakeSymCopy("b", 1));
    assert(!v);
}

static void TestVal7(void)
{
    Val *v = MakeList(MakeSymCopy("a", 1), MakeList(MakeSymCopy("b", 1), NULL));
    assert(v);
    assert(IsList(v));
    assert(!IsSym(v));
    assert(v->first);
    assert(IsSym(v->first));
    assert(!IsList(v->first));
    assert(v->first->symbol);
    assert(strcmp(v->first->symbol, "a") == 0);
    assert(IsList(v->rest));
    assert(!IsSym(v->rest));
    assert(v->rest->first);
    assert(IsSym(v->rest->first));
    assert(!IsList(v->rest->first));
    assert(v->rest->first->symbol);
    assert(strcmp(v->rest->first->symbol, "b") == 0);
    assert(!v->rest->rest);
}

// Note: Depends on EscapeStr
static void TestVal(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestVal1();
    TestVal2();
    TestVal3();
    TestVal4();
    TestVal5();
    TestVal6();
    TestVal7();
}

static void TestPrintBuf1(void)
{
    Val *v = NULL;
    char buf[100];
    int readable = 1;
    int l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 2);
    buf[l] = 0;
    assert(strcmp(buf, "[]") == 0);
}

static void TestPrintBuf2(void)
{
    Val *v = MakeSymCopy("a", 1);
    char buf[100];
    int readable = 1;
    int l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 1);
    buf[l] = 0;
    assert(strcmp(buf, "a") == 0);
}

static void TestPrintBuf3(void)
{
    Val *v = MakeSymCopy("xyz", 3);
    char buf[100];
    int readable = 1;
    int l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 3);
    buf[l] = 0;
    assert(strcmp(buf, "xyz") == 0);
}

static void TestPrintBuf4(void)
{
    char buf[100];
    int readable;
    int l;
    Val *v = MakeSymCopy("\na", 2);

    readable = 1;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 5);
    buf[l] = 0;
    assert(strcmp(buf, "\"\\na\"") == 0);

    readable = 0;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 2);
    buf[l] = 0;
    assert(strcmp(buf, "\na") == 0);
}

static void TestPrintBuf5(void)
{
    Val *v = MakeSymCopy("two words", 9);
    char buf[100];
    int readable;
    int l;

    readable = 1;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 11);
    buf[l] = 0;
    assert(strcmp(buf, "\"two words\"") == 0);

    readable = 0;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 9);
    buf[l] = 0;
    assert(strcmp(buf, "two words") == 0);
}

static void TestPrintBuf6(void)
{
    Val *v = MakeSymCopy("\"q\"", 3);
    char buf[100];
    int readable;
    int l;

    readable = 1;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 7);
    buf[l] = 0;
    assert(strcmp(buf, "\"\\\"q\\\"\"") == 0);

    readable = 0;
    l = PrintValBuf(v, buf, sizeof(buf), readable);
    assert(l == 3);
    buf[l] = 0;
    assert(strcmp(buf, "\"q\"") == 0);
}

static void TestPrintBuf7(void)
{
    Val *v = MakeSymCopy("a", 1);
    int l = PrintValBuf(v, NULL, 0, 1);
    assert(l == 1);
}

static void TestPrintBuf8(void)
{
    // [a xy]
    Val *v = MakeList(MakeSymCopy("a", 1), MakeList(MakeSymCopy("xy", 2), NULL));
    int l = PrintValBuf(v, NULL, 0, 1);
    assert(l == 6);
}

//int PrintValBuf(Val *v, char *out, int length, int readable)
static void TestPrintBuf(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestPrintBuf1();
    TestPrintBuf2();
    TestPrintBuf3();
    TestPrintBuf4();
    TestPrintBuf5();
    TestPrintBuf6();
    TestPrintBuf7();
    TestPrintBuf8();
}

static void TestPrintStr1(void)
{
    char *s = PrintValStr(MakeSymCopy("a", 1), 1);
    assert(s);
    assert(strcmp(s, "a") == 0);
}

static void TestPrintStr2(void)
{
    Val *v = MakeList(MakeSymCopy("a", 1), NULL);
    char *s = PrintValStr(v, 1);
    assert(s);
    assert(strcmp(s, "[a]") == 0);
}

static void TestPrintStr3(void)
{
    Val *(*P)(Val *, Val *);
    P = MakeList;
    Val *(*S)(const char*, int);
    S = MakeSymCopy;
    // [a "\n" c]
    Val *v = P(S("a", 1), P(S("\n", 1), P(S("c", 1), NULL)));
    char *s = PrintValStr(v, 1);
    assert(strcmp(s, "[a \"\\n\" c]") == 0);
}

static void TestPrintStr(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestPrintStr1();
    TestPrintStr2();
    TestPrintStr3();
}

static void TestPrint(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestPrintBuf();
    TestPrintStr();
}

static void TestCopy1(void)
{
    Val *v = MakeSymCopy("string", 6);
    assert(v);
    Val *c = CopyVal(v);
    assert(v);
    assert(c);
    assert(v != c);
    assert(IsSym(v) == IsSym(c));
    assert(v->symbol != c->symbol);
    assert(v->symbol);
    assert(c->symbol);
    assert(strcmp(v->symbol, c->symbol) == 0);
}

static void TestCopy(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestCopy1();
}

static void TestEqual1(void)
{
    assert(IsEqual(NULL, NULL));
}

static void TestEqual2(void)
{
    Val *v = MakeSymCopy("3", 1);
    assert(!IsEqual(v, NULL));
    FreeValRec(v);
}

static void TestEqual3(void)
{
    Val *v = MakeSymCopy("a", 1);
    assert(IsEqual(v, v));
    FreeValRec(v);
}

static void TestEqual4(void)
{
    Val *a = MakeSymCopy("3", 1);
    Val *b = MakeSymCopy("3", 1);
    assert(IsEqual(a, b));
    FreeValRec(a);
    FreeValRec(b);
}

static void TestEqual5(void)
{
    Val *a = MakeList(NULL, NULL);
    Val *b = MakeList(NULL, NULL);
    assert(IsEqual(a, b));
    FreeValRec(a);
    FreeValRec(b);
}

static void TestEqual6(void)
{
    Val *a = MakeList(MakeSymCopy("a",1), NULL);
    Val *b = MakeList(MakeSymCopy("a",1), NULL);
    assert(IsEqual(a, b));
    FreeValRec(a);
    FreeValRec(b);
}

static void TestEqual7(void)
{
    Val *a = MakeList(MakeSymCopy("a",1), NULL);
    Val *b = MakeList(MakeSymCopy("b",1), NULL);
    assert(!IsEqual(a, b));
    FreeValRec(a);
    FreeValRec(b);
}

static void TestEqual(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestEqual1();
    TestEqual2();
    TestEqual3();
    TestEqual4();
    TestEqual5();
    TestEqual6();
    TestEqual7();
}

static void TestRead1(void)
{
    // empty list
    Val *v;
    char b[] = "[]";
    int l;
    l = ReadVal(b, sizeof(b), &v);
    assert(l == 2);
    assert(v == NULL);

    char c[] = "[      ]";
    l = ReadVal(c, sizeof(c), &v);
    assert(l == 8);
    assert(v == NULL);
}

static void TestRead2(void)
{
    Val *v;
    char b[] = "x";
    int l;
    l = ReadVal(b, sizeof(b), &v);
    assert(l == 1);
    assert(v);
    assert(IsSym(v));
    assert(v->symbol);
    assert(strcmp(v->symbol, "x") == 0);

    char c[] = "   x";
    l = ReadVal(c, sizeof(c), &v);
    assert(l == 4);
    assert(v);
    assert(IsSym(v));
    assert(v->symbol);
    assert(strcmp(v->symbol, "x") == 0);
}

static void TestRead3(void)
{
    Val *v;
    char b[] = "[x]";
    int l;
    l = ReadVal(b, sizeof(b), &v);
    assert(l == 3);
    assert(v);
    assert(IsList(v));
    assert(v->first);
    assert(!v->rest);
    assert(IsSym(v->first));
    assert(strcmp(v->first->symbol, "x") == 0);

    char c[] = "  [ x ] ";
    l = ReadVal(c, sizeof(c), &v);
    assert(l == 8);
    assert(v);
    assert(IsList(v));
    assert(v->first);
    assert(!v->rest);
    assert(IsSym(v->first));
    assert(v->first->symbol);
    assert(strcmp(v->first->symbol, "x") == 0);
}

static void TestRead4(void)
{
    Val *(*P)(Val *, Val *);
    P = MakeList;
    Val *(*S)(const char*, int);
    S = MakeSymCopy;
    Val *ref = P(S("+", 1), P(P(S("*", 1), P(S("x", 1), P(S("y", 1), NULL))), P(S("1", 1), NULL)));

    Val *v;
    char b[] = "[+ [* x y] 1]";
    int l;
    l = ReadVal(b, sizeof(b), &v);
    assert(l == 13);
    assert(v);
    assert(IsEqual(v, ref));
}

static void TestRead5(void)
{
    Val *(*P)(Val *, Val *);
    P = MakeList;
    Val *(*S)(const char*, int);
    S = MakeSymCopy;
    Val *ref = P(S("a", 1), P(S("\n", 1), P(S("c", 1), NULL)));

    Val *v;
    char b[] = "[a\"\\n\"c]";
    int l;
    l = ReadVal(b, sizeof(b), &v);
    assert(l == 8);
    assert(v);
    assert(IsEqual(v, ref));
}

static void TestRead6(void)
{
    // empty string "" -> null []
    Val *v;
    char b[] = "\"\"";
    int l = ReadVal(b, sizeof(b), &v);
    assert(l == 2);
    assert(v == NULL);
}

static void TestRead7(void)
{
    // quoted symbol with quote inside: "\""
    Val *v;
    char b[] = "\"\\\"\"";
    int l = ReadVal(b, sizeof(b), &v);
    assert(l == 4);
    assert(v);
    assert(IsSym(v));
    assert(v->symbol);
    assert(strcmp(v->symbol, "\"") == 0);
}

static void TestRead8(void)
{
    Val *ref;
    char easy[] = "[z 3 14]";
    ReadVal(easy, sizeof(easy), &ref);
    // comments
    Val *v;
    char b[] = "(run zamboni) [z (zamboni) 3 (minutes) 14 (seconds)]";
    int l = ReadVal(b, sizeof(b), &v);
    assert(l == 52);
    assert(v);
    assert(IsList(v));
    assert(IsEqual(v, ref));
}

static void TestRead(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestRead1();
    TestRead2();
    TestRead3();
    TestRead4();
    TestRead5();
    TestRead6();
    TestRead7();
    TestRead8();
}

static void TestIsTrue(void)
{
    fprintf(stderr, "%s\n", __func__);
    assert(!IsTrue(NULL));
    assert(!IsTrue(MakeSymCopy("false", 5)));
    assert(IsTrue(MakeSymCopy("x", 1)));
    assert(IsTrue(MakeList(MakeSymCopy("y", 1), NULL)));
}

static void TestEval1(void)
{
    Val *i, *o;

    i = MakeSym("x");
    o = Eval(i, NULL);
    assert(i);
    assert(o);
    assert(IsEqual(i, o));

    i = MakeSym("false");
    o = Eval(i, NULL);
    assert(i);
    assert(o);
    assert(IsEqual(i, o));

    i = MakeSym("word and such");
    o = Eval(i, NULL);
    assert(i);
    assert(o);
    assert(IsEqual(i, o));
}

static void TestEval2(void)
{
    Val *i, *o;

    i = NULL;
    o = Eval(i, NULL);
    assert(!i);
    assert(o == i);
    assert(IsEqual(o, i));
}

static void TestEvalIf(void)
{
    Val *i, *o;
    const char *expr;

    expr = "[if false 1 2]";
    ReadVal(expr, strlen(expr), &i);
    assert(i);
    o = Eval(i, NULL);
    assert(o);
    assert(IsEqual(o, MakeSym("2")));

    expr = "[if x 1 2]";
    ReadVal(expr, strlen(expr), &i);
    assert(i);
    o = Eval(i, NULL);
    assert(o);
    assert(IsEqual(o, MakeSym("1")));
}

static void TestEvalDefined(void)
{
}

static void TestEvalGet(void)
{
}

static void TestEvalQuote(void)
{
}

static void TestEvalDo(void)
{
}

static void TestEvalAnd(void)
{
}

static void TestEvalOr(void)
{
}

static void TestEvalLet(void)
{
}

static void TestEvalCond(void)
{
}

static void TestEvalLambda(void)
{
}

static void TestEval(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestEval1();
    TestEval2();
    TestEvalIf();
    TestEvalDefined();
    TestEvalGet();
    TestEvalQuote();
    TestEvalDo();
    TestEvalAnd();
    TestEvalOr();
    TestEvalLet();
    TestEvalCond();
    TestEvalLambda();
}

static void TestSkipChars1(void)
{
    char buf[] = "    x";
    int i = SkipChars(buf, sizeof(buf));
    assert(i == 4);
}

static void TestSkipChars2(void)
{
    char buf[] = "(comment)x";
    int i = SkipChars(buf, sizeof(buf));
    assert(i == 9);
}

static void TestSkipChars3(void)
{
    char buf[] = "   (comment)   x";
    int i = SkipChars(buf, sizeof(buf));
    assert(i == 15);
}

static void TestSkipChars4(void)
{
    char buf[] = "   (comment)  (c2) x";
    int i = SkipChars(buf, sizeof(buf));
    assert(i == 19);
}

static void TestSkipChars5(void)
{
    char buf[] = "   (comment (nested comment))(c2) x";
    int i = SkipChars(buf, sizeof(buf));
    assert(i == 34);
}

static void TestSkipChars(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestSkipChars1();
    TestSkipChars2();
    TestSkipChars3();
    TestSkipChars4();
    TestSkipChars5();
}

static void TestMatchArgs1(void)
{
    Val *err;
    const char *form;

    form = "";
    assert(MatchArgs(form, NULL, &err));
    assert(!MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(!MatchArgs(form, MakeList(MakeList(MakeSym("x"), NULL), NULL), &err));

    form = "v";
    assert(!MatchArgs(form, NULL, &err));
    assert(MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(MatchArgs(form, MakeList(NULL, NULL), &err));

    form = "s";
    assert(!MatchArgs(form, NULL, &err));
    assert(MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(!MatchArgs(form, MakeList(MakeList(MakeSym("x"), NULL), NULL), &err));
    assert(!MatchArgs(form, MakeList(NULL, NULL), &err));

    form = "l";
    assert(!MatchArgs(form, NULL, &err));
    assert(!MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(MatchArgs(form, MakeList(MakeList(MakeSym("x"), NULL), NULL), &err));
    assert(MatchArgs(form, MakeList(NULL, NULL), &err));

    form = "L";
    assert(!MatchArgs(form, NULL, &err));
    assert(!MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(MatchArgs(form, MakeList(MakeList(MakeSym("x"), NULL), NULL), &err));
    assert(!MatchArgs(form, MakeList(NULL, NULL), &err));
}

static void TestMatchArgs2(void)
{
    Val *err;
    const char *form;

    form = "(v";
    assert(MatchArgs(form, NULL, &err));
    assert(MatchArgs(form, MakeList(MakeSym("x"), NULL), &err));
    assert(MatchArgs(form, MakeList(NULL, NULL), &err));
    assert(!MatchArgs(form, MakeList(MakeList(NULL, NULL), MakeList(NULL, NULL)), &err));
}

static void TestMatchArgs(void)
{
    fprintf(stderr, "%s\n", __func__);
    TestMatchArgs1();
    TestMatchArgs2();
}

static void Test(void)
{
    TestEscapeStr();
    TestVal();
    TestEqual();
    TestPrint();
    TestSkipChars();
    TestRead();
    TestCopy();
    TestIsTrue();
    TestMatchArgs();
    TestEval();
}

int main(void)
{
    fprintf(stderr, "Testing...\n");
    Test();
    fprintf(stderr, "Testing succeeded.\n");
    return 0;
}

