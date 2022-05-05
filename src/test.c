#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "lizp.h"

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

static void ValueToDigitTest(void)
{
    assert(ValueToDigit(1, false) == '1');
    assert(ValueToDigit(1, true) == '1');

    assert(ValueToDigit(9, true) == '9');
    assert(ValueToDigit(9, false) == '9');

    assert(ValueToDigit(10, true) == 'A');
    assert(ValueToDigit(10, false) == 'a');

    assert(ValueToDigit(15, true) == 'F');
    assert(ValueToDigit(15, false) == 'f');

    assert(ValueToDigit(16, true) == 'G');
    assert(ValueToDigit(16, false) == 'g');

    assert(ValueToDigit(35, true) == 'Z');
    assert(ValueToDigit(35, false) == 'z');

    assert(ValueToDigit(36, true) == '?');
    assert(ValueToDigit(36, false) == '?');

    assert(ValueToDigit(-1, true) == '?');
    assert(ValueToDigit(-1, false) == '?');
}

void PrintIntTest(void)
{
    int n, x, base;
    bool readable, upper;
    {
        char buffer[10] = {0};
        x = 10;
        base = 10;
        readable = false;
        upper = true;
        n = PrintInt(x, buffer, sizeof(buffer), readable, base, upper);
        assert(n == 2);
        assert(strncmp(buffer, "10", n) == 0);
        assert(buffer[n] == 0);
    }
    {
        char buffer[10] = {0};
        x = 0; base = 10; readable = false; upper = true;
        n = PrintInt(x, buffer, sizeof(buffer), readable, base, upper);
        assert(n == 1);
        assert(strncmp(buffer, "0", n) == 0);
        assert(buffer[n] == 0);
    }
    {
        char buffer[10] = {0};
        x = 0; base = 10; readable = true; upper = true;
        n = PrintInt(x, buffer, sizeof(buffer), readable, base, upper);
        assert(n == 2);
        assert(strncmp(buffer, "#0", n) == 0);
        assert(buffer[n] == 0);
    }
    {
        char buffer[10] = {0};
        x = 35; base = 36; readable = false; upper = true;
        n = PrintInt(x, buffer, sizeof(buffer), readable, base, upper);
        assert(n == 1);
        assert(strncmp(buffer, "Z", n) == 0);
        assert(buffer[n] == 0);
    }
    {
        char buffer[10] = {0};
        x = -23894; base = 10; readable = false; upper = true;
        n = PrintInt(x, buffer, sizeof(buffer), readable, base, upper);
        assert(n == 6);
        assert(strncmp(buffer, "-23894", n) == 0);
        assert(buffer[n] == 0);
    }
}

void PrintValTest(void)
{
    Val *v;
    int n;
    bool readable;
    {
        char buf[100] = {0};
        v = NULL;
        readable = true;
        n = PrintVal(v, buf, sizeof(buf), readable);
        assert(n == 2);
        assert(strncmp(buf, "[]", n) == 0);
        assert(buf[n] == 0);
    }
    {
        char buf[100] = {0};
        v = ValMakeInt(36);
        readable = true;
        n = PrintVal(v, buf, sizeof(buf), readable);
        assert(n == 3);
        assert(strncmp(buf, "#36", n) == 0);
        assert(buf[n] == 0);
    }
    {
        char buf[100] = {0};
        v = ValMakeSeq(ValMakeInt(36), NULL);
        readable = true;
        n = PrintVal(v, buf, sizeof(buf), readable);
        assert(n == 5);
        assert(strncmp(buf, "[#36]", n) == 0);
        assert(buf[n] == 0);
    }
    {
        char buf[100] = {0};
        v = ValMakeSeq(ValMakeSeq(ValMakeInt(36), NULL), NULL);
        readable = true;
        n = PrintVal(v, buf, sizeof(buf), readable);
        assert(n == 7);
        assert(strncmp(buf, "[[#36]]", n) == 0);
        assert(buf[n] == 0);
    }
}

void PrinterTest(void)
{
    ValueToDigitTest();
    PrintIntTest();
    PrintValTest();
}

static void DigitValueTest(void)
{
    assert(DigitValue('\0') < 0);
    assert(DigitValue('+') < 0);
    assert(DigitValue('[') < 0);
    assert(DigitValue(']') < 0);
    assert(DigitValue('_') < 0);

    assert(DigitValue('0') == 0);
    assert(DigitValue('1') == 1);
    assert(DigitValue('9') == 9);
    assert(DigitValue('a') == 10);
    assert(DigitValue('A') == 10);
    assert(DigitValue('f') == 15);
    assert(DigitValue('F') == 15);
    assert(DigitValue('z') == 35);
    assert(DigitValue('Z') == 35);
}

static void ReadIntTest(void)
{
    const char *s;
    int n;

    s = "";
    assert(ReadInt(s, strlen(s), &n) == 0);
    assert(n == 0);

    // error
    //s = "!";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    // error
    //s = "-";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    // error
    //s = "+";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    s = "0]";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 0);

    s = "0";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 0);

    s = "1";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 1);

    s = "#1";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == 1);

    s = "#12";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 12);

    s = "10";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == 36);

    s = "-4";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == -4);

    s = "$FF";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 0xFF);

    s = "$B3";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 0xB3);

    // 1011 0011 == $B3
    s = "%1011_0011 ";
    assert(ReadInt(s, strlen(s), &n) == 10);
    assert(n == 0xB3);

    s = "#2_022 ";
    assert(ReadInt(s, strlen(s), &n) == 6);
    assert(n == 2022);

    s = "z ";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 35);

    s = "-z";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == -35);

    s = "-$a";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == -0xa);

    s = "-#9";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == -9);

    s = "%1001";
    assert(ReadInt(s, strlen(s), &n) == 5);
    assert(n == 9);
}

static void ReadSeqTest(void)
{
    const char *s;
    int len;
    Val *seq;

    s = "";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 0);
    assert(ValSeqLength(seq) == 0);

    s = "[]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 2);
    assert(ValSeqLength(seq) == 0);

    s = "[ ]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 3);
    assert(ValSeqLength(seq) == 0);

    s = "[0]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 3);
    assert(ValSeqLength(seq) == 1);
    assert(seq->first->integer == 0);

    s = "[1 2 3]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 7);
    assert(ValSeqLength(seq) == 3);
    assert(seq->first->integer == 1);
    assert(seq->rest->first->integer == 2);
    assert(seq->rest->rest->first->integer == 3);

    s = "[ 1 2 3 ]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 9);
    assert(ValSeqLength(seq) == 3);
    assert(seq->first->integer == 1);
    assert(seq->rest->first->integer == 2);
    assert(seq->rest->rest->first->integer == 3);
}

static void ReadValTest(void)
{
    const char *s;
    int len;
    Val *v;

    v = NULL;
    s = " 5";
    len = ReadVal(s, strlen(s), &v);
    assert(len == 2);
    assert(ValIsInt(v));
    assert(v->integer == 5);

    v = NULL;
    s = "[[1]2 3]";
    len = ReadVal(s, strlen(s), &v);
    assert(len == 8);
    assert(ValIsSeq(v));
    assert(ValSeqLength(v) == 3);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(ValIsInt(v->rest->rest->first));
    assert(ValSeqLength(v->first) == 1);
    assert(ValIsInt(v->first->first));
    assert(v->first->first->integer == 1);

    // Macro for @x -> [get x]
    v = NULL;
    s = "@1";
    len = ReadVal(s, strlen(s), &v);
    assert(len == 2);
    assert(ValIsSeq(v));
    assert(ValSeqLength(v) == 2);
    assert(ValIsInt(v->first));
    assert(v->first->integer == GET);
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == 1);
}

void ReadStringTest(void)
{
    const char *s;
    int len;
    Val *v;

    v = NULL;
    s = "";
    len = ReadString(s, strlen(s), &v);
    assert(len == 0);

    v = NULL;
    s = "\"\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 2);
    assert(v);
    assert(ValIsSeq(v));
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->first->first));
    assert(v->first->first->integer == STR);
    assert(v->rest == NULL);
    assert(ValSeqLength(v) == 1);

    v = NULL;
    s = "\"X\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 3);
    assert(v);
    assert(ValSeqLength(v) == 2);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == 'X');

    v = NULL;
    s = "\" lizp\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 7);
    assert(v);
    assert(ValSeqLength(v) == 6);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == ' ');
}

void ReaderTest(void)
{
    DigitValueTest();
    ReadIntTest();
    ReadSeqTest();
    ReadStringTest();
    ReadValTest();
}

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

void ValIsStrTest(void)
{
    Val *x;

    x = NULL;
    assert(!ValIsStr(x));

    x = ValMakeSeq(NULL, NULL);
    assert(!ValIsStr(x));

    x = ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), NULL);
    assert(ValIsStr(x));
}

void ValEqualTest(void)
{
    assert(ValEqual(NULL, NULL));
    assert(ValEqual(ValMakeInt(3), ValMakeInt(3)));
    assert(!ValEqual(ValMakeInt(1), ValMakeInt(3)));
    assert(!ValEqual(NULL, ValMakeInt(3)));
}

void ValueTest(void)
{
    ValIsIntTest();
    ValIsSeqTest();
    ValMakeIntTest();
    ValMakeSeqTest();
    ValIsStrTest();
    ValEqualTest();
    ValCopyTest();
}

void Test(void)
{
    printf("  ValueTest\n");
    ValueTest();
    printf("  ReaderTest\n");
    ReaderTest();
    printf("  PrinterTest\n");
    PrinterTest();
    printf("  EvalTest\n");
    EvalTest();
    printf("  LizpTest\n");
    LizpTest();
}

int main(void)
{
    printf("Testing...\n");
    int val = setjmp(jbLizp);
    if (!val)
    {
        Test();
        printf("Testing is successful.\n");
        return 0;
    }
    else
    {
        printf("Lizp error during test\nFAIL.\n");
        return 1;
    }
}

