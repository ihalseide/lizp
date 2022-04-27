#include <assert.h>
#include <string.h>
#include "printer.h"
#include "printer.test.h"

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

