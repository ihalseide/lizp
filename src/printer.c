#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include "printer.h"
#include "lizp.h"

static int printNumBase = 10;
static bool printNumUpper = false;

int PrinterGetBase(void)
{
    return printNumBase;
}

// Allow setting the base to only 2, 10, 16, or 36.
void PrinterSetBase(int b)
{
    switch (b)
    {
        case 2:
        case 10:
        case 16:
        case 36:
            printNumBase = b;
            break;
        default:
            break;
    }
}

bool PrinterGetUpper(void)
{
    return printNumUpper;
}

void PrinterSetUpper(bool b)
{
    printNumUpper = b? true : false;
}

// Returns: number of chars written
int PrintChar(char c, char *out, int length)
{
    // Validate arguments
    if (out && length > 0)
    {
        *out = c;
        return 1;
    }
    else
    {
        return 0;
    }
}

// Returns: number of chars written
int PrintCStr(const char *s, char *out, int len)
{
    // Validate inputs
    if (s && out)
    {
        int i;
        for (i = 0; s[i] && i < len; i++)
        {
            out[i] = s[i];
        }
        return i;
    }
    return 0;
}

char ValueToDigit(int d, bool upper)
{
    if (0 <= d)
    {
        if (d <= 9)
        {
            return '0' + d;
        }
        if (d <= 35)
        {
            return (upper? 'A' : 'a') + d - 10;
        }
    }
    return '?';
}

// Returns: number of chars written
int PrintInt(int n, char *out, int len, int readable, int base, bool upper)
{
    assert(base > 1);
    assert(out);

    // Zero -> special case
    if (len >= 0 && n == 0)
    {
        *out = '0';
        return 1;
    }

    char buf[32];
    const int sz = sizeof(buf);

    // U = magnitude of N
    int u = (n >= 0)? n : -n;

    int i;
    for (i = 0; (u > 0) && (i < len); i++)
    {
        assert(i < sz);
        buf[sz - i - 1] = ValueToDigit(u % base, upper);
        u /= base;
    }

    // Loop should run at least once, even for n == 0
    assert(i >= 1);

    // Sigil for base
    if (readable)
    {
        switch (base)
        {
            case 2:
                buf[sz - i - 1] = '%';
                i++;
                break;
            case 10:
                buf[sz - i - 1] = '#';
                i++;
                break;
            case 16:
                buf[sz - i - 1] = '$';
                i++;
                break;
            case 36:
                break;
            default:
                LizpError(LE_INVALID_INT_BASE);
        }
    }

    // Minus sign for negative numbers
    if (base != 2 && n < 0)
    {
        assert(i < sz);
        buf[sz - i - 1] = '-';
        i++;
    }

    memcpy(out, buf + sz - i, i);
    return i;
}

int PrintStr(Val *seq, char *out, int length, bool readable)
{
    if (length > 0 && out && seq)
    {
        char *view = out;
        seq = seq->rest;
        if (readable)
        {
            *view = '"';
            view++;
        }
        while (ValIsSeq(seq) && seq && view < (out + length))
        {
            Val *e = seq->first;
            assert(ValIsInt(e));
            view += PrintChar((char)e->integer, view, length-(view-out));
            seq = seq->rest;
        }
        if (readable)
        {
            *view = '"';
            view++;
        }
        return view - out;
    }
    return 0;
}

int PrintSeq(Val *seq, char *out, int length, bool readable)
{
    if (length > 0 && out)
    {
        char *view = out;
        // Print opening '['
        view += PrintChar('[', view, length);
        if (seq)
        {
            // Print 1st without a space
            if (view < (out+length))
            {
                view += PrintVal(seq->first, view, length-(view-out), readable);
                seq = seq->rest;
            }
            // Print list contents
            while (ValIsSeq(seq) && seq && view < (out + length))
            {
                view += PrintChar(' ', view, length-(view-out));
                view += PrintVal(seq->first, view, length-(view-out), readable);
                seq = seq->rest;
            }
        }
        // Print closing ']'
        view += PrintChar(']', view, length-(view-out));
        return view - out;
    }
    return 0;
}

// Prints p to the given output stream
// Returns: number of chars written
int PrintVal(Val *p, char *out, int length, bool readable)
{
    if (length > 0)
    {
        if (ValIsInt(p))
        {
            return PrintInt(p->integer, out, length, readable, printNumBase, printNumUpper);
        }
        if (ValIsStr(p))
        {
            return PrintStr(p, out, length, readable);
        }
        return PrintSeq(p, out, length, readable);
    }
    return 0;
}

