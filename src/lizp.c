#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lizp.h"

jmp_buf jbLizp;
Val *pool = NULL;
Val *freelist = NULL;
Val *env = NULL;

Val *ValGet(Val *save1, Val *save2)
{
    if (!freelist)
    {
        CollectGarbage(save1, save2);
        if (!freelist)
        {
            assert(0 && "not implemented yet");
        }
    }
    Val *p = freelist;
    freelist = freelist->rest;
    return p;
}

void CollectGarbage(Val *save1, Val *save2)
{
    assert(0 && "not implemented yet");
}

// A NULL val is considered an empty sequence,
bool ValIsSeq(Val *p)
{
    return !p || p->rest != p;
}

bool ValIsInt(Val *p)
{
    return p && p->rest == p;
}

Val *ValMakeInt(long n)
{
    Val *p = ValGet(NULL, NULL);
    p->integer = n;
    p->rest = p;
    assert(ValIsInt(p));
    return p;
}

Val *ValMakeSeq(Val *first, Val *rest)
{
    Val *p = ValGet(first, rest);
    p->first = first;
    p->rest = rest;
    assert(ValIsSeq(p));
    return p;
}

Val *ValMakeEmptyStr(void)
{
    return ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), NULL);
}

// String is a special type of Seq
Val *ValMakeStr(const char *s, int len)
{
    Val *p = ValMakeEmptyStr();
    Val *ps = p;
    for (int i = 0; i < len; i++)
    {
        ps->rest = ValMakeSeq(ValMakeInt(s[i]), NULL);
        ps = ps->rest;
    }
    return p;
}

int ValSeqLength(Val *p)
{
    int i = 0;
    while (p && ValIsSeq(p))
    {
        i++;
        p = p->rest;
    }
    return i;
}

// New copy, with no structure-sharing
Val *ValCopy(Val *p)
{
    if (p)
    {
        if (ValIsInt(p))
        {
            return ValMakeInt(p->integer);
        }
        // Seq
        Val *copy = ValMakeSeq(ValCopy(p->first), NULL);
        Val *pcopy = copy;
        p = p->rest;
        while (ValIsSeq(p) && p)
        {
            pcopy->rest = ValMakeSeq(ValCopy(p->first), NULL);
            pcopy = pcopy->rest;
            p = p->rest;
        }
        return copy;
    }
    return NULL;
}

bool ValEqual(Val *x, Val *y)
{
    if (x == NULL || y == NULL)
    {
        return x == y;
    }
    if (ValIsInt(x))
    {
        return ValIsInt(y) && x->integer == y->integer;
    }
    if (ValIsSeq(x))
    {
        Val *px = x, *py = y;
        while (px && ValIsSeq(px) && py && ValIsSeq(py))
        {
            if (!ValEqual(px->first, py->first))
            {
                break;
            }
            px = px->rest;
            py = py->rest;
        }
        return px == NULL && py == NULL;
    }
    return false;
}

bool ValIsTrue(Val *p)
{
    return ValIsInt(p) && p->integer;
}

// String is a special type of Seq
// form [[str] ...]
bool ValIsStr(Val *p)
{
    return p && p->rest != p && p->first && ValIsSeq(p->first) &&
        ValIsInt(p->first->first) && p->first->first->integer == STR;
}

// Lambda is a special type of Seq
// form [[lambda args] expr]
bool ValIsLambda(Val *p)
{
    return p && ValIsSeq(p) && ValIsSeq(p->first) && p->first &&
        ValIsInt(p->first->first) && p->first->first->integer == LAMBDA;
}

bool CharIsSpace(char c)
{
    switch (c)
    {
        case '\0':
        case '"':
        case '@':
        case '[':
        case ']':
        case '#':
        case '-':
        case '_':
            // Not space:
            // * NULL char
            // * Reader macro chars: string, and get
            // * list brackets
            // * base10 integer sigil
            // * integer minus sign and underscore
            return false;
        default:
            // All other non-alphanumeric characters are space
            return !isalnum(c);
    }
}

int ReadSpace(const char *s, int len)
{
    const char *view = s;
    while ((view - s) < len && CharIsSpace(*view))
    {
        view++;
    }
    return view - s;
}

// Digit to integer value
// Returns -1 upon error
int DigitValue(char d)
{
    if ('0' <= d && d <= '9')
    {
        return d - '0';
    }
    else if ('a' <= d && d <= 'z')
    {
        return d - 'a' + 10;
    }
    else if ('A' <= d && d <= 'Z')
    {
        return d - 'A' + 10;
    }
    else
    {
        return -1;
    }
}

// Returns the number of characters read
// number read -> out
int ReadInt(const char *start, int length, int *valOut)
{
    // Validate inputs
    if (!start || length <= 0)
    {
        if (valOut)
        {
            *valOut = 0;
        }
        return 0;
    }

    const char *view = start;

    // Read prefix sigil(s)
    bool neg = false;
    int base = 0;
    if (*view == '-')
    {
        neg = true;
        view++;
    }
    switch (*view)
    {
        case '#':
            // Decimal
            base = 10;
            view++;
            break;
        default:
            // Default is base 36.
            // There is no sigil, so view does not need to be incremented
            if (isalnum(*view))
            {
                base = 36;
            }
            else
            {
                // Invalid beginning of integer
                LizpError(LE_INVALID_INT);
            }
            break;
    }

    // Keep a pointer to where the digits start
    const char *viewDigits = view;

    int n = 0;
    int d;
    while (*view && (view - start < length))
    {
        if (isalnum(*view))
        {
            d = DigitValue(*view);
            if (0 <= d && d < base)
            {
                n = (n * base) + d;
                if (n < 0)
                {
                    // There was an overflow
                    LizpError(LE_INVALID_INT_OVERFLOW);
                }
            }
            else
            {
                // Invalid digit for base
                LizpError(LE_INVALID_INT_DIGIT);
                if (valOut)
                {
                    *valOut = 0;
                }
                return 0;
            }
        }
        else if (*view != '_')
        {
            // Allow underscore to separate digits.
            // All other characters are invalid, so
            // this must be the end of the number.
            break;
        }
        view++;
    }

    // Check if there were any valid digits
    if (view == viewDigits)
    {
        // No valid digits were read after the sigils
        LizpError(LE_INVALID_INT);
        if (valOut)
        {
            *valOut = 0;
        }
        return 0;
    }

    // Apply sign
    if (neg)
    {
        n = -n;
    }

    // Return results
    if (valOut)
    {
        *valOut = n;
    }
    return view - start;
}

// Read string, with escape codes enabled
// Returns number of chars read
int ReadString(const char *start, int length, Val **toList)
{
    if (start && length > 0)
    {
        const char *view = start;

        // Consume the opening quote
        assert(*view == '"');
        view++;

        // make form: [[str] ...]
        Val *s = ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), NULL);
        Val *ps = s;
        while (*view && *view != '"' && view < start + length)
        {
            char c = *view;
            if (c == '\\')
            {
                view++;
                if (!*view || view >= start + length)
                {
                    LizpError(LE_LIST_UNFINISHED);
                }
                switch (*view)
                {
                    case '0': c = '\0'; break;
                    case 'n': c = '\n'; break;
                    case 't': c = '\t'; break;
                    case '"': c = '"'; break;
                    case '\\': c = '\\'; break;
                    default: c = *view;
                }
            }
            ps->rest = ValMakeSeq(ValMakeInt(c), NULL);
            ps = ps->rest;
            view++;
        }

        // Consume closing quote
        if (*view != '"')
        {
            LizpError(LE_LIST_UNFINISHED);
        }
        view++;

        *toList = s;
        return view - start;
    }
    return 0;
}

// Returns number of chars read
int ReadSeq(const char *start, int length, Val **toList)
{
    // Validate arguments
    if (!start || length <= 0)
    {
        return 0;
    }

    const char *view = start;

    // Consume the opening paren
    assert(*view == '[');
    view++;

    Val *s = NULL;

    // Skip whitespace
    view += ReadSpace(view, (start+length)-view);
    if (*view != ']')
    {
        // Non-empty list
        if (*view && *view != ']' && view < start+length)
        {
            Val *e;
            int len = ReadVal(view, (start+length)-view, &e);
            if (!len)
            {
                // Error reading element
                return 0;
            }
            // Create first item
            s = ValMakeSeq(e, NULL);
            view += len;
        }
        // Pointer for appending to s
        Val *ps = s;
        while (*view && *view != ']' && view < start+length)
        {
            Val *e;
            int len = ReadVal(view, (start+length)-view, &e);
            if (!len)
            {
                // Error reading element
                return 0;
            }
            // Append
            ps->rest = ValMakeSeq(e, NULL);
            ps = ps->rest;
            view += len;
        }
    }
    *toList = s;

    if (*view == ']')
    {
        // Consume the closing paren
        view++;
        return view - start;
    }
    else
    {
        // Reading error
        LizpError(LE_LIST_UNFINISHED);
        return 0;
    }
}

int ReadVal(const char *start, int length, Val **out)
{
    // Validate arguments
    if (!out || !start || length <= 0)
        return 0;

    const char *view = start;

    // Loop is for allowing comments to restart the read
    while (1)
    {
        view += ReadSpace(view, start+length-view);
        switch (*view)
        {
            case '\0':
                // End of input
                *out = NULL;
                break;
            case '@':
                // Variable getter (reader macro)
                {
                    Val *v;
                    int len = 1 + ReadVal(view+1, start+length-view-1, &v);
                    if (len)
                    {
                        view += len;
                        *out = ValMakeSeq(ValMakeInt(GET), ValMakeSeq(v, NULL));
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
            case '"':
                // String literal (reader macro)
                {
                    Val *s = NULL;
                    int len = ReadString(view, start+length-view, &s);
                    view += len;
                    if (len)
                    {
                        *out = s;
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
            case ']':
                // Unmatched list
                LizpError(LE_BRACKET_MISMATCH);
                *out = NULL;
                break;
            case '[':
                // Read sequence / list
                {
                    Val *s = NULL;
                    int len = ReadSeq(view, start+length-view, &s);
                    view += len;
                    if (len)
                    {
                        *out = s;
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
            default:
                // Read integer
                {
                    int n;
                    int len = ReadInt(view, start+length-view, &n);
                    if (len)
                    {
                        view += len;
                        *out = ValMakeInt(n);
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
        }
        break;
    }
    view += ReadSpace(view, start+length-view);

    return view - start;
}

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

    char buf[32];
    const int sz = sizeof(buf);

    // U = magnitude of N
    int u = (n >= 0)? n : -n;

    int i;
    if (u == 0)
    {
        buf[sz - 1] = '0';
        i = 1;
    }
    else
    {
        for (i = 0; (u > 0) && (i < len); i++)
        {
            assert(i < sz);
            buf[sz - i - 1] = ValueToDigit(u % base, upper);
            u /= base;
        }
    }

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
        Val *p = seq->rest;
        if (readable)
        {
            *view = '"';
            view++;
        }
        while (ValIsSeq(p) && p && view < (out + length))
        {
            Val *e = p->first;
            if (!ValIsInt(e))
            {
                // Value is not really a proper string
                return PrintSeq(seq, out, length, readable);
            }
            char c = (char)e->integer;
            if (readable)
            {
                switch (c)
                {
                    case '\0':
                        view += PrintChar('\\', view, length-(view-out));
                        c = '0';
                        break;
                    case '\n':
                        view += PrintChar('\\', view, length-(view-out));
                        c = 'n';
                        break;
                    case '\t':
                        view += PrintChar('\\', view, length-(view-out));
                        c = 't';
                        break;
                    case '"':
                        view += PrintChar('\\', view, length-(view-out));
                        c = '"';
                        break;
                    case '\\':
                        view += PrintChar('\\', view, length-(view-out));
                        c = '\\';
                        break;
                }
            }
            view += PrintChar(c, view, length-(view-out));
            p = p->rest;
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

int PrintLambda(Val *p, char *out, int length, bool readable)
{
    int base = PrinterGetBase();
    PrinterSetBase(36);
    char *view = out;
    view += PrintCStr("[lambda ", view, length-(view-out));
    view += PrintSeq(p->first->rest, view, length-(view-out), readable);
    view += PrintCStr("]", view, length-(view-out));
    PrinterSetBase(base);
    return view - out;
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
        if (ValIsLambda(p))
        {
            return PrintLambda(p, out, length, readable);
        }
        return PrintSeq(p, out, length, readable);
    }
    return 0;
}

// Concatenate lists into a single list
Val *ConcatLists(Val *lists)
{
    Val *cat = NULL;
    Val *p;
    while (lists && ValIsSeq(lists))
    {
        Val *s = lists->first;
        if (!ValIsSeq(s))
        {
            LizpError(LE_NO_FUNCTION);
        }
        while (s && ValIsSeq(s))
        {
            if (cat)
            {
                p->rest = ValMakeSeq(s->first, NULL);
                p = p->rest;
                s = s->rest;
                continue;
            }
            cat = ValMakeSeq(s->first, NULL);
            p = cat;
            s = s->rest;
        }
        lists = lists->rest;
    }
    return cat;
}

// Join a list of strings with a separator string
// sep: separator string
// strs: list of strings
Val *JoinStrings(Val *sep, Val *strs)
{
    if (strs)
    {
        Val *result = ValMakeEmptyStr();
        Val *p = result;
        // First string
        Val *s;
        s = strs->first->rest;
        while (s && ValIsSeq(s))
        {
            assert(ValIsInt(s->first));
            p->rest = ValMakeSeq(s->first, NULL);
            p = p->rest;
            s = s->rest;
        }
        strs = strs->rest;
        // Rest of the strings
        while (strs && ValIsSeq(strs))
        {
            // Sep
            s = sep->rest;
            while (s && ValIsSeq(s))
            {
                assert(ValIsInt(s->first));
                p->rest = ValMakeSeq(s->first, NULL);
                p = p->rest;
                s = s->rest;
            }
            // String
            s = strs->first->rest;
            while (s && ValIsSeq(s))
            {
                assert(ValIsInt(s->first));
                p->rest = ValMakeSeq(s->first, NULL);
                p = p->rest;
                s = s->rest;
            }
            strs = strs->rest;
        }
        return result;
    }
    return ValMakeEmptyStr();
}

// Base function for lizp code calls
static void DoPrint(Val *args, Val **env, bool readable)
{
    // Save printer vars
    int base = PrinterGetBase();
    bool upper = PrinterGetUpper();
    Val *setBase = EnvGet(env, ValMakeInt(BASE));
    Val *setUpper = EnvGet(env, ValMakeInt(UPPER));
    if (ValIsInt(setBase))
    {
        PrinterSetBase(setBase->integer);
    }
    if (ValIsInt(setUpper))
    {
        PrinterSetUpper(setUpper->integer);
    }
    Val *p = args;
    while (p && ValIsSeq(p))
    {
        print(p->first, readable);
        p = p->rest;
    }
    // Restore printer vars
    PrinterSetBase(base);
    PrinterSetUpper(upper);
}

static bool IsMacro(Val *seq)
{
    if (seq)
    {
        Val *first = seq->first;
        if (ValIsInt(first))
        {
            switch (first->integer)
            {
                case DO:
                case IF:
                case LET:
                case GET:
                case COND:
                case QUOTE:
                case LAMBDA:
                    return true;
            }
        }
    }
    return false;
}

void EnvSet(Val **env, Val *key, Val *val)
{
    if (ValIsInt(key))
    {
        Val *pair = ValMakeSeq(key, ValMakeSeq(val, NULL));
        if (*env)
        {
            (*env)->first = ValMakeSeq(pair, (*env)->first);
            return;
        }
        *env = ValMakeSeq(ValMakeSeq(pair, NULL), NULL);
    }
}

Val *EnvGet(Val **env, Val *key)
{
    if (*env && ValIsInt(key))
    {
        // Iterate environment scopes
        Val *scope = *env;
        while (scope && ValIsSeq(scope))
        {
            // Search definition list in current scope
            Val *p = scope->first;
            while (p && ValIsSeq(p))
            {
                Val *pair = p->first;
                if (pair->first->integer == key->integer)
                {
                    // Found
                    return pair->rest->first;
                }
                p = p->rest;
            }
            scope = scope->rest;
        }
    }
    // Not found anywhere
    LizpError(LE_UNKNOWN_SYM);
}

static void EnvPush(Val **env)
{
    *env = ValMakeSeq(NULL, *env);
}

static void EnvPop(Val **env)
{
    if (*env)
    {
        Val *outer_env = (*env)->rest;
        *env = outer_env;
    }
}

// Apply lambda function
// Pre-conditions:
// - called EnvPush()
// Post-requirements:
// - should do a tail call to evaluate the value
// - call EnvPop()
static Val *ApplyLambda(Val *seq, Val **env)
{
    Val *fn = seq->first;
    Val *args = seq->rest;
    Val *lArgs = fn->first->rest;
    Val *p = lArgs;
    Val *q = args;
    while (p && ValIsSeq(p) && q && ValIsSeq(q))
    {
        Val *key = p->first;
        Val *val = q->first;
        EnvSet(env, key, val);
        p = p->rest;
        q = q->rest;
    }
    if (p == NULL && q == NULL)
    {
        Val *lBody = fn->rest->first;
        return lBody;
    }
    // If this point is reached, there was an error
    EnvPop(env);
    if (p == NULL)
    {
        LizpError(LE_LAMBDA_TOO_MANY_ARGS);
    }
    if (q == NULL)
    {
        LizpError(LE_LAMBDA_TOO_FEW_ARGS);
    }
    LizpError(LE_NO_FUNCTION);
}

// Sum up a list of integers
// Returns NULL if error
Val *Sum(Val *ints)
{
    Val *p = ints;
    long sum = 0;
    while (p && ValIsSeq(p))
    {
        Val *e = p->first;
        if (!ValIsInt(e))
        {
            return NULL;
        }
        sum += e->integer;
        p = p->rest;
    }
    return ValMakeInt(sum);
}

// Apply built-in function
// Must not modify/touch/share structure with the original seq
// Returns true if new values were allocated
Val *ApplyBI(Val *seq, Val **env)
{
    Val *fn = seq->first;
    // First must be a valid function id number (a base36 name)
    if (!fn || !ValIsInt(fn))
    {
        LizpError(LE_APPLY_NOT_FUNCTION);
    }
    long nameBase36 = fn->integer;
    int numArgs = ValSeqLength(seq) - 1;
    Val *args = seq->rest;
    switch (nameBase36)
    {
        case CAT:
            // [cat list...] concatenate lists
            return ConcatLists(args);
        case JOIN:
            // [join sep str...] join strings
            if (numArgs != 0)
            {
                Val *sep = args->first;
                Val *strs = args->rest;
                return JoinStrings(sep, strs);
            }
            break;
        case NOT:
            // [not boolean]
            if (numArgs == 1)
            {
                if (ValIsTrue(args->first))
                {
                    return ValMakeInt(0);
                }
                return ValMakeInt(1);
            }
            break;
        case LEN:
            if (numArgs == 1 && ValIsSeq(args->first))
            {
                return ValMakeInt(ValSeqLength(args->first));
            }
            break;
        case FIRST:
            if (numArgs == 1 && ValIsSeq(args->first))
            {
                if (args->first)
                {
                    return args->first->first;
                }
                return NULL;
            }
            break;
        case REST:
            if (numArgs == 1 && ValIsSeq(args->first))
            {
                if (args->first)
                {
                    return args->first->rest;
                }
                return NULL;
            }
            break;
        case EQUAL:
            if (numArgs == 2)
            {
                return ValMakeInt(ValEqual(args->first, args->rest->first));
            }
            break;
        case PRINT:
            // [print expr...] readable
            if (numArgs)
            {
                bool readable = true;
                DoPrint(args, env, readable);
                return NULL;
            }
            break;
        case WRITE:
            // [write expr...] not readable
            if (numArgs)
            {
                bool readable = false;
                DoPrint(args, env, readable);
                return NULL;
            }
            break;
        case ADD:
            // [add (i)...]
            {
                Val *v = Sum(args);
                if (v)
                {
                    return v;
                }
            }
            break;
        case SUB:
            // [sub x y]
            if (numArgs == 2)
            {
                int x = args->first->integer;
                int y = args->rest->first->integer;
                return ValMakeInt(x - y);
            }
            break;
        case MUL:
            // [mul x y]
            if (numArgs == 2)
            {
                int x = args->first->integer;
                int y = args->rest->first->integer;
                return ValMakeInt(x * y);
            }
            break;
        case DIV:
            // [div x y]
            if (numArgs == 2)
            {
                int x = args->first->integer;
                int y = args->rest->first->integer;
                if (y == 0)
                {
                    LizpError(LE_DIV_ZERO);
                }
                return ValMakeInt(x / y);
            }
            break;
        case NEG:
            // [neg x]
            // Negate number
            if (numArgs == 1)
            {
                return ValMakeInt(-(args->first->integer));
            }
            break;
        case LIST:
            // [list ...]
            return args;
        case STR:
            // [str #...]
            if (numArgs)
            {
                // Make sure the arguments are all numbers
                bool valid = true;
                Val *p = args;
                while (p && ValIsSeq(p))
                {
                    if (!ValIsInt(p->first))
                    {
                        valid = false;
                        break;
                    }
                    p = p->rest;
                }
                if (valid)
                {
                    return ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), args);
                }
            }
            break;
        default:
            // Unknown
            LizpError(LE_UNKNOWN_FUNCTION);
            break;
    }
    // Given a function with invalid arguments
    LizpError(LE_NO_FUNCTION);
}

void DoMacro(Val *ast, Val **env, Val **out, bool *tail, bool *pop)
{
    *out = NULL;
    *tail = false;
    *pop = false;

    Val *args = ast->rest;
    int numArgs = ValSeqLength(args);
    switch (ast->first->integer)
    {
        case GET:
            // [get var]
            if (numArgs == 1 && ValIsInt(args->first))
            {
                *out = EnvGet(env, args->first);
                return;
            }
            break;
        case IF:
            // [if condition consequent alternative]
            if (numArgs == 2 || numArgs == 3)
            {
                if (ValIsTrue(EvalAst(args->first, env)))
                {
                    *out = args->rest->first;
                    *tail = true;
                    return;
                }
                if (numArgs == 3)
                {
                    *out = args->rest->rest->first;
                    *tail = true;
                    return;
                }
                *out = NULL;
                return;
            }
            break;
        case COND:
            // [cond (condition consequent)...]
            {
                Val *p = args;
                while (p && ValIsSeq(p))
                {
                    if (!(p->rest && ValIsSeq(p->rest)))
                    {
                        LizpError(LE_COND_FORM);
                    }
                    Val *cond = p->first;
                    Val *cons = p->rest->first;
                    if (ValIsTrue(EvalAst(cond, env)))
                    {
                        *out = cons;
                        *tail = true;
                        return;
                    }
                    p = p->rest->rest;
                }
                LizpError(LE_COND_FORM);
            }
            break;
        case QUOTE:
            // [quote expr]
            if (numArgs == 1)
            {
                *out = args->first;
                return;
            }
            break;
        case LET:
            // [let [pairs...] code]
            if (numArgs == 2)
            {
                if (ValIsSeq(args->first) && args->rest != NULL)
                {
                    EnvPush(env);
                    Val *p = args->first;
                    while (p && ValIsSeq(p))
                    {
                        if (!(p->rest && ValIsSeq(p->rest)))
                        {
                            EnvPop(env);
                            LizpError(LE_LET_FORM);
                        }
                        Val *key = p->first;
                        Val *val = EvalAst(p->rest->first, env);
                        EnvSet(env, key, val);
                        p = p->rest->rest;
                    }
                    *out = args->rest->first;
                    *tail = true;
                    *pop = true;
                    return;
                }
            }
            break;
        case DO:
            // [do ...]
            {
                Val *p = args;
                while (p && ValIsSeq(p) && p->rest)
                {
                    EvalAst(p->first, env);
                    p = p->rest;
                }
                if (p)
                {
                    *out = p->first;
                    *tail = true;
                    return;
                }
                *out = NULL;
                return;
            }
        case LAMBDA:
            // creating a lambda function
            // [lambda [(arg)...] expr]
            if (numArgs == 2 && ValIsSeq(args->first))
            {
                Val *lArgs = args->first;
                Val *p = lArgs;
                while (p && ValIsSeq(p))
                {
                    if (!ValIsInt(p->first))
                    {
                        LizpError(LE_NO_FUNCTION);
                    }
                    p = p->rest;
                }
                // make form [[lambda args] expr]
                Val *lBody = args->rest;
                *out = ValMakeSeq(ValMakeSeq(ValMakeInt(LAMBDA), lArgs), lBody);
                return;
            }
            break;
    }
    // Catch-all for a macro with invalid arguments
    LizpError(LE_NO_FUNCTION);
}

static Val *EvalEach(Val *ast, Val **env)
{
    assert(ast);
    Val *evAst = ValMakeSeq(EvalAst(ast->first, env), NULL);
    ast = ast->rest;
    Val *p = evAst;
    while (ast)
    {
        p->rest = ValMakeSeq(EvalAst(ast->first, env), NULL);
        p = p->rest;
        ast = ast->rest;
    }
    return evAst;
}

static bool IsSelfEvaluating(Val *ast)
{
    return !ast || ValIsInt(ast) || ValIsStr(ast);
}

// Always create new Val objects
Val *EvalAst(Val *ast, Val **env)
{
    int envCount = 0;
    // Loop is for tail-call optimization
    while (1)
    {
        if (IsSelfEvaluating(ast))
        {
            break;
        }
        if (ValIsSeq(ast))
        {
            if (IsMacro(ast))
            {
                bool tail, pop;
                DoMacro(ast, env, &ast, &tail, &pop);
                if (pop)
                {
                    envCount++;
                }
                if (tail)
                {
                    continue;
                }
                break;
            }
            // Evaluate sub-expressions
            Val *evAst = EvalEach(ast, env);
            assert(evAst);
            // TODO: handle APPLY/EVAL special cases
            // Lambda function call?
            if (ValIsLambda(evAst->first))
            {
                EnvPush(env);
                envCount++;
                ast = ApplyLambda(evAst, env);
                continue;
            }
            // Built-in function call
            ast = ApplyBI(evAst, env);
            break;
        }
        LizpError(LE_INVALID_VAL);
    }
    // Unwrap environment and return final value
    while (envCount > 0)
    {
        EnvPop(env);
        envCount--;
    }
    return ast;
}

_Noreturn void LizpError(int val)
{
    longjmp(jbLizp, val);
}

const char *LizpGetMessage(int val)
{
    _Static_assert(LE_COUNT == 16, "Handle every lizp error in the switch block");
    switch (val)
    {
        case LE_INVALID_INT:
            return "invalid integer value";
        case LE_INVALID_INT_OVERFLOW:
            return "integer overflow";
        case LE_INVALID_INT_DIGIT:
            return "invalid digit for base";
        case LE_INVALID_INT_BASE:
            return "invalid base when printing integer";
        case LE_LIST_UNFINISHED:
            return "unexpected end of string while reading list";
        case LE_BRACKET_MISMATCH:
            return "mismatched ']' closing bracket";
        case LE_UNKNOWN_FUNCTION:
            return "unknown function number";
        case LE_APPLY_NOT_FUNCTION:
            return "first item in list is not a function number";
        case LE_NO_FUNCTION:
            return "invalid arguments for function or macro";
        case LE_INVALID_VAL:
            return "invalid lizp value";
        case LE_UNKNOWN_SYM:
            return "undefined symbol";
        case LE_DIV_ZERO:
            return "division by zero";
        case LE_LET_FORM:
            return "invalid binding list for \"let\"";
        case LE_COND_FORM:
            return "invalid condition and consequence list for \"cond\"";
        case LE_LAMBDA_TOO_MANY_ARGS:
            return "too many arguments passed to lambda function";
        case LE_LAMBDA_TOO_FEW_ARGS:
            return "too few arguments passed to lambda function";
        default:
            return "(unknown error type)";
    }
}

static void LizpPrintMessage(int val)
{
    const char *msg = LizpGetMessage(val);
    fprintf(stderr, "lizp error: %s\n", msg);
}

// Does: Read a form from the stream
// Returns: the form, which may be NULL
Val *read (const char *start, int length)
{
    // Validate inputs
    if (!start || (length < 0))
    {
        return NULL;
    }

    Val *x;
    int len = ReadVal(start, length, &x);

    if (len <= 0)
    {
        return NULL;
    }
    return x;
}

Val *eval (Val *ast, Val **env)
{
    return EvalAst(ast, env);
}

void print (Val *expr, int readable)
{
    static char buffer[2 * 1024];
    int p_len = PrintVal(expr, buffer, sizeof(buffer), readable);
    printf("%.*s", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Val **env)
{
    int val = setjmp(jbLizp);
    if (!val)
    {
        print(eval(read(start, length), env), 1);
        putchar('\n');
    }
    else
    {
        LizpPrintMessage(val);
    }
}

void InitLizp(void)
{
    const int n = 50;
    pool = malloc(sizeof(*pool) * n);
    for (int i = 0; i < n; i++)
    {
        pool[i].first = NULL;
        pool[i].rest = &pool[i + 1];
    }
    pool[n - 1].rest = NULL;
    freelist = pool;
}

