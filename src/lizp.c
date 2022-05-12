#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define STRNDUP_IMPL
#include "strndup.h"
#include "lizp.h"

// Allocate value
Val *AllocVal(void)
{
    return malloc(sizeof(Val));
}

// Free value 
void FreeVal(Val *p)
{
    free(p);
}

// Free value recursively
void FreeValRec(Val *v)
{
    if (IsSeq(v))
    {
        // Sequence or NULL
        Val *p = v;
        Val *n;
        while (p && IsSeq(p))
        {
            FreeValRec(p->first);
            p->first = NULL;
            n = p->rest;
            FreeVal(p);
            p = n;
        }
    }
    else
    {
        // Symbol
        if (v->symbol)
        {
            free(v->symbol);
            v->symbol = NULL;
        }
        FreeVal(v);
    }
}

int IsEqual(Val *x, Val *y)
{
    if (x == NULL || y == NULL)
    {
        return x == y;
    }
    if (IsSym(x))
    {
        char *a = x->symbol;
        char *b = y->symbol;
        while (*a && *b && *a == *b)
        {
            a++;
            b++;
        }
        return *a == *b;
    }
    if (IsSeq(x))
    {
        Val *px = x, *py = y;
        while (px && IsSeq(px) && py && IsSeq(py))
        {
            if (!IsEqual(px->first, py->first))
            {
                break;
            }
            px = px->rest;
            py = py->rest;
        }
        return px == NULL && py == NULL;
    }
    return 0;
}

// Check if a value is a sequence
// NULL is also considered an empty sequence
int IsSeq(Val *p)
{
    return !p || (p && !(p->flag & F_SYM));
}

// Check if a value is a symbol
int IsSym(Val *p)
{
    return p && (p->flag & F_SYM);
}

// Make symbol
// - empty string -> null []
Val *MakeSym(char *s)
{
    if (!s || *s == 0)
    {
        return NULL;
    }
    Val *p = AllocVal();
    if (p)
    {
        p->flag = F_SYM;
        p->symbol = s;
    }
    return p;
}

// Make symbol
// - copies buf to take as a name
// - empty string -> null []
Val *MakeSymCopy(const char *buf, int len)
{
    if (!buf || len <= 0)
    {
        return NULL;
    }
    return MakeSym(strndup(buf, len));
}

// Make sequence
// - first: sym or seq (null included)
// - rest: seq (null included)
Val *MakeSeq(Val *first, Val *rest)
{
    if (rest && !IsSeq(rest))
    {
        return NULL;
    }
    Val *p = AllocVal();
    if (p)
    {
        p->flag = 0;
        p->first = first;
        p->rest = rest;
    }
    return p;
}

// New copy, with no structure-sharing
Val *CopyVal(Val *p)
{
    if (p)
    {
        if (IsSym(p))
        {
            return MakeSymCopy(p->symbol, strlen(p->symbol));
        }
        // Seq
        Val *copy = MakeSeq(CopyVal(p->first), NULL);
        Val *pcopy = copy;
        p = p->rest;
        while (IsSeq(p) && p)
        {
            pcopy->rest = MakeSeq(CopyVal(p->first), NULL);
            pcopy = pcopy->rest;
            p = p->rest;
        }
        return copy;
    }
    return NULL;
}

// String needs quotes?
// Check if a string for a symbol name needs to be quoted
// in order to be printed "readably".
int StrNeedsQuotes(const char *s)
{
    while (*s)
    {
        switch (*s)
        {
            case '[':
            case ']':
            case '\n':
            case '\t':
            case '"':
            case '\\':
                return 1;
            default:
                if (isspace(*s))
                {
                    return 1;
                }
        }
        s++;
    }
    return 0;
}

// Escape a string.
// Modifies the string in-place
int EscapeStr(char *str, int len)
{
    if (!str || len <= 0)
    {
        return 0;
    }
    int i = 0;
    int j = 0;
    while (i < len)
    {
        char c = str[i];
        if (c == '\\')
        {
            i++;
            c = str[i];
            switch (c)
            {
                case 'n':
                    c = '\n';
                    break;
                case 't':
                    c = '\t';
                    break;
            }
        }
        str[j] = c;
        i++;
        j++;
    }
    str[j] = '\0';
    return j;
}

// Read symbol from input stream
static int ReadSym(const char *str, int len, Val **out)
{
    int i = 0;
    // leading space
    while (i < len && isspace(str[i]))
    {
        i++;
    }
    switch (str[i])
    {
        case '\0':
            // No symbol
            return 0;
        case '"':
            // Quoted symbol
            {
                i++;
                const int j = i;
                int done = 0, good = 0; 
                while (!done && i < len)
                {
                    switch (str[i])
                    {
                        case '\0':
                            done = 1;
                            break;
                        case '\\':
                            i++;
                            if (str[i] == '"')
                            {
                                i++;
                            }
                            break;
                        case '"':
                            done = 1;
                            good = 1;
                            i++;
                            break;
                        default:
                            i++;
                            break;
                    }
                }
                if (good)
                {
                    if (out)
                    {
                        int len = i - j - 1;
                        Val *sym = MakeSymCopy(str + j, len);
                        if (sym)
                        {
                            EscapeStr(sym->symbol, len);
                        }
                        *out = sym;
                    }
                    return i;
                }
                else
                {
                    assert(0 && "unexpected end of string error not handled");
                }
                break;
            }
        default:
            // Symbol
            {
                const int j = i;
                int done = 0; 
                while (!done && i < len)
                {
                    switch (str[i])
                    {
                        case '\0':
                        case '"':
                        case '[':
                        case ']':
                            done = 1;
                            break;
                        default:
                            if (isspace(str[i]))
                            {
                                done = 1;
                                break;
                            }
                            i++;
                            break;
                    }
                }
                if (out)
                {
                    *out = MakeSymCopy(str + j, i - j);
                }
                return i;
            }
    }
}

// Read value from input stream
// str = string characters
// len = string length
// out = value to return
// returns number of chars read
int ReadVal(const char *str, int len, Val **out)
{
    if (!out || !str || !len)
    {
        return 0;
    }

    int i = 0;
    // Space
    while (i < len && isspace(str[i]))
    {
        i++;
    }
    switch (str[i])
    {
        case '\0':
            // end of string
            *out = NULL;
            return i;
        case '[':
            // begin list
            {
                i++;
                // space
                while (i < len && isspace(str[i]))
                {
                    i++;
                }
                // elements
                Val *list = NULL;
                if (str[i] != ']')
                {
                    // first item
                    Val *e;
                    int l = ReadVal(str + i, len - i, &e);
                    i += l;
                    if (l)
                    {
                        // Space
                        while (i < len && isspace(str[i]))
                        {
                            i++;
                        }
                        list = MakeSeq(e, NULL);
                        Val *p = list;
                        // rest of items
                        while (i < len && str[i] != ']')
                        {
                            Val *e;
                            int l = ReadVal(str + i, len - i, &e);
                            i += l;
                            if (!l)
                            {
                                break;
                            }
                            // Space
                            while (i < len && isspace(str[i]))
                            {
                                i++;
                            }
                            p->rest = MakeSeq(e, NULL);
                            p = p->rest;
                        }
                    }
                }
                if (str[i] == ']')
                {
                    i++;
                }
                else
                {
                    assert(0 && "unexpected end of list error not handled");
                }
                *out = list;
                return i;
            }
        case ']':
            // end list
            assert(0 && "unmatched bracket error not handled");
            break;
        default:
            // Symbol
            {
                Val *sym = NULL;
                int slen = ReadSym(str + i, len - i, &sym);
                i += slen;
                *out = sym;
                return i;
            }
    }
}

// Prints p to the given `out` buffer.
// Does not do null termination.
// If out is NULL, it just calculates the print length
// Returns: number of chars written
int PrintValBuf(Val *v, char *out, int length, int readable)
{
    // String output count / index
    int i = 0;
    if (IsSeq(v))
    {
        if (out && i < length)
        {
            out[i] = '[';
        }
        i++;
        if (v)
        {
            // first item
            if (out)
            {
                i += PrintValBuf(v->first, out + i, length - i, readable);
            }
            else
            {
                i += PrintValBuf(v->first, NULL, 0, readable);
            }
            v = v->rest;
            while (v)
            {
                // space
                if (out && i < length)
                {
                    out[i] = ' ';
                }
                i++;
                // item
                if (out)
                {
                    i += PrintValBuf(v->first, out + i, length - i, readable);
                }
                else
                {
                    i += PrintValBuf(v->first, NULL, 0, readable);
                }
                v = v->rest;
            }
        }
        if (out && i < length)
        {
            out[i] = ']';
        }
        i++;
    }
    else if (IsSym(v))
    {
        // Symbol
        char *s = v->symbol;
        int quoted = readable && StrNeedsQuotes(s);
        if (quoted)
        {
            // Opening quote
            if (out && i < length)
            {
                out[i] = '"';
            }
            i++;
        }
        // Contents
        while (*s)
        {
            char c = *s;
            if (quoted)
            {
                // escaping
                int esc = 0;
                switch (c)
                {
                    case '\n':
                        c = 'n';
                        esc = 1;
                        break;
                    case '\t':
                        c = 't';
                        esc = 1;
                        break;
                    case '"':
                        c = '"';
                        esc = 1;
                        break;
                    case '\\':
                        c = '\\';
                        esc = 1;
                        break;
                }
                if (esc)
                {
                    if (out && i < length)
                    {
                        out[i] = '\\';
                    }
                    i++;
                }
            }
            if (out && i < length)
            {
                out[i] = c;
            }
            i++;
            s++;
        }
        if (quoted)
        {
            // Closing quote
            if (out && i < length)
            {
                out[i] = '"';
            }
            i++;
        }
    }
    else
    {
        assert(0 && "invalid Val type");
    }
    return i;
}

// Print value to a new string
char *PrintValStr(Val *v, int readable)
{
    int len1 = PrintValBuf(v, NULL, 0, readable);
    if (len1 <= 0)
    {
        return NULL;
    }
    char *new = malloc(len1 + 1);
    if (new)
    {
        int len2 = PrintValBuf(v, new, len1, readable);
        (void)len2;
        new[len1] = '\0';
    }
    return new;
}

// Print value to a file
void PrintValFile(FILE *f, Val *v)
{
    char *s = PrintValStr(v, 1);
    fprintf(f, "%s", s);
    free(s);
}

