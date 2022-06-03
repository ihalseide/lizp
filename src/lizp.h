#ifndef _lizp_h_
#define _lizp_h_

// Lizp core functions requires evaluation
#ifdef LIZP_CORE_FUNCTIONS
#define LIZP_EVAL

typedef struct Val Val;
typedef enum ValKind ValKind;
typedef Val *LizpFunc(Val *args);
typedef struct FuncRecord FuncRecord;

enum ValKind
{
    VK_SYMBOL,
    VK_LIST,
};

struct Val
{
    ValKind kind;
    union
    {
        void *any;
        char *symbol;
        struct
        {
            Val *first;
            Val *rest;
        };
    };
};

struct FuncRecord
{
    const char *name;
    const char *form;
    LizpFunc *func;
};

Val *AllocVal(void);
Val *CopyVal(Val *p);
void FreeVal(Val *p);
void FreeValRec(Val *p);

Val *MakeList(Val *first, Val *rest);
Val *MakeSym(char *s);
Val *MakeSymCopy(const char *name, int len);
Val *MakeSymInt(long n);
Val *MakeSymStr(const char *s);

int IsEqual(Val *x, Val *y);
int IsError(Val *v);
int IsInt(Val *v);
int IsList(Val *v);
int IsSym(Val *v);
int ListLength(Val *l);
ValKind KindOf(Val *v);

int ReadVal(const char *start, int length, Val **out);
int PrintValBuf(Val *p, char *out, int length, int readable);
char *PrintValStr(Val *p, int readable);

#endif /* LIZP_CORE_FUNCTIONS */
#ifdef LIZP_EVAL

Val *MakeTrue(void);
Val *MakeError(Val *rest);
Val *MakeErrorMessage(const char *msg);
Val *MakeFalse(void);

int IsTrue(Val *v);
int IsFunc(Val *v);
int IsLambda(Val *v);

int MatchArgs(const char *form, Val *args, Val **err);

Val *Eval(Val *ast, Val *env);
int EnvGet(Val *env, Val *key, Val **out);
int EnvSet(Val *env, Val *key, Val *val);
int EnvSetFunc(Val *env, const char *name, LizpFunc * func);
int EnvSetFuncEx(Val *env, const char *name, const char *form, LizpFunc *f);
int EnvSetSym(Val *env, const char *symbol, Val *val);
void EnvPop(Val *env);
void EnvPush(Val *env);

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS
// For optional core Lizp functions that would be useful for most lizp scripts

void LizpRegisterCoreFuncs(Val *env);
Val *Lreverse(Val *args);    // [reverse list] reverse a list
Val *Lconcat(Val *args);     // [concat list.1 (list.N)...] concatenate lists together
Val *Ljoin(Val *args);       // [join separator (list)...] join together each list with the separator list in between
Val *Lwithout(Val *args);    // [without item list] remove all occurrences of item from the list
Val *Lreplace(Val *args);    // [replace item1 item2 list] replace all occurrences of item1 in list with item2
Val *Lreplace1(Val *args);   // [replaceN item1 item2 list n] replace up to n of item1 with item2 in list
Val *LreplaceI(Val *args);   // [replaceI index item list] replace element in list at index with item
Val *Lzip(Val *args);        // [zip list.1 (list.N)...]
Val *Lappend(Val *args);     // [append val list]
Val *Lprepend(Val *args);    // [prepend val list]
Val *Lprint(Val *args);      // [print (v)...]
Val *Lplus(Val *args);       // [+ (e:integer)...] sum
Val *Lmultiply(Val *args);   // [+ (e:integer)...] product
Val *Lsubtract(Val *args);   // [- x:int (y:int)] subtraction
Val *Ldivide(Val *args);     // [/ x:int y:int] division
Val *Lmod(Val *args);        // [% x:int y:int] modulo
Val *Lequal(Val *args);      // [= x y (expr)...] check equality
Val *Lnot(Val *args);        // [not expr] boolean not
Val *Lsymbol_q(Val *args);   // [symbol? val] check if value is a symbol
Val *Linteger_q(Val *args);  // [integer? val] check if value is a integer symbol
Val *Llist_q(Val *args);     // [list? val] check if value is a list
Val *Lempty_q(Val *args);    // [empty? val] check if value is a the empty list
Val *Lnth(Val *args);        // [nth index list] get the nth item in a list
Val *Llist(Val *args);       // [list (val)...] create list from arguments (variadic)
Val *Llength(Val *args);     // [length list]
Val *Llambda_q(Val *args);   // [lambda? v]
Val *Lfunction_q(Val *args); // [function? v]
Val *Lnative_q(Val *args);   // [native? v]
Val *Lincreasing(Val *args); // [<= x y (expr)...] check number order
Val *Ldecreasing(Val *args); // [>= x y (expr)...] check number order
Val *Lstrictly_increasing(Val *args);   // [< x y (expr)...] check number order
Val *Lstrictly_decreasing(Val *args);   // [> x y (expr)...] check number order
Val *Lchars(Val *args);      // [chars sym] -> list
Val *Lsymbol(Val *args);     // [symbol list] -> symbol
Val *Lmember_q(Val *args);   // [member? item list]
Val *Lcount(Val *args);      // [count item list] -> int
Val *Lposition(Val *args);   // [position item list] -> list
Val *Lslice(Val *args);      // [slice list start (end)] gets a sublist "slice" inclusive of start and end

#endif /* LIZP_CORE_FUNCTIONS */
#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stb_ds.h"

// Dynamic array of function pointers
FuncRecord *da_funcs;

// Put function in the dynamic array
// Meant to be used by `EnvSetFunc` to bind native functions
size_t PutFunc(const char *name, const char *form, LizpFunc *func)
{
    FuncRecord new = (FuncRecord)
    {
        .name = name,
        .form = form,
        .func = func,
    };
    size_t id = arrlen(da_funcs);
    arrput(da_funcs, new);
    return id;
}

// Get function from the dynamic array
// Meant to be used by `Apply` to look up native functions
FuncRecord *GetFunc(size_t id)
{
    size_t len = arrlen(da_funcs);
    if (id < 0 || id >= len)
    {
        return NULL;
    }
    return &(da_funcs[id]);
}

// Allocate a new value
Val *AllocVal(void)
{
    Val *p = malloc(sizeof(Val));
    return p;
}

// Free value
void FreeVal(Val *p)
{
    if (IsSym(p) && p->symbol)
    {
        free(p->symbol);
    }
    free(p);
}

// Free value recursively
void FreeValRec(Val *v)
{
    if (!v)
    {
        // NULL
        return;
    }
    if (IsSym(v))
    {
        // Symbol
        FreeVal(v);
        return;
    }
    // List
    assert(IsList(v));
    Val *p = v;
    Val *n;
    while (p && IsList(p))
    {
        FreeValRec(p->first);
        n = p->rest;
        FreeVal(p);
        p = n;
    }
    return;
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
    if (IsList(x))
    {
        Val *px = x, *py = y;
        while (px && IsList(px) && py && IsList(py))
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

// Get a value's kind a.k.a type
ValKind KindOf(Val *v)
{
    if (!v)
    {
        return VK_LIST;
    }
    return v->kind;
}

// Check if a value is a list
// NULL is also considered an empty list
int IsList(Val *p)
{
    return KindOf(p) == VK_LIST;
}

// Check if a value is a symbol
int IsSym(Val *p)
{
    return KindOf(p) == VK_SYMBOL;
}

//  check if a value is a integer symbol
int IsInt(Val *v)
{
    if (!IsSym(v))
    {
        return 0;
    }
    char *end;
    int base = 10;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}

// Make symbol
// NOTE: does not make a copy of the `s` string
Val *MakeSym(char *s)
{
    assert(s);
    Val *p = AllocVal();
    if (p)
    {
        p->kind = VK_SYMBOL;
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
    char *new = malloc(len + 1);
    memcpy(new, buf, len);
    new[len] = 0;
    return MakeSym(new);
}

// Make symbol by copying the null-terminated `str`
Val *MakeSymStr(const char *str)
{
    int len = strlen(str);
    char *new = malloc(len + 1);
    memcpy(new, str, len);
    new[len] = 0;
    return MakeSym(new);
}

// Make a symbol for an integer
Val *MakeSymInt(long n)
{
    const char *fmt = "%ld";
    const int sz = snprintf(NULL, 0, fmt, n);
    char buf[sz + 1];
    snprintf(buf, sizeof(buf), fmt, n);
    return MakeSymCopy(buf, sz);
}

// Make a proper list.
// - first: sym or list (null included)
// - rest: list (NULL included)
// NOTE: rest must be NULL or a list Val.
Val *MakeList(Val *first, Val *rest)
{
    if (!IsList(rest))
    {
        return NULL;
    }
    Val *p = AllocVal();
    if (p)
    {
        p->kind = VK_LIST;
        p->first = first;
        p->rest = rest;
    }
    return p;
}

// New copy, with no structure-sharing
Val *CopyVal(Val *p)
{
    if (!p)
    {
        return p;
    }
    if (!IsList(p))
    {
        return MakeSymStr(p->symbol);
    }
    // List
    Val *copy = MakeList(CopyVal(p->first), NULL);
    Val *pcopy = copy;
    p = p->rest;
    while (IsList(p) && p)
    {
        pcopy->rest = MakeList(CopyVal(p->first), NULL);
        pcopy = pcopy->rest;
        p = p->rest;
    }
    return copy;
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
            case '(':
            case ')':
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
                        case '\n':
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
                if (good && out)
                {
                    // Make escaped symbol string
                    int len = i - j - 1;
                    if (len == 0)
                    {
                        *out = NULL;
                        return i;
                    }
                    assert(len > 0);
                    char *str1 = malloc(len + 1);
                    memcpy(str1, str + j, len);
                    str1[len] = 0;
                    int len2 = EscapeStr(str1, len);
                    *out = MakeSymCopy(str1, len2);
                    free(str1);
                    return i;
                }
                // invalid
                if (out)
                {
                    *out = NULL;
                }
                return i;
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
                        case '(':
                        case ')':
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

// Skip space and nested comments within `str`
// Returns the next index into `str`
int SkipChars(const char *str, int len)
{
    int i = 0;
    int level = 0; // nesting level
    while (i < len)
    {
        char c = str[i];
        if (!c)
        {
            break;
        }
        if (c == '(')
        {
            i++;
            level++;
            continue;
        }
        if (level && c == ')')
        {
            i++;
            level--;
            continue;
        }
        if (!level && !isspace(c))
        {
            break;
        }
        i++;
    }
    return i;
}

// Read value from input stream
// str = string characters
// len = string length
// out = value to return
// returns the number of chars read
int ReadVal(const char *str, int len, Val **out)
{
    if (!out || !str || !len)
    {
        return 0;
    }
    int i = 0;
    while (1)
    {
        i += SkipChars(str + i, len - i);
        switch (str[i])
        {
            case '\0': // end of string
            case ']': // unmatched list
            case '(': // uncaught comment
            case ')': // uncaught comment
                *out = NULL;
                return i;
            case '[':
                // begin list
                {
                    i++;
                    // empty list?
                    i += SkipChars(str + i, len - i);
                    if (str[i] == ']')
                    {
                        i++;
                        *out = NULL;
                        return i;
                    }
                    // first item
                    Val *e;
                    int l = ReadVal(str + i, len - i, &e);
                    if (!l)
                    {
                        *out = NULL;
                        return i;
                    };
                    i += l;
                    Val *list = MakeList(e, NULL);
                    Val *p = list;
                    // rest of items
                    while (i < len && str[i] != ']')
                    {
                        Val *e;
                        int l = ReadVal(str + i, len - i, &e);
                        i += l;
                        p->rest = MakeList(e, NULL);
                        p = p->rest;
                        if (l <= 0)
                        {
                            *out = list;
                            return l;
                        }
                        i += SkipChars(str + i, len - i);
                    }
                    *out = list;
                    if (str[i] == ']')
                    {
                        i++;
                        i += SkipChars(str + i, len - i);
                    }
                    return i;
                }
            default:
                // Symbol
                {
                    Val *sym = NULL;
                    int slen = ReadSym(str + i, len - i, &sym);
                    i += slen;
                    *out = sym;
                    i += SkipChars(str + i, len - i);
                    return i;
                }
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
    if (IsList(v))
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
        assert(len1 == len2);
        new[len1] = '\0';
    }
    return new;
}

// Get the length of a list.
// Returns 0 for a non-list value.
int ListLength(Val *l)
{
    int len = 0;
    while (l && IsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}

// Check if two values do not share structure
int IsSeparate(Val *a, Val *b)
{
    if (!a || !b)
    {
        return 1;
    }
    if (a == b)
    {
        return 0;
    }
    if (IsSym(a) && IsSym(b))
    {
        return a->symbol != b->symbol;
    }
    if (IsList(a) && IsList(b))
    {
        return IsSeparate(a->first, b->first)
            && IsSeparate(a->first, b->rest)
            && IsSeparate(a->rest, b->first)
            && IsSeparate(a->rest, b->rest);
    }
    // symbol and list
    // make sure `a` is list and `b` is symbol
    if (IsSym(a))
    {
        // swap a and b
        Val *t = b;
        b = a;
        a = t;
    }
    return IsSeparate(a->first, b)
        && IsSeparate(a->rest, b);
}

#ifdef LIZP_EVAL

static int Match1Arg(char c, Val *arg, Val **err)
{
    switch (c)
    {
        case 'v':
            // any value
            return 1;
        case 'l':
            // list
            if (IsList(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = MakeSymStr("should be a list");
            }
            return 0;
        case 'L':
            // non-empty list
            if (arg && IsList(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = MakeSymStr("should be a non-empty list");
            }
            return 0;
        case 's':
            // symbol
            if (IsSym(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = MakeSymStr("should be a symbol");
            }
            return 0;
        case 'n':
            // symbol for number/integer
            if (IsInt(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = MakeSymStr("should be a symbol for an integer");
            }
            return 0;
        default:
            // error
            assert(0 && "should be caught by the caller");
            return 0;
    }
}

// Match Arguments
// Check if the `args` list matches the given `form`
// If the `args` do not match, then `err` is set to a new value (which can be
//   passed to `MakeError`)
// Meanings for characters in the `form` string:
// - "v" : any value
// - "l" : a list
// - "s" : a symbol
// - "L" : a non-empty list
// - "n" : an integer symbol (number)
// - "(" : mark the rest of the arguments as optional. must be last
// - "&" : variadic, mark the rest of the arguments as optional and all with the same type of the
//   very next character. must be last
int MatchArgs(const char *form, Val *args, Val **err)
{
    int i = 0;
    int optional = 0;
    Val *p = args;
    while (form[i] && (optional? (p != NULL) : 1))
    {
        switch (form[i])
        {
        case 'v':
        case 'l':
        case 's':
        case 'L':
        case 'n':
            // types
            if (!p)
            {
                if (err)
                {
                    int n = strcspn(form, "&(");
                    char *arguments = (n == 1)? "argument" : "arguments";
                    *err = MakeList(MakeSymStr("not enough arguments: requires at least"),
                                    MakeList(MakeSymInt(n),
                                             MakeList(MakeSymStr(arguments),
                                                      NULL)));
                }
                return 0;
            }
            if (!Match1Arg(form[i], p->first, err))
            {
                if (err)
                {
                    // wrap message with more context
                    *err = MakeList(MakeSymStr("argument"),
                                    MakeList(MakeSymInt(i + 1),
                                             MakeList(*err,
                                                      NULL)));
                }
                return 0;
            }
            p = p->rest;
            i++;
            break;
        case '(':
            // optional marker
            if (optional)
            {
                if (err)
                {
                    *err = MakeSymStr(
                        "`MatchArgs` invalid `form` string: optional marker '(' may only appear once");
                }
                return 0;
            }
            optional = 1;
            i++;
            break;
        case '&':
            // variadic marker
            i++;
            switch (form[i])
            {
            case 'v':
            case 'l':
            case 's':
            case 'L':
            case 'n':
                // the rest of the arguments should match the given type
                while (p)
                {
                    if (!Match1Arg(form[i], p->first, err))
                    {
                        return 0;
                    }
                    p = p->rest;
                }
                i++;
                if (!form[i])
                {
                    return 1;
                }
                if (err)
                {
                    *err = MakeSymStr(
                        "`MatchArgs` invalid `form` string: '&' requires a only directive after it");
                }
                return 1;
            default:
                // invalid char
                if (err)
                {
                    *err = MakeSymStr(
                        "`MatchArgs` invalid `form` string: '&' requires a directive after it");
                }
                return 0;
            }
        default:
            // invalid char
            if (err)
            {
                *err = MakeSymStr(
                    "`MatchArgs` invalid `form` string: requires a directive");
            }
            return 0;
        }
    }
    if (p)
    {
        if (err)
        {
            int n = strlen(form) - (optional? 1 : 0);
            char *arguments = (n == 1) ? "argument" : "arguments";
            *err = MakeList(MakeSymStr("too many arguments, requires at most"),
                            MakeList(MakeSymInt(n),
                                     MakeList(MakeSymStr(arguments),
                                              NULL)));
        }
        return 0;
    }
    return 1;
}

// Check whether a value is considered as true
int IsTrue(Val *v)
{
    // "false" and [] are set to be the only false values
    if (!v)
    {
        return 0;
    }
    if (IsSym(v) && !strcmp(v->symbol, "false"))
    {
        return 0;
    }
    return 1;
}

Val *MakeTrue(void)
{
    return MakeSymCopy("true", 4);
}

Val *MakeFalse(void)
{
    return MakeSymCopy("false", 5);
}

// make a list of the form [error rest...]
Val *MakeError(Val *rest)
{
    Val *e = MakeSymStr("error");
    if (IsList(rest))
    {
        return MakeList(e, rest);
    }
    return MakeList(e, MakeList(rest, NULL));
}

Val *MakeErrorMessage(const char *msg)
{
    return MakeError(MakeSymStr(msg));
}

// Check whether a value is a lambda value (special list)
int IsLambda(Val *v)
{
    if (!v || !IsList(v))
    {
        return 0;
    }
    Val *l = v->first;
    if (!l || !IsSym(l))
    {
        return 0;
    }
    if (strcmp(l->symbol, "lambda"))
    {
        return 0;
    }
    if (!v->rest)
    {
        return 0;
    }
    Val *params = v->rest->first;
    if (!IsList(params))
    {
        return 0;
    }
    Val *pp = params;
    while (pp && IsList(pp))
    {
        if (!IsSym(pp->first))
        {
            return 0;
        }
        pp = pp->rest;
    }
    if (!v->rest->rest)
    {
        return 0;
    }
    return 1;
}

// Check if a list is a wrapper for a C function
// (a "native func")
// [func number]
int IsFunc(Val *v)
{
    if (!v || !IsList(v))
    {
        return 0;
    }
    Val *sym = v->first;
    if (!sym || !IsSym(sym) || strcmp(sym->symbol, "native func"))
    {
        return 0;
    }
    if (!v->rest)
    {
        return 0;
    }
    Val *id = v->rest->first;
    if (!IsSym(id))
    {
        return 0;
    }
    if (v->rest->rest)
    {
        // too many items
        return 0;
    }
    return 1;
}

// check if the value matches the form [error ...]
int IsError(Val *v)
{
    if (!v || !IsList(v))
    {
        return 0;
    }
    Val *first = v->first;
    return IsSym(first) && !strcmp("error", first->symbol);
}

// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
int EnvSet(Val *env, Val *key, Val *val)
{
    if (!env || !IsList(env))
    {
        return 0;
    }
    Val *pair = MakeList(key, MakeList(val, NULL));
    if (!pair)
    {
        return 0;
    }
    // push key-value pair onto the front of the list
    env->first = MakeList(pair, env->first);
    return 1;
}

// Get value in environment, does not return a copy
int EnvGet(Val *env, Val *key, Val **out)
{
    if (!env)
    {
        return 0;
    }
    Val *scope = env;
    while (scope && IsList(scope))
    {
        Val *p = scope->first;
        while (p && IsList(p))
        {
            Val *pair = p->first;
            if (pair && IsList(pair) && IsEqual(pair->first, key))
            {
                // found
                if (!pair->rest)
                {
                    // env is improperly set up
                    return 0;
                }
                if (out)
                {
                    *out = pair->rest->first;
                }
                return 1;
            }
            p = p->rest;
        }
        // outer scope
        scope = scope->rest;
    }
    return 0;
}

// push a new context onto the environment
void EnvPush(Val *env)
{
    if (!env)
    {
        return;
    }
    env->rest = MakeList(env->first, env->rest);
    env->first = NULL;
}

// pop off the latest context from the environment
void EnvPop(Val *env)
{
    if (!env)
    {
        return;
    }
    FreeValRec(env->first);
    Val *pair = env->rest;
    env->first = pair->first;
    env->rest = pair->rest;
    // Only free the one pair
    FreeVal(pair);
}

// Evaluate a macro
// Returns whether `first` is a symbol for a macro. If it is a macro,
// the macro code is executed, otherwise this function does nothing.
// NOTE: Return values must not share structure with first, args, or env
static int Macro(Val *first, Val *args, Val *env, Val **out)
{
    char *s = first->symbol;
    Val *err;
    if (!strcmp("defined?", s))
    {
        // [defined? v]
        if (!MatchArgs("s", args, &err))
        {
            assert(IsList(err));
            *out = MakeError(MakeList(CopyVal(first), err));
            return 1;
        }
        Val *sym = args->first;
        *out = EnvGet(env, sym, NULL)? MakeTrue() : MakeFalse();
        return 1;
    }
    if (!strcmp("get", s))
    {
        // [get key (if-undefined)] for getting value with a default of if-undefined
        if (!MatchArgs("s(v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *key = args->first;
        Val *val;
        if (EnvGet(env, key, &val))
        {
            *out = CopyVal(val);
            return 1;
        }
        if (args->rest)
        {
            // if-undefined default value
            *out = CopyVal(args->rest->first);
            return 1;
        }
        // default is error
        *out = MakeErrorMessage("undefined symbol");
        return 1;
    }
    if (!strcmp("let", s))
    {
        // [let [(key val)...] (expr)]
        if (!MatchArgs("lv", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *bindings = args->first;
        Val *body = args->rest->first;
        // create and check bindings
        EnvPush(env);
        Val *p_binds = bindings;
        while (p_binds && IsList(p_binds))
        {
            Val *sym = p_binds->first;
            if (!IsSym(sym) || !p_binds->rest || !IsList(p_binds->rest))
            {
                // invalid symbol or uneven amount of args
                EnvPop(env);
                *out = MakeErrorMessage("`let` bindings list must consist of alternating symbols and expressions");
                return 1;
            }
            p_binds = p_binds->rest;
            Val *expr = p_binds->first;
            Val *val = Eval(expr, env);
            if (IsError(val))
            {
                // eval error
                *out = val;
                EnvPop(env);
                return 1;
            }
            EnvSet(env, CopyVal(sym), val);
            p_binds = p_binds->rest;
        }
        // eval body
        *out = Eval(body, env);
        // destroy bindings
        EnvPop(env);
        return 1;
    }
    if (!strcmp("if", s))
    {
        // [if condition consequent (alternative)]
        if (!MatchArgs("vv(v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *f = Eval(args->first, env);
        if (IsError(f))
        {
            // eval error
            *out = f;
            return 1;
        }
        int t = IsTrue(f);
        FreeValRec(f);
        if (t)
        {
            Val *consequent = args->rest->first;
            *out = Eval(consequent, env);
            return 1;
        }
        Val *alt_list = args->rest->rest;
        if (!alt_list)
        {
            // no alternative
            *out = NULL;
            return 1;
        }
        Val *alternative = alt_list->first;
        *out = Eval(alternative, env);
        return 1;
    }
    if (!strcmp("quote", s))
    {
        // [quote expr]
        if (!MatchArgs("v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        *out = CopyVal(args->first);
        return 1;
    }
    if (!strcmp("do", s))
    {
        // [do (expr)...]
        Val *p = args;
        Val *e = NULL;
        while (p && IsList(p))
        {
            e = Eval(p->first, env);
            if (IsError(e))
            {
                // eval error
                *out = e;
                return 1;
            }
            p = p->rest;
            if (p)
            {
                // free all values except the last one
                FreeValRec(e);
            }
        }
        *out = e;
        return 1;
    }
    if (!strcmp("and", s))
    {
        // [and expr1 (expr)...]
        if (!MatchArgs("v&v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *p = args;
        while (p && IsList(p))
        {
            Val *e = Eval(p->first, env);
            if (IsError(e))
            {
                *out = e;
                return 1;
            }
            if (!IsTrue(e))
            {
                // item is false
                *out = e;
                return 1;
            }
            p = p->rest;
            if (!p)
            {
                // last item is true
                *out = e;
                return 1;
            }
            FreeValRec(e);
        }
        // malformed list
        *out = NULL;
        return 1;
    }
    if (!strcmp("or", s))
    {
        // [or expr1 (expr)...]
        if (!MatchArgs("v&v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *p = args;
        while (p && IsList(p))
        {
            Val *e = Eval(p->first, env);
            if (IsError(e))
            {
                *out = e;
                return 1;
            }
            if (IsTrue(e))
            {
                // item is true
                *out = e;
                return 1;
            }
            p = p->rest;
            if (!p)
            {
                // last item is false
                *out = e;
                return 1;
            }
            FreeValRec(e);
        }
        // malformed list
        *out = NULL;
        return 1;
    }
    if (!strcmp("cond", s))
    {
        // [cond (condition result)...] (no nested lists)
        if (!MatchArgs("vv&v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        if (ListLength(args) % 2 != 0)
        {
            *out = MakeErrorMessage("`cond` requires an even amount of"
                                    " alternating condition expressions and"
                                    " consequence expressions");
            return 1;
        }
        Val *p = args;
        while (p && IsList(p))
        {
            Val *e = Eval(p->first, env);
            if (IsError(e))
            {
                *out = e;
                return 1;
            }
            if (IsTrue(e))
            {
                FreeValRec(e);
                assert(p->rest);
                *out = Eval(p->rest->first, env);
                return 1;
            }
            FreeValRec(e);
            p = p->rest;
            assert(p);
            p = p->rest;
        }
        // no condition matched
        *out = NULL;
        return 1;
    }
    if (!strcmp("lambda", s))
    {
        // [lambda [(symbol)...] (expr)]
        if (!MatchArgs("l(v", args, &err))
        {
            *out = MakeError(err);
            return 1;
        }
        Val *params = args->first;
        if (!IsList(params))
        {
            *out = MakeErrorMessage("`lambda` first argument must be a list of"
                                    " symbols");
            return 1;
        }
        Val *p = params;
        // params must be symbols
        while (p && IsList(p))
        {
            Val *e = p->first;
            if (!IsSym(e))
            {
                *out = MakeError(MakeList(CopyVal(e), MakeList(
                                MakeSymStr("is not a symbol"), NULL)));
                return 1;
            }
            p = p->rest;
        }
        // make lambda... with an explicit NULL body if a body is not provided
        Val *body = args->rest;
        if (body)
        {
            body = body->first;
        }
        *out = MakeList(CopyVal(first), MakeList(CopyVal(params),
                MakeList(CopyVal(body), NULL)));
        return 1;
    }
    // not a macro
    return 0;
}

// Return values must not share structure with first, args, or env
static Val *ApplyLambda(Val *first, Val *args)
{
    Val *params = first->rest->first;
    Val *body = first->rest->rest->first;
    // push env
    Val *env = MakeList(NULL, NULL);
    // bind values
    Val *p_params = params;
    Val *p_args = args;
    while (p_params && IsList(p_params) && p_args && IsList(p_args))
    {
        Val *param = p_params->first;
        // parameter beginning with '&' binds the rest of the arguments
        if ('&' == param->symbol[0])
        {
            if (p_params->rest)
            {
                // error: not the last parameter
                FreeValRec(env);
                return NULL;
            }
            EnvSet(env, CopyVal(param), CopyVal(p_args));
            // p_params and p_args will both be non-null
            break;
        }
        // normal parameter
        EnvSet(env, CopyVal(param), CopyVal(p_args->first));
        p_params = p_params->rest;
        p_args = p_args->rest;
    }
    // check a parameters-arguments arity mismatch
    if ((p_params == NULL) != (p_args == NULL))
    {
        // error
        FreeValRec(env);
        return NULL;
    }
    Val *result = Eval(body, env);
    assert(IsSeparate(result, env));
    assert(IsSeparate(result, args));
    FreeValRec(env);
    return result;
}

static Val *ApplyNative(Val *first, Val *args)
{
    long id = atol(first->rest->first->symbol);
    FuncRecord *record = GetFunc(id);
    if (!record || !record->func)
    {
        // error: invalid function id or pointer
        return MakeError(MakeList(CopyVal(first),
                                  MakeList(MakeSymStr("is not a native function id"),
                                           NULL)));
    }
    if (record->form)
    {
        Val *err;
        int match = MatchArgs(record->form, args, &err);
        if (!match && record->name)
        {
            return MakeError(MakeList(MakeSymStr("native function"),
                                      MakeList(MakeSymStr(record->name),
                                               err)));
        }
        if (!match)
        {
            return MakeError(MakeList(MakeSymStr("native function #"),
                                      MakeList(MakeSymInt(id),
                                               err)));
        }
    }
    Val *result = record->func(args);
    assert(IsSeparate(result, args));
    if (IsError(result))
    {
        // patch-in more function info
        Val *info;
        if (record->func)
        {
            info = MakeList(MakeSymStr("native function "),
                            MakeList(MakeSymStr(record->name),
                                     result->rest));
            result->rest = info;
            return result;
        }
        // no name
        info = MakeList(MakeSymStr("native function #"),
                        MakeList(MakeSymInt(id),
                                 result->rest));
        result->rest = info;
        return result;
    }
    return result;
}

// Apply functions
// Return values must not share structure with first, args, or env
Val *Apply(Val *first, Val *args)
{
    if (IsLambda(first))
    {
        return ApplyLambda(first, args);
    }
    if (IsFunc(first))
    {
        return ApplyNative(first, args);
    }
    // invalid function
    return MakeError(MakeList(CopyVal(first),
                              MakeList(MakeSymStr("is not a function"),
                                       NULL)));
}

// Evaluate a Val value
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs for bindings
// Returns the evaluated value
// NOTE: must only return new values that do not share any
//       structure with the ast or the env
Val *Eval(Val *ast, Val *env)
{
    if (!ast)
    {
        // empty list
        return ast;
    }
    if (IsSym(ast))
    {
        // lookup symbol value
        Val *val;
        if (EnvGet(env, ast, &val))
        {
            return CopyVal(val);
        }
        // a symbol evaluates to itself if not found
        return CopyVal(ast);
    }
    if (IsLambda(ast))
    {
        // lambda values are self-evaluating
        return CopyVal(ast);
    }
    // eval lists...
    assert(ast);
    assert(IsList(ast));
    // eval first element
    Val *first = Eval(ast->first, env);
    assert(IsSeparate(first, ast->first));
    assert(IsSeparate(first, env));
    if (IsError(first))
    {
        return first;
    }
    // macro?
    if (IsSym(first))
    {
        Val *result;
        if (Macro(first, ast->rest, env, &result))
        {
            assert(IsSeparate(result, first));
            assert(IsSeparate(result, env));
            assert(IsSeparate(result, ast->rest));
            return result;
        }
    }
    // not a macro
    // eval rest of elements for apply
    Val *list = MakeList(first, NULL);
    Val *p_list = list;
    Val *p_ast = ast->rest;
    while (p_ast && IsList(p_ast))
    {
        Val *e = Eval(p_ast->first, env);
        assert(IsSeparate(e, ast));
        assert(IsSeparate(e, env));
        if (IsError(e))
        {
            FreeValRec(list);
            return e;
        }
        p_list->rest = MakeList(e, NULL);
        p_list = p_list->rest;
        p_ast = p_ast->rest;
    }
    Val *result = Apply(first, list->rest);
    assert(IsSeparate(result, list));
    assert(IsSeparate(result, env));
    assert(IsSeparate(result, ast));
    FreeValRec(list);
    return result;
}

// Environment Set Function Extended.
// Set a symbol value to be associated with a C function
// Also, use the form string to always check the arguments before the function
// is called (see `MatchArgs`).
int EnvSetFuncEx(Val *env, const char *name, const char *form, LizpFunc *func)
{
    if (!env || !name || !func)
    {
        return 0;
    }
    Val *key = MakeSymStr(name);
    if (!key)
    {
        return 0;
    }
    long handle = PutFunc(name, form, func);
    Val *val = MakeList(MakeSymCopy("native func", 11), MakeList(MakeSymInt(handle), NULL));
    if (!val)
    {
        return 0;
    }
    int success = EnvSet(env, key, val);
    if (!success)
    {
        FreeValRec(key);
        FreeValRec(val);
        return 0;
    }
    return success;
}

// Environment Set Function.
// Set a symbol value to be associated with a C function
int EnvSetFunc(Val *env, const char *name, LizpFunc *func)
{
    return EnvSetFuncEx(env, name, NULL, func);
}

// Environment Set Symbol
int EnvSetSym(Val *env, const char *sym, Val *v)
{
    if (!env || !sym)
    {
        return 0;
    }
    return EnvSet(env, MakeSymStr(sym), v);
}

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS
void LizpRegisterCoreFuncs(Val *env)
{
    EnvSetFunc(env, "+", Lplus);
    EnvSetFunc(env, "*", Lmultiply);
    EnvSetFunc(env, "/", Ldivide);
    EnvSetFunc(env, "-", Lsubtract);
    EnvSetFunc(env, "%", Lmod);
    EnvSetFunc(env, "=", Lequal);
    EnvSetFunc(env, "<=", Lincreasing);
    EnvSetFunc(env, ">=", Ldecreasing);
    EnvSetFunc(env, "<", Lstrictly_increasing);
    EnvSetFunc(env, ">", Lstrictly_decreasing);
    EnvSetFunc(env, "empty?", Lempty_q);
    EnvSetFunc(env, "member?", Lmember_q);
    EnvSetFunc(env, "symbol?", Lsymbol_q);
    EnvSetFunc(env, "integer?", Linteger_q);
    EnvSetFunc(env, "list?", Llist_q);
    EnvSetFunc(env, "lambda?", Llambda_q);
    EnvSetFunc(env, "function?", Lfunction_q);
    EnvSetFunc(env, "native?", Lnative_q);
    EnvSetFunc(env, "chars", Lchars);
    EnvSetFunc(env, "symbol", Lsymbol);
    EnvSetFunc(env, "list", Llist);
    EnvSetFunc(env, "count", Lcount);
    EnvSetFunc(env, "position", Lposition);
    EnvSetFunc(env, "slice", Lslice);
    EnvSetFunc(env, "length", Llength);
    EnvSetFunc(env, "not", Lnot);
    EnvSetFunc(env, "nth", Lnth);
    EnvSetFunc(env, "prepend", Lprepend);
    EnvSetFunc(env, "append", Lappend);
}

// [reverse list]
Val *Lreverse(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [concat list.1 (list.N)...]
Val *Lconcat(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [join separator (list)...]
Val *Ljoin(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [without item list]
Val *Lwithout(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [replace item1 item2 list]
Val *Lreplace(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [replaceN item1 item2 list n]
Val *Lreplace1(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [replaceI index item list]
Val *LreplaceI(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [zip list.1 (list.N)...]
Val *Lzip(Val *args)
{
    // TODO: implement
    assert(0 && "not implemented yet");
}

// [append val list]
Val *Lappend(Val *args)
{
    Val *err;
    if (!MatchArgs("vl", args, &err))
    {
        return MakeError(err);
    }
    Val *v = args->first;
    Val *list = args->rest->first;
    Val *last = MakeList(CopyVal(v), NULL);
    if (!list)
    {
        // empty list -> single-item list
        return last;
    }
    // Create a new list and put "last" at the end
    Val *new = CopyVal(list);
    Val *p = new;
    while (p->rest)
    {
        p = p->rest;
    }
    p->rest = last;
    return new;
}

// [prepend val list]
Val *Lprepend(Val *args)
{
    Val *err;
    if (!MatchArgs("vl", args, &err))
    {
        return MakeError(err);
    }
    Val *v = args->first;
    Val *list = args->rest->first;
    return MakeList(CopyVal(v), CopyVal(list));
}

// [+ (integer)...] sum
Val *Lplus(Val *args)
{
    Val *err;
    if (!MatchArgs("&n", args, &err))
    {
        return MakeError(err);
    }
    long sum = 0;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        long x = atol(e->symbol);
        sum += x;
        p = p->rest;
    }
    return MakeSymInt(sum);
}

// [* (e:integer)...] product
Val *Lmultiply(Val *args)
{
    Val *err;
    if (!MatchArgs("&n", args, &err))
    {
        return MakeError(err);
    }
    long product = 1;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        long x = atol(e->symbol);
        product *= x;
        p = p->rest;
    }
    return MakeSymInt(product);
}

// [- x:int (y:int)] subtraction
Val *Lsubtract(Val *args)
{
    Val *err;
    if (!MatchArgs("n(n", args, &err))
    {
        return MakeError(err);
    }
    Val *vx = args->first;
    long x = atol(vx->symbol);
    if (!args->rest)
    {
        return MakeSymInt(-x);
    }
    Val *vy = args->rest->first;
    long y = atol(vy->symbol);
    return MakeSymInt(x - y);
}

// [/ x:int y:int] division
Val *Ldivide(Val *args)
{
    Val *err;
    if (!MatchArgs("nn", args, &err))
    {
        return MakeError(err);
    }
    Val *vx = args->first;
    Val *vy = args->rest->first;
    long x = atol(vx->symbol);
    long y = atol(vy->symbol);
    if (y == 0)
    {
        // division by zero
        return MakeErrorMessage("division by zero");
    }
    return MakeSymInt(x / y);
}

// [% x:int y:int] modulo
Val *Lmod(Val *args)
{
    Val *err;
    if (!MatchArgs("nn", args, &err))
    {
        return MakeError(err);
    }
    Val *vx = args->first;
    Val *vy = args->rest->first;
    long x = atol(vx->symbol);
    long y = atol(vy->symbol);
    if (y == 0)
    {
        // division by zero
        return MakeErrorMessage("division by zero");
    }
    return MakeSymInt(x % y);
}

// [= x y (expr)...] check equality
Val *Lequal(Val *args)
{
    Val *err;
    if (!MatchArgs("vv&v", args, &err))
    {
        return MakeError(err);
    }
    Val *f = args->first;
    Val *p = args->rest;
    while (p && IsList(p))
    {
        if (!IsEqual(f, p->first))
        {
            return MakeFalse();
        }
        p = p->rest;
    }
    return MakeTrue();
}

// [not expr] boolean not
Val *Lnot(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return IsTrue(args->first)? MakeFalse() : MakeTrue();
}

// [symbol? val] check if value is a symbol
Val *Lsymbol_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    Val *v = args->first;
    return !IsSym(v)? MakeTrue() : MakeFalse();
}

// [integer? val] check if value is a integer symbol
Val *Linteger_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return IsInt(args->first)? MakeTrue() : MakeFalse();
}

// [list? val] check if value is a list
Val *Llist_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return IsList(args->first)? MakeTrue() : MakeFalse();
}

// [empty? val] check if value is a the empty list
Val *Lempty_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return (!args->first)? MakeTrue() : MakeFalse();
}

// [nth index list] get the nth item in a list
Val *Lnth(Val *args)
{
    Val *err;
    if (!MatchArgs("nl", args, &err))
    {
        return MakeError(err);
    }
    Val *i = args->first;
    Val *list = args->rest->first;
    long n = atol(i->symbol);
    if (n < 0)
    {
        // index negative
        return MakeErrorMessage("index cannot be negative");
    }
    Val *p = list;
    while (n > 0 && p && IsList(p))
    {
        p = p->rest;
        n--;
    }
    if (p)
    {
        return CopyVal(p->first);
    }
    return MakeErrorMessage("index too big");
}

// [list (val)...] create list from arguments (variadic)
Val *Llist(Val *args)
{
    return CopyVal(args);
}

// [length list]
Val *Llength(Val *args)
{
    Val *err;
    if (!MatchArgs("l", args, &err))
    {
        return MakeError(err);
    }
    return MakeSymInt(ListLength(args->first));
}

// [lambda? v]
Val *Llambda_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return IsLambda(args->first)? MakeTrue() : MakeFalse();
}

// [function? v]
Val *Lfunction_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    return IsFunc(args->first)? MakeTrue() : MakeFalse();
}

// [native? v]
Val *Lnative_q(Val *args)
{
    Val *err;
    if (!MatchArgs("v", args, &err))
    {
        return MakeError(err);
    }
    Val *v = args->first;
    return (IsFunc(v) || IsLambda(v))? MakeTrue() : MakeFalse();
}

// [<= x y (expr)...] check number order
Val *Lincreasing(Val *args)
{
    Val *err;
    if (!MatchArgs("nn&n", args, &err))
    {
        return MakeError(err);
    }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x <= y))
        {
            return MakeFalse();
        }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [>= x y (expr)...] check number order
Val *Ldecreasing(Val *args)
{
    Val *err;
    if (!MatchArgs("nn&n", args, &err))
    {
        return MakeError(err);
    }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x >= y))
        {
            return MakeFalse();
        }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [< x y (expr)...] check number order
Val *Lstrictly_increasing(Val *args)
{
    Val *err;
    if (!MatchArgs("nn&n", args, &err))
    {
        return MakeError(err);
    }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x < y))
        {
            return MakeFalse();
        }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [> x y (expr)...] check number order
Val *Lstrictly_decreasing(Val *args)
{
    Val *err;
    if (!MatchArgs("nn&n", args, &err))
    {
        return MakeError(err);
    }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x > y))
        {
            return MakeFalse();
        }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [chars sym] -> list
Val *Lchars(Val *args)
{
    Val *err;
    if (!MatchArgs("s", args, &err))
    {
        return MakeError(err);
    }
    Val *sym = args->first;
    char *s = sym->symbol;
    Val *result = MakeList(MakeSymCopy(s, 1), NULL);
    s++;
    Val *p = result;
    while (*s)
    {
        p->rest = MakeList(MakeSymCopy(s, 1), NULL);
        p = p->rest;
        s++;
    }
    return result;
}

// [symbol list] -> symbol
Val *Lsymbol(Val *args)
{
    Val *err;
    if (!MatchArgs("L", args, &err))
    {
        return MakeError(err);
    }
    Val *list = args->first;
    int len = ListLength(list);
    char *sym = malloc(1 + len);
    int i = 0;
    Val *p = list;
    while (p && IsList(list))
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            free(sym);
            return MakeErrorMessage("list must only contain symbols");
        }
        sym[i] = e->symbol[0];
        i++;
        p = p->rest;
    }
    sym[len] = 0;
    // ok because sym was created with malloc()
    return MakeSym(sym);
}

// [member? item list]
Val *Lmember_q(Val *args)
{
    Val *err;
    if (!MatchArgs("vl", args, &err))
    {
        return MakeError(err);
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    while (list && IsList(list))
    {
        if (IsEqual(list->first, item))
        {
            return MakeTrue();
        }
        list = list->rest;
    }
    return MakeFalse();
}

// [count item list] -> int
Val *Lcount(Val *args)
{
    Val *err;
    if (!MatchArgs("vl", args, &err))
    {
        return MakeError(err);
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    long count = 0;
    while (list && IsList(list))
    {
        if (IsEqual(list->first, item))
        {
            count++;
        }
        list = list->rest;
    }
    return MakeSymInt(count);
}

// [position item list] -> list
Val *Lposition(Val *args)
{
    Val *err;
    if (!MatchArgs("vl", args, &err))
    {
        return MakeError(err);
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    long i = 0;
    while (list && IsList(list))
    {
        if (IsEqual(item, list->first))
        {
            return MakeSymInt(i);
        }
        i++;
        list = list->rest;
    }
    return MakeFalse();
}

// [slice list start (end)]
// gets a sublist "slice" inclusive of start and end
Val *Lslice(Val *args)
{
    Val *err;
    if (!MatchArgs("ln(n", args, &err))
    {
        return MakeError(err);
    }
    Val *list = args->first;
    Val *start = args->rest->first;
    long start_i = atol(start->symbol);
    if (start_i < 0)
    {
        return MakeErrorMessage("start index cannot be negative");
    }
    if (!args->rest->rest)
    {
        // [slice list start]
        while (start_i > 0 && list && IsList(list))
        {
            list = list->rest;
            start_i--;
        }
        if (!list)
        {
            // TODO: what causes this error?
            return NULL;
        }
        Val *result = MakeList(CopyVal(list->first), NULL);
        list = list->rest;
        Val *p_result = result;
        while (list && IsList(list))
        {
            p_result->rest = MakeList(CopyVal(list->first), NULL);
            p_result = p_result->rest;
            list = list->rest;
        }
        return result;
    }
    // [slice list start end]
    Val *end = args->rest->rest->first;
    long end_i = atol(end->symbol);
    if (end_i <= start_i)
    {
        return MakeErrorMessage("start index must be less than the end index");
    }
    while (start_i > 0 && list && IsList(list))
    {
        list = list->rest;
        start_i--;
    }
    if (!list)
    {
        // TODO: what error is this?
        return NULL;
    }
    Val *result = MakeList(CopyVal(list->first), NULL);
    list = list->rest;
    Val *p_result = result;
    long i = end_i - start_i;
    while (i > 0 && list && IsList(list))
    {
        p_result->rest = MakeList(CopyVal(list->first), NULL);
        p_result = p_result->rest;
        list = list->rest;
        i--;
    }
    return result;
}

#endif /* LIZP_CORE_FUNCTIONS */
#endif /* LIZP_IMPLEMENTATION */
