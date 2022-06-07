/*
LIZP

    Programming language and linked list data serialization.

    This is written in the way of the C99 standard and should also work
    with C++.

    The license for this is at the end of the file.

Notes:

    FIXME: Currently this program also requires "stb_ds.h".

Usage

    You only need to include this header file in your project. You can

        #include "lizp.h"

    in any file that needs it. Then in one place, you need to type:

        #define LIZP_IMPLEMENTATION
        #include "lizp.h"

    If you want to use lizp as a scripting language and need to enable
    evaluation, then you should define:

        #define LIZP_EVAL

    And if you want to include some useful macros and functions, then
    you should define:

        #define LIZP_CORE_FUNCTIONS

    Since the lizp core functions implies that you are doing evaluation,
    this also automatically defines LIZP_EVAL, so you don't need to
    define it too.

Data types

    The only real data type is the "Value" known as Val_t, which can can
    either by a symbol or a list. Symbols are arbitrary null-terminated
    strings, and lists are linked lists of 0 or more values. The empty
    list is a NULL pointer.

    Symbols can additionally be interpretted as more data types if you wish,
    but you would have to provide the parsing functions to convert a string
    into the desired data type. In this header file, the AsInt() function
    is an example of this.

*/

#ifndef _lizp_h_
#define _lizp_h_

// Using Lizp core functions requires evaluation
#ifdef LIZP_CORE_FUNCTIONS
#define LIZP_EVAL
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef enum ValKind
{
    VK_SYMBOL,
    VK_LIST,
} ValKind_t;

// Val: (tagged union)
typedef struct Val
{
    ValKind_t kind;
    union
    {
        char *symbol;
        struct
        {
            struct Val *first;
            struct Val *rest;
        };
    };
} Val_t;

// memory management
Val_t *AllocVal(void);
Val_t *CopyVal(Val_t *p);
void FreeVal(Val_t *p);
void FreeValRec(Val_t *p);

// value creation
Val_t *MakeInt(long n);
Val_t *MakeList(Val_t *first, Val_t *rest);
Val_t *MakeSym(char *s);                       // "s" MUST be a free-able string
Val_t *MakeSymCopy(const char *name, int len);
Val_t *MakeSymStr(const char *s);

// value type checking
ValKind_t KindOf(Val_t *v);
int IsInt(Val_t *v);
int IsList(Val_t *v);
int IsSym(Val_t *v);

// value utility functions
int ListGetItemAfterSym(Val_t *list, const char *symbol, Val_t **out);
int IsEqual(Val_t *x, Val_t *y);
int ListLength(Val_t *l);
int MatchArgs(const char *form, Val_t *args, Val_t **err);
long AsInt(Val_t *v);

// value serialization
// - use read to deserialize
// - use print to serialize
int ReadVal(const char *start, int length, Val_t **out);
int PrintValBuf(Val_t *p, char *out, int length, int readable);
char *PrintValStr(Val_t *p, int readable);

#ifdef LIZP_EVAL

typedef Val_t *LizpFunc_t(Val_t *args);
typedef Val_t *LizpMacro_t(Val_t *args, Val_t *env);

typedef struct FuncRecord
{
    const char *name;
    const char *form;
    LizpFunc_t *func;
} FuncRecord_t;

typedef struct MacroRecord
{
    const char *name;
    const char *form;
    LizpMacro_t *macro;
} MacroRecord_t;

Val_t *MakeTrue(void);
Val_t *MakeError(Val_t *rest);
Val_t *MakeErrorMessage(const char *msg);
Val_t *MakeFalse(void);

int IsError(Val_t *v);
int IsFunc(Val_t *v);
int IsLambda(Val_t *v);
int IsTrue(Val_t *v);

Val_t *Eval(Val_t *ast, Val_t *env);
Val_t *EvalEach(Val_t *list, Val_t *env);
int EnvGet(Val_t *env, Val_t *key, Val_t **out);
int EnvSet(Val_t *env, Val_t *key, Val_t *val);
int EnvSetFunc(Val_t *env, const char *name, LizpFunc_t *func);
int EnvSetMacro(Val_t *env, const char *name, LizpMacro_t *macro);
int EnvSetFuncEx(Val_t *env, const char *name, const char *form, LizpFunc_t *func);
int EnvSetMacroEx(Val_t *env, const char *name, const char *form, LizpMacro_t *macro);
int EnvSetSym(Val_t *env, const char *symbol, Val_t *val);
void EnvPop(Val_t *env);
void EnvPush(Val_t *env);

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS
// For optional core Lizp functions that would be useful for most lizp scripts

void LizpRegisterCoreFuncs(Val_t *env);
Val_t *Lreverse(Val_t *args);    // [reverse list] reverse a list
Val_t *Lconcat(Val_t *args);     // [concat list.1 (list.N)...] concatenate lists together
Val_t *Ljoin(Val_t *args);       // [join separator (list)...] join together each list with the separator list in between
Val_t *Lwithout(Val_t *args);    // [without item list] remove all occurrences of item from the list
Val_t *Lreplace(Val_t *args);    // [replace item1 item2 list] replace all occurrences of item1 in list with item2
Val_t *Lreplace1(Val_t *args);   // [replaceN item1 item2 list n] replace up to n of item1 with item2 in list
Val_t *LreplaceI(Val_t *args);   // [replaceI index item list] replace element in list at index with item
Val_t *Lzip(Val_t *args);        // [zip list.1 (list.N)...]
Val_t *Lappend(Val_t *args);     // [append val list]
Val_t *Lprepend(Val_t *args);    // [prepend val list]
Val_t *Lprint(Val_t *args);      // [print (v)...]
Val_t *Lplus(Val_t *args);       // [+ (e:integer)...] sum
Val_t *Lmultiply(Val_t *args);   // [* (e:integer)...] product
Val_t *Lsubtract(Val_t *args);   // [- x:int (y:int)] subtraction
Val_t *Ldivide(Val_t *args);     // [/ x:int y:int] division
Val_t *Lmod(Val_t *args);        // [% x:int y:int] modulo
Val_t *Lequal(Val_t *args);      // [= x y (expr)...] check equality
Val_t *Lnot(Val_t *args);        // [not expr] boolean not
Val_t *Lsymbol_q(Val_t *args);   // [symbol? val] check if value is a symbol
Val_t *Linteger_q(Val_t *args);  // [integer? val] check if value is a integer symbol
Val_t *Llist_q(Val_t *args);     // [list? val] check if value is a list
Val_t *Lempty_q(Val_t *args);    // [empty? val] check if value is a the empty list
Val_t *Lnth(Val_t *args);        // [nth index list] get the nth item in a list
Val_t *Llist(Val_t *args);       // [list (val)...] create list from arguments (variadic)
Val_t *Llength(Val_t *args);     // [length list]
Val_t *Llambda_q(Val_t *args);   // [lambda? v]
Val_t *Lfunction_q(Val_t *args); // [function? v]
Val_t *Lnative_q(Val_t *args);   // [native? v]
Val_t *Lincreasing(Val_t *args); // [<= x y (expr)...] check number order
Val_t *Ldecreasing(Val_t *args); // [>= x y (expr)...] check number order
Val_t *Lstrictly_increasing(Val_t *args);   // [< x y (expr)...] check number order
Val_t *Lstrictly_decreasing(Val_t *args);   // [> x y (expr)...] check number order
Val_t *Lchars(Val_t *args);      // [chars sym] -> list
Val_t *Lsymbol(Val_t *args);     // [symbol list] -> symbol
Val_t *Lmember_q(Val_t *args);   // [member? item list]
Val_t *Lcount(Val_t *args);      // [count item list] -> int
Val_t *Lposition(Val_t *args);   // [position item list] -> list
Val_t *Lslice(Val_t *args);      // [slice list start (end)] gets a sublist "slice" inclusive of start and end

// macros
Val_t *Lquote(Val_t *args, Val_t *env);   // [quote expr]
Val_t *Lif(Val_t *args, Val_t *env);      // [if condition consequence alternative]
Val_t *Lcond(Val_t *args, Val_t *env);    // [cond condition1 consequence1 condition2 consequence2 ...]
Val_t *Ldo(Val_t *args, Val_t *env);      // [do expr ...]
Val_t *Llambda(Val_t *args, Val_t *env);  // [lambda [args ...] body-expr]
Val_t *Land(Val_t *args, Val_t *env);     // [and expr1 expr2 ...]
Val_t *Lor(Val_t *args, Val_t *env);      // [or expr1 expr2 ...]
Val_t *Llet(Val_t *args, Val_t *env);     // [let [sym1 expr1 sym2 expr2 ...] body-expr]

#endif /* LIZP_CORE_FUNCTIONS */
#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stb_ds.h"

// Meant to be used by MatchArgs()
static int Match1Arg(char c, Val_t *arg, Val_t **err)
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
int MatchArgs(const char *form, Val_t *args, Val_t **err)
{
    int i = 0;
    int optional = 0;
    Val_t *p = args;
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
                                    MakeList(MakeInt(n),
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
                                    MakeList(MakeInt(i + 1),
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
                            MakeList(MakeInt(n),
                                     MakeList(MakeSymStr(arguments),
                                              NULL)));
        }
        return 0;
    }
    return 1;
}

// Allocate a new value
Val_t *AllocVal(void)
{
    Val_t *p = malloc(sizeof(Val_t));
    return p;
}

// Free value
void FreeVal(Val_t *p)
{
    if (IsSym(p) && p->symbol)
    {
        free(p->symbol);
    }
    free(p);
}

// Free value recursively
void FreeValRec(Val_t *v)
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
    Val_t *p = v;
    Val_t *n;
    while (p && IsList(p))
    {
        FreeValRec(p->first);
        n = p->rest;
        FreeVal(p);
        p = n;
    }
    return;
}

int IsEqual(Val_t *x, Val_t *y)
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
        Val_t *px = x, *py = y;
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
ValKind_t KindOf(Val_t *v)
{
    if (!v)
    {
        return VK_LIST;
    }
    return v->kind;
}

// Check if a value is a list
// NULL is also considered an empty list
int IsList(Val_t *p)
{
    return KindOf(p) == VK_LIST;
}

// Check if a value is a symbol
int IsSym(Val_t *p)
{
    return KindOf(p) == VK_SYMBOL;
}

//  check if a value is a integer symbol
int IsInt(Val_t *v)
{
    if (!IsSym(v)) { return 0; }
    char *end;
    int base = 10;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}

// Make symbol
// NOTE: "s" MUST be a free-able string
// NOTE: does not make a copy of the "s" string
Val_t *MakeSym(char *s)
{
    Val_t *p = AllocVal();
    if (!p) { return p; }
    p->kind = VK_SYMBOL;
    p->symbol = s;
    return p;
}

// Make symbol
// - copies buf to take as a name
// - empty string -> null []
Val_t *MakeSymCopy(const char *buf, int len)
{
    if (!buf || len <= 0) { return NULL; }
    char *s = malloc(len + 1);
    memcpy(s, buf, len);
    s[len] = 0;
    return MakeSym(s);
}

// Make symbol by copying the null-terminated `str`
Val_t *MakeSymStr(const char *str)
{
    int len = strlen(str);
    char *s = malloc(len + 1);
    memcpy(s, str, len);
    s[len] = 0;
    return MakeSym(s);
}

// Make a symbol for an integer
Val_t *MakeInt(long n)
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
// NOTE: rest must be NULL or a list Val_t.
Val_t *MakeList(Val_t *first, Val_t *rest)
{
    if (!IsList(rest)) { return NULL; }
    Val_t *p = AllocVal();
    if (!p) { return p; }
    p->kind = VK_LIST;
    p->first = first;
    p->rest = rest;
    return p;
}

// New copy, with no structure-sharing
Val_t *CopyVal(Val_t *p)
{
    if (!p) { return p; }
    if (IsSym(p)) { return MakeSymStr(p->symbol); }
    if (!IsList(p)) {return NULL; }
    // Copy list
    Val_t *copy = MakeList(CopyVal(p->first), NULL);
    Val_t *pcopy = copy;
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
//   (or is this called "un-escaping"?)
// Converts escape sequences into the corresponding real ASCII
// values.
// Modifies the string in-place
int EscapeStr(char *str, int len)
{
    if (!str || len <= 0) { return 0; }
    int r = 0; // read index
    int w = 0; // write index
    while (r < len && str[r])
    {
        char c = str[r];
        if (c == '\\')
        {
            r++;
            c = str[r];
            switch (c)
            {
                case 'a':
                    c = '\a';
                    break;
                case 'b':
                    c = '\b';
                    break;
                case 'e': // escape
                    c = '\x1b';
                    break;
                case 'n':
                    c = '\n';
                    break;
                case 'r':
                    c = '\r';
                    break;
                case 't':
                    c = '\t';
                    break;
                default:
                    // default result character is itself
                    // - '
                    // - \
                    // - "
                    break;
            }
        }
        str[w] = c;
        r++;
        w++;
    }
    str[w] = '\0';
    return w;
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
        if (!c) { break; }
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
        if (!level && !isspace(c)) { break; }
        i++;
    }
    return i;
}

// Read symbol from input stream
static int ReadSym(const char *str, int len, Val_t **out)
{
    int i = 0;
    // leading space
    i += SkipChars(str, len);
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
                            if (str[i] == '"') { i++; }
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

// Read value from input stream
// str = string characters
// len = string length
// out = value to return
// returns the number of chars read
int ReadVal(const char *str, int len, Val_t **out)
{
    if (!out || !str || len <= 0) { return 0; }
    int i = 0;
    i += SkipChars(str + i, len - i);
    switch (str[i])
    {
    case '\0': // end of string
    case ']':  // unmatched list
    case '(':  // uncaught comment
    case ')':  // uncaught comment
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
            Val_t *e;
            int l = ReadVal(str + i, len - i, &e);
            if (!l)
            {
                *out = NULL;
                return i;
            };
            i += l;
            Val_t *list = MakeList(e, NULL);
            Val_t *p = list;
            // rest of items
            while (i < len && str[i] != ']')
            {
                Val_t *e;
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
            Val_t *sym = NULL;
            int slen = ReadSym(str + i, len - i, &sym);
            i += slen;
            *out = sym;
            i += SkipChars(str + i, len - i);
            return i;
        }
    }
}

// Prints p to the given `out` buffer.
// Does not do null termination.
// If out is NULL, it just calculates the print length
// Returns: number of chars written
int PrintValBuf(Val_t *v, char *out, int length, int readable)
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
                    case '\r':
                        c = 'r';
                        esc = 1;
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
    return i;
}

// Print value to a new string
char *PrintValStr(Val_t *v, int readable)
{
    int len1 = PrintValBuf(v, NULL, 0, readable);
    if (len1 <= 0) { return NULL; }
    char *s = malloc(len1 + 1);
    if (!s) { return NULL; }
    int len2 = PrintValBuf(v, s, len1, readable);
    if (len1 != len2) { return NULL; } // should not happen unless there is a bug in PrintValBuf()
    s[len2] = '\0';
    return s;
}

// Get the length of a list.
// Returns 0 for a non-list value.
int ListLength(Val_t *l)
{
    int len = 0;
    while (l && IsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}

long AsInt(Val_t *v)
{
    if (!IsInt(v)) { return 0; }
    return atol(v->symbol);
}

// Get value right after a symbol in a list
int ListGetItemAfterSym(Val_t *list, const char *symname, Val_t **out)
{
    if (!IsList(list)) { return 0; }
    while (list)
    {
        Val_t *e = list->first;
        if (IsSym(e) && !strcmp(e->symbol, symname))
        {
            // found a spot
            list = list->rest;
            if (!list) { return 0; }
            *out = list->first;
            return 1;
        }
        list = list->rest;
    }
    return 0;
}

// Get the Nth item in a list (0-based index)
Val_t *NthItem(Val_t *list, int n)
{
    if (n < 0) { return NULL; }
    if (!IsList(list)) { return NULL; }
    for (; n > 0; n--)
    {
        list = list->rest;
    }
    return list? list->first : list;
}

// Check if two values do not share structure
int IsSeparate(Val_t *a, Val_t *b)
{
    // empty list (NULL) is separate from anything
    if (!a || !b) { return 1; }
    // the same object is not separate from itself
    if (a == b) { return 0; }
    // symbols are separate if they have different symbol strings
    if (IsSym(a) && IsSym(b)) { return a->symbol != b->symbol; }
    // lists are separate if everything under them are separate
    if (IsList(a) && IsList(b))
    {
        return IsSeparate(a->first, b->first)
            && IsSeparate(a->first, b->rest)
            && IsSeparate(a->rest, b->first)
            && IsSeparate(a->rest, b->rest);
    }
    // symbol and list are separate if the list
    // (and sublists) never contains the symbol
    if (IsSym(a))
    {
        // swap to make sure `a` is list and `b` is symbol
        Val_t *t = b;
        b = a;
        a = t;
    }
    return IsSeparate(a->first, b)
        && IsSeparate(a->rest, b);
}

#ifdef LIZP_EVAL

// Dynamic array of function pointers for native Lizp Functions
FuncRecord_t *da_funcs;

// Dynamic array of function pointers for native Lizp Macros
MacroRecord_t *da_macros;

// Put function in the dynamic array
// Meant to be used by `EnvSetFunc` to bind native functions
static size_t PutFunc(const char *name, const char *form, LizpFunc_t *func)
{
    FuncRecord_t r = (FuncRecord_t)
    {
        .name = name,
        .form = form,
        .func = func,
    };
    size_t id = arrlen(da_funcs);
    arrput(da_funcs, r);
    return id;
}

// Get function from the dynamic array
// Meant to be used by Apply() to look up native functions
static FuncRecord_t *GetFunc(size_t id)
{
    size_t len = arrlen(da_funcs);
    if (id < 0 || id >= len) { return NULL; }
    return &(da_funcs[id]);
}

// Put a macro function into the dynamic array
// Meant to be used by `EnvSetMacro` to bind native macros
static size_t PutMacro(const char *name, const char *form, LizpMacro_t *macro)
{
    MacroRecord_t r = (MacroRecord_t)
    {
        .name = name,
        .form = form,
        .macro = macro,
    };
    size_t id = arrlen(da_macros);
    arrput(da_macros, r);
    return id;
}

// Get macro from the dynamic array
// Meant to be used by Eval() to look up native macros
static MacroRecord_t *GetMacro(size_t id)
{
    size_t len = arrlen(da_macros);
    if (id < 0 || id >= len) { return NULL; }
    return &(da_macros[id]);
}

// Check whether a value is considered as true
int IsTrue(Val_t *v)
{
    return v != NULL;
}

Val_t *MakeTrue(void)
{
    return MakeSymCopy("true", 4);
}

Val_t *MakeFalse(void)
{
    return NULL;
}

// make a list of the form [error rest...]
Val_t *MakeError(Val_t *rest)
{
    Val_t *e = MakeSymStr("error");
    if (IsList(rest)) { return MakeList(e, rest); }
    return MakeList(e, MakeList(rest, NULL));
}

Val_t *MakeErrorMessage(const char *msg)
{
    return MakeError(MakeSymStr(msg));
}

// Check whether a value is a lambda value (special list)
int IsLambda(Val_t *v)
{
    if (!v || !IsList(v)) { return 0; }
    Val_t *l = v->first;
    if (!l || !IsSym(l)) { return 0; }
    if (strcmp(l->symbol, "lambda")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *params = v->rest->first;
    if (!IsList(params)) { return 0; }
    Val_t *pp = params;
    while (pp && IsList(pp))
    {
        if (!IsSym(pp->first)) { return 0; }
        pp = pp->rest;
    }
    if (!v->rest->rest) { return 0; }
    return 1;
}

// Check if a list is a wrapper for a C function
// (a "native func")
// [func number]
int IsFunc(Val_t *v)
{
    if (!v || !IsList(v)) { return 0; }
    Val_t *sym = v->first;
    if (!sym) { return 0; }
    if (!IsSym(sym)) { return 0; }
    if (strcmp(sym->symbol, "native func")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *id = v->rest->first;
    if (!IsInt(id)) { return 0; }
    if (v->rest->rest) { return 0; } // too many items
    return 1;
}

// Check if a list is a wrapper for a native C macro
// (a "native macro")
// [macro number]
int IsMacro(Val_t *v)
{
    if (!v || !IsList(v)) { return 0; }
    Val_t *sym = v->first;
    if (!sym ) { return 0; }
    if (!IsSym(sym)) { return 0; }
    if (strcmp(sym->symbol, "native macro")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *id = v->rest->first;
    if (!IsInt(id)) { return 0; }
    if (v->rest->rest) { return 0; } // too many items 
    return 1;
}

// check if the value matches the form [error ...]
int IsError(Val_t *v)
{
    if (!v || !IsList(v)) { return 0; }
    Val_t *first = v->first;
    return IsSym(first) && !strcmp("error", first->symbol);
}

// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
int EnvSet(Val_t *env, Val_t *key, Val_t *val)
{
    if (!env || !IsList(env)) { return 0; }
    Val_t *pair = MakeList(key, MakeList(val, NULL));
    if (!pair) { return 0; }
    // push key-value pair onto the front of the list
    env->first = MakeList(pair, env->first);
    return 1;
}

// Get value in environment, does not return a copy
int EnvGet(Val_t *env, Val_t *key, Val_t **out)
{
    if (!env) { return 0; }
    Val_t *scope = env;
    while (scope && IsList(scope))
    {
        Val_t *p = scope->first;
        while (p && IsList(p))
        {
            Val_t *pair = p->first;
            if (pair && IsList(pair) && IsEqual(pair->first, key))
            {
                // found
                if (!pair->rest)
                {
                    // env is improperly set up
                    return 0;
                }
                if (out) { *out = pair->rest->first; }
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
void EnvPush(Val_t *env)
{
    if (!env) { return; }
    env->rest = MakeList(env->first, env->rest);
    env->first = NULL;
}

// pop off the latest context from the environment
void EnvPop(Val_t *env)
{
    if (!env) { return; }
    FreeValRec(env->first);
    Val_t *pair = env->rest;
    env->first = pair->first;
    env->rest = pair->rest;
    // Only free the one pair
    FreeVal(pair);
}

// Return values must not share structure with first, args, or env
static Val_t *ApplyLambda(Val_t *first, Val_t *args)
{
    Val_t *params = first->rest->first;
    Val_t *body = first->rest->rest->first;
    // push env
    Val_t *env = MakeList(NULL, NULL);
    // bind values
    Val_t *p_params = params;
    Val_t *p_args = args;
    while (p_params && IsList(p_params) && p_args && IsList(p_args))
    {
        Val_t *param = p_params->first;
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
    Val_t *result = Eval(body, env);
    FreeValRec(env);
    return result;
}

// Apply a native function
static Val_t *ApplyNative(Val_t *first, Val_t *args)
{
    long id = atol(first->rest->first->symbol);
    FuncRecord_t *record = GetFunc(id);
    if (!record || !record->func)
    {
        // error: invalid function id or pointer
        return MakeError(MakeList(CopyVal(first),
                                  MakeList(MakeSymStr("is not a native function id"),
                                           NULL)));
    }
    if (record->form)
    {
        Val_t *err;
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
                                      MakeList(MakeInt(id),
                                               err)));
        }
    }
    Val_t *result = record->func(args);
    if (IsError(result))
    {
        // patch-in more function info
        Val_t *info;
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
                        MakeList(MakeInt(id),
                                 result->rest));
        result->rest = info;
        return result;
    }
    return result;
}

// Apply functions
// Return values must not share structure with first, args, or env
Val_t *Apply(Val_t *first, Val_t *args)
{
    if (IsLambda(first)) { return ApplyLambda(first, args); }
    if (IsFunc(first)) { return ApplyNative(first, args); }
    // invalid function
    return MakeError(MakeList(CopyVal(first),
                              MakeList(MakeSymStr("is not a function"),
                                       NULL)));
}

static Val_t *ApplyMacro(Val_t *macro, Val_t *args, Val_t *env)
{
    long id = atol(macro->rest->first->symbol);
    MacroRecord_t *record = GetMacro(id);
    if (!record || !record->macro)
    {
        // error: invalid function id or pointer
        return MakeError(MakeList(CopyVal(macro),
                                  MakeList(MakeSymStr("is not a native macro id"),
                                           NULL)));
    }
    if (record->form)
    {
        Val_t *err;
        int match = MatchArgs(record->form, args, &err);
        if (!match && record->name)
        {
            return MakeError(MakeList(MakeSymStr("native macro"),
                                      MakeList(MakeSymStr(record->name),
                                               err)));
        }
        if (!match)
        {
            return MakeError(MakeList(MakeSymStr("native macro #"),
                                      MakeList(MakeInt(id),
                                               err)));
        }
    }
    Val_t *result = record->macro(args, env);
    if (IsError(result))
    {
        // patch-in more function info
        Val_t *info;
        if (record->macro)
        {
            info = MakeList(MakeSymStr("native macro "),
                            MakeList(MakeSymStr(record->name),
                                     result->rest));
            result->rest = info;
            return result;
        }
        // no name
        info = MakeList(MakeSymStr("native macro #"),
                        MakeList(MakeInt(id),
                                 result->rest));
        result->rest = info;
        return result;
    }
    return result;
}

// Evaluate each item in a list
Val_t *EvalEach(Val_t *list, Val_t *env)
{
    if (!list || !IsList(list)) { return NULL; }
    Val_t *result = MakeList(NULL, NULL);
    Val_t *p_result = result;
    while (list && IsList(list))
    {
        Val_t *e = Eval(list->first, env);
        if (IsError(e))
        {
            FreeValRec(result);
            return e;
        }
        p_result->first = e;
        if (list->rest)
        {
            p_result->rest = MakeList(NULL, NULL);
        }
        p_result = p_result->rest;
        list = list->rest;
    }
    return result;
}

// Evaluate a Val_t value
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs for bindings
// Returns the evaluated value
// NOTE: must only return new values that do not share any
//       structure with the ast or the env
Val_t *Eval(Val_t *ast, Val_t *env)
{
    if (!ast) { return ast; } // empty list
    if (IsInt(ast)) { return CopyVal(ast); } // integers are self-evaluating
    if (IsLambda(ast)) { return CopyVal(ast); } // lambda values are self-evaluating
    if (IsSym(ast))
    {
        // lookup symbol value
        Val_t *val;
        if (EnvGet(env, ast, &val))
        {
            return CopyVal(val);
        }
        // symbol not found
        Val_t *name = CopyVal(ast);
        return MakeError(
            MakeList(name,
                     MakeList(MakeSymStr("is undefined"),
                              NULL)));
    }
    // Eval list application...
    Val_t *first = Eval(ast->first, env);
    if (IsError(first)) { return first; }
    if (IsMacro(first)) { return ApplyMacro(first, ast->rest, env); }
    // Eval rest of elements for normal function application
    Val_t *args = EvalEach(ast->rest, env);
    if (IsError(args))
    {
        FreeValRec(first);
        return args;
    }
    Val_t *result = Apply(first, args);
    FreeValRec(first);
    FreeValRec(args);
    return result;
}

// Environment Set Function Extended.
// Set a symbol value to be associated with a C function
// Also, use the form string to always check the arguments before the function
// is called (see `MatchArgs`).
int EnvSetFuncEx(Val_t *env, const char *name, const char *form, LizpFunc_t *func)
{
    if (!env || !name || !func) { return 0; }
    Val_t *key = MakeSymStr(name);
    if (!key) { return 0; }
    long handle = PutFunc(name, form, func);
    Val_t *val = MakeList(MakeSymCopy("native func", 11), MakeList(MakeInt(handle), NULL));
    if (!val)
    {
        FreeValRec(key);
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
int EnvSetFunc(Val_t *env, const char *name, LizpFunc_t *func)
{
    return EnvSetFuncEx(env, name, NULL, func);
}

// Environment set macro extended.
// Add a custom C macro to the environment.
int EnvSetMacroEx(Val_t *env, const char *name, const char *form, LizpMacro_t *m)
{
    if (!env || !name || !m) { return 0; }
    Val_t *key = MakeSymStr(name);
    if (!key) { return 0; }
    long handle = PutMacro(name, form, m);
    Val_t *val = MakeList(MakeSymCopy("native macro", 12), MakeList(MakeInt(handle), NULL));
    if (!val)
    {
        FreeValRec(key);
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

// Environment set macro.
// Add a custom C macro to the environment.
int EnvSetMacro(Val_t *env, const char *name, LizpMacro_t *m)
{
    return EnvSetMacroEx(env, name, NULL, m);
}

// Environment Set Symbol
int EnvSetSym(Val_t *env, const char *sym, Val_t *v)
{
    if (!env || !sym) { return 0; }
    return EnvSet(env, MakeSymStr(sym), v);
}

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS

void LizpRegisterCoreFuncs(Val_t *env)
{
    // macros
    EnvSetMacroEx(env, "quote", "v", Lquote);
    EnvSetMacroEx(env, "if", "vv&v", Lif);
    EnvSetMacroEx(env, "cond", "vv&v", Lcond);
    EnvSetMacroEx(env, "do", "&v", Ldo);
    EnvSetMacroEx(env, "^", "l(v", Llambda);
    EnvSetMacroEx(env, "and", "v&v", Land);
    EnvSetMacroEx(env, "or", "v&v", Lor);
    EnvSetMacroEx(env, "let", "L&v", Llet);
    // functions
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
    EnvSetFunc(env, "without", Lwithout);
}

// [reverse list]
Val_t *Lreverse(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [concat list.1 (list.N)...]
Val_t *Lconcat(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [join separator (list)...]
Val_t *Ljoin(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [without item list]
// Create a list without the given item
Val_t *Lwithout(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *item = NthItem(args, 0);
    Val_t *list = NthItem(args, 1);
    if (!list) { return NULL; }
    Val_t *result = NULL;
    Val_t *p = result;
    while (list)
    {
        Val_t *e = list->first;
        if (IsEqual(e, item))
        {
            list = list->rest;
            continue;
        }
        if (result)
        {
            p->rest = MakeList(CopyVal(e), NULL);
            p = p->rest;
            list = list->rest;
            continue;
        }
        // This is the first time adding an item
        result = MakeList(CopyVal(e), NULL);
        p = result;
        list = list->rest;
    }
    return result;
}

// [replace item1 item2 list]
Val_t *Lreplace(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [replaceN item1 item2 list n]
Val_t *Lreplace1(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [replaceI index item list]
Val_t *LreplaceI(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [zip list.1 (list.N)...]
Val_t *Lzip(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [append val list]
Val_t *Lappend(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *v = args->first;
    Val_t *list = args->rest->first;
    Val_t *last = MakeList(CopyVal(v), NULL);
    if (!list)
    {
        // empty list -> single-item list
        return last;
    }
    // Create a new list and put "last" at the end
    Val_t *copy = CopyVal(list);
    Val_t *p = copy;
    while (p->rest)
    {
        p = p->rest;
    }
    p->rest = last;
    return copy;
}

// [prepend val list]
Val_t *Lprepend(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *v = args->first;
    Val_t *list = args->rest->first;
    return MakeList(CopyVal(v), CopyVal(list));
}

// [+ (integer)...] sum
Val_t *Lplus(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("&n", args, &err)) { return MakeError(err); }
    long sum = 0;
    Val_t *p = args;
    while (p)
    {
        Val_t *e = p->first;
        sum += AsInt(e);
        p = p->rest;
    }
    return MakeInt(sum);
}

// [* (integer)...] product
Val_t *Lmultiply(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("&n", args, &err)) { return MakeError(err); }
    long product = 1;
    Val_t *p = args;
    while (p)
    {
        Val_t *e = p->first;
        product *= AsInt(e);
        p = p->rest;
    }
    return MakeInt(product);
}

// [- x:int (y:int)] subtraction
Val_t *Lsubtract(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("n(n", args, &err)) { return MakeError(err); }
    Val_t *vx = args->first;
    long x = atol(vx->symbol);
    if (!args->rest) { return MakeInt(-x); }
    Val_t *vy = args->rest->first;
    long y = atol(vy->symbol);
    return MakeInt(x - y);
}

// [/ x:int y:int] division
Val_t *Ldivide(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn", args, &err)) { return MakeError(err); }
    long x = AsInt(NthItem(args, 0));
    long y = AsInt(NthItem(args, 1));
    if (y == 0)
    {
        // division by zero
        return MakeErrorMessage("division by zero");
    }
    return MakeInt(x / y);
}

// [% x:int y:int] modulo
Val_t *Lmod(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn", args, &err)) { return MakeError(err); }
    long x = AsInt(NthItem(args, 0));
    long y = AsInt(NthItem(args, 1));
    if (y == 0)
    {
        // division by zero
        return MakeErrorMessage("division by zero");
    }
    return MakeInt(x % y);
}

// [= x y (expr)...] check equality
Val_t *Lequal(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vv&v", args, &err)) { return MakeError(err); }
    Val_t *f = args->first;
    Val_t *p = args->rest;
    while (p && IsList(p))
    {
        if (!IsEqual(f, p->first)) { return MakeFalse(); }
        p = p->rest;
    }
    return MakeTrue();
}

// [not expr] boolean not
Val_t *Lnot(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return IsTrue(args->first)? MakeFalse() : MakeTrue();
}

// [symbol? val] check if value is a symbol
Val_t *Lsymbol_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    Val_t *v = args->first;
    return !IsSym(v)? MakeTrue() : MakeFalse();
}

// [integer? val] check if value is a integer symbol
Val_t *Linteger_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return IsInt(args->first)? MakeTrue() : MakeFalse();
}

// [list? val] check if value is a list
Val_t *Llist_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return IsList(args->first)? MakeTrue() : MakeFalse();
}

// [empty? val] check if value is a the empty list
Val_t *Lempty_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return (!args->first)? MakeTrue() : MakeFalse();
}

// [nth index list] get the nth item in a list
Val_t *Lnth(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nl", args, &err)) { return MakeError(err); }
    Val_t *i = args->first;
    Val_t *list = args->rest->first;
    long n = atol(i->symbol);
    if (n < 0)
    {
        // index negative
        return MakeErrorMessage("index cannot be negative");
    }
    Val_t *p = list;
    while (n > 0 && p && IsList(p))
    {
        p = p->rest;
        n--;
    }
    if (p) { return CopyVal(p->first); }
    return MakeErrorMessage("index too big");
}

// [list (val)...] create list from arguments (variadic)
Val_t *Llist(Val_t *args)
{
    return CopyVal(args);
}

// [length list]
Val_t *Llength(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("l", args, &err)) { return MakeError(err); }
    return MakeInt(ListLength(args->first));
}

// [lambda? v]
Val_t *Llambda_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return IsLambda(args->first)? MakeTrue() : MakeFalse();
}

// [function? v]
Val_t *Lfunction_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return IsFunc(args->first)? MakeTrue() : MakeFalse();
}

// [native? v]
Val_t *Lnative_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    Val_t *v = args->first;
    return (IsFunc(v) || IsLambda(v))? MakeTrue() : MakeFalse();
}

// [<= x y (expr)...] check number order
Val_t *Lincreasing(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn&n", args, &err)) { return MakeError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && IsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x <= y)) { return MakeFalse(); }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [>= x y (expr)...] check number order
Val_t *Ldecreasing(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn&n", args, &err)) { return MakeError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && IsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x >= y)) { return MakeFalse(); }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [< x y (expr)...] check number order
Val_t *Lstrictly_increasing(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn&n", args, &err)) { return MakeError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && IsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x < y)) { return MakeFalse(); }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [> x y (expr)...] check number order
Val_t *Lstrictly_decreasing(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("nn&n", args, &err)) { return MakeError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && IsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x > y)) { return MakeFalse(); }
        x = y;
        p = p->rest;
    }
    return MakeTrue();
}

// [chars sym] -> list
Val_t *Lchars(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("s", args, &err)) { return MakeError(err); }
    Val_t *sym = args->first;
    char *s = sym->symbol;
    Val_t *result = MakeList(MakeSymCopy(s, 1), NULL);
    s++;
    Val_t *p = result;
    while (*s)
    {
        p->rest = MakeList(MakeSymCopy(s, 1), NULL);
        p = p->rest;
        s++;
    }
    return result;
}

// [symbol list] -> symbol
Val_t *Lsymbol(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("L", args, &err)) { return MakeError(err); }
    Val_t *list = args->first;
    int len = ListLength(list);
    char *sym = malloc(1 + len);
    int i = 0;
    Val_t *p = list;
    while (p && IsList(list))
    {
        Val_t *e = p->first;
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
Val_t *Lmember_q(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
    while (list && IsList(list))
    {
        if (IsEqual(list->first, item)) { return MakeTrue(); }
        list = list->rest;
    }
    return MakeFalse();
}

// [count item list] -> int
Val_t *Lcount(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
    long count = 0;
    while (list && IsList(list))
    {
        if (IsEqual(list->first, item)) { count++; }
        list = list->rest;
    }
    return MakeInt(count);
}

// [position item list] -> list
Val_t *Lposition(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("vl", args, &err)) { return MakeError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
    long i = 0;
    while (list && IsList(list))
    {
        if (IsEqual(item, list->first)) { return MakeInt(i); }
        i++;
        list = list->rest;
    }
    return MakeFalse();
}

// [slice list start (end)]
// gets a sublist "slice" inclusive of start and end
Val_t *Lslice(Val_t *args)
{
    Val_t *err;
    if (!MatchArgs("ln(n", args, &err)) { return MakeError(err); }
    Val_t *list = args->first;
    Val_t *start = args->rest->first;
    long start_i = atol(start->symbol);
    if (start_i < 0) { return MakeErrorMessage("start index cannot be negative"); }
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
        Val_t *result = MakeList(CopyVal(list->first), NULL);
        list = list->rest;
        Val_t *p_result = result;
        while (list && IsList(list))
        {
            p_result->rest = MakeList(CopyVal(list->first), NULL);
            p_result = p_result->rest;
            list = list->rest;
        }
        return result;
    }
    // [slice list start end]
    Val_t *end = args->rest->rest->first;
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
    Val_t *result = MakeList(CopyVal(list->first), NULL);
    list = list->rest;
    Val_t *p_result = result;
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

// (macro) [let [key val...] expr]
Val_t *Llet(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("Lv", args, &err)) { return MakeError(err); }
    Val_t *bindings = args->first;
    Val_t *body = args->rest->first;
    // create and check bindings
    EnvPush(env);
    Val_t *p_binds = bindings;
    while (p_binds && IsList(p_binds))
    {
        Val_t *sym = p_binds->first;
        if (!IsSym(sym) || !p_binds->rest || !IsList(p_binds->rest))
        {
            // invalid symbol or uneven amount of args
            EnvPop(env);
            return MakeErrorMessage("`let` bindings list must consist of alternating symbols and expressions");
        }
        p_binds = p_binds->rest;
        Val_t *expr = p_binds->first;
        Val_t *val = Eval(expr, env);
        if (IsError(val))
        {
            // eval error
            EnvPop(env);
            return val;
        }
        EnvSet(env, CopyVal(sym), val);
        p_binds = p_binds->rest;
    }
    // eval body
    Val_t *result = Eval(body, env);
    // destroy bindings
    EnvPop(env);
    return result;
}

// (macro) [if condition consequent (alternative)]
Val_t *Lif(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("vv(v", args, &err)) { return MakeError(err); }
    Val_t *f = Eval(args->first, env);
    if (IsError(f)) { return f; } // eval error
    int t = IsTrue(f);
    FreeValRec(f);
    if (t)
    {
        Val_t *consequent = args->rest->first;
        return Eval(consequent, env);
    }
    Val_t *alt_list = args->rest->rest;
    if (!alt_list) { return NULL; } // no alternative
    Val_t *alternative = alt_list->first;
    return Eval(alternative, env);
}

// (macro) [quote expr]
Val_t *Lquote(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("v", args, &err)) { return MakeError(err); }
    return CopyVal(args->first);
}

// (macro) [do (expr)...]
Val_t *Ldo(Val_t *args, Val_t *env)
{
    Val_t *p = args;
    Val_t *e = NULL;
    while (p && IsList(p))
    {
        e = Eval(p->first, env);
        if (IsError(e)) { return e; } // eval error
        p = p->rest;
        if (p) { FreeValRec(e); } // free all values except the last one
    }
    return e;
}

// (macro) [and expr1 (expr)...]
Val_t *Land(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("v&v", args, &err)) { return MakeError(err); }
    Val_t *p = args;
    while (p && IsList(p))
    {
        Val_t *e = Eval(p->first, env);
        if (IsError(e)) { return e; }
        if (!IsTrue(e)) { return e; } // item is false
        p = p->rest;
        if (!p) { return e; } // last item is true
        FreeValRec(e);
    }
    // malformed list
    return NULL;
}

// (macro) [or expr1 (expr)...]
Val_t *Lor(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("v&v", args, &err)) { return MakeError(err); }
    Val_t *p = args;
    while (p && IsList(p))
    {
        Val_t *e = Eval(p->first, env);
        if (IsError(e)) { return e; }
        if (IsTrue(e)) { return e; } // item is true
        p = p->rest;
        if (!p) { return e; } // last item is false
        FreeValRec(e);
    }
    // malformed list
    return NULL;
}

// (macro) [cond (condition result)...] (no nested lists)
Val_t *Lcond(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("vv&v", args, &err)) { return MakeError(err); }
    if (ListLength(args) % 2 != 0)
    {
        return MakeErrorMessage("`cond` requires an even amount of"
                                " alternating condition expressions and"
                                " consequence expressions");
    }
    Val_t *p = args;
    while (p && IsList(p))
    {
        Val_t *e = Eval(p->first, env);
        if (IsError(e)) { return e; }
        if (IsTrue(e))
        {
            FreeValRec(e);
            return Eval(p->rest->first, env);
        }
        FreeValRec(e);
        p = p->rest;
        p = p->rest;
    }
    // no condition matched
    return NULL;
}

// (macro) [lambda [(symbol)...] (expr)]
Val_t *Llambda(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!MatchArgs("l(v", args, &err)) { return MakeError(err); }
    Val_t *params = args->first;
    if (!IsList(params))
    {
        return MakeErrorMessage("`lambda` first argument must be a list of"
                                " symbols");
    }
    Val_t *p = params;
    // params must be symbols
    while (p && IsList(p))
    {
        Val_t *e = p->first;
        if (!IsSym(e))
        {
            return MakeError(
                MakeList(CopyVal(e),
                         MakeList(MakeSymStr("is not a symbol"),
                                  NULL)));
        }
        p = p->rest;
    }
    // make lambda... with an explicit NULL body if a body is not provided
    Val_t *body = args->rest;
    if (body) { body = body->first; }
    return MakeList(MakeSymCopy("lambda", 6),
                    MakeList(CopyVal(params),
                             MakeList(CopyVal(body),
                                      NULL)));
}

#endif /* LIZP_CORE_FUNCTIONS */
#endif /* LIZP_IMPLEMENTATION */

#ifdef __cplusplus
} /* extern "C" */
#endif

/*
MIT License

Copyright (c) 2022 Izak Nathanael Halseide

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
