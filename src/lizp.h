#ifndef _lizp_h_
#define _lizp_h_

typedef struct Val Val;
typedef Val *LizpFunc(Val *args);

// A Value is a List or a Symbol.
// A symbol has symbol name
// A list has a first and a rest
struct Val
{
    unsigned int is_sym : 1;
    union
    {
        Val *first;
        char *symbol;
    };
    Val *rest;
};

Val *AllocVal(void);
Val *CopyVal(Val *p);
void FreeVal(Val *p);
void FreeValRec(Val *p);

Val *MakeArgumentsError(const char *func, int min, int max);
Val *MakeError(Val *rest);
Val *MakeErrorMessage(const char *msg);
Val *MakeFalse(void);
Val *MakeList(Val *first, Val *rest);
Val *MakeSym(char *s);
Val *MakeSymCopy(const char *name, int len);
Val *MakeSymInt(long n);
Val *MakeTrue(void);

int IsEqual(Val *x, Val *y);
int IsError(Val *v);
int IsFunc(Val *v);
int IsLambda(Val *v);
int IsList(Val *p);
int IsSym(Val *p);
int IsSymInt(Val *v);
int IsTrue(Val *v);

int EscapeStr(char *str, int len);
int StrNeedsQuotes(const char *s);
long ListLength(Val *l);

int ReadVal(const char *start, int length, Val **out);
Val *Eval(Val *ast, Val *env);
int PrintValBuf(Val *p, char *out, int length, int readable);
char *PrintValStr(Val *p, int readable);

int EnvGet(Val *env, Val *key, Val **out);
int EnvSet(Val *env, Val *key, Val *val);
int EnvSetFunc(Val *env, const char *name, LizpFunc * func);
void EnvPop(Val *env);
void EnvPush(Val *env);

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
LizpFunc **da_funcs;

// Add function to dynamic array
long PutFunc(LizpFunc *func)
{
    long id = arrlen(da_funcs);
    arrput(da_funcs, func);
    return id;
}

// Find function in dynamic array
LizpFunc *GetFunc(long id)
{
    int len = arrlen(da_funcs);
    if (id < 0 || id >= len)
    {
        return NULL;
    }
    return da_funcs[id];
}

// Allocate a new value
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
    if (IsList(v))
    {
        // List or NULL
        Val *p = v;
        Val *n;
        while (p && IsList(p))
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

// Check if a value is a list
// NULL is also considered an empty list
int IsList(Val *p)
{
    return !p || (p && !p->is_sym);
}

// Check if a value is a symbol
int IsSym(Val *p)
{
    return p && p->is_sym;
}

//  check if a value is a integer symbol
int IsSymInt(Val *v)
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
        p->is_sym = 1;
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
    if (rest && !IsList(rest))
    {
        return NULL;
    }
    Val *p = AllocVal();
    if (p)
    {
        p->is_sym = 0;
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
        return MakeSym(strdup(p->symbol));
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
                    char *str1 = strndup(str + j, len);
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
int ReadVal(const char *str, int len, Val **out)
{
    if (!out || !str || !len)
    {
        return 0;
    }

    int i = 0;
    while (1)
    {
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
            case ']':
                // unmated list
                return i;
            case ')':
                // unmatched comment
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
                        if (l <= 0)
                        {
                            *out = e;
                            return l;
                        }
                        i += l;
                        // Space
                        while (i < len && isspace(str[i]))
                        {
                            i++;
                        }
                        list = MakeList(e, NULL);
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
                            // Space
                            while (i < len && isspace(str[i]))
                            {
                                i++;
                            }
                        }
                    }
                    *out = list;
                    if (str[i] == ']')
                    {
                        i++;
                        // Space
                        while (i < len && isspace(str[i]))
                        {
                            i++;
                        }
                    }
                    return i;
                }
            case '(':
                // comment
                {
                    int level = 1;
                    while (i < len && str[i] && level > 0)
                    {
                        i++;
                        switch (str[i])
                        {
                            case '(':
                                level++;
                                break;
                            case ')':
                                level--;
                                break;
                        }
                    }
                    if (str[i] == ')')
                    {
                        i++;
                    }
                    break;
                }
            default:
                // Symbol
                {
                    Val *sym = NULL;
                    int slen = ReadSym(str + i, len - i, &sym);
                    i += slen;
                    *out = sym;
                    // Space
                    while (i < len && isspace(str[i]))
                    {
                        i++;
                    }
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
    Val *e = MakeSym(strdup("error"));
    if (IsList(rest))
    {
        return MakeList(e, rest);
    }
    return MakeList(e, MakeList(rest, NULL));
}

Val *MakeErrorMessage(const char *msg)
{
    return MakeError(MakeSym(strdup(msg)));
}

// make an error for improper number of arguments
// if max is negative, there is no max
Val *MakeArgumentsError(const char *func, int min, int max)
{
    if (min == max)
    {
        // exact amount of arguments
        const char *fmt = "`%s` requires %d argument(s)";
        int sz = snprintf(NULL, 0, fmt, func, min);
        char *buf = malloc(sz + 1);
        snprintf(buf, sz + 1, fmt, func, min);
        return MakeError(MakeSym(buf));
    }
    if (max > 0)
    {
        const char *fmt = "`%s` requires at least %d argument(s) and at most %d argument(s)";
        int sz = snprintf(NULL, 0, fmt, func, min, max);
        char *buf = malloc(sz + 1);
        snprintf(buf, sz + 1, fmt, func, min, max);
        return MakeError(MakeSym(buf));
    }
    // no maximum
    const char *fmt = "`%s` requires at least %d argument(s)";
    int sz = snprintf(NULL, 0, fmt, func, min);
    char *buf = malloc(sz + 1);
    snprintf(buf, sz + 1, fmt, func, min);
    return MakeError(MakeSym(buf));
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
        return 0;
    }
    return 1;
}

// check if the value matches the form [error ...]
int IsError(Val *v)
{
    if (!IsList(v))
    {
        return 0;
    }
    Val *first = v->first;
    return IsSym(first) && !strcmp("error", first->symbol);
}

// Set value in environment
// Returns non-zero upon success
int EnvSet(Val *env, Val *key, Val *val)
{
    if (!env || !IsList(env))
    {
        return 0;
    }
    Val *pair = MakeList(key, MakeList(val, NULL));
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
    if (!strcmp("defined?", s))
    {
        // [defined? v]
        if (!args || args->rest)
        {
            *out = MakeArgumentsError(s, 1, 1);
            return 1;
        }
        Val *sym = args->first;
        if (!IsSym(sym))
        {
            *out = MakeErrorMessage("first argument must be a symbol");
            return 1;
        }
        *out = EnvGet(env, sym, NULL)? MakeTrue() : MakeFalse();
        return 1;
    }
    if (!strcmp("get", s))
    {
        // [get key (if-undefined)] for getting value with a default of if-undefined
        if (!args || (args->rest && args->rest->rest))
        {
            *out = MakeArgumentsError(s, 1, 2);
            return 1;
        }
        Val *key = args->first;
        if (!IsSym(key))
        {
            *out = MakeError(MakeList(CopyVal(key), MakeList(
                            MakeSym(strdup("is not a symbol")), NULL)));
            return 1;
        }
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
        if (!args || !args->rest || args->rest->rest)
        {
            *out = MakeArgumentsError(s, 2, 2);
            return 1;
        }
        Val *bindings = args->first;
        Val *body = args->rest->first;
        if (!IsList(bindings))
        {
            *out = MakeErrorMessage("`let` first argument must be a list");
            return 1;
        }
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
            EnvSet(env, CopyVal(sym), Eval(expr, env));
            p_binds = p_binds->rest;
        }
        Val *result = Eval(body, env);
        EnvPop(env);
        *out = result;
        return 1;
    }
    if (!strcmp("if", s))
    {
        // [if condition consequent (alternative)]
        if (!args || !args->rest || (args->rest->rest && args->rest->rest->rest))
        {
            *out = MakeArgumentsError(s, 2, 3);
            return 1;
        }
        Val *f = Eval(args->first, env);
        int t = IsTrue(f);
        FreeValRec(f);
        if (t)
        {
            Val *consequent = args->rest->first;
            *out =  Eval(consequent, env);
            return 1;
        }
        Val *alt_list = args->rest->rest;
        if (!alt_list )
        {
            // default alternative
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
        if (!args || args->rest)
        {
            *out = MakeArgumentsError(s, 1, 1);
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
        if (!args)
        {
            *out = NULL;
            return 1;
        }
        Val *p = args;
        while (p && IsList(p))
        {
            Val *e = Eval(p->first, env);
            if (!IsTrue(e))
            {
                // item is false
                *out = CopyVal(e);
                return 1;
            }
            p = p->rest;
            if (!p)
            {
                // last item is true
                *out = CopyVal(e);
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
        if (!args)
        {
            *out = NULL;
            return 1;
        }
        Val *p = args;
        while (p && IsList(p))
        {
            Val *e = Eval(p->first, env);
            if (IsTrue(e))
            {
                // item is true
                *out = CopyVal(e);
                return 1;
            }
            p = p->rest;
            if (!p)
            {
                // last item is false
                *out = CopyVal(e);
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
        if (!args || !args->rest)
        {
            *out = MakeArgumentsError(s, 2, -1);
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
        if (!args || !args->rest || args->rest->rest)
        {
            *out = MakeArgumentsError(s, 2, 2);
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
                                MakeSym(strdup("is not a symbol")), NULL)));
                return 1;
            }
            p = p->rest;
        }
        Val *body = args->rest;
        if (body)
        {
            body = body->first;
        }
        // make lambda... with an explicit NULL body if a body is not provided
        *out = MakeList(CopyVal(first), MakeList(CopyVal(params),
                MakeList(CopyVal(body), NULL)));
        return 1;
    }
    // not a macro
    return 0;
}

// Get the length of a list.
// Returns 0 for a non-list value.
long ListLength(Val *l)
{
    long len = 0;
    while (l && IsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}

// Apply functions
// Return values must not share structure with first, args, or env
// TODO: handle when first is a lambda
Val *Apply(Val *first, Val *args, Val *env)
{
    if (IsLambda(first))
    {
        // lambda function
        Val *params = first->rest->first;
        Val *body = first->rest->rest->first;
        if (args)
        {
            // push env
            EnvPush(env);
            // bind values
            Val *p_params = params;
            Val *p_args = args;
            while (p_params && IsList(p_params) && p_args && IsList(p_args))
            {
                Val *param = p_params->first;
                if ('&' == param->symbol[0])
                {
                    // symbol beginning with '&' binds the rest of the arguments
                    if (p_params->rest)
                    {
                        // not the last parameter
                        EnvPop(env);
                        return NULL;
                    }
                    EnvSet(env, CopyVal(param), CopyVal(p_args));
                    // p_params and p_args will both be non-null
                    break;
                }
                EnvSet(env, CopyVal(param), CopyVal(p_args->first));
                p_params = p_params->rest;
                p_args = p_args->rest;
            }
            if ((p_params == NULL) != (p_args == NULL))
            {
                // parameters-arguments arity mismatch
                EnvPop(env);
                return NULL;
            }
            Val *result = Eval(body, env);
            EnvPop(env);
            return result;
        }
        return Eval(body, env);
    }
    if (IsFunc(first))
    {
        // native function
        long id = atol(first->rest->first->symbol);
        LizpFunc *func = GetFunc(id);
        if (!func)
        {
            // invalid function id or pointer
            return NULL;
        }
        return func(args);
    }
    // invalid function
    return MakeError(MakeList(CopyVal(first), MakeList(
                    MakeSym(strdup("is not a function")), NULL)));
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
        if (IsError(e))
        {
            FreeValRec(list);
            return e;
        }
        p_list->rest = MakeList(e, NULL);
        p_list = p_list->rest;
        p_ast = p_ast->rest;
    }
    Val *result = Apply(first, list->rest, env);
    FreeValRec(list);
    return result;
}

// Set a symbol value to be associated with a C function
int EnvSetFunc(Val *env, const char *name, LizpFunc *func)
{
    if (!env || !name || !func)
    {
        return 0;
    }
    Val *key = MakeSym(strdup(name));
    if (!key)
    {
        return 0;
    }
    long handle = PutFunc(func);
    Val *val = MakeList(MakeSymCopy("native func", 11), MakeList(MakeSymInt(handle), NULL));
    if (!val)
    {
        return 0;
    }
    int success = EnvSet(env, key, val);
    if (success)
    {
        return success;
    }
    FreeValRec(key);
    FreeValRec(val);
    return 0;
}

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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *v = args->first;
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        return NULL;
    }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *v = args->first;
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        return NULL;
    }
    return MakeList(CopyVal(v), CopyVal(list));
}

// [+ (e:integer)...] sum
Val *Lplus(Val *args)
{
    long sum = 0;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
        long x = atol(e->symbol);
        sum += x;
        p = p->rest;
    }
    return MakeSymInt(sum);
}

// [+ (e:integer)...] product
Val *Lmultiply(Val *args)
{
    long product = 1;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
        long x = atol(e->symbol);
        product *= x;
        p = p->rest;
    }
    return MakeSymInt(product);
}

// [- x:int (y:int)] subtraction
Val *Lsubtract(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    Val *vx = args->first;
    if (!IsSym(vx))
    {
        return NULL;
    }
    long x = atol(vx->symbol);
    if (args->rest)
    {
        Val *vy = args->rest->first;
        if (!IsSym(vy))
        {
            return NULL;
        }
        long y = atol(vy->symbol);
        return MakeSymInt(x - y);
    }
    return MakeSymInt(-x);
}

// [/ x:int y:int] division
Val *Ldivide(Val *args)
{
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *vx = args->first;
    if (!IsSym(vx))
    {
        return NULL;
    }
    long x = atol(vx->symbol);
    Val *vy = args->rest->first;
    if (!IsSym(vy))
    {
        return NULL;
    }
    long y = atol(vy->symbol);
    if (y == 0)
    {
        // division by zero
        return NULL;
    }
    return MakeSymInt(x / y);
}

// [% x:int y:int] modulo
Val *Lmod(Val *args)
{
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *vx = args->first;
    if (!IsSym(vx))
    {
        return NULL;
    }
    long x = atol(vx->symbol);
    Val *vy = args->rest->first;
    if (!IsSym(vy))
    {
        return NULL;
    }
    long y = atol(vy->symbol);
    if (y == 0)
    {
        // division by zero
        return NULL;
    }
    return MakeSymInt(x % y);
}

// [= x y (expr)...] check equality
Val *Lequal(Val *args)
{
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *f = args->first;
    Val *p = args->rest;
    while (p && IsList(p))
    {
        if (!IsEqual(f, p->first))
        {
            return NULL;
        }
        p = p->rest;
    }
    return MakeTrue();
}

// [not expr] boolean not
Val *Lnot(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    if (IsTrue(args->first))
    {
        return NULL;
    }
    return MakeTrue();
}

// [symbol? val] check if value is a symbol
Val *Lsymbol_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    Val *v = args->first;
    return !IsSym(v)? MakeTrue() : MakeFalse();
}

// [integer? val] check if value is a integer symbol
Val *Linteger_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    return IsSymInt(args->first)? MakeTrue() : MakeFalse();
}

// [list? val] check if value is a list
Val *Llist_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    if (!IsList(args->first))
    {
        return NULL;
    }
    return MakeTrue();
}

// [empty? val] check if value is a the empty list
Val *Lempty_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    if (args->first)
    {
        return NULL;
    }
    return MakeTrue();
}

// [nth index list] get the nth item in a list
Val *Lnth(Val *args)
{
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *i = args->first;
    if (!IsSym(i))
    {
        // 1st arg not a symbol
        return NULL;
    }
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        // 2nd arg not a list
        return NULL;
    }
    long n = atol(i->symbol);
    if (n < 0)
    {
        // index negative
        return NULL;
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
    // index too big
    return NULL;
}

// [list (val)...] create list from arguments (variadic)
Val *Llist(Val *args)
{
    return CopyVal(args);
}

// [length list]
Val *Llength(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    if (!IsList(args->first))
    {
        return NULL;
    }
    return MakeSymInt(ListLength(args->first));
}

// [lambda? v]
Val *Llambda_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    Val *v = args->first;
    return IsLambda(v)? MakeTrue() : MakeFalse();
}

// [function? v]
Val *Lfunction_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    Val *v = args->first;
    return IsFunc(v)? MakeTrue() : MakeFalse();
}

// [native? v]
Val *Lnative_q(Val *args)
{
    if (!args)
    {
        return NULL;
    }
    Val *v = args->first;
    return (IsFunc(v) || IsLambda(v))? MakeTrue() : MakeFalse();
}

// [<= x y (expr)...] check number order
Val *Lincreasing(Val *args)
{
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *f = args->first;
    if (!IsSym(f))
    {
        return NULL;
    }
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *f = args->first;
    if (!IsSym(f))
    {
        return NULL;
    }
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *f = args->first;
    if (!IsSym(f))
    {
        return NULL;
    }
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *f = args->first;
    if (!IsSym(f))
    {
        return NULL;
    }
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && IsList(p))
    {
        Val *e = p->first;
        if (!IsSym(e))
        {
            return NULL;
        }
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
    if (!args)
    {
        return NULL;
    }
    Val *sym = args->first;
    if (!IsSym(sym))
    {
        return NULL;
    }
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
    if (!args)
    {
        return NULL;
    }
    Val *list = args->first;
    if (!list || !IsList(list))
    {
        return NULL;
    }
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
            return NULL;
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        return NULL;
    }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        return NULL;
    }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *item = args->first;
    Val *list = args->rest->first;
    if (!IsList(list))
    {
        return NULL;
    }
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
    if (!args || !args->rest)
    {
        return NULL;
    }
    Val *list = args->first;
    if (!IsList(list))
    {
        return NULL;
    }
    Val *start = args->rest->first;
    if (!IsSym(start))
    {
        return NULL;
    }
    long start_i = atol(start->symbol);
    if (start_i < 0)
    {
        return NULL;
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
    if (!IsSym(end))
    {
        return NULL;
    }
    long end_i = atol(end->symbol);
    if (end_i <= start_i)
    {
        return NULL;
    }
    while (start_i > 0 && list && IsList(list))
    {
        list = list->rest;
        start_i--;
    }
    if (!list)
    {
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
