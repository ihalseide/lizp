#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"
#define STRNDUP_IMPL
#include "strndup.h"
#include "lizp.h"

// Dynamic array of function pointers
LizpFunc **da_funcs;

long PutFunc(LizpFunc *func)
{
    long id = arrlen(da_funcs);
    arrput(da_funcs, func);
    return id;
}

LizpFunc *GetFunc(long id)
{
    int len = arrlen(da_funcs);
    if (id < 0 || id >= len)
    {
        return NULL;
    }
    return da_funcs[id];
}

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

// Make a symbol for an integer
Val *MakeSymInt(long n)
{
    const char *fmt = "%ld";
    const int sz = snprintf(NULL, 0, fmt, n);
    char buf[sz + 1];
    snprintf(buf, sizeof(buf), fmt, n);
    return MakeSymCopy(buf, sz);
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
    if (!p)
    {
        return p;
    }
    if (!IsSeq(p))
    {
        return MakeSym(strdup(p->symbol));
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
                    if (l <= 0)
                    {
                        *out = NULL;
                        return l;
                    }
                    i += l;
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
                        p->rest = MakeSeq(e, NULL);
                        p = p->rest;
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
        case ']':
            // end list
            return i;
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
        assert(len1 == len2);
        new[len1] = '\0';
    }
    return new;
}

// Print value to a file
void PrintValFile(FILE *f, Val *v, int readable)
{
    char *s = PrintValStr(v, readable);
    fprintf(f, "%s", s);
    free(s);
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

// Check whether a value is a lambda value (special list)
int IsLambda(Val *v)
{
    if (!v || !IsSeq(v))
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
    if (!IsSeq(params))
    {
        return 0;
    }
    Val *pp = params;
    while (pp && IsSeq(pp))
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
// [func number]
int IsFunc(Val *v)
{
    if (!v || !IsSeq(v))
    {
        return 0;
    }
    Val *sym = v->first;
    if (!sym || !IsSym(sym) || strcmp(sym->symbol, "func"))
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

// Set value in environment
// Returns non-zero upon success
int EnvSet(Val *env, Val *key, Val *val)
{
    if (!env || !IsSeq(env))
    {
        return 0;
    }
    Val *pair = MakeSeq(key, MakeSeq(val, NULL));
    // push key-value pair onto the front of the list
    env->first = MakeSeq(pair, env->first);
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
    while (scope && IsSeq(scope))
    {
        Val *p = scope->first;
        while (p && IsSeq(p))
        {
            Val *pair = p->first;
            if (pair && IsSeq(pair) && IsEqual(pair->first, key))
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

void EnvPush(Val *env)
{
    if (!env)
    {
        return;
    }
    env->rest = MakeSeq(env->first, env->rest);
    env->first = NULL;
}

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
    FreeVal(pair);
}

// Eval macro
// Return values must not share structure with first, args, or env
static int Macro(Val *first, Val *args, Val *env, Val **out)
{
    char *s = first->symbol;
    if (!strcmp("defined?", s))
    {
        // [defined? v]
        if (!args)
        {
            *out = NULL;
            return 1;
        }
        Val *sym = args->first;
        if (!IsSym(sym))
        {
            *out = NULL;
            return 1;
        }
        if (EnvGet(env, sym, NULL))
        {
            *out = MakeTrue();
            return 1;
        }
        *out = NULL;
        return 1;
    }
    if (!strcmp("get", s))
    {
        // [get key] for getting value with a default of null
        if (!args || args->rest)
        {
            *out = NULL;
            return 1;
        }
        Val *key = args->first;
        Val *val;
        if (EnvGet(env, key, &val))
        {
            *out = CopyVal(val);
            return 1;
        }
        *out = NULL;
        return 1;
    }
    if (!strcmp("let", s))
    {
        // [let [(key val)...] (expr)] (remember to remove `set` afterwards)
        if (!args || !args->rest)
        {
            *out = NULL;
            return 1;
        }
        Val *bindings = args->first;
        Val *body = args->rest->first;
        if (!IsSeq(bindings))
        {
            *out = NULL;
            return 1;
        }
        EnvPush(env);
        Val *p_binds = bindings;
        while (p_binds && IsSeq(p_binds))
        {
            Val *sym = p_binds->first;
            if (!IsSym(sym) || !p_binds->rest || !IsSeq(p_binds->rest))
            {
                // invalid symbol or uneven amount of args
                EnvPop(env);
                *out = NULL;
                return 1;
            }
            EnvSet(env, CopyVal(sym), Eval(p_binds->rest->first, env));
            p_binds = p_binds->rest;
            p_binds = p_binds->rest;
        }
        Val *result = Eval(body, env);
        EnvPop(env);
        *out = result;
        return 1;
    }
    if (!strcmp("if", s))
    {
        // [if condition consequent alternative]
        if (!args || !args->rest)
        {
            *out = NULL;
            return 1;
        }
        Val *f = Eval(args->first, env);
        int t = IsTrue(f);
        FreeValRec(f);
        if (t)
        {
            *out =  Eval(args->rest->first, env);
            return 1;
        }
        if (args->rest->rest)
        {
            *out = Eval(args->rest->rest->first, env);
            return 1;
        }
        *out = NULL;
        return 1;
    }
    if (!strcmp("quote", s))
    {
        // [quote expr]
        if (!args || args->rest)
        {
            *out = NULL;
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
        while (p && IsSeq(p))
        {
            e = Eval(p->first, env);
            p = p->rest;
            if (p)
            {
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
        while (p && IsSeq(p))
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
        while (p && IsSeq(p))
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
    }
    if (!strcmp("cond", s))
    {
        // [cond (condition result)...] (no nested lists)
        if (!args)
        {
            *out = NULL;
            return 1;
        }
        Val *p = args;
        while (p && IsSeq(p))
        {
            Val *e = Eval(p->first, env);
            if (IsTrue(e))
            {
                FreeValRec(e);
                if (!p->rest)
                {
                    // Uneven amount of items
                    *out = NULL;
                    return 1;
                }
                *out = Eval(p->rest->first, env);
                return 1;
            }
            FreeValRec(e);
            p = p->rest;
            if (!p)
            {
                // Uneven amount of items
                *out = NULL;
                return 1;
            }
            p = p->rest;
        }
        return 1;
    }
    if (!strcmp("lambda", s))
    {
        // [lambda [(symbol)...] (expr)]
        if (!args)
        {
            *out = NULL;
            return 1;
        }
        Val *params = args->first;
        if (!IsSeq(params))
        {
            *out = NULL;
            return 1;
        }
        Val *p = params;
        // params must be symbols
        while (p && IsSeq(p))
        {
            if (!IsSym(p->first))
            {
                *out = NULL;
                return 1;
            }
            p = p->rest;
        }
        Val *body = args->rest;
        if (body)
        {
            if (body->rest)
            {
                // too many arguments to lambda
                *out = NULL;
                return 1;
            }
            body = body->first;
        }
        // make lambda... with an explicit NULL body if a body is not provided
        *out = MakeSeq(CopyVal(first), MakeSeq(CopyVal(params),
                MakeSeq(CopyVal(body), NULL)));
        return 1;
    }

    return 0;
}

// Apply functions
// Return values must not share structure with first, args, or env
// TODO: handle when first is a lambda
Val *Apply(Val *first, Val *args, Val *env)
{
    if (!first)
    {
        return NULL;
    }

    if (IsLambda(first))
    {
        // lambda function application
        Val *params = first->rest->first;
        Val *body = first->rest->rest->first;
        if (args)
        {
            // push env
            EnvPush(env);
            // bind values
            Val *p_params = params;
            Val *p_args = args;
            while (p_params && IsSeq(p_params) && p_args && IsSeq(p_args))
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
                    break;
                }
                EnvSet(env, CopyVal(param), CopyVal(p_args->first));
                p_params = p_params->rest;
                p_args = p_args->rest;
            }
            if ((p_params == NULL) != (p_args == NULL))
            {
                // arity mismatch
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
        // C function binding
        long id = atol(first->rest->first->symbol);
        LizpFunc *func = GetFunc(id);
        if (!func)
        {
            // invalid function id or pointer
            return NULL;
        }
        return func(args);
    }

    if (!IsSym(first))
    {
        // invalid built-in function
        return NULL;
    }

    char *s = first->symbol;
    if (!strcmp("print", s))
    {
        int readable = 0;
        Val *p = args;
        while (p)
        {
            PrintValFile(stdout, p->first, readable);
            p = p->rest;
        }
        return NULL;
    }
    if(!strcmp("+", s))
    {
        // [+ (e:integer)...] sum
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
    if(!strcmp("*", s))
    {
        // [+ (e:integer)...] product
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
    if(!strcmp("-", s))
    {
        // [- x:int (y:int)] subtraction
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
    if(!strcmp("/", s))
    {
        // [/ x:int y:int] division
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
    if(!strcmp("%", s))
    {
        // [% x:int y:int] modulo
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
    if (!strcmp("=", s))
    {
        // [= x y (expr)...] check equality
        if (!args || !args->rest)
        {
            return NULL;
        }
        Val *f = args->first;
        Val *p = args->rest;
        while (p && IsSeq(p))
        {
            if (!IsEqual(f, p->first))
            {
                return NULL;
            }
            p = p->rest;
        }
        return MakeTrue();
    }
    if (!strcmp("not", s))
    {
        // [not expr] boolean not
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
    if (!strcmp("symbol?", s))
    {
        // [symbol? val] check if value is a symbol
        if (!args)
        {
            return NULL;
        }
        if (!IsSym(args->first))
        {
            return NULL;
        }
        return MakeTrue();
    }
    if (!strcmp("list?", s))
    {
        // [list? val] check if value is a list
        if (!args)
        {
            return NULL;
        }
        if (!IsSeq(args->first))
        {
            return NULL;
        }
        return MakeTrue();
    }
    if (!strcmp("empty?", s))
    {
        // [empty? val] check if value is a the empty list
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
    if (!strcmp("nth", s))
    {
        // [nth index list] get the nth item in a list
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
        if (!IsSeq(list))
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
        while (n > 0 && p && IsSeq(p))
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
    if (!strcmp("list", s))
    {
        // [list (val)...] create list from arguments (variadic)
        return CopyVal(args);
    }
    if (!strcmp("length", s))
    {
        // [length list]
        if (!args)
        {
            return NULL;
        }
        if (!IsSeq(args->first))
        {
            return NULL;
        }
        long len = 0;
        Val *p = args->first;
        while (p && IsSeq(p))
        {
            len++;
            p = p->rest;
        }
        return MakeSymInt(len);
    }
    if (!strcmp("lambda?", s))
    {
        // [lambda? v]
        if (!args)
        {
            return NULL;
        }
        Val *v = args->first;
        if (!IsLambda(v))
        {
            return NULL;
        }
        return CopyVal(v);
    }
    if (!strcmp("<=", s))
    {
        // [<= x y (expr)...] check number order
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
        while (p && IsSeq(p))
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
    if (!strcmp(">=", s))
    {
        // [>= x y (expr)...] check number order
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
        while (p && IsSeq(p))
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
    if (!strcmp("<", s))
    {
        // [< x y (expr)...] check number order
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
        while (p && IsSeq(p))
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
    if (!strcmp(">", s))
    {
        // [> x y (expr)...] check number order
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
        while (p && IsSeq(p))
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
    // TODO:
    // [nil? v]
    // [chars sym] -> list
    // [symbol list] -> symbol
    // [reverse list]
    // [member item list] -> bool
    // [count item list] -> int
    // [concat list (list)...]
    // [append list val]
    // [prepend val list]
    // [join separator (list)...] -> list
    // [position item list] -> list
    // [without item list] -> list
    // [replace item1 item2 list] -> list
    // [replaceI index item list] -> list
    // [slice list start (end)]
    // [zip list (list)...]

    return NULL;
}

// Evaluate a Val
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs
// Returns evaluated value
// - must only return new values that do not share structure with ast or env
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
    assert(ast);
    assert(IsSeq(ast));
    // eval first element
    Val *first = Eval(ast->first, env);
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
    Val *list = MakeSeq(first, NULL);
    Val *p_list = list;
    Val *p_ast = ast->rest;
    while (p_ast && IsSeq(p_ast))
    {
        p_list->rest = MakeSeq(Eval(p_ast->first, env), NULL);
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
    Val *val = MakeSeq(MakeSymCopy("func", 4), MakeSeq(MakeSymInt(handle), NULL));
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
