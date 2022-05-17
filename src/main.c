// Main REPL (read-eval-print loop) program

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lizp.h"

#define BUF_SZ (2*1024)

// TODO: add functions
// [reverse list]
// [concat list.1 (list.N)...]
// [append list val]
// [prepend val list]
// [join separator (list)...] -> list
// [without item list] -> list
// [replace item1 item2 list] -> list
// [replaceI index item list] -> list
// [zip list.1 (list.N)...]

Val *Lprint(Val *args)
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

Val *Lplus(Val *args)
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

Val *Lmultiply(Val *args)
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

Val *Lsubtract(Val *args)
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

Val *Ldivide(Val *args)
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

Val *Lmod(Val *args)
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

Val *Lequal(Val *args)
{
    // [= x y (expr)...] check equality
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

Val *Lnot(Val *args)
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

Val *Lsymbol_q(Val *args)
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

Val *Llist_q(Val *args)
{
    // [list? val] check if value is a list
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

Val *Lempty_q(Val *args)
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

Val *Lnth(Val *args)
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

Val *Llist(Val *args)
{
    // [list (val)...] create list from arguments (variadic)
    return CopyVal(args);
}

Val *Llength(Val *args)
{
    // [length list]
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

Val *Llambda_q(Val *args)
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

Val *Lincreasing(Val *args)
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

Val *Ldecreasing(Val *args)
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

Val *Lstrictly_increasing(Val *args)
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

Val *Lstrictly_decreasing(Val *args)
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

Val *Lchars(Val *args)
{
    // [chars sym] -> list
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

Val *Lsymbol(Val *args)
{
    // [symbol list] -> symbol
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

Val *Lmember_q(Val *args)
{
    // [member? item list]
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

Val *Lcount(Val *args)
{
    // [count item list] -> int
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

Val *Lposition(Val *args)
{
    // [position item list] -> list
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

Val *Lslice(Val *args)
{
    // [slice list start (end)]
    // gets a sublist "slice" inclusive of start and end
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

void RegisterFuncs(Val *env)
{
    EnvSetFunc(env, "print", Lprint);
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
    EnvSetFunc(env, "list?", Llist_q);
    EnvSetFunc(env, "lambda?", Llambda_q);
    EnvSetFunc(env, "chars", Lchars);
    EnvSetFunc(env, "symbol", Lsymbol);
    EnvSetFunc(env, "list", Llist);
    EnvSetFunc(env, "count", Lcount);
    EnvSetFunc(env, "position", Lposition);
    EnvSetFunc(env, "slice", Lslice);
    EnvSetFunc(env, "length", Llength);
    EnvSetFunc(env, "not", Lnot);
    EnvSetFunc(env, "nth", Lnth);
}

// Read value from buffer with comments
int ReadValC(const char *s, int len, Val **out)
{
    int len2 = strcspn(s, ";");
    return ReadVal(s, len2, out);
}

void REPL(Val *env)
{
    printf("\nLIZP read-eval-print loop:");
    char buffer[BUF_SZ];
    while (1)
    {
        // Read
        printf("\n>>> ");
        if (!fgets(buffer, sizeof(buffer), stdin))
        {
            printf("end of input\n");
            break;
        }
        int len = strlen(buffer);
        if (len <= 0)
        {
            printf("end of input\n");
            break;
        }
        Val *expr = NULL;
        int readlen = ReadValC(buffer, len, &expr);
        if (!readlen)
        {
            continue;
        }

        // Eval
        Val *val = Eval(expr, env);

        // Print
        putchar('\n');
        PrintValFile(stdout, val, 1);

        FreeValRec(expr);
        FreeValRec(val);
    }
}

int main (int argc, char **argv)
{
    if (argc > 1)
    {
        fprintf(stderr, "%s: error: too many arguments\n", argv[0]);
        return 1;
    }
    Val *env = MakeList(NULL, NULL);
    RegisterFuncs(env);
    REPL(env);
    return 0;
}

