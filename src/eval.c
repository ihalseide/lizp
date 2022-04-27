#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "eval.h"
#include "lizp.h"
#include "printer.h"

static void EnvPush(Val **env)
{
    *env = ValMakeSeq(NULL, *env);
}

static void EnvPop(Val **env)
{
    if (*env)
    {
        *env = (*env)->rest;
    }
}

static void EnvLet(Val **env, Val *key, Val *val)
{
    if (*env && ValIsInt(key))
    {
        Val *pair = ValMakeSeq(key, ValMakeSeq(val, NULL));
        (*env)->first = ValMakeSeq(pair, (*env)->first);
    }
}

static Val *EnvGet(Val **env, Val *key)
{
    if (*env && ValIsInt(key))
    {
        Val *scope = *env;
        while (scope && ValIsSeq(scope))
        {
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
    // Not found
    LizpError(LE_UNKNOWN_SYM);
}

// Apply function or macro
// Must not modify/touch/share structure with the original seq
static Val *Apply(Val *seq, Val **env)
{
    Val *fn = seq->first;
    // First must be a valid function id number (a base36 name)
    if (!ValIsInt(fn) && !(ValIsInt(fn->first) && fn->first->integer == STR))
    {
        LizpError(LE_APPLY_NOT_FUNCTION);
    }
    int nameBase36 = fn->integer;
    int numArgs = ValSeqLength(seq) - 1;
    Val *args = seq->rest;
    switch (nameBase36)
    {
        case PRINT_OP:
            // [print expr]
            if (numArgs == 1)
            {
                PRINT(args->first, 1);
                return NULL;
            }
            break;
        case ADD:
            // [add x y]
            if (numArgs == 2)
            {
                int x = args->first->integer;
                int y = args->rest->first->integer;
                return ValMakeInt(x + y);
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
        case QUOTE:
            // [quote expr]
            if (numArgs == 1)
            {
                return args->first;
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
                        Val *val = p->rest->first;
                        EnvLet(env, key, val);
                        p = p->rest->rest;
                    }
                    Val *result = EvalAst(args->rest->first, env);
                    EnvPop(env);
                    return result;
                }
            }
            break;
        case GET:
            // [get k]
            if (numArgs == 1 && ValIsInt(args->first))
            {
                return EnvGet(env, args->first);
            }
            break;
        case DO:
            // [do ...]
            {
                Val *p = args;
                Val *v = NULL;
                while (p && ValIsSeq(p))
                {
                    v = EvalAst(p->first, env);
                    p = p->rest;
                }
                return v;
            }
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

static bool IsMacro(Val *seq)
{
    assert(seq);
    Val *first = seq->first;
    if (ValIsInt(first))
    {
        switch (first->integer)
        {
            case DO:
            case GET:
            case LET:
            case QUOTE:
                return true;
        }
    }
    return false;
}

static bool IsSelfEvaluating(Val *ast)
{
    return !ast || ValIsInt(ast) || ValIsStr(ast);
}

// Always create new Val objects
Val *EvalAst(Val *ast, Val **env)
{
    if (IsSelfEvaluating(ast))
    {
        return ast;
    }
    if (ValIsSeq(ast))
    {
        // evSeq = evaluated sequence
        Val *evAst = ast;
        if (!IsMacro(ast))
        {
            evAst = ValMakeSeq(EvalAst(ast->first, env), NULL);
            Val *p = evAst;
            ast = ast->rest;
            while (ast)
            {
                p->rest = ValMakeSeq(EvalAst(ast->first, env), NULL);
                p = p->rest;
                ast = ast->rest;
            }
        }
        // Apply this sequence
        return Apply(evAst, env);
    }
    LizpError(LE_INVALID_VAL);
}

