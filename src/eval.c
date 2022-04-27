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

// Apply function or macro
// Must not modify/touch/share structure with the original seq
static Val *Apply(Val *seq, Val **env)
{
    Val *fn = seq->first;
    Val *args = seq->rest;
    if (ValIsLambda(fn))
    {
        EnvPush(env);
        Val *lArgs = fn->first->rest;
        Val *p = lArgs;
        Val *q = args;
        while (p && ValIsSeq(p) && q && ValIsSeq(q))
        {
            Val *key = p->first;
            Val *val = q->first;
            EnvLet(env, key, val);
            p = p->rest;
            q = q->rest;
        }
        if (p == NULL && q == NULL)
        {
            Val *lBody = fn->rest->first;
            Val *result = EvalAst(lBody, env);
            EnvPop(env);
            return result;
        }
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
    // First must be a valid function id number (a base36 name)
    if (!ValIsInt(fn) && !(ValIsInt(fn->first) && fn->first->integer == STR))
    {
        LizpError(LE_APPLY_NOT_FUNCTION);
    }
    int nameBase36 = fn->integer;
    int numArgs = ValSeqLength(seq) - 1;
    switch (nameBase36)
    {
        case LAMBDA:
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
                Val *lBody = args->rest;
                // make form [[lambda args] expr]
                Val *result = ValMakeSeq(ValMakeSeq(ValMakeInt(LAMBDA), lArgs), lBody);
                assert(ValIsLambda(result));
                return result;
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
        case IF:
            // [if condition consequent alternative]
            if (numArgs == 2 || numArgs == 3)
            {
                if (ValIsTrue(EvalAst(args->first, env)))
                {
                    return EvalAst(args->rest->first, env);
                }
                if (numArgs == 3)
                {
                    return EvalAst(args->rest->rest->first, env);
                }
                return NULL;
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
                        return EvalAst(cons, env);
                    }
                    p = p->rest->rest;
                }
                LizpError(LE_COND_FORM);
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
            // [print expr] readable
            if (numArgs == 1)
            {
                print(args->first, 1);
                return NULL;
            }
            break;
        case PPRINT:
            // [pprint expr] pretty, not readable
            if (numArgs == 1)
            {
                print(args->first, 0);
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
                        Val *val = EvalAst(p->rest->first, env);
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
            case IF:
            case GET:
            case LET:
            case COND:
            case QUOTE:
            case LAMBDA:
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

