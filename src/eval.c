#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "eval.h"
#include "lizp.h"
#include "printer.h"

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
        *env = (*env)->rest;
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

// Apply built-in function
// Must not modify/touch/share structure with the original seq
Val *ApplyBI(Val *seq, Val **env)
{
    Val *fn = seq->first;
    // First must be a valid function id number (a base36 name)
    if (!ValIsInt(fn) && !(ValIsInt(fn->first) && fn->first->integer == STR))
    {
        LizpError(LE_APPLY_NOT_FUNCTION);
    }
    long nameBase36 = fn->integer;
    int numArgs = ValSeqLength(seq) - 1;
    Val *args = seq->rest;
    switch (nameBase36)
    {
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
                    print(p->first, 0);
                    p = p->rest;
                }
                // Restore printer vars
                PrinterSetBase(base);
                PrinterSetUpper(upper);
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
        printf("Eval ast: ");
        print(ast, 1);
        putchar('\n');
        if (IsSelfEvaluating(ast))
        {
            printf("self-evaluating\n");
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
            printf("not a macro\n");
            // Evaluate sub-expressions
            Val *evAst = EvalEach(ast, env);
            assert(evAst);
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
    // Handle environment "popping" and return final value
    if (envCount)
    {
        printf("envCount: %d\n", envCount);
    }
    while (envCount > 0)
    {
        EnvPop(env);
        envCount--;
    }
    return ast;
}

