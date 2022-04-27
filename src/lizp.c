#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>
#include "lizp.h"
#include "reader.h"
#include "eval.h"
#include "printer.h"

jmp_buf jbLizp = {0};

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

