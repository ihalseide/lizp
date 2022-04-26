#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <setjmp.h>

#include "lizp.h"
#include "sequence.h"
#include "reader.h"
#include "eval.h"
#include "printer.h"

static jmp_buf jbLizp = {0};

_Noreturn void LizpError(int val)
{
    longjmp(jbLizp, val);
}

static void LizpPrintMessage(int val)
{
    const char *msg;
    switch (val)
    {
        case LE_INVALID_INT:
            msg = "invalid integer value";
            break;
        case LE_INVALID_INT_OVERFLOW:
            msg = "integer overflow";
            break;
        case LE_INVALID_INT_DIGIT:
            msg = "invalid digit for base";
            break;
        case LE_INVALID_INT_BASE:
            msg = "invalid base when printing integer";
            break;
        case LE_LIST_UNFINISHED:
            msg = "unexpected end of string while reading list";
            break;
        case LE_BRACKET_MISMATCH:
            msg = "mismatched ']' closing bracket";
            break;
        case LE_UNKNOWN_FUNCTION:
            msg = "unknown function number";
            break;
        case LE_APPLY_NOT_FUNCTION:
            msg = "first item in list is not a function number";
            break;
        case LE_NO_FUNCTION:
            msg = "undefined function";
            break;
        case LE_INVALID_VAL:
            msg = "invalid lizp value";
            break;
        case LE_UNKNOWN_SYM:
            msg = "undefined symbol";
            break;
        case LE_DIV_ZERO:
            msg = "division by zero";
            break;
        default:
            msg = "(unknown error type)";
            break;
    }
    fprintf(stderr, "lizp error: %s\n", msg);
}

// Does: Read a form from the stream
// Returns: the form, which may be NULL
Val *READ (const char *start, int length)
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

// Returns newly allocated value
Val *EVAL (Val *ast, Seq **env)
{
    return EvalAst(ast, env);
}

void PRINT (Val *expr, bool readable)
{
    if (!expr)
    {
        return;
    }
    static char buffer[2 * 1024];
    int p_len = PrintVal(expr, buffer, sizeof(buffer), readable);
    printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Seq **env)
{
    Val *a = NULL;
    Val *b = NULL;

    int val = setjmp(jbLizp);
    if (!val)
    {
        a = READ(start, length);
        b = EVAL(a, env);
        PRINT(b, 1);
    }
    else
    {
        // There was an error (jump)
        LizpPrintMessage(val);
    }

    // Free the new values
    if (a)
    {
        ValFreeAll(a); 
    }
    if (b && a != b)
    {
        ValFreeAll(b); 
    }
}

