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
	bool isError = true;
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
		case LE_LIST_UNFINISHED:
			msg = "unexpected end of string while reading list";
			break;
		case LE_BRACKET_MISMATCH:
			msg = "unmatched ']'";
			break;
		case LE_UNKNOWN_FUNCTION:
			msg = "unknown function number";
			break;
		case LE_APPLY_NOT_FUNCTION:
			msg = "first item in list is not a function number";
			break;
		case LE_INVALID_VAL:
			msg = "invalid lizp value";
			break;
		case LE_INVALID_INT_BASE:
			msg = "invalid base when printing integer";
			break;
		case LE_NO_FUNCTION:
			msg = "undefined function";
			break;
		case LE_UNKNOWN_SYM:
			msg = "undefined symbol";
			break;
		default:
			msg = "(unknown error type)";
			break;
	}
	if (msg)
	{
		if (isError)
		{
			fprintf(stderr, "error: %s\n", msg);
		}
		else
		{
			fprintf(stderr, "%s\n", msg);
		}
	}
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

	// If len is 0, the reading failed
	if (len)
	{
		return x;
	}
	else
	{
		return NULL;
	}
}

Val *EVAL (Val *ast, Seq **env)
{
	return EvalAst(ast, env);
}

void PRINT (Val *expr, bool readable)
{
	static char buffer[2 * 1024];
	int p_len = PrintVal(expr, buffer, sizeof(buffer), readable);
	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Seq **env)
{
    Val *a = NULL, *b = NULL;

	int val = setjmp(jbLizp);
	if (!val)
	{
		a = READ(start, length);
		b = EVAL(a, env);
		if (b)
		{
			PRINT(b, 1);
		}
	}
	else
	{
        // There was an error (jump)
		LizpPrintMessage(val);
	}

    // Free the new values
    if (a) { ValFree(a); }
    if (b && a != b) { ValFree(b); }
}

