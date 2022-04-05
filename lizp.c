#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "lizp.h"
#include "sequence.h"
#include "reader.h"
#include "eval.h"
#include "printer.h"

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

Val *EVAL (Val *ast, Val *env)
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
void rep (const char *start, int length, Val *env)
{
	Val *a = READ(start, length);
	Val *b = EVAL(a, env);
	PRINT(b, 1);
}

void LizpTest(void)
{
	SequenceTest();
	ReaderTest();
	PrinterTest();
}

