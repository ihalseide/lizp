#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>

#include "lizp.h"
#include "sequence.h"
#include "reader.h"
#include "printer.h"

// Does: Read a form from the stream
// Returns: the form, which may be NULL
Val *READ (const char *start, int length)
{
	// Validate inputs
	if (!start || (length < 0))
		return NULL;

	Val *x;
	ReadVal(start, length, &x);
	return x;
}

Val *EVAL (Val *ast, Val *env)
{
	return ast;
}

void PRINT (Val *expr)
{
	static char buffer[2 * 1024];
	int p_len = PrintVal(expr, buffer, sizeof(buffer), 1);
	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Val *env)
{
	Val *a = READ(start, length);
	Val *b = EVAL(a, env);
	PRINT(b);
}
