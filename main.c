#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "lizp.h"

int main (void)
{
	// Initialize the REPL environment symbols
	repl_env = init(2000, 8 * 1024);
	if (!repl_env)
		return 1;

	// Initialization code
	const char *code1 = "[def! load-file [fn* [f] [eval [read-string [str \"[do \" [slurp f] \"\nnil]\n\"]]]]]";
	EVAL(READ(code1, strlen(code1)), repl_env);
	const char *code2 = "[load-file \"lizp.lizp\"]";
	EVAL(READ(code2, strlen(code2)), repl_env);

	// REPL
	char buffer[1024];
	while (1)
	{
		printf("LZP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
			break;
		rep(buffer, strlen(buffer), repl_env);
		printf("\n");
	}

	return 0;
}

