#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "lizp.h"

int main (void)
{
	// Initialize the REPL environment symbols
	Cell *repl_env = init(9000);
	if (!repl_env)
		return 1;

	printf("debug: ");
	PRINT(repl_env);
	printf("\n");

	/* Initialization lizp code
	const char *code = "[do\n"
					   "  [def! load-file\n"
					   "    [fn* [f]\n"
					   "      [eval [read-string [str `[do ' [slurp f] `\nnil]\n']]]]]\n"
	                   "  [load-file `lizp.lizp']]\n";
	EVAL(READ(code, strlen(code)), repl_env);
	*/

	// REPL
	char buffer[2 * 1024];
	while (1)
	{
		printf("LIZP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
			break;

		rep(buffer, strlen(buffer), repl_env);
		printf("\n");
	}

	return 0;
}

