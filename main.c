#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "lizp.h"

void lizp_c_rep (const char *src)
{
	EVAL(READ(src, strlen(src)), repl_env);
}

int main (void)
{
	// Initialize the REPL environment symbols
	repl_env = init(2000, 8 * 1024);
	if (!repl_env)
		return 1;

	// Initialization lizp code
	lizp_c_rep("[def! load-file [fn* [f] [eval [read-string [str \"[do \" [slurp f] \"\nnil]\n\"]]]]]");
	lizp_c_rep("[load-file \"lizp.lizp\"]");

	// REPL
	char buffer[2 * 1024];
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

