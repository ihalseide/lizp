#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <setjmp.h>

#include "reader.h"
#include "printer.h"
#include "sequence.h"
#include "lizp.h"

int main (int argc, char **argv)
{
	LizpTest();
	printf("tests complete\n");
	// REPL
	PrinterSetBase(10);
	char buffer[2 * 1024];
	while (1)
	{
		printf("LIZP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
			break;

		rep(buffer, strlen(buffer), NULL);
	}
	return 0;
}

