#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>

#include "error.h"
#include "lizp.h"

jmp_buf jb_top_level;

static int n_extra = 0;
static const char *signal = NULL;
static Cell *val_p1 = NULL;

_Noreturn void error_raise1 (const char *msg, Cell *p1)
{
	signal = msg;
	n_extra = 1;
	val_p1 = p1;
	longjmp(jb_top_level, 1);
}

_Noreturn void error_raise (const char *msg)
{
	signal = msg;
	n_extra = 0;
	longjmp(jb_top_level, 1);
}

void error_display (void)
{
	switch (n_extra)
	{
		case 1:
			printf("error : %s", signal);
			PRINT(val_p1);
			printf("\n");
			break;
		default:
			printf("error : %s\n", signal);
			break;
	}
}

