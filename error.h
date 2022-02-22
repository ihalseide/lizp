#ifndef _ERROR_H
#define _ERROR_H

#include <setjmp.h>

#include "cell.h"

extern jmp_buf jb_top_level;

_Noreturn void error_raise (const char *msg);

_Noreturn void error_raise1 (const char *msg, Cell *p1);

void error_display (void);

#endif /* _ERROR_H */
