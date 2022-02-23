#ifndef _FUNCTION_H
#define _FUNCTION_H

#include "cell.h"

void print_nonreadably (Cell *expr);
Cell *apply_built_in(Native_fn_t fn, Cell *args);

#endif /* _FUNCTION_H */
