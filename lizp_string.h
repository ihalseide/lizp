#ifndef _LIZP_STRING_H
#define _LIZP_STRING_H

#include "cell.h"

int string_to_list (const char *start, int length, int escape, Cell **out);

Cell *string_join (Cell *items, char sep, int readable);

int string_step (const char **stream, int *length, int n);

void string_skip_white(const char **stream, int *length);

#endif /* _LIZP_STRING_H */
