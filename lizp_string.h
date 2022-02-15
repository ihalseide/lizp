#ifndef _LIZP_STRING_H
#define _LIZP_STRING_H

#include "cell.h"

extern char *char_free;
extern char *char_pool;
extern int char_pool_cap;

extern const char *s_nil,
	   *s_false,
	   *s_true,
	   *s_def_bang,
	   *s_let_star,
	   *s_if,
	   *s_fn_star,
	   *s_do,
	   *s_quote;

Cell *insert_string (const char *str);

int init_strings (int nchars);

int symbol_eq (const Cell *s1, const Cell *s2);

Cell *string_join (const Cell *args, char sep, int readable);

int string_can_alloc (int length);

char *string_alloc (int length);

char *string_pool_write (const char *start, int length);

char *string_pool_write_c (const char *str);

int string_equal (const Cell *string_cell, const char *str, int length);

Cell *find_string (const char *start, int length);

Cell *intern_string (const char *start, int length);

void string_step (const char **stream, int *length, int n);

void string_skip_white(const char **stream, int *length);

#endif /* _LIZP_STRING_H */
