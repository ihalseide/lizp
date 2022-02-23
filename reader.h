#ifndef _READER_H
#define _READER_H

#include "cell.h"

int char_symbolp(char c);
int read_until(const char *start, int len, char sentinel);
int read_int(const char *start, int len, Cell **out);
int read_sym(const char *start, int len, Cell **out);
int read_quoted_string(const char *start, int len, Cell **out);
int read_list(const char *start, int len, Cell **out);
int read_str(const char *start, int len, Cell **out);

#endif /* _READER_H */
