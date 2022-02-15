#ifndef _READER_H
#define _READER_H

#include "cell.h"

int char_is_symbol (char c);

int parse_int (const char *start, int length, int *out);

int read_string_literal (const char *start, int length, Cell **out);

int read_item (const char *start, int length, Cell **out);

int read_list (const char *start, int length, Cell **out);

int read_until (const char *start, int length, char sentinel);

int read_str (const char *start, int length, Cell **out);

#endif /* _READER_H */
