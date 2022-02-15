#ifndef _PRINTER_H
#define _PRINTER_H

#include "cell.h"

int print_char (char c, char *out, int length);

int print_cstr (char *s, char *out, int length);

int print_string (const char *str, char *out, int length, int readable);

int print_int (int n, char *out, int length);

int print_list (const Cell *list, char *out, int length, int readable);

int pr_str (const Cell *x, char *out, int length, int readable);

#endif /* _PRINTER_H */
