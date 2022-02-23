#ifndef _PRINTER_H
#define _PRINTER_H

#include "cell.h"

int pr_str(Cell *x, char *out, int len, int readable);
int print_char(char c, char *out, int len);
int print_cstr(const char *s, char *out, int len);
int print_error(Cell *list, char *out, int len);
int print_int(int n, char *out, int len);
int print_list(Cell *list, char *out, int len, int readable);
int print_list_as_string(const Cell *list, char *out, int len, int readable);
int print_string (const char *str, char *out, int len, int readable);

#endif /* _PRINTER_H */
