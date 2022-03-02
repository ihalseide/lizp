#ifndef __PRINTER_H
#define __PRINTER_H

#include "sequence.h"
#include "value.h"

int PrintChar(char c, char *out, int length);
int PrintCStr(const char *s, char *out, int len);
int PrintInt(int n, char *out, int len, int base);
int PrintSeq(Seq *list, char *out, int length, int readable);
int PrintVal(Val *p, char *out, int length, int readable);

#endif /* __PRINTER_H */
