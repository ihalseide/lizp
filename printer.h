#ifndef __PRINTER_H
#define __PRINTER_H

#include <stdbool.h>

#include "sequence.h"
#include "value.h"

bool PrinterGetUpper(void);
char ValueToDigit(int d, bool upper);
int PrintCStr(const char *s, char *out, int len);
int PrintChar(char c, char *out, int length);
int PrintInt(int n, char *out, int len, int readable, int base, bool upper);
int PrintSeq(Seq *list, char *out, int length, int readable);
int PrintVal(Val *p, char *out, int length, int readable);
int PrinterGetBase(void);
void PrinterSetBase(int b);
void PrinterSetUpper(bool b);

#endif /* __PRINTER_H */
