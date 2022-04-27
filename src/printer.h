#ifndef __PRINTER_H
#define __PRINTER_H

#include <stdbool.h>

#include "value.h"

void PrinterTest(void);

int PrinterGetBase(void);
void PrinterSetBase(int b);

bool PrinterGetUpper(void);
void PrinterSetUpper(bool b);

char ValueToDigit(int d, bool upper);

int PrintCStr(const char *s, char *out, int len);
int PrintChar(char c, char *out, int length);
int PrintInt(int n, char *out, int len, int readable, int base, bool upper);
int PrintStr(Val *seq, char *out, int length);
int PrintSeq(Val *list, char *out, int length, bool readable);
int PrintVal(Val *p, char *out, int length, bool readable);

#endif /* __PRINTER_H */
