#ifndef __READER_H
#define __READER_H

#include <stdbool.h>
#include "value.h"

bool CharIsSpace(char c);
int DigitValue(char d);
int ReadInt(const char *start, int length, int *valOut);
int ReadSeq(const char *start, int length, Val **toList);
int ReadVal(const char *start, int length, Val **out);
int ReadString(const char *start, int length, Val **toList);

#endif /* __READER_H */
