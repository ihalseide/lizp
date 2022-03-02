#ifndef __READER_H
#define __READER_H

#include "value.h"
#include "sequence.h"

void ReaderTest(void);

bool CharIsSpace(char c);
int DigitValue(char d);
int ReadInt(const char *start, int length, int *valOut);
int ReadList(const char *start, int length, Seq *toList);
int ReadVal(const char *start, int length, Val **out);

#endif /* __READER_H */
