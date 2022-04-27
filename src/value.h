#ifndef __VALUE_H
#define __VALUE_H

#include <stdbool.h>

// Base 36 numbers for names
#define PRINT    43274297
#define PPRINT   1554928697
#define ADD      13441
#define SUB      37379
#define MUL      29613
#define DIV      17527
#define NEG      30328 
#define LIST     1004141
#define QUOTE    45101858
#define LET      27749
#define GET      21269
#define DO       492
#define STR      37359

typedef struct Val Val;
struct Val
{
    union
    {
        long integer;
        Val *first;
    };
    Val *rest;
};

Val *ValAlloc(void);
Val *ValMakeInt(long n);
Val *ValMakeSeq(Val *first, Val *rest);
Val *ValMakeStr(const char *s, int len);
Val *ValCopy(Val *p);
bool ValIsInt(Val *p);
bool ValIsSeq(Val *p);
bool ValIsStr(Val *p);
int ValSeqLength(Val *p);

#endif /* __VALUE_H */
