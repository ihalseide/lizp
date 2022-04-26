#ifndef __VALUE_H
#define __VALUE_H

#include <stdbool.h>
#include "sequence.h"

typedef enum ValKind ValKind;
enum ValKind
{
    CK_INT,
    CK_SEQ,
};

typedef struct Val Val;
struct Val
{
    ValKind kind;
    union
    {
        int integer;
        Seq *sequence;
    };
};

int getCount(void);

Val *ValAlloc(void);
void ValFree(Val *p);
void ValFreeAll(Val *p);
Val *ValMakeInt(int n);
Val *ValMakeSeq(Seq *s);
Val *ValCopy(const Val *p);
int ValIsInt(const Val *p);
int ValIsSeq(const Val *p);

#endif /* __VALUE_H */
