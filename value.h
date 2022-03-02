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

Val *ValAlloc(void);
Val *ValMakeInt(int n);
Val *ValMakeSeq(Seq *s);
bool SeqEqual(const Seq *a, const Seq *b);
bool ValEqual(const Val *a, const Val *b);
int ValIsInt(const Val *p);
int ValIsSeq(const Val *p);
void ValFree(Val *p);
void ValFreeRec(Val *p);

#endif /* __VALUE_H */
