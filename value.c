#include <stdlib.h>

#include "value.h"
#include "sequence.h"

Val *ValAlloc(void)
{
	Val *new;
	new = malloc(sizeof(*new));
	return new;
}

void ValFree(Val *p)
{
	free(p);
}

// Recursively free everything under value
void ValFreeRec(Val *p)
{
	if (ValIsSeq(p))
	{
		Seq *s = p->sequence;
		for (int i = 0; i < SeqLength(s); i++)
		{
			ValFreeRec(SeqGet(s, i));
		}
		SeqFree(s);
	}
	ValFree(p);
}

int ValIsSeq(const Val *p)
{
	return p && p->kind == CK_SEQ;
}

int ValIsInt(const Val *p)
{
	return p && p->kind == CK_INT;
}

Val *ValMakeInt(int n)
{
	Val *p = ValAlloc();
	if (p)
	{
		p->kind = CK_INT;
		p->integer = n;
	}
	return p;
}

Val *ValMakeSeq(Seq *s)
{
	Val *p = ValAlloc();
	if (p)
	{
		p->kind = CK_SEQ;
		p->sequence = s;
	}
	return p;
}

bool SeqEqual(const Seq *a, const Seq *b)
{
	if (a == b)
	{
		return true;
	}
	else if (SeqLength(a) != SeqLength(b))
	{
		return false;
	}
	else
	{
		for (int i = 0; i < SeqLength(a); i++)
		{
			Val *aVal = (Val*)SeqGet(a, i);
			Val *bVal = (Val*)SeqGet(b, i);
			if (!ValEqual(aVal, bVal))
			{
				return false;
			}
		}
		return true;
	}
}

bool ValEqual(const Val *a, const Val *b)
{
	if (!a || !b)
	{
		return false;
	}
	else if (a == b)
	{
		return true;
	}
	else if (a->kind != b->kind)
	{
		return false;
	}
	else
	{
		switch (a->kind)
		{
			case CK_INT:
				return a->integer == b->integer;
			case CK_SEQ:
				return SeqEqual(a->sequence, b->sequence);
			default:
				return false;
		}
	}
}

