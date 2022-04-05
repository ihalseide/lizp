#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "sequence.h"

struct Seq
{
	void *first;
	Seq *rest;
};

static Seq *SeqAlloc(void)
{
	Seq *new;
	new = malloc(sizeof(*new));
	return new;
}

Seq *SeqInit(void *first, Seq *rest)
{
	Seq *new = SeqAlloc();
	if (new)
	{
		new->first = first;
		new->rest = rest;
	}
	return new;
}

void SeqFree(Seq *p)
{
	if (p)
	{
		free(p);
	}
}

int SeqLength(const Seq *p)
{
	int len = 0;
	while (p)
	{
		len++;
		p = p->rest;
	}
	return len;
}

// Get N'th first slot
void **SeqNth(Seq *p, int n)
{
	while (n > 0)
	{
		if (!p)
		{
			return NULL;
		}
		p = p->rest;
		n--;
	}
	return &p->first;
}

void SeqSet(Seq *p, int i, void *e)
{
	if (i >= 0 && i < SeqLength(p))
	{
		void **slot = SeqNth(p, i);
		if (slot)
		{
			*slot = e;
		}
	}
}

void *SeqGet(Seq *p, int i)
{
	if (i >= 0 && i < SeqLength(p))
	{
		return *SeqNth(p, i);
	}
	return NULL;
}

// Get the last slot in a sequence
Seq *SeqLast(Seq *p)
{
	if (!p)
	{
		return NULL;
	}
	while(p->rest)
	{
		p = p->rest;
	}
	return p;
}

bool SeqIsEmpty(Seq *p)
{
	return !p;
}

void SeqPush(Seq **p, void *item)
{
	assert(p);
	*p = SeqInit(item, *p);
}

void SeqAppend(Seq **p, void *item)
{
	assert(p);
	Seq *newCell = SeqInit(item, NULL);
	assert(newCell);
	if (*p)
	{
		Seq *last = SeqLast(*p);
		assert(last);
		last->rest = newCell;
	}
	else
	{
		*p = newCell;
	}
}

void *SeqVal(Seq *p)
{
	if (p)
	{
		return p->first;
	}
	else
	{
		return NULL;
	}
}

Seq *SeqNext(Seq *p)
{
	if (p)
	{
		return p->rest;
	}
	else
	{
		return NULL;
	}
}

void SeqAppendTest(void)
{
	Seq *s;
	void *p = SeqAppendTest;

	// Append to empty once
	s = NULL;
	SeqAppend(&s, p);
	assert(s);
	assert(SeqLength(s) == 1);
	assert(SeqGet(s, 0) == p);
	SeqFree(s);

	// Append to empty twice
	s = NULL;
	SeqAppend(&s, p);
	assert(s);
	assert(SeqLength(s) == 1);
	assert(SeqGet(s, 0) == p);
	SeqAppend(&s, p);
	assert(s);
	assert(SeqLength(s) == 2);
	assert(SeqGet(s, 0) == p);
	assert(SeqGet(s, 1) == p);
	SeqFree(s);

	// Append
	s = SeqInit(NULL, NULL);
	assert(s);
	SeqAppend(&s, p);
	assert(s);
	assert(SeqLength(s) == 2);
	assert(SeqGet(s, 0) == NULL);
	assert(SeqGet(s, 1) == p);
	SeqFree(s);
}

void SequenceTest(void)
{
	SeqAppendTest();
}

