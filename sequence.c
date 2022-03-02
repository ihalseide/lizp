#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "sequence.h"

struct Seq
{
	int cap;  // total capacity
	int fill;  // index to next slot
	void **start;  // start of array
};

Seq *SeqAlloc(void)
{
	Seq *new;
	new = malloc(sizeof(*new));
	return new;
}

Seq *SeqInit(int capacity)
{
	Seq *new = SeqAlloc();
	if (new)
	{
		new->cap = capacity;
		new->start = malloc(sizeof(*new->start) * capacity);
		new->fill = 0;
	}
	return new;
}

void SeqFree(Seq *p)
{
	if (p->start)
	{
		free(p->start);
	}
	free(p);
}

int SeqLength(const Seq *p)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return 0;
	}

	return p->fill;
}

int SeqCapacity(const Seq *p)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return 0;
	}

	return p->cap;
}

void SeqSet(Seq *p, int i, void *e)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return;
	}

	if (i < SeqLength(p))
	{
		p->start[i] = e;
	}
}

void *SeqGet(Seq *p, int i)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return 0;
	}

	if (i < SeqLength(p))
	{
		return p->start[i];
	}
	else
	{
		fprintf(stderr, "%s : index %d out of seq range %p\n", __func__, i, p);
		return NULL;
	}
}

bool SeqIsEmpty(Seq *p)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return 0;
	}

	return p->fill == 0;
}

bool SeqIsFull(Seq *p)
{
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return 0;
	}

	return p->fill == p->cap;
}

void SeqGrow(Seq *p)
{
	// Validate inputs
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return;
	}

	int newCap = 1 + p->cap * 2;
	if (p->start)
	{
		// start is valid pointer
		p->start = realloc(p->start, sizeof(*p->start) * newCap);
	}
	else
	{
		// start is null
		p->start = malloc(sizeof(*p->start) * newCap);
	}
	p->cap = newCap;
}

// Set capacity to fill value
void SeqTrim(Seq *p)
{
	// Validate inputs
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return;
	}

	if (p->start)
	{
		p->start = realloc(p->start, sizeof(*p->start) * p->fill);
	}
	p->cap = p->fill;
}

void SeqAppend(Seq *p, void *item)
{
	// Validate inputs
	if (!p)
	{
		fprintf(stderr, "%s : invalid seq %p\n", __func__, p);
		return;
	}

	if (SeqIsFull(p))
	{
		SeqGrow(p);
	}

	p->start[p->fill] = item;
	p->fill++;
}

void SeqIsFullTest(void)
{
	Seq *s;

	// Empty-capacity list is full
	s = SeqInit(0);
	assert(SeqLength(s) == 0);
	assert(SeqCapacity(s) == 0);
	assert(SeqIsFull(s));
	SeqFree(s);

	// 1-item list
	s = SeqInit(1);
	assert(SeqLength(s) == 0);
	assert(SeqCapacity(s) == 1);
	assert(!SeqIsFull(s));
	s->fill++;
	assert(SeqLength(s) == 1);
	assert(SeqCapacity(s) == 1);
	assert(SeqIsFull(s));
	SeqFree(s);
}

void SeqGrowTest(void)
{
	Seq *s;

	// Grow 0-length 
	s = SeqInit(0);
	SeqGrow(s);
	assert(SeqLength(s) == 0);
	assert(SeqCapacity(s) > 0);
	SeqFree(s);

	// Grow 0-length twice
	s = SeqInit(0);
	SeqGrow(s);
	assert(SeqLength(s) == 0);
	assert(SeqCapacity(s) > 0);
	SeqGrow(s);
	assert(SeqLength(s) == 0);
	assert(SeqCapacity(s) > 1);
	SeqFree(s);

	// Grow 1-length 
	s = SeqInit(1);
	SeqGrow(s);
	assert(SeqCapacity(s) > 1);
	assert(SeqLength(s) == 0);
	SeqFree(s);

	// Grow 1-length twice
	s = SeqInit(1);
	SeqGrow(s);
	assert(SeqCapacity(s) > 1);
	assert(SeqLength(s) == 0);
	SeqGrow(s);
	assert(SeqCapacity(s) > 2);
	assert(SeqLength(s) == 0);
	SeqFree(s);

	// Grow 3-length with 2 items
	s = SeqInit(3);
	assert(SeqCapacity(s) == 3);
	assert(SeqLength(s) == 0);
	s->fill = 2;
	SeqGrow(s);
	assert(SeqCapacity(s) > 3);
	assert(SeqLength(s) == 2);
	SeqFree(s);
}

void SeqAppendTest(void)
{
	Seq *s;
	void *p = SeqAppendTest;

	// Append to empty once
	s = SeqInit(0);
	SeqAppend(s, p);
	assert(SeqLength(s) == 1);
	assert(SeqCapacity(s) >= 1);
	assert(SeqGet(s, 0) == p);
	SeqFree(s);

	// Append to empty twice
	s = SeqInit(0);
	SeqAppend(s, p);
	assert(SeqLength(s) == 1);
	assert(SeqCapacity(s) >= 1);
	assert(SeqGet(s, 0) == p);
	SeqAppend(s, p);
	assert(SeqLength(s) == 2);
	assert(SeqCapacity(s) >= 2);
	assert(SeqGet(s, 0) == p);
	assert(SeqGet(s, 1) == p);
	SeqFree(s);

	// Append to cap-1 once
	s = SeqInit(1);
	SeqAppend(s, p);
	assert(SeqLength(s) == 1);
	assert(SeqCapacity(s) >= 1);
	assert(SeqGet(s, 0) == p);
	SeqFree(s);
}

void SequenceTest(void)
{
	SeqIsFullTest();
	SeqGrowTest();
	SeqAppendTest();
}

