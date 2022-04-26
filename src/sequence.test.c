#include <assert.h>
#include <stdlib.h>
#include "sequence.h"
#include "sequence.test.h"

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

void SeqNthTest(void)
{
	int a = 'a', b = 'b', c = 'c';
	Seq *s = SeqInit(&a, SeqInit(&b, SeqInit(&c, NULL)));
	void **p;
	int n;

	n = 0;
	p = SeqNth(s, n);
	assert(*p == &a);
}

void SequenceTest(void)
{
	SeqNthTest();
	SeqAppendTest();
}

