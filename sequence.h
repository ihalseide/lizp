#ifndef __SEQUENCE_H
#define __SEQUENCE_H

typedef struct seq Seq;
struct seq
{
	int cap;  // total capacity
	int fill;  // index to next slot
	void **start;  // start of array
};

void SequenceTest(void);

Seq *SeqAlloc(void);
Seq *SeqInit(int length);
int SeqCapacity(const Seq *p);
int SeqLength(const Seq *p);
void *SeqGet(Seq *p, int i);
void SeqAppend(Seq *p, void *item);
void SeqFree(Seq *p);
void SeqSet(Seq *p, int i, void *e);

#endif /* __SEQUENCE_H */
