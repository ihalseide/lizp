#ifndef __SEQUENCE_H
#define __SEQUENCE_H

typedef struct Seq Seq;
struct Seq
{
	void *first;
	Seq *rest;
};

Seq *SeqInit(void *val, Seq *rest);   // Allocate and initialize new Seq
void SeqFree(Seq *p);                 
void SeqFreeAll(Seq *p);

void **SeqNth(Seq *p, int n);         // Get the n-th seq slot of p
void *SeqGet(Seq *p, int i);          // Get the i-th item of p
void *SeqVal(Seq *p);                 // Get seq->first
Seq *SeqNext(Seq *p);                 // Get seq->rest

int SeqLength(const Seq *p);          // Get length of p
int SeqIsEmpty(Seq *p);               // Get if a sequence is empty

void SeqSet(Seq *p, int i, void *e);  // Set the i-th item of p to e
void SeqPush(Seq **p, void *item);    // Add item to front of list
void SeqAppend(Seq **p, void *item);  // Add item to end of list
void SeqReverse(Seq **p);             // Reverse nodes of p in-place (modifies it)

#endif /* __SEQUENCE_H */
