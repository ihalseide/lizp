#ifndef __SEQUENCE_H
#define __SEQUENCE_H

#include <stdbool.h>

typedef struct Seq Seq;
struct Seq;

void SequenceTest(void);

Seq *SeqAlloc(void);                  // Allocate new Seq
Seq *SeqInit(int capacity);           // Allocate and initialize new Seq
void SeqFree(Seq *p);                 

void *SeqGet(Seq *p, int i);          // Get the i-th item of p
void SeqSet(Seq *p, int i, void *e);  // Set the i-th item of p to e

bool SeqIsEmpty(Seq *p);              // Predicate function for if a Seq is empty (no items fill)
bool SeqIsFull(Seq *p);               // Predicate function for if a Seq is full (items fill = capacity)

int SeqCapacity(const Seq *p);        // Get capacity of p
int SeqLength(const Seq *p);          // Get length of p

void SeqAppend(Seq *p, void *item);   // Add item to p and grow the capacity if needed
void SeqGrow(Seq *p);                 // Increase the capacity of p
void SeqTrim(Seq *p);                 // Reduce the capacity of p to it's length

#endif /* __SEQUENCE_H */