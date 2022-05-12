#ifndef _lizp_h_
#define _lizp_h_

#define F_SYM  1

// A Value is a Sequence or a Symbol.
// Sequence: first (Val), rest (Seq)
// Symbol: symbol string
typedef struct Val Val;
struct Val
{
    unsigned int flag;
    union
    {
        char *symbol;
        Val *first;
    };
    Val *rest;
};

Val *AllocVal(void);
Val *MakeSeq(Val *first, Val *rest);
Val *MakeSym(char *s);
Val *MakeSymCopy(const char *name, int len);
Val *CopyVal(Val *p);

void FreeVal(Val *p);
void FreeValRec(Val *p);

int IsSeq(Val *p);
int IsSym(Val *p);
int IsEqual(Val *x, Val *y);

int ReadVal(const char *start, int length, Val **out);
int PrintValBuf(Val *p, char *out, int length, int readable);

char *PrintValStr(Val *p, int readable);
void PrintToFile(FILE *f, Val *v);

int EscapeStr(char *str, int len);
int StrNeedsQuotes(const char *s);

#endif /* _lizp_h_ */
