#ifndef _lizp_h_
#define _lizp_h_

typedef struct Val Val;
typedef Val *LizpFunc(Val *args);

#define F_SYM  1

// A Value is a Sequence or a Symbol.
// Sequence: first (Val), rest (Seq)
// Symbol: symbol string
struct Val
{
    unsigned int flag;
    union
    {
        Val *first;
        char *symbol;
    };
    Val *rest;
};

Val *AllocVal(void);
Val *MakeSeq(Val *first, Val *rest);
Val *MakeSym(char *s);
Val *MakeSymCopy(const char *name, int len);
Val *MakeSymInt(long n);
Val *CopyVal(Val *p);

Val *MakeTrue(void);
Val *MakeFalse(void);

void FreeVal(Val *p);
void FreeValRec(Val *p);

int EnvSet(Val *env, Val *key, Val *val);
int EnvGet(Val *env, Val *key, Val **out);
int EnvSetFunc(Val *env, const char *name, LizpFunc * func);
void EnvPush(Val *env);
void EnvPop(Val *env);

int IsSeq(Val *p);
int IsSym(Val *p);
int IsTrue(Val *v);
int IsEqual(Val *x, Val *y);
int IsLambda(Val *v);
long ListLength(Val *l);

int ReadVal(const char *start, int length, Val **out);
int PrintValBuf(Val *p, char *out, int length, int readable);

char *PrintValStr(Val *p, int readable);
void PrintValFile(FILE *f, Val *v, int readable);

int EscapeStr(char *str, int len);
int StrNeedsQuotes(const char *s);

Val *Eval(Val *ast, Val *env);

#endif /* _lizp_h_ */
