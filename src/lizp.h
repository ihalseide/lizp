#ifndef _lizp_h_
#define _lizp_h_

typedef struct Val Val;
typedef Val *LizpFunc(Val *args);

#define F_SYM  1

// A Value is a List or a Symbol.
// List: first (Val), rest (List)
// Symbol: string
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
Val *MakeList(Val *first, Val *rest);
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

int IsList(Val *p);
int IsSym(Val *p);

int EscapeStr(char *str, int len);
int StrNeedsQuotes(const char *s);
long ListLength(Val *l);

int IsTrue(Val *v);
int IsEqual(Val *x, Val *y);
int IsLambda(Val *v);
int IsFunc(Val *v);

int ReadVal(const char *start, int length, Val **out);
int PrintValBuf(Val *p, char *out, int length, int readable);

char *PrintValStr(Val *p, int readable);

Val *Eval(Val *ast, Val *env);

#endif /* _lizp_h_ */
