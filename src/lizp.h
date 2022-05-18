#ifndef _lizp_h_
#define _lizp_h_

typedef struct Val Val;
typedef Val *LizpFunc(Val *args);

// A Value is a List or a Symbol.
// A symbol has symbol name
// A list has a first and a rest
struct Val
{
    unsigned int is_sym : 1;
    union
    {
        Val *first;
        char *symbol;
    };
    Val *rest;
};

Val *AllocVal(void);
Val *CopyVal(Val *p);
void FreeVal(Val *p);
void FreeValRec(Val *p);

Val *MakeArgumentsError(const char *func, int min, int max);
Val *MakeError(Val *rest);
Val *MakeErrorMessage(const char *msg);
Val *MakeFalse(void);
Val *MakeList(Val *first, Val *rest);
Val *MakeSym(char *s);
Val *MakeSymCopy(const char *name, int len);
Val *MakeSymInt(long n);
Val *MakeTrue(void);

int IsEqual(Val *x, Val *y);
int IsError(Val *v);
int IsFunc(Val *v);
int IsLambda(Val *v);
int IsList(Val *p);
int IsSym(Val *p);
int IsSymInt(Val *v);
int IsTrue(Val *v);

int EscapeStr(char *str, int len);
int StrNeedsQuotes(const char *s);
long ListLength(Val *l);

int ReadVal(const char *start, int length, Val **out);
Val *Eval(Val *ast, Val *env);
int PrintValBuf(Val *p, char *out, int length, int readable);
char *PrintValStr(Val *p, int readable);

int EnvGet(Val *env, Val *key, Val **out);
int EnvSet(Val *env, Val *key, Val *val);
int EnvSetFunc(Val *env, const char *name, LizpFunc * func);
void EnvPop(Val *env);
void EnvPush(Val *env);

#endif /* _lizp_h_ */
