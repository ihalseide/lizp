#ifndef _lizp_h_
#define _lizp_h_

#include <stdbool.h>

// Base 36 numbers for names
#define PRINT    43274297
#define WRITE    55031810
#define ADD      13441
#define SUB      37379
#define MUL      29613
#define DIV      17527
#define LIST     1004141
#define QUOTE    45101858
#define LET      27749
#define GET      21269
#define DO       492
#define LEN      27743
#define NTH      30869
#define FIRST    26070077
#define REST     1278893
#define EQUAL    24766941
#define COND     591817
#define IF       663
#define NOT      30701
#define CAT      15941
#define JOIN     918239
#define AND      13801
#define OR       891
#define ISINT    31563641
#define ISLIST   1136424557
#define ISSTR    31576815
#define ISNULL   1136533161
#define EXCEPTION 902532737

// value to indicate a list is a string
#define STR      37359

// value to indicate a list is a lambda function
#define LAMBDA   21

typedef struct Val Val;
struct Val
{
    unsigned int is_seq : 1;
    unsigned int is_int : 1;
    unsigned int is_func : 1;
    unsigned int is_mark : 1;
    unsigned int is_macro : 1;
    union
    {
        long integer;
        Val *first;
        Val *(*func)(Val *);
    };
    Val *rest;
};

void InitLizp(void);
Val *InitLizpEnv(void);

void EnvSetName(Val **env, const char *base36_name, int len, Val *val);
void EnvSet(Val **env, long symbol, Val *val);
bool EnvGet(Val *env, long symbol, Val **val);

void InitEnv(void);
void InitPool(void);

char ValueToDigit(int d, bool upper);
int DigitValue(char d);
bool CharIsSpace(char c);

int PrintCStr(const char *s, char *out, int len);
int PrintChar(char c, char *out, int length);
int PrintInt(int n, char *out, int len, int readable, bool base10, bool upper);
int PrintStr(Val *seq, char *out, int length, bool readable);
int PrintSeq(Val *list, char *out, int length, bool readable);
int PrintVal(Val *p, char *out, int length, bool readable);

int ReadInt(const char *start, int length, long *valOut);
int ReadSeq(const char *start, int length, Val **toList);
int ReadString(const char *start, int length, Val **toList);
int ReadVal(const char *start, int length, Val **out);

Val *read(const char *start, int length);
Val *eval(Val *ast);
void print(Val *expr, int readable);

Val *eval_sub(Val *ast);

Val *ConcatLists(Val *lists);
Val *Copy(Val *p);
Val *DoAnd(Val *ast);
Val *DoOr(Val *ast);
Val *JoinStrings(Val *sep, Val *strs);
Val *Product(Val *ints);
Val *Sum(Val *ints);
Val *Length(Val *p);

void CollectGarbage(Val *save1, Val *save2);
Val *GetVal(Val *save1, Val *save2);
void FreeVal(Val *p);
void Mark(Val *p);

Val *MakeEmptyStr(void);
Val *MakeFunc(Val *func(Val *));
Val *MakeMacro(Val *func(Val *));
Val *MakeInt(long n);
Val *MakeSeq(Val *first, Val *rest);
Val *MakeStr(const char *s, int len);

bool IsEqual(Val *x, Val *y);
bool IsFunc(Val *p);
bool IsMacro(Val *p);
bool IsInt(Val *p);
bool IsLambda(Val *p);
bool IsSeq(Val *p);
bool IsStr(Val *p);
bool IsTrue(Val *p);

#endif /* _lizp_h_ */
