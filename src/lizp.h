#ifndef _lizp_h_
#define _lizp_h_

#include <stdbool.h>
#include <setjmp.h>

// Base 36 numbers for names
#define PRINT    43274297
#define WRITE    55031810
#define ADD      13441
#define SUB      37379
#define MUL      29613
#define DIV      17527
#define NEG      30328 
#define LIST     1004141
#define QUOTE    45101858
#define LET      27749
#define GET      21269
#define DO       492
#define STR      37359
#define LEN      27743
#define FIRST    26070077
#define REST     1278893
#define EQUAL    24766941
#define COND     591817
#define IF       663
#define NOT      30701
#define LAMBDA   21
#define BASE     527198
#define UPPER    51587811
#define CAT      15941
#define JOIN     918239
#define AND      13801
#define OR       891

typedef struct Val Val;
struct Val
{
    char flag;
    union
    {
        long integer;
        Val *first;
    };
    Val *rest;
};

Val *EvalAst(Val *ast, Val **env);

void EnvSet(Val **env, Val *key, Val *val);

Val *EnvGet(Val **env, Val *key);

void PrinterTest(void);

int PrinterGetBase(void);

void PrinterSetBase(int b);

bool PrinterGetUpper(void);

void PrinterSetUpper(bool b);

char ValueToDigit(int d, bool upper);

int PrintCStr(const char *s, char *out, int len);

int PrintChar(char c, char *out, int length);

int PrintInt(int n, char *out, int len, int readable, int base, bool upper);

int PrintStr(Val *seq, char *out, int length, bool readable);

int PrintSeq(Val *list, char *out, int length, bool readable);

int PrintVal(Val *p, char *out, int length, bool readable);

bool CharIsSpace(char c);

int DigitValue(char d);

int ReadInt(const char *start, int length, int *valOut);

int ReadSeq(const char *start, int length, Val **toList);

int ReadVal(const char *start, int length, Val **out);

int ReadString(const char *start, int length, Val **toList);

enum LizpErrorEnum
{
    LE_INVALID_INT = 1,
    LE_INVALID_INT_OVERFLOW,
    LE_INVALID_INT_DIGIT,
    LE_LIST_UNFINISHED,
    LE_BRACKET_MISMATCH,
    LE_UNKNOWN_FUNCTION,
    LE_APPLY_NOT_FUNCTION,
    LE_INVALID_VAL,
    LE_INVALID_INT_BASE,
    LE_NO_FUNCTION,
    LE_UNKNOWN_SYM,
    LE_DIV_ZERO,
    LE_LET_FORM,
    LE_COND_FORM,
    LE_LAMBDA_TOO_MANY_ARGS,
    LE_LAMBDA_TOO_FEW_ARGS,
    // put values above here only
    LE_ENUM_END,
    // do not put values below here
};
#define LE_COUNT (LE_ENUM_END - LE_INVALID_INT)

void InitLizp(void);

_Noreturn void LizpError(int val);

const char *LizpGetMessage(int val);

Val *ValGet(Val *save1, Val *save2);

void ValFree(Val *p);

void ValFreeRec(Val *v);

Val *read(const char *start, int length);

Val *eval(Val *ast, Val **env);

void print(Val *expr, int readable);

void rep(const char *start, int length, Val **env);

Val *ValMakeInt(long n);

Val *ValMakeSeq(Val *first, Val *rest);

Val *ValMakeStr(const char *s, int len);

Val *ValMakeEmptyStr(void);

Val *ValCopy(Val *p);

bool ValIsInt(Val *p);

bool ValIsSeq(Val *p);

bool ValIsStr(Val *p);

bool ValIsLambda(Val *p);

bool ValIsTrue(Val *p);

bool ValEqual(Val *x, Val *y);

int ValSeqLength(Val *p);

Val *ConcatLists(Val *lists);

Val *JoinStrings(Val *sep, Val *strs);

void Mark(Val *p);

void CollectGarbage(Val *save1, Val *save2);

Val *Sum(Val *ints);

Val *Product(Val *ints);

Val *DoAnd(Val *ast, Val **env);

Val *DoOr(Val *ast, Val **env);

void InitEnv(void);

void InitPool(void);

// State
extern jmp_buf jbLizp;
extern Val *env;

#endif /* _lizp_h_ */
