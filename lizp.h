#ifndef _LIZP_H

#include "sequence.h"
#include "value.h"
#include <stdbool.h>

// Number of special form symbols
#define SPECIAL_COUNT 6

typedef enum Native_fn Native_fn_t;
enum Native_fn
{
	FN_INVALID,
	FN_STR,
	FN_PR_STR,
	FN_PRN,
	FN_PRINTLN,
	FN_LIST,
	FN_EVAL,
	FN_SLURP,
	FN_READ_STR,
	FN_EMPTY_P,
	FN_LIST_P,
	FN_INT_P,
	FN_STRING_P,
	FN_FUNCTION_P,
	FN_SYMBOL_P,
	FN_COUNT,
	FN_EQ,
	FN_LT,
	FN_GT,
	FN_LTE,
	FN_GTE,
	FN_ADD,
	FN_SUB,
	FN_MUL,
	FN_DIV,
	FN_CONCAT,
	FN_FIRST,
	FN_REST,
};
Val *READ(const char *start, int length);
Val *EVAL(Val *ast, Val *env);
void PRINT(Val *expr);
void rep(const char *start, int length, Val *env);

bool ValEqual(const Val *a, const Val *b);

#endif /* _LIZP_H */

