#ifndef _LIZP_H

#include <setjmp.h>
#include "value.h"

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
};

_Noreturn void LizpError(int val);
Val *read(const char *start, int length);
Val *eval(Val *ast, Val **env);
void print(Val *expr, int readable);
void rep(const char *start, int length, Val **env);

extern jmp_buf jbLizp;

#endif /* _LIZP_H */

