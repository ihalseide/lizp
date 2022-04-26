#ifndef _LIZP_H

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
};

_Noreturn void LizpError(int val);
Val *READ(const char *start, int length);
Val *EVAL(Val *ast, Seq **env);
void PRINT(Val *expr, bool readable);
void rep(const char *start, int length, Seq **env);

#endif /* _LIZP_H */

