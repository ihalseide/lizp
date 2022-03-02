#ifndef _LIZP_H

#include "value.h"
#include <stdbool.h>

Val *READ(const char *start, int length);
Val *EVAL(Val *ast, Val *env);
void PRINT(Val *expr, bool readable);
void rep(const char *start, int length, Val *env);

#endif /* _LIZP_H */

