#ifndef _EVAL_H
#define _EVAL_H

#include "value.h"

typedef struct Func
{
    int nameBase36;  // number that represents its name in base-36
    int numParams;  // number of parameters
} Func;

Val *EvalAst(Val *ast, Val **env);

#endif /* _EVAL_H */
