#ifndef _EVAL_H
#define _EVAL_H

#include "value.h"

Val *EvalAst(Val *ast, Val **env);
void EnvSet(Val **env, Val *key, Val *val);
Val *EnvGet(Val **env, Val *key);

#endif /* _EVAL_H */
