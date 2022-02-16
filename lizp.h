#ifndef _LIZP_H
#define _LIZP_H

#include "cell.h"

extern Cell *repl_env;

Cell *eval_each (Cell *list, Cell *env);

Cell *eval_ast (Cell *ast, Cell *env);

void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);

int eval_special (Cell *head, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out);

Cell *READ (const char *start, int length);

Cell *EVAL (Cell *ast, Cell *env);

void PRINT (Cell *expr);

void rep (const char *start, int length, Cell *env);

void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func);

Cell *init (int ncells);

#endif /* _LIZP_H */
