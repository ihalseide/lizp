#ifndef _LIZP_H
#define _LIZP_H

#include "cell.h"

extern Cell *repl_env;

void rep(const char *start, int length, Cell *env);
Cell *init(int ncells);

Cell *READ(const char *start, int length);
Cell *EVAL(Cell *ast, Cell *env);
void PRINT(Cell *expr);

Cell *eval_each(Cell *list, Cell *env);
Cell *eval_ast(Cell *ast, Cell *env);
void eval_special(Cell *sym, Cell *ast, Cell *env, Cell **val_out, Cell **env_out);
void apply(Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);
void env_setup_fn(Cell *env, const char *str, Native_fn_t id);

#endif /* _LIZP_H */
