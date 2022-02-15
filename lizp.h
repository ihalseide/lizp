#ifndef _LIZP_H
#define _LIZP_H

#include "cell.h"

extern Cell *repl_env;

int symbol_eq (const Cell *s1, const Cell *s2);

Cell *alist_assoc (const Cell *sym, Cell *alist);

Cell *env_find (Cell *env, const Cell *sym);

Cell *env_get (Cell *env, Cell *sym);

int env_set (Cell *env, Cell *sym, Cell *val);

Cell *env_create (Cell *env_outer, const Cell *binds, Cell *exprs);

Cell *eval_each (Cell *list, Cell *env);

Cell *make_bool_sym (int val);

Cell *eval_ast (Cell *ast, Cell *env);

void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);

int truthy (Cell *x);

int eval_special (Cell *head, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out);

Cell *READ (const char *start, int length);

Cell *EVAL (Cell *ast, Cell *env);

void PRINT (Cell *expr);

void rep (const char *start, int length, Cell *env);

void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func);

Cell *init (int ncells, int nchars);

#endif /* _LIZP_H */
