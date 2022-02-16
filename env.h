#ifndef _ENV_H
#define _ENV_H

#include "cell.h"

Cell *env_find (Cell *env, const Cell *sym);

Cell *env_get (Cell *env, const Cell *sym);

int env_set (Cell *env, const Cell *sym, Cell *val);

Cell *env_create (Cell *env_outer, Cell *binds, Cell *exprs);

#endif /* _ENV_H */
