#ifndef _SYMBOL_H
#define _SYMBOL_H

#include "cell.h"

extern Cell *symbol_list;

int init_symbols (void);

const Cell *intern_find_symbol (const Cell *name);

const Cell *intern_insert (const Cell *string);

const Cell *intern_symbol (const Cell *name);

#endif /* _SYMBOL_H */
