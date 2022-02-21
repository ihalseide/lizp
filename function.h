#ifndef _FUNCTION_H
#define _FUNCTION_H

#include "cell.h"

Cell *fn_str (Cell *args);
Cell *fn_pr_str (Cell *args);
Cell *fn_prn (Cell *args);
Cell *fn_println (Cell *args);
Cell *fn_list (Cell *args);
Cell *fn_deref (Cell *args); 
Cell *fn_atom_p (Cell *args);
Cell *fn_atom (Cell *args);
Cell *fn_eval (Cell *args);
Cell *fn_slurp (Cell *args);
Cell *fn_read_str (Cell *args);
Cell *fn_empty_p (Cell *args);
Cell *fn_count (Cell *args);
Cell *fn_list_p (Cell *args);
Cell *fn_int_p (Cell *args);
Cell *fn_reset_bang (Cell *args);
Cell *fn_eq (Cell *args);
Cell *fn_lt (Cell *args);
Cell *fn_gt (Cell *args);
Cell *fn_lte (Cell *args);
Cell *fn_gte (Cell *args);
Cell *fn_add (Cell *args);
Cell *fn_sub (Cell *args);
Cell *fn_mul (Cell *args);
Cell *fn_div (Cell *args);
Cell *fn_swap_bang (Cell *args);
Cell *fn_pair (Cell *args);
Cell *fn_concat (Cell *args);
Cell *fn_assoc (Cell *args);
Cell *fn_first (Cell *args);
Cell *fn_rest (Cell *args);
void print_nonreadably (Cell *expr);

#endif /* _FUNCTION_H */
