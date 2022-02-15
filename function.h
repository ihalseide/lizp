#ifndef _FUNCTION_H
#define _FUNCTION_H

#include "cell.h"

void fn_str (Cell *args, Cell *env, Cell **val_out);

void fn_pr_str (Cell *args, Cell *env, Cell **val_out);

void fn_prn (Cell *args, Cell *env, Cell **val_out);

void fn_println (Cell *args, Cell *env, Cell **val_out);

void fn_list (Cell *args, Cell *env, Cell **val_out);

void fn_deref (Cell *args, Cell *env, Cell **val_out); 

void fn_atom_p (Cell *args, Cell *env, Cell **val_out);

void fn_atom (Cell *args, Cell *env, Cell **val_out);

void fn_eval (Cell *args, Cell *env, Cell **val_out);

void fn_slurp (Cell *args, Cell *env, Cell **val_out);

void fn_read_str (Cell *args, Cell *env, Cell **val_out);

void fn_empty_p (Cell *args, Cell *env, Cell **val_out);

void fn_count (Cell *args, Cell *env, Cell **val_out);

void fn_list_p (Cell *args, Cell *env, Cell **val_out);

void fn_int_p (Cell *args, Cell *env, Cell **val_out);

void fn_reset_bang (Cell *args, Cell *env, Cell **val_out);

void fn_eq (Cell *args, Cell *env, Cell **val_out);

void fn_lt (Cell *args, Cell *env, Cell **val_out);

void fn_gt (Cell *args, Cell *env, Cell **val_out);

void fn_lte (Cell *args, Cell *env, Cell **val_out);

void fn_gte (Cell *args, Cell *env, Cell **val_out);

void fn_add (Cell *args, Cell *env, Cell **val_out);

void fn_sub (Cell *args, Cell *env, Cell **val_out);

void fn_mul (Cell *args, Cell *env, Cell **val_out);

void fn_div (Cell *args, Cell *env, Cell **val_out);

void fn_swap_bang (Cell *args, Cell *env, Cell **val_out);

void fn_pair (Cell *args, Cell *env, Cell **val_out);

void fn_concat (Cell *args, Cell *env, Cell **val_out);

void fn_assoc (Cell *args, Cell *env, Cell **val_out);

#endif /* _FUNCTION_H */
