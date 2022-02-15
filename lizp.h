#ifndef _LIZP_H
#define _LIZP_H

#include "cells.h"

Cell *repl_env;

int char_is_symbol (char c);

int iter_remember (void);

Cell *list_iter_get_node (void);

int list_iter_begin (Cell *with_list);

int list_iter_endp (void);

Cell *list_iter_next (void);

Cell *list_iter_peek (void);

void list_iter_finish (void);

int string_can_alloc (int length);

char *string_alloc (int length);

char *string_pool_write (const char *start, int length);

char *string_pool_write_c (const char *str);

int string_equal (const Cell *string_cell, const char *str, int length);

Cell *find_string (const char *start, int length);

Cell *insert_string (const char *str);

Cell *intern_string (const char *start, int length);

void string_step (const char **stream, int *length, int n);

void string_skip_white(const char **stream, int *length);

int symbol_eq (const Cell *s1, const Cell *s2);

Cell *alist_assoc (const Cell *sym, Cell *alist);

Cell *env_find (Cell *env, const Cell *sym);

Cell *env_get (Cell *env, Cell *sym);

int env_set (Cell *env, Cell *sym, Cell *val);

Cell *env_create (Cell *env_outer, const Cell *binds, Cell *exprs);

int parse_int (const char *start, int length, int *out);

int read_string_literal (const char *start, int length, Cell **out);

int read_item (const char *start, int length, Cell **out);

int read_str (const char *start, int length, Cell **out);

char char_end_brace (char x);

int read_list (const char *start, int length, Cell **out);

int read_until (const char *start, int length, char sentinel);

int read_str (const char *start, int length, Cell **out);

int print_char (char c, char *out, int length);

int print_cstr (char *s, char *out, int length);

int print_string (const char *str, char *out, int length, int readable);

int print_int (int n, char *out, int length);

int print_list (Cell *list, char *out, int length, int readable);

int pr_str (Cell *x, char *out, int length, int readable);

Cell *READ (const char *start, int length);

Cell *eval_each (Cell *list, Cell *env);

int cell_eq (Cell *a, Cell *b);

Cell *make_bool_sym (int val);

Cell *string_join (const Cell *args, char sep, int readable);

Cell *eval_ast (Cell *ast, Cell *env);

void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);

int truthy (Cell *x);

int eval_special (Cell *head, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out);

Cell *EVAL (Cell *ast, Cell *env);

void PRINT (Cell *expr);

void rep (const char *start, int length, Cell *env);

void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func);

Cell *init (int ncells, int nchars);

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

#endif /* _LIZP_H */
