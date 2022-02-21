#ifndef _CELL_H
#define _CELL_H

enum Cell_kind
{
	CK_INVALID,
	CK_FUNCTION,
	CK_INTEGER,
	CK_SYMBOL,
	CK_PAIR,
};

typedef struct cell Cell;
typedef void (*Native_fn)(Cell* args, Cell *env, Cell **out);

struct cell
{
	enum Cell_kind kind;
	union
	{
		int integer;
		const Cell *sym_name;
		Native_fn func;
		struct
		{
			Cell *first;
			Cell *rest;
		};
	};
};

// Static symbols
extern Cell sym_nil,
	 sym_t,
	 sym_f,
	 sym_native_fn,
	 sym_fn,
	 sym_def_bang,
	 sym_let_star,
	 sym_fn_star,
	 sym_if,
	 sym_do,
	 sym_quote,
	 sym_string;

Cell *alist_assoc (const Cell *key, Cell *alist);
Cell *cell_alloc (void);
Cell *cell_init (enum Cell_kind k);
Cell *get_bool_sym (int v);
Cell *intern_find_symbol (const Cell *name);
Cell *intern_symbol (const Cell *name);
Cell *list_pop (Cell **list);
Cell *make_empty_list (void);
Cell *make_int (int n);
Cell *make_lizp_fn (Cell *params, Cell *body);
Cell *make_native_fn (Native_fn func);
Cell *make_pair (Cell *first, Cell *rest);
Cell *make_pair_valid (Cell *first, Cell *rest);
Cell *make_single_list (Cell *p);
Cell *make_string_start (void);
Cell *make_symbol (const Cell *name);
Cell *make_void (const void *vp);
Cell *make_wrapped_native_fn (int n_params, Native_fn func);
Cell *string_join (Cell *items, char sep, int readable);
enum Cell_kind kind_of (const Cell *p);
int cell_eq (const Cell *a, const Cell *b);
int cell_validp (const Cell *p);
int emptyp (const Cell *x);
int fn_arity (const Cell *p);
int function_nativep (const Cell *p);
int functionp (const Cell *p);
int init_cells (int ncells);
int init_symbols (void);
int intern_insert (Cell *sym);
int intp (const Cell *p);
int list_length (const Cell *list);
int list_push (Cell *item, Cell **list);
int native_fnp (const Cell *p);
int nonempty_listp (const Cell *p);
int pairp (const Cell *p);
int string_step (const char **stream, int *length, int n);
int string_to_list (const char *start, int length, int escape, Cell **out);
int stringp (const Cell *x);
int symbolp (const Cell *p);
int truthy (Cell *x);
void cell_free (Cell *p);
void cell_free_all (Cell *p);
void string_skip_white(const char **stream, int *length);

#endif /* _CELL_H */
