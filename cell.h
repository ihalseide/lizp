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

int init_symbols (void);
Cell *intern_find_symbol (const Cell *name);
int intern_insert (Cell *sym);
Cell *intern_symbol (const Cell *name);
int native_fnp (const Cell *p);
int truthy (Cell *x);
enum Cell_kind kind_of (const Cell *p);
int cell_validp (const Cell *p);
Cell *int_to_p (int a);
int p_to_int (Cell *x);
int init_cells (int ncells);
Cell *cell_alloc (void);
void cell_free (Cell *p);
void cell_free_all (Cell *p);
Cell *cell_init (enum Cell_kind k);
int is_kind (const Cell *p, enum Cell_kind kind);
int functionp (const Cell *p);
int fn_arity (const Cell *p);
Cell *make_int (int n);
Cell *make_symbol (const Cell *name);
Cell *make_pair (Cell *first, Cell *rest);
Cell *make_pair_valid (Cell *first, Cell *rest);
Cell *make_empty_list (void);
Cell *make_single_list (Cell *p);
Cell *make_void (const void *vp);
Cell *make_lizp_fn (Cell *params, Cell *body);
Cell *make_native_fn (Native_fn func);
Cell *make_wrapped_native_fn (int n_params, Native_fn func);
int emptyp (const Cell *x);
int stringp (const Cell *x);
int nonempty_listp (const Cell *p);
int list_length (const Cell *list);
int list_push (Cell *item, Cell **list);
Cell *list_pop (Cell **list);
int cell_eq (const Cell *a, const Cell *b);
Cell *get_bool_sym (int v);
Cell *alist_assoc (const Cell *key, Cell *alist);
Cell *make_string_start (void);
int string_to_list (const char *start, int length, int escape, Cell **out);
Cell *string_join (Cell *items, char sep, int readable);
int string_step (const char **stream, int *length, int n);
void string_skip_white(const char **stream, int *length);
int function_nativep (const Cell *p);

#endif /* _CELL_H */
