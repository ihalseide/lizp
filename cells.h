#ifndef _CELLS_H
#define _CELLS_H

enum Cell_kind
{
	CK_INT,
	CK_STRING,
	CK_SYMBOL,
	CK_FUNC,
	CK_NATIVE_FUNC,
	CK_PAIR,
	CK_ATOM,
};

typedef struct cell Cell;
typedef void (*Native_fn)(Cell* args, Cell *env, Cell **out);

struct cell
{
	enum Cell_kind kind;
	union
	{
		int as_int;
		const char *as_str;
		Cell *as_atom;
		struct
		{
			Cell *first;
			Cell *rest;
		} as_pair;
		struct
		{
			int n_params;
			Native_fn func;
		} as_native_fn;
		struct
		{
			const Cell *params;
			const Cell *ast;
			Cell *env;
		} as_fn;
	};
};

int init_cells (int ncells);

Cell *cell_alloc ();

void cell_free (Cell *x);

Cell *cell_init (enum Cell_kind k);

int is_kind (const Cell *x, enum Cell_kind kind);

int is_function (const Cell *x);

Cell *make_int (int n);

Cell *make_native_fn (int n_params, Native_fn func);

Cell *make_pair (Cell *first, Cell *rest);

Cell *make_atom (Cell *ref);

Cell *make_symbol (const char *str);

Cell *make_string (const char *str);

Cell *make_fn (const Cell *params, const Cell *body, Cell *outer_env);

Cell *make_empty_list ();

int is_empty_list (const Cell *x);

int is_nonempty_list (const Cell *x);

int list_length (Cell *list);

void list_push (Cell *item, Cell **list);

Cell *list_pop (Cell **list);

int iter_remember (void);

Cell *list_iter_get_node (void);

int list_iter_begin (Cell *with_list);

int list_iter_endp (void);

Cell *list_iter_next (void);

Cell *list_iter_peek (void);

void list_iter_finish (void);

#endif /* _CELLS_H */
