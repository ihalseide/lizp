#ifndef _CELL_H
#define _CELL_H

enum Cell_kind
{
	CK_INVALID,
	CK_VOID,
	CK_INT,
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
		int integer;           // CK_INT
		const Cell *sym_name;  // CK_SYMBOL
		const void *pointer;   // CK_VOID
		struct                 // CK_PAIR
		{
			Cell *first;
			Cell *rest;
		};
	};
};

enum Cell_kind kind_of (const Cell *p);

int cell_validp (const Cell *p);

Cell *int_to_p (int a);

int p_to_int (Cell *x);

int init_cells (int ncells);

Cell *cell_alloc (void);

void cell_free (Cell *p);

Cell *cell_init (enum Cell_kind k);

int is_kind (const Cell *p, enum Cell_kind kind);

int functionp (Cell *x);

Cell *make_int (int n);

Cell *make_symbol (const Cell *name);

Cell *make_pair (Cell *first, Cell *rest);

Cell *make_empty_list (void);

Cell *make_void (const void *vp);

Cell *make_native_fn (int n_params, Native_fn func);

int emptyp (const Cell *x);

int nonempty_listp (const Cell *p);

int list_length (const Cell *list);

void list_push (Cell *item, Cell **list);

Cell *list_pop (Cell **list);

int cell_eq (const Cell *a, const Cell *b);

Cell *string_to_list (const char *str);

Cell *get_bool_sym (int v);

Cell *alist_assoc (const Cell *key, Cell *alist);

int nilp (const Cell *p);

int truep (const Cell *p);

int falsep (const Cell *p);

int native_fnp (const Cell *p);

int truthy (Cell *x);

#endif /* _CELL_H */
