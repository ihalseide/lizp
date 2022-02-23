#ifndef _CELL_H
#define _CELL_H

typedef enum Cell_kind Cell_kind_t;
enum Cell_kind
{
	CK_INVALID = 0,
	CK_INTEGER,
	CK_SYMBOL,
	CK_PAIR,
	CK_STREAM,
};

typedef struct cell Cell;
struct cell
{
	Cell_kind_t kind;
	union
	{
		int integer;
		Cell *sym_name;
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

int string_step(const char **stream, int *length, int n);
int string_to_list(const char *start, int length, int escape, Cell **out);
void string_skip_white(const char **stream, int *length);

Cell *alist_assoc(const Cell *key, Cell *alist);
Cell *cell_alloc(void);
Cell *cell_init(int kind);
Cell *get_bool_sym(int v);
Cell *intern_find_symbol(const Cell *name);
Cell *intern_symbol(Cell *name);
Cell *list_pop(Cell **list);
Cell *make_empty_list(void);
Cell *make_fn_native (int id);
Cell *make_int(int n);
Cell *make_lizp_fn(Cell *params, Cell *body);
Cell *make_pair(Cell *first, Cell *rest);
Cell *make_pair_valid(Cell *first, Cell *rest);
Cell *make_single_list(Cell *p);
Cell *make_string_start(void);
Cell *make_symbol(Cell *name);
Cell *make_void(const void *vp);
Cell *string_join(Cell *items, char sep, int readable);
int cell_can_alloc(int n_cells);
int cell_eq(const Cell *a, const Cell *b);
int cell_validp(const Cell *p);
int emptyp(const Cell *p);
int function_nativep(const Cell *p);
int functionp(const Cell *p);
int init_cells(int ncells);
int init_symbols(void);
int intern_insert(Cell *sym);
int intp(const Cell *p);
int kind_of(const Cell *p);
int list_length(const Cell *list);
int list_push(Cell *item, Cell **list);
int native_fnp(const Cell *p);
int nonempty_listp(const Cell *p);
int pairp(const Cell *p);
int stringp(const Cell *x);
int symbolp(const Cell *p);
int truthy(Cell *x);
void cell_free(Cell *p);
void cell_free_all(Cell *p);

#endif /* _CELL_H */
