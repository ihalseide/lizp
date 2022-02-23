#ifndef _ALL_H

// Number of special form symbols
#define SPECIAL_COUNT 6

typedef enum Native_fn Native_fn_t;
enum Native_fn
{
	FN_INVALID,
	FN_STR,
	FN_PR_STR,
	FN_PRN,
	FN_PRINTLN,
	FN_LIST,
	FN_EVAL,
	FN_SLURP,
	FN_READ_STR,
	FN_EMPTY_P,
	FN_LIST_P,
	FN_INT_P,
	FN_STRING_P,
	FN_FUNCTION_P,
	FN_COUNT,
	FN_EQ,
	FN_LT,
	FN_GT,
	FN_LTE,
	FN_GTE,
	FN_ADD,
	FN_SUB,
	FN_MUL,
	FN_DIV,
	FN_CONCAT,
	FN_FIRST,
	FN_REST,
};

typedef enum Cell_kind Cell_kind_t;
enum Cell_kind
{
	CK_INVALID,
	CK_INTEGER,
	CK_SYMBOL,
	CK_PAIR,
};

typedef struct cell Cell;
struct cell
{
	Cell_kind_t kind;
	union
	{
		int integer;
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
	 sym_cond,
	 sym_do,
	 sym_quote,
	 sym_string;

Cell *EVAL(Cell *ast, Cell *env);
Cell *READ(const char *start, int length);
void PRINT(Cell *expr);

Cell *alist_assoc(const Cell *key, Cell *alist);
Cell *apply_built_in(Native_fn_t fn, Cell *args);
Cell *cell_alloc(void);
Cell *cell_init(int kind);
Cell *env_create (Cell *env_outer, Cell *binds, Cell *exprs);
Cell *env_find (Cell *env, const Cell *sym);
Cell *env_get (Cell *env, const Cell *sym);
Cell *eval_ast(Cell *ast, Cell *env);
Cell *eval_each(Cell *list, Cell *env);
Cell *get_bool_sym(int v);
Cell *init(int ncells);
Cell *intern_find_symbol(const Cell *name);
Cell *intern_symbol(Cell *name);
Cell *list_pop(Cell **list);
Cell *make_empty_list(void);
Cell *make_fn(Cell *params, Cell *body);
Cell *make_fn_native (Native_fn_t id);
Cell *make_int(int n);
Cell *make_pair(Cell *first, Cell *rest);
Cell *make_pair_valid(Cell *first, Cell *rest);
Cell *make_single_list(Cell *p);
Cell *make_string_start(void);
Cell *make_symbol(Cell *name);
Cell *make_void(const void *vp);
Cell *string_join(Cell *items, char sep, int readable);
extern Cell *repl_env;
int cell_can_alloc(int n_cells);
int cell_eq(const Cell *a, const Cell *b);
int cell_validp(const Cell *p);
int char_symbolp(char c);
int emptyp(const Cell *p);
int env_set (Cell *env, Cell *sym, Cell *val);
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
int pr_str(Cell *x, char *out, int len, int readable);
int print_char(char c, char *out, int len);
int print_cstr(const char *s, char *out, int len);
int print_error(Cell *list, char *out, int len);
int print_int(int n, char *out, int len);
int print_list(Cell *list, char *out, int len, int readable);
int print_list_as_string(const Cell *list, char *out, int len, int readable);
int print_string (const char *str, char *out, int len, int readable);
int read_int(const char *start, int len, Cell **out);
int read_list(const char *start, int len, Cell **out);
int read_quoted_string(const char *start, int len, Cell **out);
int read_str(const char *start, int len, Cell **out);
int read_sym(const char *start, int len, Cell **out);
int read_until(const char *start, int len, char sentinel);
int specialp(const Cell *p);
int string_step(const char **stream, int *length, int n);
int string_to_list(const char *start, int length, int escape, Cell **out);
int stringp(const Cell *x);
int symbolp(const Cell *p);
int truthy(Cell *x);
void apply(Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);
void cell_free(Cell *p);
void cell_free_all(Cell *p);
void env_setup_fn(Cell *env, const char *str, Native_fn_t id);
void eval_special(Cell *sym, Cell *ast, Cell *env, Cell **val_out, Cell **env_out);
void print_nonreadably (Cell *expr);
void rep(const char *start, int length, Cell *env);
void string_skip_white(const char **stream, int *length);

#endif /* _ALL_H */
