#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <setjmp.h>

#include "lizp.h"

// Handle errors with longjmp
jmp_buf eval_error;
static const char *error_msg = NULL;

// Cell allocator
static Cell *cell_pool = NULL;
static int cell_pool_cap = 0;

// List of internal symbols
static Cell *symbol_list = NULL;

// Top-level REPL environment
Cell *repl_env;

// Constant built-in symbols
// "static"
Cell sym_nil,
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

static _Noreturn void error(const char *msg)
{
	error_msg = msg;
	longjmp(eval_error, 99);
}

const char *get_error_msg(void)
{
	return error_msg;
}

int specialp(const Cell *p)
{
	_Static_assert(SPECIAL_COUNT == 6, "handle all special symbols");
	return p == &sym_def_bang
		|| p == &sym_let_star
		|| p == &sym_fn_star
		|| p == &sym_cond
		|| p == &sym_do
		|| p == &sym_quote;
}

int kind_of(const Cell *p)
{
	if (cell_validp(p))
		return p->kind;
	else
		return CK_INVALID;
}

// Returns: whether a cell pointer is a special / "static" pointer
int static_symp(const Cell *p)
{
	return p == &sym_nil
		|| p == &sym_t
		|| p == &sym_f
		|| p == &sym_native_fn
		|| p == &sym_fn
		|| p == &sym_def_bang
		|| p == &sym_let_star
		|| p == &sym_fn_star
		|| p == &sym_cond
		|| p == &sym_do
		|| p == &sym_quote
		|| p == &sym_string;
}

// Returns: whether a pointer is a valid pointer in the cell pool
int cell_valid_pooledp(const Cell *p)
{
	return cell_pool && (p >= cell_pool) && (p < (cell_pool + cell_pool_cap));
}

// Valid cell pointer?
int cell_validp(const Cell *p)
{
	return p && (static_symp(p) || cell_valid_pooledp(p));
}

// Is there room to allocate n_cells?
int cell_can_alloc(int n_cells)
{
	return cell_pool && (cell_pool + n_cells <= cell_pool + cell_pool_cap);
}

// Get a new cell (un-initialized)
Cell *cell_alloc(void)
{
	if (!cell_pool)
		error("cells not initialized");

	Cell *p = cell_pool->rest;

	if (cell_validp(p))
	{
		// Remove the cell from the free list
		cell_pool->rest = p->rest;
		return p;
	}
	else
	{
		error("cell_alloc : error : out of memory");
	}
}

void cell_free(Cell *p)
{
	// Do not try to free non-pool cells
	if (cell_valid_pooledp(p))
	{
		// Turn x into a pair and put it into the free list
		p->kind = CK_PAIR;
		p->first = NULL;
		p->rest = cell_pool->rest;
		cell_pool->rest = p;
	}
}

// Free the cell and all sub-items if its a list
void cell_free_all(Cell *p)
{
	while (nonempty_listp(p))
	{
		cell_free_all(p->first);
		p = p->rest;
	}
	cell_free(p);
}

// Returns: a new cell with its kind set to k
// (every other field is not guaranteed to have a certain value)
Cell *cell_init(int k)
{
	Cell *x = cell_alloc();
	if (x)
		x->kind = k;
	return x;
}

// Returns: if a the cell pointer is a valid pointer of the kind
static int is_kind(const Cell *p, int kind)
{
	return cell_validp(p) && (kind_of(p) == kind);
}

int symbolp(const Cell *p)
{
	return is_kind(p, CK_SYMBOL);
}

int pairp(const Cell *p)
{
	return is_kind(p, CK_PAIR);
}

int intp(const Cell *p)
{
	return is_kind(p, CK_INTEGER);
}

Cell *make_int(int n)
{
	Cell *p = cell_init(CK_INTEGER);
	if (cell_validp(p))
		p->integer = n;
	return p;
}

Cell *make_symbol(Cell *name)
{
	Cell *p = cell_init(CK_SYMBOL);
	if (cell_validp(p))
		p->first = name;
	return p;
}

Cell *make_pair(Cell *first, Cell *rest)
{
	Cell *p = cell_init(CK_PAIR);
	if (cell_validp(p))
	{
		p->first = first;
		p->rest = rest;
	}
	return p;
}

// Like make_pair, but fails if any of the arguments are invalid
Cell *make_pair_valid(Cell *first, Cell *rest)
{
	if (cell_validp(first) && cell_validp(rest))
		return make_pair(first, rest);
	else
		return NULL;
}

// Returns: []
Cell *make_empty_list(void)
{
	return make_pair(NULL, &sym_nil);
}

// Returns: [<p>]
Cell *make_single_list(Cell *p)
{
	return make_pair_valid(p, &sym_nil);
}

// Returns: [{string}]
Cell *make_string_start(void)
{
	return make_single_list(&sym_string);
}

// Returns: list of the form [{fn} params body]
Cell *make_fn(Cell *params, Cell *body)
{
	// Validate arguments
	assert(pairp(params));
	assert(cell_validp(body));

	// Check that the parameters are all symbols
	Cell *p = params;
	while (nonempty_listp(p))
	{
		if (!symbolp(p->first))
			error("parameter list must all be symbols");

		// Next
		p = p->rest;
	}

	// Create list of the form [{fn} params body]
	return make_pair_valid(&sym_fn,
			make_pair_valid(params,
				make_single_list(body)));
}

// Returns: list of the form [{native-fn} id]
Cell *make_fn_native(Native_fn_t id)
{
	return make_pair_valid(&sym_native_fn,
			make_single_list(make_int(id)));
}

// The only valid false values nil and #f
int truthy(Cell *x)
{
	if (!cell_validp(x))
		return 0;
	else
		return x != &sym_nil && x != &sym_f;
}

// Is empty list?
// Note: NULL is not considered a list
int emptyp(const Cell *p)
{
	return is_kind(p, CK_PAIR) && (p->first == NULL);
}

int nonempty_listp(const Cell *p)
{
	return is_kind(p, CK_PAIR) && !emptyp(p);
}

int list_length(const Cell *list)
{
	int n;
	for (n = 0; nonempty_listp(list); n++)
		list = list->rest;
	return n;
}

// Push an item in front of a list (which may be empty)
// Returns 0 upon success
int list_push(Cell *item, Cell **list)
{
	// Validate arguments
	if (!cell_validp(item) || !is_kind(*list, CK_PAIR))
		return 1;

	if (emptyp(*list))
	{
		// An empty list has nothing in the first slot yet
		(*list)->first = item;
		return 0;
	}
	else
	{
		// Non-empty list
		Cell *node = make_pair(item, *list);
		if (cell_validp(node))
		{
			*list = node;
			return 0;
		}
		else
		{
			return 1;
		}
	}
}

// Remove and return the item in the front of a list
Cell *list_pop(Cell **list)
{
	if (!is_kind(*list, CK_PAIR) || emptyp(*list))
		return NULL;

	Cell *val = (*list)->first;
	*list = (*list)->rest;
	return val;
}

int cell_eq(const Cell *a, const Cell *b)
{
	// While loop for tail-calls on lists
	while (1)
	{
		// Nothing equals NULL
		if (!a || !b)
			return 0;

		// Compare pointers
		if (a == b)
			return 1;

		// Must be the same kind
		if (a->kind != b->kind)
			return 0;

		switch (a->kind)
		{
			case CK_INTEGER:
				return a->integer == b->integer;
			case CK_PAIR:
				if (emptyp(a))
				{
					return emptyp(b);
				}
				else if (!cell_eq(a->first, b->first))
				{
					return 0;
				}
				else if (!a->rest && !b->rest)
				{
					return 1;
				}
				else
				{
					// Tail call
					a = a->rest;
					b = b->rest;
					continue;
				}
			case CK_SYMBOL:
				// Compare name pointers
				return a->first == b->first;
			default:
				assert(0 && "invalid cell kind");
				return 0;
		}
	}
}

// Find a symbol in an alist.
// Returns the slot with [symbol | value]
// An alist is a list of the form [[symbol . value] [symbol . value] ....]
Cell *alist_assoc(const Cell *key, Cell *alist)
{
	// Validate inputs
	if (!key || !is_kind(alist, CK_PAIR))
		return NULL;

	// Iterate through the list
	Cell *p = alist;
	while (nonempty_listp(p))
	{
		// Check if slot has same key
		Cell *slot = p->first;
		if (is_kind(slot, CK_PAIR) && cell_eq(slot->first, key))
			return slot;

		// Next
		p = p->rest;
	}

	// Not found
	return NULL;
}

// Returns: symbol cell or NULL
Cell *intern_find_symbol(const Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	// Search symbol list and compare names
	Cell *p = symbol_list;
	while (nonempty_listp(p))
	{
		Cell *sym = p->first;
		assert(symbolp(sym));
		if (cell_eq(name, sym->first))
			return sym;

		// Next node
		p = p->rest;
	}

	// Symbol not found
	return NULL;
}

// Returns 0 upon success
int intern_insert(Cell *symbol)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(symbol, CK_SYMBOL))
		return 1;

	// Push to front of symbol list
	return list_push(symbol, &symbol_list);
}

// Returns symbol cell or NULL
Cell *intern_symbol(Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	Cell *result = intern_find_symbol(name);
	if (result)
	{
		return result;
	}
	else
	{
		Cell *new_sym = make_symbol(name);
		// (intern_insert returns 0 upon success)
		if (intern_insert(new_sym))
			return NULL;
		else
			return new_sym;
	}
}

Cell *get_bool_sym(int v)
{
	if (v)
		return &sym_t;
	else
		return &sym_f;
}

int stringp(const Cell * p)
{
	return nonempty_listp(p) && p->first == &sym_string;
}

// Initialize a special symbol
// Returns 0 upon success
void init_static_sym(Cell *p, const char *name)
{
	assert(cell_validp(p));
	assert(name);

	Cell *string = NULL;
	int string_len = strlen(name);
	int parsed_len = string_to_list(name, string_len, 0, &string);
	assert(stringp(string));
	assert(parsed_len == string_len);

	// Initialize the symbol
	p->kind = CK_SYMBOL;
	p->first = string;

	// Add it to the internal symbol list
	intern_insert(p);
}

// Return 0 upon success
int init_symbols(void)
{
	symbol_list = make_empty_list();

	if (!symbol_list)
		return 1;

	init_static_sym(&sym_nil,       "nil");
	init_static_sym(&sym_t,         "#t");
	init_static_sym(&sym_f,         "#f");
	init_static_sym(&sym_def_bang,  "def!");
	init_static_sym(&sym_fn_star,   "fn*");
	init_static_sym(&sym_let_star,  "let*");
	init_static_sym(&sym_do,        "do");
	init_static_sym(&sym_cond,      "cond");
	init_static_sym(&sym_quote,     "quote");
	init_static_sym(&sym_native_fn, "{c-function}");
	init_static_sym(&sym_fn,        "{function}");
	init_static_sym(&sym_string,    "{string}");

	return 0;
}

// Returns 0 upon success
int init_cells(int ncells)
{
	if (ncells <= 0)
		return 1;

	// Allocate the cell array
	// Once it is allocated, the pointer should not be changed
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
		return 1;

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	for (int i = 0; i < cell_pool_cap; i++)
	{
		cell_pool[i].kind = CK_PAIR;
		cell_pool[i].first = NULL;
		cell_pool[i].rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].rest = &sym_nil;

	return init_symbols();
}

// Returns: the number of chars parsed.
// [{string} ...] -> out
int string_to_list(const char *start, int length, int escape, Cell **out)
{
	// Validate args
	if (!start || length <= 0)
		return 0;

	const char *view = start;
	Cell *list = make_string_start();
	Cell *p = list;

	while (pairp(p) && *view && length)
	{
		// Get next char to add to end of string
		char c = *view;

		// Maybe process escape codes
		if (escape && length && (c == '\\'))
		{
			string_step(&view, &length, 1);
			if (!length)
			{
				if (out)
					*out = NULL;
				cell_free_all(list);
				return view - start;
			}

			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
				case '0': c = '\0'; break;
				default: c = c; break;
			}
		}
		string_step(&view, &length, 1);

		// Put list with char in REST slot
		p->rest = make_single_list(make_int(c));
		p = p->rest;
	}

	// See if p being invalid is what caused the while loop to stop
	if (pairp(p))
	{
		if (out)
			*out = list;
		return view - start;
	}
	else
	{
		if (out)
			*out = NULL;
		cell_free_all(list);
		return view - start;
	}
}

// Join together the string representation of the items into a new string
Cell *string_join(Cell *items, char sep, int readable)
{
	Cell *result = make_string_start();
	Cell *p_result = result;

	// Make the separator character
	Cell *sep_val;
	if (sep)
		sep_val = make_int(sep);
	else
		sep_val = NULL;

	Cell *p = items;
	int firstloop = 1;
	while (nonempty_listp(p))
	{
		Cell *item = p->first;

		// Add separator before the item (except the first item)
		if (sep_val && !firstloop)
		{
			p_result->rest = make_single_list(sep_val);
			p_result = p_result->rest;
		}

		// Make sure the item is always a string
		if (!stringp(item))
		{
			char buffer[4 * 1024];
			int len = pr_str(item, buffer, sizeof(buffer) - 1, readable);
			int len2 = string_to_list(buffer, len, readable, &item);
			assert(len == len2);
		}
		assert(stringp(item));

		// Use the item string's content
		Cell *p_item = item->rest;
		while (nonempty_listp(p_item))
		{
			Cell *sub_item = p_item->first;
			assert(intp(sub_item));

			// Add sub-item character to result string
			p_result->rest = make_single_list(sub_item);
			p_result = p_result->rest;

			// Next sub-item
			p_item = p_item->rest;
		}

		// Next item
		p = p->rest;
		firstloop = 0;
	}

	return result;
}

// For string reading and writing
// Will not modify characters in the stream
int string_step(const char **stream, int *length, int n)
{
	// Validate inputs
	assert(stream);
	assert(length);
	assert(*stream);
	if (n > *length)
		return 0;

	*stream += n;
	*length -= n;
	return n;
}

// Will not modify characters in the stream
void string_skip_white(const char **stream, int *length)
{
	if (!stream)
		return;

	const char *view = *stream;
	int rem = *length;
	while (isspace(*view) && (rem > 0))
		string_step(&view, &rem, 1);
	*stream = view;
	*length = rem;
}

// Special pair: function
int functionp(const Cell *p)
{
	return pairp(p)
		&& (p->first == &sym_native_fn || p->first == &sym_fn)
		&& pairp(p->rest);
}

// Returns: whether p is a native (built-in) function list
int function_nativep(const Cell *p)
{
	return functionp(p) && (p->first == &sym_native_fn);
}

// Find the innermost env which contains symbol
Cell *env_find (Cell *env, const Cell *sym)
{
	assert(pairp(env));
	assert(symbolp(sym));

	// Search up the environment hierarchy
	// Note: env = [alist | outer]
	while (nonempty_listp(env))
	{
		// Search current environment
		if (alist_assoc(sym, env->first))
			return env;

		// Next
		env = env->rest;
	}

	return NULL;
}

// Get the innermost definition for the symbol.
// Returns:
//   when found -> the slot containing (symbol . value)
//   when not found -> nil
Cell *env_get (Cell *env, const Cell *sym)
{
	assert(pairp(env));
	assert(symbolp(sym));

	// Find the environment which contains the symbol
	env = env_find(env, sym);
	if (env)
		return alist_assoc(sym, env->first);

	// Symbol not found
	return NULL;
}

// Returns 0 upon success
int env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!pairp(env) || !symbolp(sym))
		return 1;

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = alist_assoc(sym, env->first);
	if (slot)
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->rest = val;
		return 0;
	}
	else
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		slot = make_pair_valid(sym, val);
		return pairp(slot) && list_push(slot, &(env->first));
	}
}

// An environment is a list of the form (alist . outer-env).
// Create an environment with each item of the
// "binds" list set to the corresponding item in the "exprs" list.
Cell *env_create (Cell *env_outer, Cell *binds, Cell *exprs)
{
	assert(env_outer == &sym_nil || pairp(env_outer));
	Cell *env = make_pair_valid(make_empty_list(), env_outer);

	// Create the bindings by iterating both lists
	while (nonempty_listp(binds) && nonempty_listp(exprs))
	{
		// Bind 1 pair
		Cell *sym = binds->first;
		Cell *val = exprs->first;
		int fail = env_set(env, sym, val);
		if (fail)
			break;

		// Next
		binds = binds->rest;
		exprs = exprs->rest;
	}

	// Check if all of the lists were used up
	if (nonempty_listp(binds) || nonempty_listp(exprs))
		return NULL;
	else
		return env;
}

void print_nonreadably (Cell *expr)
{
	static char buffer[2 * 1024];
	int p_len = pr_str(expr, buffer, sizeof(buffer), 0);
	printf("%.*s\n", p_len, buffer);
}

static Cell *apply_built_in_var (Native_fn_t id, Cell *args)
{
	assert(pairp(args));
	switch (id)
	{
		case FN_STR:
			// [str a b c ...] -> "abc..." (prints non-readably)
			return string_join(args, 0, 0);
		case FN_PR_STR:
			// [pr-str a b c ...] -> "a b c ..." (prints readably)
			return string_join(args, ' ', 1);
		case FN_PRN:
			// [prn a b c ...] -> nil (prints readably)
			{
				Cell *s = string_join(args, ' ', 1);
				if (stringp(s))
				{
					PRINT(s);
					cell_free_all(s);
				}
				return &sym_nil;
			}
		case FN_PRINTLN:
			// [println a b c ...] -> nil (prints non-readably)
			{
				Cell *s = string_join(args, ' ', 0);
				if (stringp(s))
				{
					print_nonreadably(s);
					cell_free_all(s);
				}
				return &sym_nil;
			}
		case FN_LIST:
			// [list ...] -> [...] (variadic)
			return args;
		case FN_CONCAT:
			// [concat l1 l2 ...] -> list
			{
				Cell *list = make_empty_list();
				Cell *p = list;

				while (nonempty_listp(args))
				{
					// Current list from arguments
					Cell *a = args->first;
					if (!pairp(a) || functionp(a) || stringp(a))
						error("arguments must be lists");

					// Add all of the items from the current list
					while (nonempty_listp(a))
					{
						// Put item into the list
						p->first = a->first;
						p->rest = make_empty_list();
						p = p->rest;

						// Next argument
						a = a->rest;
					}
					// The list should end with null instead of an empty list
					p->rest = NULL;

					// Next argument
					args = args->rest;
				}

				return list;
			}
		default:
			assert(0);
	}
}

static Cell *apply_built_in_1 (Native_fn_t id, Cell *args)
{
	if (1 != list_length(args))
		error("function takes 1 argument");

	Cell *p1 = args->first;
	if (!cell_validp(p1))
		error("invalid argument");

	switch (id)
	{
		case FN_EVAL:
			assert(0 && "should not reach here");
		case FN_SYMBOL_P:
			return get_bool_sym(symbolp(p1));
		case FN_SLURP:
			// [slurp "file name"] -> "file contents"
			{
				// Validate arguments
				if (!stringp(p1))
					error("1st argument must be a string file name");

				// Get string of file name
				char path[1024];
				int len = pr_str(p1, path, sizeof(path) - 1, 0);
				path[len] = '\0';

				FILE *f = fopen(path, "r");
				if (!f)
					error("could not read file");

				// Get file length
				fseek(f, 0, SEEK_END);
				long fsize = ftell(f);
				fseek(f, 0, SEEK_SET);

				// See if we have enough room for this string data
				if (!cell_can_alloc(fsize + 1))
				{
					fclose(f);
					error("not enough memory to read file");
				}

				// Read the char data and null-terminate it.
				// Stack allocate the file contents because it's
				// getting converte to a lisp string anyways...
				char content[fsize + 1];
				fread(content, fsize, 1, f);
				fclose(f);
				content[fsize] = 0;

				// Convert to lisp string
				Cell *s;
				int parse = string_to_list(content, fsize, 0, &s);
				assert(parse == fsize);

				return s;
			}
		case FN_READ_STR:
			// [read-string "str"] -> any value
			{
				if (stringp(p1))
				{
					char buffer[4 * 1024];
					int len = pr_str(p1, buffer, sizeof(buffer), 0);
					assert((unsigned) len < sizeof(buffer));
					Cell *b;
					read_str(buffer, len, &b);
					return b;
				}
				else
				{
					error("argument must be a string");
				}
			}
		case FN_EMPTY_P:
			// [empty? x]
			return get_bool_sym(emptyp(p1));
		case FN_STRING_P:
			// [string? x]
			return get_bool_sym(stringp(p1));
		case FN_FUNCTION_P:
			// [function? x]
			return get_bool_sym(functionp(p1));
		case FN_COUNT:
			// [count list]
			if (pairp(p1))
				return make_int(list_length(p1));
			else
				error("first argument must be a list");
		case FN_LIST_P:
			// [list? x]
			return get_bool_sym(pairp(p1));
		case FN_INT_P:
			// [int? x]
			return get_bool_sym(intp(p1));
		case FN_FIRST:
			// [first pair]
			if (emptyp(p1))
				return &sym_nil;
			else if (pairp(p1))
				return p1->first;
			else
				error("first argument must be a list");
		case FN_REST:
			// [rest pair]
			if (emptyp(p1))
				return &sym_nil;
			else if (pairp(p1))
				return p1->rest;
			else
				error("first argument must be a list");
		default:
			assert(0);
	}
}

static Cell *apply_built_in_2int (Native_fn_t id, Cell *p1, Cell *p2)
{
	int n1, n2;

	if (intp(p1))
		n1 = p1->integer;
	else
		error("first argument must be an integer");

	if (intp(p2))
		n2 = p2->integer;
	else
		error("second argument must be an integer");

	switch (id)
	{
		case FN_LT: 
			return get_bool_sym(n1 < n2);
		case FN_GT: 
			return get_bool_sym(n1 > n2);
		case FN_LTE:
			return get_bool_sym(n1 <= n2);
		case FN_GTE:
			return get_bool_sym(n1 >= n2);
		case FN_ADD:
			return make_int(n1 + n2);
		case FN_SUB:
			return make_int(n1 - n2);
		case FN_MUL:
			return make_int(n1 * n2);
		case FN_DIV:
			if (n2 == 0)
				error("division by zero");
			else
				return make_int(n1 / n2);
		default:
			assert(0);
	}
}

static Cell *apply_built_in_2 (Native_fn_t id, Cell *args)
{
	assert(pairp(args));
	
	if (2 != list_length(args))
		error("functions requires 2 arguments");

	Cell *p1 = args->first;
	if (!cell_validp(p1))
		error("invalid first argument");

	Cell *p2 = args->rest->first;
	if (!cell_validp(p2))
		error("invalid second argument");

	switch (id)
	{
		case FN_EQ:
			// [= x y]
			return get_bool_sym(cell_eq(p1, p2));
		case FN_LT:
		case FN_GT:
		case FN_LTE:
		case FN_GTE:
		case FN_ADD:
		case FN_SUB:
		case FN_MUL:
		case FN_DIV:
			return apply_built_in_2int(id, p1, p2);
		default:
			assert(0);
	}
}

Cell *apply_built_in (Native_fn_t id, Cell *args)
{
	assert(pairp(args));

	switch (id)
	{
		case FN_EVAL:
			assert(0 && "should not be handled here");
		case FN_SLURP:
		case FN_READ_STR:
		case FN_EMPTY_P:
		case FN_COUNT:
		case FN_LIST_P:
		case FN_INT_P:
		case FN_FIRST:
		case FN_REST:
		case FN_STRING_P:
		case FN_FUNCTION_P:
		case FN_SYMBOL_P:
			return apply_built_in_1(id, args);
		case FN_EQ:
		case FN_LT:
		case FN_GT:
		case FN_LTE:
		case FN_GTE:
		case FN_ADD:
		case FN_SUB:
		case FN_MUL:
		case FN_DIV:
			return apply_built_in_2(id, args);
		case FN_STR:
		case FN_PR_STR:
		case FN_PRN:
		case FN_PRINTLN:
		case FN_LIST:
		case FN_CONCAT:
			return apply_built_in_var(id, args);
		case FN_INVALID:
		default:
			assert(0 && "invalid built-in function id");
	}
}

// Does: evaluate each item of list x, without modifying x.
// Returns: a new list.
Cell *eval_each (Cell *list, Cell *env)
{
	assert(pairp(list));
	assert(pairp(env));

	if (emptyp(list))
		return list;

	// Eval the first element
	Cell *y = make_single_list(EVAL(list->first, env));
	Cell *p_y = y;

	// eval the rest of the elements
	list = list->rest;
	while (list && p_y)
	{
		if (!pairp(list))
		{
			// dotted list
			p_y->rest = EVAL(list, env);
			break;
		}

		// Fill in next slot of y
		p_y->rest = make_pair(EVAL(list->first, env), NULL);

		// next
		list = list->rest;
		p_y = p_y->rest;
	}

	return y;
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	// Validate args
	assert(env);
	if (!cell_validp(ast))
		error("eval : invalid expression");
	if (!pairp(env))
		error("eval : invalid environment");

	switch (ast->kind)
	{
		case CK_INTEGER:
			// Self-evaluating
			return ast;
		case CK_PAIR:
			// Evaluate each element in a list
			return eval_each(ast, env);
		case CK_SYMBOL:
			// Look up symbol's value...
			if (cell_eq(ast, &sym_nil) || cell_eq(ast, &sym_t) || cell_eq(ast, &sym_f))
			{
				// These special symbols are self-evaluating
				return ast;
			}
			else
			{
				// Get the value out of the environment's slot
				Cell *slot = env_get(env, ast);
				if (pairp(slot))
					return slot->rest;
				else
				{
					PRINT(ast);
					error("undefined symbol");
				}
			}
		default:
			assert(0);
	}
}

// Returns values through val_out, env_out
// For proper tail-call recursion, the next ast and env are both returned for EVAL to use.
// Null environment indicates that no further evaluation is needed
void eval_special (Cell *sym, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out)
{
	assert(symbolp(sym));
	assert(pairp(env));
	assert(ast_out);
	assert(env_out);

	if (ast == &sym_nil)
		ast = make_empty_list();
	assert(pairp(ast));

	_Static_assert(SPECIAL_COUNT == 6, "handle all special form symbols");
	if (sym == &sym_do)
	{
		// [do ... last] -> last:
		if (emptyp(ast))
		{
			// [do] -> nil
			*ast_out = &sym_nil;
			*env_out = NULL;
		}
		else
		{
			// Evaluate all but the last item
			Cell *p = ast;
			while (nonempty_listp(p) && (p->rest != &sym_nil))
			{
				EVAL(p->first, env);
				p = p->rest;
			}
			// Tail call on the last item
			*ast_out = p->first;
			*env_out = env;
		}
	}
	else if (sym == &sym_quote)
	{
		// [quote expr]
		if (1 == list_length(ast))
		{
			*ast_out = ast->first;
			*env_out = NULL;
		}
		else
		{
			// quote takes only 1 argument
			*ast_out = NULL;
			*env_out = NULL;
		}
	}
	else if (sym == &sym_def_bang)
	{
		// [def! symbol expr]
		if (2 != list_length(ast))
			error("def! requires 2 arguments");

		Cell *p1 = ast->first;
		if (!symbolp(p1))
			error("def! requires first argument to be a symbol");

		Cell *p2 = ast->rest->first;
		if(!cell_validp(p2))
			error("def! requires second argument to be valid");

		p2 = EVAL(p2, env);
		if (!cell_validp(p2))
			error("def! requires second argument to be valid");

		if (env_set(repl_env, p1, p2))
		{
			PRINT(p1);
			error("^ cannot define symbol");
		}

		*ast_out = p2;
		*env_out = NULL;
		return;
	}
	else if (sym == &sym_let_star)
	{
		// [let* [s1 expr1 s2 expr2 ...] expr]
		if (2 == list_length(ast))
		{
			Cell *p1 = ast->first;
			Cell *p2 = ast->rest->first;
			if (pairp(p1) && cell_validp(p2))
			{
				if (list_length(p1) % 2 == 0)
				{
					Cell *new_env = env_create(env, &sym_nil, &sym_nil);
					if (pairp(new_env))
					{
						Cell *p = p1;
						int fail = 0;
						while (nonempty_listp(p))
						{
							Cell *a, *b;
							a = p->first;
							b = p->rest->first;
							if (!symbolp(a) || !cell_validp(b))
							{
								fail = 1;
								break;
							}

							b = EVAL(b, new_env);
							if (!cell_validp(b))
							{
								fail = 1;
								break;
							}

							env_set(new_env, a, b);

							// next x2
							p = p->rest->rest;
						}

						if (!fail)
						{
							*ast_out = p2;
							*env_out = new_env;
							return;
						}
					}
				}
			}
		}
		*ast_out = NULL;
		*env_out = NULL;
	}
	else if (sym == &sym_fn_star)
	{
		// [fn* [s1 ...] expr]
		if (2 != list_length(ast))
			error("fn* requires 2 arguments");

		Cell *a = ast->first;
		if (!pairp(a))
			error("fn* requires the first argument to be a list");

		Cell *b = ast->rest->first;
		if (!cell_validp(b))
			error("fn* requires the second argument to be valid");

		*ast_out = make_fn(a, b);
		*env_out = NULL;
		if (!ast_out)
			error("fn* argument list is invalid");
	}
	else if (sym == &sym_cond)
	{
		// [cond c1 r1 c2 r2 ...]
		if (list_length(ast) % 2 == 0)
		{
			Cell *p = ast;
			while (nonempty_listp(p))
			{
				Cell *a, *b;
				a = p->first;
				b = p->rest->first;

				if (!cell_validp(a) || !cell_validp(b))
					break;

				// Evaluate condition a
				a = EVAL(a, env);
				if (!cell_validp(a))
					break;

				// If a is true, do a tail call to eval b
				if (truthy(a))
				{
					*ast_out = b;
					*env_out = env;
					return;
				}

				// Next x2
				p = p->rest->rest;
			}
		}
		*ast_out = NULL;
		*env_out = NULL;
	}
	else
	{
		assert(0);
	}
}

// Returns values through val_out, env_out
// For proper tail-call recursion, the next ast and env are both returned for EVAL to use.
// Null environment indicates that no further evaluation is needed
void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out)
{
	assert(cell_validp(fn));
	assert(cell_validp(args));
	assert(pairp(env));
	assert(val_out);
	assert(env_out);

	if (functionp(fn))
	{
		// Make sure the arguments are a list
		if (!pairp(args))
			args = make_empty_list();
		assert(pairp(args));

		if (function_nativep(fn))
		{
			// Native / built-in function
			int id = fn->rest->first->integer;
			if (id == FN_EVAL)
			{
				// Eval is a special case
				*val_out = args->first;
				*env_out = env;
			}
			else
			{
				*val_out = apply_built_in(id, args);
				*env_out = NULL;
			}
		}
		else
		{
			// Lizp-defined function (by fn*)
			Cell *new_env = env_create(env, fn->rest->first, args); // use function parameters
			if (new_env)
			{
				*val_out = fn->rest->rest->first; // use function body
				*env_out = new_env;
			}
			else
			{
				*val_out = NULL;
				*env_out = NULL;
			}
		}
	}
	else
	{
		// Not a function
		*val_out = NULL;
		*env_out = NULL;
	}
}

// Does: Read a form from the stream
// Returns: the form, which may be NULL
Cell *READ (const char *start, int length)
{
	// Validate inputs
	if (!start || (length < 0))
		return NULL;

	Cell *x;
	read_str(start, length, &x);
	return x;
}

Cell *EVAL (Cell *ast, Cell *env)
{
	while (1)
	{
		assert(env);

		// Non-lists
		if (!pairp(ast))
			return eval_ast(ast, env);

		// Special lists that are self-evaluating:
		//   Empty list  []
		//   String
		//   Function
		if (stringp(ast) || emptyp(ast) || functionp(ast))
			return ast;

		if (specialp(ast->first))
		{
			// Special form...
			eval_special(ast->first, ast->rest, env, &ast, &env);
		}
		else
		{
			// Normal function application...

			// Evaluate all list items
			Cell *new_ast = eval_ast(ast, env);
			if (!pairp(new_ast))
				error("eval: could not evaluate items in list");

			// Apply the function
			apply(new_ast->first, new_ast->rest, env, &ast, &env);
		}

		// A null environment signals that a tail call is not necessary.
		// Otherwise, continue the loop with the new ast and the new env.
		if (!env)
			return ast;
	}
}

void PRINT (Cell *expr)
{
	static char buffer[2 * 1024];
	int p_len = pr_str(expr, buffer, sizeof(buffer), 1);
	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Cell *env)
{
	Cell *p;

	if (!setjmp(eval_error))
		p = READ(start, length);
	else
		p = NULL;

	if (!cell_validp(p))
	{
		printf("read error: %s\n", get_error_msg());
		return;
	}

	if (!setjmp(eval_error))
		p = EVAL(p, env);
	else
		p = NULL;

	if (!cell_validp(p))
	{
		printf("error: %s\n", get_error_msg());
		return;
	}

	PRINT(p);
}

void env_setup_fn (Cell *env, const char *str, Native_fn_t id)
{
	assert(pairp(env));
	assert(str);

	Cell *name;
	string_to_list(str, strlen(str), 0, &name);
	assert(stringp(name));
	Cell *sym = intern_symbol(name);
	assert(symbolp(sym));

	Cell *fn = make_fn_native(id);
	assert(fn);

	int fail = env_set(env, sym, fn);
	assert(!fail);
}

// Returns global environment
Cell *init (int ncells)
{
	if (init_cells(ncells))
	{
		printf("init : error : could not initialize cells\n");
		return NULL;
	}

	// Setup the global environment now
	Cell *env = env_create(&sym_nil, &sym_nil, &sym_nil);
	assert(env);
	env_setup_fn(env, "*", FN_MUL);
	env_setup_fn(env, "+", FN_ADD);
	env_setup_fn(env, "-", FN_SUB);
	env_setup_fn(env, "/", FN_DIV);
	env_setup_fn(env, "<", FN_LT);
	env_setup_fn(env, "<=", FN_LTE);
	env_setup_fn(env, "=", FN_EQ);
	env_setup_fn(env, ">", FN_GT);
	env_setup_fn(env, ">=", FN_GTE);
	env_setup_fn(env, "concat", FN_CONCAT);
	env_setup_fn(env, "count", FN_COUNT);
	env_setup_fn(env, "empty?", FN_EMPTY_P);
	env_setup_fn(env, "list?", FN_LIST_P);
	env_setup_fn(env, "string?", FN_STRING_P);
	env_setup_fn(env, "function?", FN_FUNCTION_P);
	env_setup_fn(env, "symbol?", FN_SYMBOL_P);
	env_setup_fn(env, "eval", FN_EVAL);
	env_setup_fn(env, "first", FN_FIRST);
	env_setup_fn(env, "int?", FN_INT_P);
	env_setup_fn(env, "list", FN_LIST);
	env_setup_fn(env, "pr-str", FN_PR_STR);
	env_setup_fn(env, "println", FN_PRINTLN);
	env_setup_fn(env, "prn", FN_PRN);
	env_setup_fn(env, "read-string", FN_READ_STR);
	env_setup_fn(env, "rest", FN_REST);
	env_setup_fn(env, "slurp", FN_SLURP);
	env_setup_fn(env, "str", FN_STR);
	return env;
}

// Returns: number of chars written
int print_char(char c, char *out, int length)
{
	// Validate arguments
	if (!out || length <= 0)
		return 0;

	*out = c;
	return 1;
}

// Returns: number of chars written
int print_cstr(const char *s, char *out, int len)
{
	// Validate inputs
	if (!s || !out)
		return 0;

	int i;
	for (i = 0; s[i] && i < len; i++)
		out[i] = s[i];

	return i;
}

// Returns: number of chars written
int print_int(int n, char *out, int len)
{
	assert(out);

	// Zero -> special case
	if (len >= 0 && n == 0)
	{
		*out = '0';
		return 1;
	}

	char buf[20];
	const int sz = sizeof(buf);

	// U = magnitude of N
	int u = (n >= 0)? n : -n;

	int i;
	for (i = 0; (u > 0) && (i < len); i++)
	{
		assert(i < sz);
		buf[sz - i - 1] = '0' + (u % 10);
		u /= 10;
	}

	// Loop should run at least once, even for n == 0
	assert(i >= 1);

	// Minus sign for negative numbers
	if (n < 0)
	{
		assert(i < sz);
		buf[sz - i - 1] = '-';
		i++;
	}

	memcpy(out, buf + sz - i, i);
	return i;
}

int print_list_as_string(const Cell *list, char *out, int length, int readable)
{
	// Validate inputs
	if (!(pairp(list) || cell_eq(list, &sym_nil)) || !out || length <= 0)
		return 0;

	char *view = out;

	// Opening quote
	if (readable)
		string_step((const char**) &view, &length, print_char('"', view, length));

	// String contents
	const Cell *p = list;
	while ((length > 1) && nonempty_listp(p))
	{
		// Get character value in list
		Cell *e = p->first;
		// If an element is not an integer, the string stops printing out
		if (!intp(e))
			break;
		char c = (char) e->integer;

		if (readable)
		{
			// Do string escaping...
			char esc;
			switch (c)
			{
				case '\n': esc = 'n'; break;
				case '\t': esc = 't'; break;
				case '\0': esc = '0'; break;
				case '\\': esc = '\\'; break;
				default: esc = 0; break;
			}

			if (esc)
			{
				// Print a slash and get ready to print the escape char next
				string_step((const char**) &view, &length, print_char('\\', view, length));
				c = esc;
			}
		}

		// Write char
		string_step((const char**) &view, &length, print_char(c, view, length));

		// Next list item
		p = p->rest;
	}

	// Closing quote
	if (readable)
		string_step((const char**) &view, &length, print_char('"', view, length));

	// Return length, including quotes that were written
	return view - out;
}

int print_list(Cell *list, char *out, int length, int readable)
{
	// Validate arguments
	assert(pairp(list));
	if (!out || (length <= 0))
		return 0;

	char *view = out;

	// Print opening '['
	string_step(&view, &length, print_char('[', view, length));

	if (nonempty_listp(list))
	{
		// Print the first item with no leading space
		string_step(&view, &length, pr_str(list->first, view, length, readable));
		// Next item
		list = list->rest;

		// Print the rest of the items
		while (nonempty_listp(list))
		{
			// Print item with leading space
			string_step(&view, &length, print_char(' ', view, length));
			string_step(&view, &length, pr_str(list->first, view, length, readable));
			// Next item
			list = list->rest;
		}
		assert(cell_eq(list, &sym_nil));
	}

	// Print closing ']'
	string_step(&view, &length, print_char(']', view, length));

	return view - out;
}

int print_symbol(Cell *sym, char *out, int length)
{
	assert(symbolp(sym));
	assert(stringp(sym->first));
	return print_list_as_string(sym->first->rest, out, length, 0);
}

// This function is necessary because there are a bunch of special types of pairs
int print_pair(Cell *p, char *out, int length, int readable)
{
	assert(out);
	assert(pairp(p));
	if (length <= 0)
		return 0;

	if (stringp(p))
		return print_list_as_string(p->rest, out, length, readable);
	else if (functionp(p))
		return print_cstr("#<function>", out, length);
	else
		return print_list(p, out, length, readable);
}

// Does: Prints p to the given output stream
// Returns: number of chars written
int pr_str(Cell *p, char *out, int length, int readable)
{
	// Validate arguments
	assert(out);
	if (length <= 0)
		return 0;

	if (!cell_validp(p))
		return print_cstr("#<invalid>", out, length);
	switch (p->kind)
	{
		case CK_INTEGER:
			return print_int(p->integer, out, length);
		case CK_SYMBOL:
			return print_symbol(p, out, length);
		case CK_PAIR:
			return print_pair(p, out, length, readable);
		default:
			assert(0);
	}
}

int char_symbolp (char c)
{
	return (c > ' ')
		&& (c != '"')
		&& (c != ';')
		&& (c != '\'')
		&& (c != '[') && (c != ']');
}

// Returns the number of characters read
// number read -> out
int read_int (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0)
		return 0;

	int n = 0;
	int i;
	for (i = 0; (i < length) && isdigit(start[i]); i++)
		n = (n * 10) + (start[i] - '0');

	if (out)
		*out = make_int(n);

	return i;
}

// Read and intern symbol
// Returns number of chars read
int read_sym (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0)
		return 0;

	// Find symbol length
	int i;
	for (i = 0; (i < length) && char_symbolp(start[i]); i++)
		;
	int symbol_len = i;

	// Parse the symbol name
	Cell *name;
	int parse_len = string_to_list(start, symbol_len, 0, &name);
	assert(stringp(name));
	assert(symbol_len == parse_len);

	// Intern the symbol name
	Cell *interned = intern_symbol(name);
	assert(symbolp(interned));

	// Free name if it was already interned before
	if (interned->first != name)
		cell_free_all(name);

	if (out)
		*out = interned;
	return symbol_len;
}

// Convert a string to a list of characters
int read_quoted_string (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0 || *start != '"')
		return 0;

	const char *view = start;

	// Read opening quote
	string_step(&view, &length, 1);
	
	// Do a pass to find the end quote
	int quoted_len = read_until(view, length, '"');
	string_step(&view, &length, quoted_len);

	if (length && *view == '"')
	{
		// Read closing quote
		string_step(&view, &length, 1);

		Cell *string = NULL;
		int parsed_len = string_to_list(start + 1, quoted_len, 1, &string);
		if (parsed_len == 0 && quoted_len == 0)
		{
			// Empty string
			if (out)
				*out = make_string_start();
			return view - start;
		}
		else if (parsed_len == quoted_len && cell_validp(string))
		{
			// Successfully processed non-empty string
			if (out)
				*out = string;
			return view - start;
		}
		else
		{
			// could not parse string contents
			if (out)
				*out = NULL;
			return view - start;
		}
	}
	else
	{
		error("unexpected end of input in string literal");
	}
}

int read_list (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	if (!start || length <= 0)
	{
		*out = NULL;
		return 0;
	}

	const char *view = start;

	// Consume the opening paren
	string_step(&view, &length, 1);
	string_skip_white(&view, &length);

	Cell *list = make_empty_list();
	if (!list)
		return view - start;

	if (*view && *view != ']')
	{
		Cell *p = list;

		// Read the first element
		int n = string_step(&view, &length, read_str(view, length, &(p->first)));
		if (!n)
		{
			// no items
			*out = NULL;
			return view - start;
		}

		// Read the rest of the normal elements (don't handle the "dot")
		while ((length > 0) && *view && (*view != ']'))
		{
			// Read an element
			Cell *e = NULL;
			if (!string_step(&view, &length, read_str(view, length, &e)) || !e)
			{
				error("could not read item in list");
				return view - start;
			}

			p->rest = make_single_list(e);
			p = p->rest;
		}
	}

	if (*view == ']')
	{
		// Consume the final character
		string_step(&view, &length, 1);
		*out = list;
	}
	else
	{
		error("unexpected end of input");
	}

	return view - start;
}

int read_until (const char *start, int length, char sentinel)
{
	int i = 0;
	while (start[i] && (i < length) && (start[i] != sentinel))
		i++;
	return i;
}

// Read a form from an input stream/string
// Returns: the number of characters read, and writes the result to "out"
int read_str (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out || !start || length <= 0)
		return 0;

	const char *view = start;
	int rem = length;

	// Loop is for allowing comments to restart the read
	int loop = 1;
	while (loop)
	{
		loop = 0;
		string_skip_white(&view, &rem);
		switch (*view)
		{
			case '\0':
				// End of input
				*out = NULL;
				break;
			case ';':
				// Line comment
				*out = NULL;
				string_step(&view, &rem, read_until(view, rem, '\n'));
				loop = 1;
				break;
			case ']':
				error("unmatched closing ']'");
			case '[':
				// Opening paren, for lists
				string_step(&view, &rem, read_list(view, rem, out));
				break;
			case '"': 
				// Quoted string literal
				string_step(&view, &rem, read_quoted_string(view, rem, out));
				break;
			case '\'':
				// Shortcut for special form quote
				{
					Cell *ast;
					string_step(&view, &rem, 1);
					string_step(&view, &rem, read_str(view, rem, &ast));
					*out = make_pair_valid(&sym_quote, make_single_list(ast));
				}
				break;
			default:
				// Symbol or number
				if (isdigit(*view))
					string_step(&view, &rem, read_int(view, rem, out));
				else
					string_step(&view, &rem, read_sym(view, rem, out));
				break;
		}
	}
	string_skip_white(&view, &rem);

	return view - start;
}

