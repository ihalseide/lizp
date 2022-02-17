#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "cell.h"

// Cell allocator
static Cell *cell_pool = NULL;
static int cell_pool_cap = 0;

// List of internal symbols
static Cell *symbol_list = NULL;

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
	 sym_if,
	 sym_do,
	 sym_quote,
	 sym_string;

enum Cell_kind kind_of (const Cell *p)
{
	if (cell_validp(p))
		return p->kind;
	else
		return CK_INVALID;
}

int static_symp (const Cell *p)
{
	return p == &sym_nil
		|| p == &sym_t
		|| p == &sym_f
		|| p == &sym_native_fn
		|| p == &sym_fn
		|| p == &sym_def_bang
		|| p == &sym_let_star
		|| p == &sym_fn_star
		|| p == &sym_if
		|| p == &sym_do
		|| p == &sym_quote
		|| p == &sym_string;
}

int cell_valid_pooledp (const Cell *p)
{
	if (cell_pool)
		return p && (p >= cell_pool) && (p < (cell_pool + cell_pool_cap));
	else
		return 0;
}

// Valid cell pointer?
int cell_validp (const Cell *p)
{
	if (!p)
		return 0;
	else if (static_symp(p))
		return 1;
	else
		return cell_valid_pooledp(p);
}

Cell *int_to_p (int a)
{
	Cell *p = &cell_pool[a];
	if (cell_validp(p))
		return p;
	else
		return NULL;
}

int p_to_int (Cell *p)
{
	if (cell_validp(p))
		return p - cell_pool;
	else
		return -1;
}

// Get a new cell
Cell *cell_alloc (void)
{
	if (!cell_pool)
	{
		printf("cell_alloc : error : cells not initialized\n");
		return NULL;
	}

	Cell *p = cell_pool->rest;

	if (cell_validp(p))
	{
		// Remove the cell from the free list
		cell_pool->rest = p->rest;
		return p;
	}
	else
	{
		printf("cell_alloc : error : out of memory for cells\n");
		assert(0);
		return NULL;
	}
}

void cell_free (Cell *p)
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
void cell_free_all (Cell *p)
{
	while (nonempty_listp(p))
	{
		cell_free_all(p->first);
		p = p->rest;
	}
	cell_free(p);
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_alloc();
	if (x)
		x->kind = k;
	return x;
}

int is_kind (const Cell *p, enum Cell_kind kind)
{
	return cell_validp(p) && (kind_of(p) == kind);
}

Cell *make_int (int n)
{
	Cell *p = cell_init(CK_INTEGER);
	if (cell_validp(p))
		p->integer = n;
	return p;
}

Cell *make_symbol (const Cell *name)
{
	Cell *p = cell_init(CK_SYMBOL);
	if (cell_validp(p))
		p->sym_name = name;
	return p;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *p = cell_init(CK_PAIR);
	if (cell_validp(p))
	{
		p->first = first;
		p->rest = rest;
	}
	return p;
}

// Function for C code
Cell *make_native_fn (Native_fn func)
{
	Cell *p = cell_init(CK_FUNCTION);
	if (cell_validp(p))
	{
		p->func = func;
	}
	return p;
}

Cell *make_wrapped_native_fn (int n_params, Native_fn func)
{
	return make_pair(&sym_native_fn, make_pair(make_int(n_params), make_native_fn(func)));
}

Cell *make_empty_list (void)
{
	return make_pair(NULL, &sym_nil);
}

Cell *make_single_list (Cell *p)
{
	return make_pair(p, &sym_nil);
}

Cell *make_string_start (void)
{
	return make_single_list(&sym_string);
}

int native_fnp (const Cell *p)
{
	if (!is_kind(p, CK_PAIR))
		return 0;

	return p->first == &sym_native_fn;
}

// The only valid false values nil and #f
int truthy (Cell *x)
{
	if (!cell_validp(x))
		return 0;
	else
		return x != &sym_nil && x != &sym_f;
}

// Is empty list?
// Note: NULL is not considered a list
int emptyp (const Cell *p)
{
	return is_kind(p, CK_PAIR) && (p->first == NULL);
}

int nonempty_listp (const Cell *p)
{
	return is_kind(p, CK_PAIR) && !emptyp(p);
}

int list_length (const Cell *list)
{
	int n;
	for (n = 0; nonempty_listp(list); n++)
		list = list->rest;
	return n;
}

// Push an item in front of a list (which may be empty)
// Returns 0 upon success
int list_push (Cell *item, Cell **list)
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
Cell *list_pop (Cell **list)
{
	if (!is_kind(*list, CK_PAIR) || emptyp(*list))
		return NULL;

	Cell *val = (*list)->first;
	*list = (*list)->rest;
	return val;
}

int cell_eq (const Cell *a, const Cell *b)
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
				return a->sym_name == b->sym_name;
			case CK_FUNCTION:
				// Just compare function pointers
				return a->func == b->func;
			default:
				assert(0 && "invalid cell kind");
				return 0;
		}
	}
}

// Find a symbol in an alist.
// Returns the slot with [symbol | value]
// An alist is a list of the form [[symbol . value] [symbol . value] ....]
Cell *alist_assoc (const Cell *key, Cell *alist)
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
Cell *intern_find_symbol (const Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	// Search symbol list and compare names
	Cell *p = symbol_list;
	while (nonempty_listp(p))
	{
		Cell *sym = p->first;
		assert(is_kind(sym, CK_SYMBOL));
		if (cell_eq(name, sym->sym_name))
			return sym;

		// Next node
		p = p->rest;
	}

	// Symbol not found
	return NULL;
}

// Returns 0 upon success
int intern_insert (Cell *symbol)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(symbol, CK_SYMBOL))
		return 1;

	// Push to front of symbol list
	return list_push(symbol, &symbol_list);
}

// Returns symbol cell or NULL
Cell *intern_symbol (const Cell *name)
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


Cell *get_bool_sym (int v)
{
	if (v)
		return &sym_t;
	else
		return &sym_f;
}

int stringp (const Cell * p)
{
	return nonempty_listp(p) && p->first == &sym_string;
}

// Returns 0 upon success
int init_static_sym (Cell *p, const char *name)
{
	assert(cell_validp(p));
	assert(name);

	Cell *string = NULL;
	int string_len = strlen(name);
	int parsed_len = string_to_list(name, string_len, 0, &string);

	if ((parsed_len == string_len) && stringp(string))
	{
		// Initialize the symbol
		p->kind = CK_SYMBOL;
		p->sym_name = string;

		// Add it to the internal symbol list
		return list_push(p, &symbol_list);
	}
	else
	{
		return 1;
	}
}

// Return 0 upon success
int init_symbols (void)
{
	symbol_list = make_empty_list();
	if (!symbol_list)
		return 1;

	// Stop interning if one of these calls doesn't succeed
	return init_static_sym(&sym_nil, "nil")
		|| init_static_sym(&sym_t, "#t")
		|| init_static_sym(&sym_f, "#f")
		|| init_static_sym(&sym_native_fn, "_native-fn_")
		|| init_static_sym(&sym_fn, "_fn_")
		|| init_static_sym(&sym_def_bang, "def!")
		|| init_static_sym(&sym_fn_star, "fn*")
		|| init_static_sym(&sym_let_star, "let*")
		|| init_static_sym(&sym_do, "do")
		|| init_static_sym(&sym_if, "if")
		|| init_static_sym(&sym_quote, "quote")
		|| init_static_sym(&sym_string, "string");
}

// Returns 0 upon success
int init_cells (int ncells)
{
	if (ncells <= 0)
		return 1;

	// Allocate the cell array
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
		return 1;

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	// Note: int_to_p will make the last cell's next pointer
	//       be NULL because the index is out of range.
	for (int i = 0; i < cell_pool_cap; i++)
	{
		Cell *x = int_to_p(i);
		x->kind = CK_PAIR;
		x->first = NULL;
		x->rest = int_to_p(i + 1);
	}

	return init_symbols();
}

// Returns the number of chars parsed
int string_to_list (const char *start, int length, int escape, Cell **out)
{
	// Validate args
	if (!start || length <= 0 || !out)
		return 0;

	const char *view = start;
	Cell *list = make_string_start();
	Cell *p = list;

	while (is_kind(p, CK_PAIR) && *view && length)
	{
		// Get next char to add to end of string
		char c = *view;

		// Maybe process escape codes
		if (escape && length && (c == '\\'))
		{
			string_step(&view, &length, 1);
			if (!length)
			{
				*out = NULL;
				cell_free_all(list);
				return view - start;
			}

			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
			}
		}
		string_step(&view, &length, 1);

		// Put list with char in REST slot
		p->rest = make_single_list(make_int(c));
		p = p->rest;
	}

	// See if p being invalid is what caused the while loop to stop
	if (is_kind(p, CK_PAIR))
	{
		*out = list;
		return view - start;
	}
	else
	{
		*out = NULL;
		cell_free_all(list);
		return view - start;
	}
}

// TODO: fixme
Cell *string_join (Cell *items, char sep, int readable)
{
	Cell *p_items = items;

	Cell *result = make_empty_list();
	Cell *p_result = result;

	// Do the first item without separator
	if (is_kind(p_result, CK_PAIR) && nonempty_listp(p_items))
	{
		p_result->first = p_items->first;

		// Next
		p_items = p_items->rest;
		p_result = p_result->rest;
	}

	Cell *separator = make_int(sep);

	// Do the rest of the items with separator
	while (is_kind(p_result, CK_PAIR) && nonempty_listp(p_items))
	{
		// Add separator
		p_result->rest = make_pair(separator, NULL);
		p_result = p_result->rest;

		// Add item
		p_result->rest = make_pair(p_items->first, NULL);

		// Next
		p_items = p_items->rest;
		p_result = p_result->rest;
	}

	return result;
}

// For string reading and writing
int string_step (const char **stream, int *length, int n)
{
	if (n <= *length)
	{
		*stream += n;
		*length -= n;
		return n;
	}
	return 0;
}

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

int functionp (const Cell *p)
{
	if (!is_kind(p, CK_PAIR))
		return 0;
	else if (p->first == &sym_native_fn || p->first == &sym_fn)
		return is_kind(p->rest, CK_PAIR);
	else
		return 0;
}

int fn_arity (const Cell *p)
{
	(void)p;
	assert(0 && "not implemented");
}

int function_nativep (const Cell *p)
{
	(void)p;
	assert(0 && "not implemented");
}

