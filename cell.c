#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include "cell.h"
#include "lizp_string.h"

// Cell allocator
static Cell *cell_pool = NULL;
static int cell_pool_cap = 0;

// List of internal symbols
static Cell *symbol_list = NULL;

// Static symbols
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
	 sym_quote;

enum Cell_kind kind_of (const Cell *p)
{
	if (cell_validp(p))
		return p->kind;
	else
		return CK_INVALID;
}

enum Cell_variant var_of (const Cell *p)
{
	if (cell_validp(p))
		return p->var;
	else
		return CV_INVALID;
}

// Valid cell pointer?
int cell_validp (const Cell *p)
{
	if (cell_pool)
		return p && (p >= cell_pool) && (p < (cell_pool + cell_pool_cap));
	else
		return 0;
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
		return NULL;
	}
}

void cell_free (Cell *p)
{
	if (cell_validp(p))
	{
		// Turn x into a pair and put it into the free list
		p->kind = CK_PAIR;
		p->first = NULL;
		p->rest = cell_pool->rest;
		cell_pool->rest = p;
	}
	else
	{
		printf("cell_free : error : cannot free invalid pointer\n");
		return;
	}
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

int is_variant (const Cell *p, enum Cell_variant var)
{
	return cell_validp(p) && (var_of(p) == var);
}

Cell *make_int (int n)
{
	Cell *p = cell_init(CK_INT);
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

Cell *make_empty_list (void)
{
	return make_pair(NULL, &sym_nil);
}

Cell *make_single_list (Cell *p)
{
	return make_pair(p, &sym_nil);
}

Cell *make_void (const void *vp)
{
	Cell *p = cell_init(CK_VOID);
	if (p)
		p->pointer = vp;
	return p;
}

// Function for C code
Cell *make_native_fn (int n_params, Native_fn func)
{
	return make_pair(&sym_native_fn, make_pair(make_int(n_params), make_void(func)));
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

// Is nil?
int nilp (const Cell *p)
{
	return p == &sym_nil;
}

// Is #t?
int truep (const Cell *p)
{
	return p == &sym_t;
}

// Is #f?
int falsep (const Cell *p)
{
	return p == &sym_f;
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
void list_push (Cell *item, Cell **list)
{
	// Validate arguments
	if (!item || !is_kind(*list, CK_PAIR))
		return;

	if (emptyp(*list))
		// An empty list has nothing in the first slot yet
		(*list)->first = item;
	else
		*list = make_pair(item, *list);
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
			case CK_INT:
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
				else
				{
					// Tail call
					a = a->rest;
					b = b->rest;
					continue;
				}
			case CK_SYMBOL:
				// Compare pointers
				return a->sym_name == b->sym_name;
			case CK_VOID:
				// Just compare pointers
				return a->pointer == b->pointer;
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

int stringp (const Cell *p)
{
	return is_kind(p, CK_PAIR) && is_variant(p, CV_STRING);
}

// Returns: symbol cell or NULL
const Cell *intern_find_symbol (const Cell *name)
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

	// Create new node with symbol
	Cell *node = make_pair(symbol, symbol_list->rest);
	if (is_kind(node, CK_PAIR))
	{
		// Insert node
		symbol_list->rest = node;
		return 0;
	}
	else
	{
		return 1;
	}
}

// Returns symbol cell or NULL
const Cell *intern_symbol (const Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	const Cell *result = intern_find_symbol(name);
	if (result)
	{
		return result;
	}
	else
	{
		Cell *new_sym = make_symbol(name);
		if (intern_insert(new_sym))
			return new_sym;
		else
			return NULL;
	}
}


Cell *get_bool_sym (int v)
{
	if (v)
		return &sym_t;
	else
		return &sym_f;
}

// Returns 0 upon success
int init_static_sym (Cell *p, const char *name)
{
	Cell *string = string_to_list(name);
	if (stringp(string))
	{
		p->kind = CK_SYMBOL;
		p->sym_name = string;
		return 0;
	}
	else
	{
		printf("init_static_sym : could not create \"%s\"\n", name);
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
		|| init_static_sym(&sym_quote, "quote");
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

