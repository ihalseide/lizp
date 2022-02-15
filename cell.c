#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "cell.h"
#include "lizp_string.h"

/* Cell allocator */
static Cell *cell_pool = NULL;
static int cell_pool_cap = 0;

int init_cells (int ncells)
{
	assert(ncells > 0);

	// Allocate the cell array
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
		return 1;

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	for (int i = 0; i < (cell_pool_cap - 1); i++)
		cell_pool[i].as_pair.rest = &cell_pool[i + 1];
	cell_pool[cell_pool_cap - 1].as_pair.rest = NULL;

	return 0;
}

// Get a new cell
Cell *cell_alloc ()
{
	if (!cell_pool)
	{
		printf("cell_alloc : error : cells not initialized\n");
		return NULL;
	}

	Cell *x = cell_pool->as_pair.rest;

	// Remove the cell from the free list
	if (x)
	{
		cell_pool->as_pair.rest = x->as_pair.rest;
	}
	else
	{
		printf("cell_alloc : error : out of memory for cells\n");
		return NULL;
	}

	return x;
}

void cell_free (Cell *x)
{
	if (!x)
	{
		printf("cell_free : error : cannot free NULL\n");
		return;
	}

	if (!cell_pool)
	{
		printf("cell_free : error : cells not initialized\n");
		return;
	}

	// Turn x into a pair and put it into the free list
	x->kind = CK_PAIR;
	x->as_pair.first = NULL;
	x->as_pair.rest = cell_pool->as_pair.rest;
	cell_pool->as_pair.rest = x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_alloc();
	if (x)
		x->kind = k;
	return x;
}

int is_kind (const Cell *x, enum Cell_kind kind)
{
	return x && (x->kind == kind);
}

int is_function (const Cell *x)
{
	return is_kind(x, CK_FUNC) || is_kind(x, CK_NATIVE_FUNC);
}

Cell *make_int (int n)
{
	Cell *x = cell_init(CK_INT);
	if (x)
		x->as_int = n;
	return x;
}

Cell *make_native_fn (int n_params, Native_fn func)
{
	if (n_params < 0 || !func)
		return NULL;

	Cell *x = cell_init(CK_NATIVE_FUNC);
	if (x)
	{
		x->as_native_fn.n_params = n_params;
		x->as_native_fn.func = func;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(CK_PAIR);
	if (x)
	{
		x->as_pair.first = first;
		x->as_pair.rest = rest;
	}
	return x;
}

Cell *make_atom (Cell *ref)
{
	Cell *x = cell_init(CK_ATOM);
	if (x)
	{
		x->as_atom = ref;
	}
	return x;
}

Cell *make_symbol (const char *str)
{
	Cell *x = cell_init(CK_SYMBOL);
	if (x)
		x->as_str = str;
	return x;
}

Cell *make_string (const char *str)
{
	Cell *x = cell_init(CK_STRING);
	if (x)
		x->as_str = str;
	return x;
}

// A custom lisp function is a really just a list.
// The form of that list is: (params env . body)
Cell *make_fn (Cell *params, Cell *body, Cell *outer_env)
{
	if (!is_kind(params, CK_PAIR) || !body || !is_kind(outer_env, CK_PAIR))
		return NULL;

	Cell *x = cell_init(CK_FUNC);
	if (x)
	{
		x->as_fn.params = params;
		x->as_fn.ast = body;
		x->as_fn.env = outer_env;
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, NULL);
}

// Note: NULL is not considered a list
int is_empty_list (const Cell *x)
{
	return is_kind(x, CK_PAIR) && (x->as_pair.first == NULL);
}

int is_nonempty_list (const Cell *x)
{
	return is_kind(x, CK_PAIR) && !is_empty_list(x);
}

int list_length (const Cell *list)
{
	int n;
	for (n = 0; is_nonempty_list(list); n++)
		list = list->as_pair.rest;
	return n;
}

// Push an item in front of a list (which may be empty)
void list_push (Cell *item, Cell **list)
{
	// Validate arguments
	if (!item || !is_kind(*list, CK_PAIR))
		return;

	if (is_empty_list(*list))
		// An empty list has nothing in the first slot yet
		(*list)->as_pair.first = item;
	else
		*list = make_pair(item, *list);
}

// Remove and return the item in the front of a list
Cell *list_pop (Cell **list)
{
	if (!is_kind(*list, CK_PAIR) || is_empty_list(*list))
		return NULL;

	Cell *val = (*list)->as_pair.first;
	*list = (*list)->as_pair.rest;
	return val;
}

Cell *make_bool_sym (int val)
{
	if (val)
		return make_symbol(s_true);
	else
		return make_symbol(s_false);
}

int cell_eq (const Cell *a, const Cell *b)
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
			return a->as_int == b->as_int;
		case CK_SYMBOL:
		case CK_STRING:
			return a->as_str == b->as_str;
		case CK_FUNC:
			return 0;
		case CK_NATIVE_FUNC:
			return (a->as_native_fn.func == b->as_native_fn.func)
				&& (a->as_native_fn.n_params == b->as_native_fn.n_params);
		case CK_PAIR:
			if (is_empty_list(a))
				return is_empty_list(b);
			else
				return cell_eq(a->as_pair.first, b->as_pair.first)
					&& cell_eq(a->as_pair.rest, b->as_pair.rest);
		default:
			assert(0 && "invalid cell kind");
			return 0;
	}
}

