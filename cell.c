#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "cell.h"
#include "lizp_string.h"

// Special cell sentinel values
static Cell nil;
static Cell bool_true;
static Cell bool_false;
static Cell sym_native_fn;

// Cell allocator
static Cell *cell_pool = NULL;
static int cell_pool_cap = 0;

enum Cell_kind kind_of (const Cell *p)
{
	if (cell_validp(p))
		return p->kind;
	else
		return CK_INVALID;
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

// Returns 0 upon success
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
	// Note: int_to_p will make the last cell's next pointer
	//       be NULL because the index is out of range.
	for (int i = 0; i < cell_pool_cap; i++)
	{
		Cell *x = int_to_p(i);
		x->kind = CK_PAIR;
		x->first = NULL;
		x->rest = int_to_p(i + 1);
	}

	return 0;
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

Cell *get_bool_sym (int v)
{
	if (v)
		return &bool_true;
	else
		return &bool_false;
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

Cell *string_to_list (const char *str)
{
	Cell *list = make_empty_list();
	if (!cell_validp(list))
		return NULL;

	Cell *p = list;
	if (*str)
	{
		p->first = make_int(*str);
		str++;
	}
	while (*str)
	{
		Cell *e = make_pair(make_int(*str), &nil);
		if (cell_validp(e))
		{
			p->rest = e;
			p = p->rest;
			str++;
		}
		else
		{
			return NULL;
		}
	}

	return list;
}

Cell *make_empty_list (void)
{
	return make_pair(NULL, &nil);
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
		return x != &nil && x != &bool_false;
}

// Is nil?
int nilp (const Cell *p)
{
	return p == &nil;
}

// Is #t?
int truep (const Cell *p)
{
	return p == &bool_true;
}

// Is #f?
int falsep (const Cell *p)
{
	return p == &bool_false;
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

