#include <stdlib.h>
#include <assert.h>
#include "cell.h"
#include "symbol.h"

// List of internal symbols
Cell *symbol_list = NULL;

// Return 0 upon success
int init_symbols (void)
{
	symbol_list = make_empty_list();
	if (!symbol_list)
		return 1;

	// Set up the internal string list
	intern_symbol(string_to_list(""));
	intern_symbol(string_to_list("nil"));
	intern_symbol(string_to_list("#t"));
	intern_symbol(string_to_list("#f"));
	intern_symbol(string_to_list("def!"));
	intern_symbol(string_to_list("fn*"));
	intern_symbol(string_to_list("let*"));
	intern_symbol(string_to_list("do"));
	intern_symbol(string_to_list("if"));
	intern_symbol(string_to_list("quote"));

	return 0;
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

// Returns symbol cell or NULL
const Cell *intern_insert (const Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	// Create symbol with name
	Cell *sym = make_symbol(name);
	if (!cell_validp(sym))
		return NULL;

	// Create new node with symbol
	Cell *node = make_pair(sym, symbol_list->rest);
	if (!cell_validp(sym))
		return NULL;

	// Insert node
	symbol_list->rest = node;

	return sym;
}

// Returns symbol cell or NULL
const Cell *intern_symbol (const Cell *name)
{
	// Validate arguments
	if (!cell_validp(symbol_list) || !is_kind(name, CK_PAIR))
		return NULL;

	const Cell *result = intern_find_symbol(name);
	if (result)
		return result;
	else
		return intern_insert(name);
}

