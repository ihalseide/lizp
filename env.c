#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "cell.h"
#include "env.h"

// Find the innermost env which contains symbol
Cell *env_find (Cell *env, const Cell *sym)
{
	assert(is_kind(env, CK_PAIR) && is_kind(sym, CK_SYMBOL));

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
	assert(is_kind(env, CK_PAIR) && is_kind(sym, CK_SYMBOL));

	// Find the environment which contains the symbol
	env = env_find(env, sym);
	if (env)
		return alist_assoc(sym, env->first);

	// Symbol not found
	return NULL;
}

// Returns 0 upon success
// TODO: Do not allow nil, true, or false to be defined
int env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
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
		slot = make_pair(sym, val);
		if (is_kind(slot, CK_PAIR))
		{
			list_push(slot, &(env->first));
			return 0;
		}
		else
		{
			return 1;
		}
	}
}

// An environment is a list of the form (alist . outer-env).
// Create an environment with each item of the
// "binds" list set to the corresponding item in the "exprs" list.
Cell *env_create (Cell *env_outer, Cell *binds, Cell *exprs)
{
	// Validate args
	if (env_outer != &sym_nil && !is_kind(env_outer, CK_PAIR))
	{
		printf("env_create : error : invalid outer environment\n");
		return NULL;
	}

	// Create the new environment
	Cell *env = make_pair_valid(make_empty_list(), env_outer);
	if (!env)
		return NULL;

	// Create the bindings by iterating both lists
	while (nonempty_listp(binds) && nonempty_listp(exprs))
	{
		// Bind 1 pair
		Cell *sym = binds->first;
		Cell *val = exprs->first;

		// Make sure it only binds symbols
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("env_create : error : a member of the bindings list is not a symbol\n");
			return NULL;
		}
		env_set(env, sym, val);

		// Next
		binds = binds->rest;
		exprs = exprs->rest;
	}

	if (nonempty_listp(binds) && emptyp(exprs))
	{
		// Left over symbols in the bindings list
		printf("env_create : error : not enough values to bind to symbols list\n");
		cell_free_all(env);
		return NULL;
	}
	else if (nonempty_listp(exprs) && emptyp(binds))
	{
		// Left over values in the exprs list
		printf("env_create : error : too many values to bind to symbols list\n");
		cell_free_all(env);
		return NULL;
	}

	return env;
}

