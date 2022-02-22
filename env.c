#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "cell.h"
#include "env.h"

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

