#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "cell.h"
#include "env.h"
#include "function.h"
#include "lizp.h"
#include "printer.h"
#include "reader.h"

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
	if (!cell_validp(ast) || !pairp(env))
		return NULL;

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
				if (cell_validp(slot))
					return slot->rest;
				else
					// undefined symbol : ast
					return NULL;
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
			assert(0 && "not implemented");
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
				// Eval ast failed
				return NULL;

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
	Cell * form = READ(start, length);
	if (form)
	{
		Cell *value = EVAL(form, env);
		if (value)
			PRINT(value);
	}
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
	env_setup_fn(env, "eval", FN_EVAL);
	env_setup_fn(env, "first", FN_FIRST);
	env_setup_fn(env, "int?", FN_INT_P);
	env_setup_fn(env, "list", FN_LIST);
	env_setup_fn(env, "list?", FN_LIST_P);
	env_setup_fn(env, "pr-str", FN_PR_STR);
	env_setup_fn(env, "println", FN_PRINTLN);
	env_setup_fn(env, "prn", FN_PRN);
	env_setup_fn(env, "read-string", FN_READ_STR);
	env_setup_fn(env, "rest", FN_REST);
	env_setup_fn(env, "slurp", FN_SLURP);
	env_setup_fn(env, "str", FN_STR);
	return env;
}

