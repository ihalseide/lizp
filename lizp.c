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

// Does: evaluate each item of list x, without modifying x.
// Returns: a new list.
Cell *eval_each (Cell *list, Cell *env)
{
	// Validate arguments
	if (!pairp(list) || !pairp(env))
		return NULL;

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
	if (!cell_validp(ast) || !pairp(env))
		return NULL;

	switch (ast->kind)
	{
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
					return make_error_c("eval_ast : undefined symbol");
			}
		case CK_FUNCTION:
		case CK_INTEGER:
			// Self-evaluating values
			return ast;
		default:
			// Invalid kind
			return NULL;
	}
}

// Returns values through val_out, env_out
// For proper tail-call recursion, the next ast and env are both returned for EVAL to use.
// Null environment indicates that no further evaluation is needed
void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out)
{
	if (!pairp(args) || !env || !val_out || !env_out)
	{
		if (val_out)
			*val_out = make_error_c("apply : invalid arguments");
		if (env_out)
			*env_out = NULL;
		return;
	}

	if (functionp(fn))
	{
		// Check that the function arity matches the actual number of arguments
		// (if arity is 0, it is variadic)
		int n_params = function_arity(fn);
		if (n_params && n_params != list_length(args))
		{
			*val_out = make_error_c("apply : function requires fixed number of argument(s): ");
			*env_out = NULL;
			return;
		}

		if (function_nativep(fn))
		{
			// Built-in native C function...

			if (fn->rest->rest->func == fn_eval)
			{
				// Special case for eval, because
				// it only needs to do what EVAL already does.
				// Pass-through
				*val_out = args->first;
				*env_out = env;
			}
			else
			{
				// Call native function
				Native_fn f = fn->rest->rest->func; // 3rd item is function
				// (val_out is modified by f)
				*val_out = f(args);
				*env_out = NULL;
			}
		}
		else
		{
			// Function created by "fn*"
			assert(fn->first == &sym_fn);

			// Create new environment by binding params and args
			Cell *params = fn->rest->first;
			Cell *body   = fn->rest->rest->first;
			Cell *fn_env = env_create(env, params, args);

			// Allow a tail call of the lizp-defined function
			*val_out = body;
			*env_out = fn_env;
		}
	}
	else
	{
		// Not a function
		*val_out = make_error_c("apply : not a function");
		*env_out = NULL;
	}
}

Cell *EVAL (Cell *ast, Cell *env)
{
	assert(env);

	while (1)
	{
		if (!ast)
			break;

		if (!pairp(ast))
			return eval_ast(ast, env);

		// Special lists...
		// String -> itself
		// Empty list -> itself
		// Function -> itself
		if (stringp(ast) || emptyp(ast) || functionp(ast))
			return ast;

		// Normal function application...

		// Evaluate all list items
		ast = eval_ast(ast, env);
		if (!ast)
			break;
		Cell *fn = ast->first;
		Cell *args = ast->rest;

		// Make sure that the arguments are a list
		if (!pairp(args))
			args = make_empty_list();

		// Apply the function
		apply(fn, args, env, &ast, &env);

		// A null environment signals that a tail call is not necessary.
		// Otherwise, continue the loop with the new ast and the new env.
		if (!env)
			return ast;
	}

	return make_error_c("eval : invalid ast");
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

// Returns 0 upon success
// Note: when n_params == 0, that means the function is variadic
void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func)
{
	assert(env);
	assert(name);
	assert(n_params >= 0);
	assert(func);

	// Create lisp string for the name
	Cell *newname = NULL;
	string_to_list(name, strlen(name), 0, &newname);
	assert(stringp(newname));

	Cell *sym = intern_symbol(newname);
	assert(symbolp(sym));
	env_set(env, sym, make_wrapped_native_fn(n_params, func));
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

	env_set_native_fn(env, "*",            2, fn_mul);
	env_set_native_fn(env, "+",            2, fn_add);
	env_set_native_fn(env, "-",            2, fn_sub);
	env_set_native_fn(env, "/",            2, fn_div);
	env_set_native_fn(env, "<",            2, fn_lt);
	env_set_native_fn(env, "<=",           2, fn_lte);
	env_set_native_fn(env, "=",            2, fn_eq);
	env_set_native_fn(env, ">",            2, fn_gt);
	env_set_native_fn(env, ">=",           2, fn_gte);
	env_set_native_fn(env, "assoc",        2, fn_assoc);
	env_set_native_fn(env, "concat",       0, fn_concat);
	env_set_native_fn(env, "count",        1, fn_count);
	env_set_native_fn(env, "empty?",       1, fn_empty_p);
	env_set_native_fn(env, "eval",         1, fn_eval);
	env_set_native_fn(env, "first",        1, fn_first);
	env_set_native_fn(env, "int?",         1, fn_int_p);
	env_set_native_fn(env, "list",         0, fn_list);
	env_set_native_fn(env, "list?",        1, fn_list_p);
	env_set_native_fn(env, "pair",         2, fn_pair);
	env_set_native_fn(env, "pr-str",       0, fn_pr_str);
	env_set_native_fn(env, "println",      0, fn_println);
	env_set_native_fn(env, "prn",          0, fn_prn);
	env_set_native_fn(env, "read-string",  1, fn_read_str);
	env_set_native_fn(env, "rest",         1, fn_rest);
	env_set_native_fn(env, "slurp",        1, fn_slurp);
	env_set_native_fn(env, "str",          0, fn_str);
	return env;
}

