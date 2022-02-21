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
				{
					return slot->rest;
				}
				else
				{
					printf("eval_ast : error : undefined symbol : ");
					PRINT(ast);
					return NULL;
				}
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
		printf("apply : error : invalid arguments\n");
		return;
	}

	if (functionp(fn))
	{
		// Check that the function arity matches the actual number of arguments
		// (if arity is 0, it is variadic)
		int n_params = function_arity(fn);
		if (n_params && n_params != list_length(args))
		{
			printf("apply : error : function requires %d argument(s)\n", n_params);
			*val_out = NULL;
			*env_out = NULL;
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
		printf("apply : error : not a function\n");
		*val_out = NULL;
		*env_out = NULL;
	}
}

// Returns 1 or 0 for if it is a special form or not
int eval_special (Cell *head, Cell *given_ast, Cell *env, Cell **ast_out, Cell **env_out)
{
	if (!ast_out || !env_out)
		return 0;

	// Head must be a symbol
	if (!symbolp(head))
		return 0;

	Cell *ast;
	if (given_ast)
		ast = given_ast;
	else
		ast = make_empty_list();

	if (cell_eq(head, &sym_def_bang))
	{
		// [def! <symbol> value]
		if (list_length(ast) != 2)
		{
			printf("def! : error : requires 2 expressions\n");
			return 1;
		}

		Cell *sym = ast->first;
		if (!symbolp(sym))
		{
			printf("def! : error : argument 1 must be a symbol\n");
			return 1;
		}

		Cell *val = EVAL(ast->rest->first, env);
		if (!val)
		{
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}

		// Note: env_set returns 0 upon success
		if (env_set(env, sym, val))
		{
			printf("def! : error : cannot define symbol\n");
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}
		else
		{
			*ast_out = val;
			*env_out = NULL;
			return 1;
		}
	}
	else if (cell_eq(head, &sym_let_star))
	{
		// [let* [sym1 expr1 sym2 expr2...] expr]
		if (list_length(ast) != 2)
		{
			// Error: invalid ast
			printf("let* : error : requires 2 expressions\n");
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}

		// Go through the bindings list and add the
		// bindings to the environment
		Cell *let_env = env_create(env, NULL, NULL);
		Cell *p = ast->first; // pointer to bindings list
		while (pairp(p))
		{
			if (!pairp(p->rest))
			{
				// Error: odd amount of arguments in first list
				printf("let* : error : requires an even amount of items in bindings list\n");
				*ast_out = NULL;
				*env_out = NULL;
				return 1;
			}
			if (!symbolp(p->first))
			{
				// Error: even element in bindings list not a symbol
				printf("let* : error : requires even elements in bindings list to be symbols\n");
				*ast_out = NULL;
				*env_out = NULL;
				return 1;
			}

			env_set(let_env, p->first, EVAL(p->rest->first, let_env));

			// Next pair (next twice)
			p = p->rest->rest;
		}

		// Tail call on the body expr
		*env_out = let_env;
		*ast_out = ast->rest->first;
		return 1;
	}
	else if (cell_eq(head, &sym_fn_star))
	{
		// [fn* [symbol1 symbol2 ...] expr]
		if (list_length(ast) != 2)
		{
			printf("fn* : error : requires 2 expressions\n");
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}

		Cell *new_fn = make_lizp_fn(ast->first, ast->rest->first);

		// New function, no tail call
		*ast_out = new_fn;
		*env_out = NULL;
		return 1;
	}
	else if (cell_eq(head, &sym_if))
	{
		// [if expr t-expr f-expr] OR [if expr t-expr]
		int len = list_length(ast);
		if (!pairp(ast) || (len != 2 && len != 3))
		{
			// Error: too few or too many arguments
			printf("if : error : must have 2 or 3 arguments\n");
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}

		Cell *cond = EVAL(ast->first, env);
		if (cond)
		{
			if (truthy(cond))
			{
				// Tail call with t-expr
				*env_out = env;
				*ast_out = ast->rest->first;
				return 1;
			}
			else if (ast->rest->rest)
			{
				// Tail call with f-expr
				*env_out = env;
				*ast_out = ast->rest->rest->first;
				return 1;
			}
			else
			{
				// No f-expr, so return nil
				*ast_out = &sym_nil;
				*env_out = NULL;
				return 1;
			}
		}
		else
		{
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}
	}
	else if (cell_eq(head, &sym_do))
	{
		// [do exprs...]

		// Evaluate all but the last item
		while (pairp(ast) && !emptyp(ast) && ast->rest)
		{
			EVAL(ast->first, env);
			ast = ast->rest;
		}

		// Has a last item, so do a tail call to evaluate that
		if (pairp(ast) && !emptyp(ast))
		{
			*ast_out = ast->first;
			*env_out = env;
			return 1;
		}
		else
		{
			// If this point is reached, there were no items
			*ast_out = &sym_nil;
			*env_out = NULL;
			return 1;
		}
	}
	else if (cell_eq(head, &sym_quote))
	{
		// [quote expr]
		if (list_length(ast) != 1)
		{
			printf("quote : error : requires 1 expression");
			*ast_out = NULL;
			*env_out = NULL;
			return 1;
		}
		else
		{
			*ast_out = ast->first;
			*env_out = NULL;
			return 1;
		}
	}
	else
	{
		// Not a known name for a special form
		*env_out = NULL;
		*ast_out = NULL;
		// Free the unused new list
		if (given_ast != ast)
			cell_free_all(ast);
		return 0;
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

		// Special form evaluation...
		Cell *new_env = NULL;
		Cell *new_ast = NULL;
		if (eval_special(ast->first, ast->rest, env, &new_ast, &new_env))
		{
			if (!new_ast)
				// Error in special form
				return NULL;

			if (!new_env)
				// Value is final, no tail call
				return new_ast;

			// Tail call loop if new_env is a valid one
			env = new_env;
			ast = new_ast;
			continue;
		}

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

	return NULL;
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
		{
			PRINT(value);
			return;
		}
	}

	printf("no value\n");
}

// Returns 0 upon success
// Note: when n_params == 0, that means the function is variadic
int env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func)
{
	// Validate params
	if (!env || !name || (n_params < 0) || !func)
		return 1;

	Cell *newname = NULL;
	string_to_list(name, strlen(name), 0, &newname);
	if (stringp(newname))
	{
		Cell *sym = intern_symbol(newname);
		if (symbolp(sym))
			return env_set(env, sym, make_wrapped_native_fn(n_params, func));
		else
			return 1;
	}
	else
	{
		return 1;
	}
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
	if (env_set_native_fn(env, "*",      2, fn_mul)
			|| env_set_native_fn(env, "+",            2, fn_add)
			|| env_set_native_fn(env, "-",            2, fn_sub)
			|| env_set_native_fn(env, "/",            2, fn_div)
			|| env_set_native_fn(env, "<",            2, fn_lt)
			|| env_set_native_fn(env, "<=",           2, fn_lte)
			|| env_set_native_fn(env, "=",            2, fn_eq)
			|| env_set_native_fn(env, ">",            2, fn_gt)
			|| env_set_native_fn(env, ">=",           2, fn_gte)
			|| env_set_native_fn(env, "count",        1, fn_count)
			|| env_set_native_fn(env, "empty?",       1, fn_empty_p)
			|| env_set_native_fn(env, "eval",         1, fn_eval)
			|| env_set_native_fn(env, "int?",         1, fn_int_p)
			|| env_set_native_fn(env, "list",         0, fn_list)
			|| env_set_native_fn(env, "list?",        1, fn_list_p)
			|| env_set_native_fn(env, "pair",         2, fn_pair)
			|| env_set_native_fn(env, "concat",       0, fn_concat)
			|| env_set_native_fn(env, "assoc",        2, fn_assoc)
			|| env_set_native_fn(env, "first",        1, fn_first)
			|| env_set_native_fn(env, "slurp",        1, fn_slurp)
			|| env_set_native_fn(env, "read-string",  1, fn_read_str)
			|| env_set_native_fn(env, "println",      0, fn_println)
			|| env_set_native_fn(env, "prn",          0, fn_prn)
			|| env_set_native_fn(env, "pr-str",       0, fn_pr_str)
			|| env_set_native_fn(env, "str",          0, fn_str)
			|| env_set_native_fn(env, "rest",         1, fn_rest))
	{
		printf("init : error : could not setup environment\n");
		return NULL;
	}
	else
	{
		return env;
	}
}

