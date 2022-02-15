#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

#include "cell.h"
#include "function.h"
#include "lizp.h"
#include "printer.h"
#include "reader.h"
#include "lizp_string.h"

// REPL environment
Cell *repl_env = NULL;

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
	while (is_nonempty_list(p))
	{
		// Check if slot has same key
		Cell *slot = p->as_pair.first;
		if (is_kind(slot, CK_PAIR) && cell_eq(slot->as_pair.first, key))
			return slot;

		// Next
		p = p->as_pair.rest;
	}

	// Not found
	return NULL;
}

// Find the innermost env which contains symbol
Cell *env_find (Cell *env, const Cell *sym)
{
	assert(is_kind(env, CK_PAIR) && is_kind(sym, CK_SYMBOL));

	// Search up the environment hierarchy
	// Note: env = [alist | outer]
	while (is_nonempty_list(env))
	{
		// Search current environment
		if (alist_assoc(sym, env->as_pair.first))
			return env;

		// Next
		env = env->as_pair.rest;
	}

	return NULL;
}

// Get the innermost definition for the symbol.
// Returns:
//   when found -> the slot containing (symbol . value)
//   when not found -> nil
Cell *env_get (Cell *env, Cell *sym)
{
	assert(is_kind(env, CK_PAIR) && is_kind(sym, CK_SYMBOL));

	// Find the environment which contains the symbol
	env = env_find(env, sym);
	if (env)
		return alist_assoc(sym, env->as_pair.first);

	// Symbol not found
	return NULL;
}

// Returns whether it succeeded (1) or not (0)
int env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
		return 0;

	// Do not allow nil, true, or false to be defined
	if (sym->as_str == s_nil || sym->as_str == s_false || sym->as_str == s_true)
		return 0;

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = alist_assoc(sym, env->as_pair.first);
	if (slot)
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->as_pair.rest = val;
	}
	else
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		slot = make_pair(sym, val);
		if (!slot)
			return 0;
		list_push(slot, &(env->as_pair.first));
	}

	return 1;
}

// An environment is a list of the form (alist . outer-env).
// Create an environment with each item of the
// "binds" list set to the corresponding item in the "exprs" list.
Cell *env_create (Cell *env_outer, const Cell *binds, Cell *exprs)
{
	// Validate args (env_outer is allowed to be NULL)
	if (env_outer && !is_kind(env_outer, CK_PAIR))
	{
		printf("env_create : error : invalid outer environment\n");
		return NULL;
	}

	Cell *env = make_pair(make_empty_list(), env_outer);
	if (!env)
		return NULL;

	// Create the bindings by iterating both lists
	while (is_kind(binds, CK_PAIR) && is_kind(exprs, CK_PAIR) && !is_empty_list(binds) && !is_empty_list(exprs))
	{
		// Bind 1 pair
		Cell *sym = binds->as_pair.first;
		Cell *val = exprs->as_pair.first;

		// Make sure it only binds symbols
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("env_create : error : a member of the bindings list is not a symbol\n");
			return NULL;
		}
		env_set(env, sym, val);

		// Next
		binds = binds->as_pair.rest;
		exprs = exprs->as_pair.rest;
	}

	// Left over symbols in the bindings list
	if (binds && !is_empty_list(binds))
	{
		printf("env_create : error : not enough values to bind to symbols list\n");
		return NULL;
	}

	// Left over values in the exprs list
	if (exprs && !is_empty_list(exprs))
	{
		printf("env_create : error : too many values to bind to symbols list\n");
		return NULL;
	}

	return env;
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

// Does: evaluate each item of list x, without modifying x.
// Returns: a new list.
Cell *eval_each (Cell *list, Cell *env)
{
	// Validate arguments
	if (!is_kind(list, CK_PAIR) || !is_kind(env, CK_PAIR))
		return NULL;

	if (is_empty_list(list))
		return list;

	// Eval the first element
	Cell *y = make_pair(EVAL(list->as_pair.first, env), NULL);
	Cell *p_y = y;

	// eval the rest of the elements
	list = list->as_pair.rest;
	while (list && p_y)
	{
		if (!is_kind(list, CK_PAIR))
		{
			// dotted list
			p_y->as_pair.rest = EVAL(list, env);
			break;
		}

		// Fill in next slot of y
		p_y->as_pair.rest = make_pair(EVAL(list->as_pair.first, env), NULL);

		// next
		list = list->as_pair.rest;
		p_y = p_y->as_pair.rest;
	}

	return y;
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	// Validate args
	if (!ast || !env)
		return NULL;

	switch (ast->kind)
	{
		case CK_PAIR:
			// Evaluate each element in a list
			return eval_each(ast, env);
		case CK_SYMBOL:
			// Look up symbol's value...
			if (ast->as_str == s_nil || ast->as_str == s_true || ast->as_str == s_false)
			{
				// These special symbols are self-evaluating
				return ast;
			}
			else
			{
				// Get the value out of the environment's slot
				Cell *slot = env_get(env, ast);
				if (slot)
				{
					return slot->as_pair.rest;
				}
				else
				{

					printf("eval_ast : error : undefined symbol `%s'\n", ast->as_str);
					return NULL;
				}
			}
		case CK_INT:
		case CK_STRING:
		case CK_FUNC:
		case CK_NATIVE_FUNC:
		case CK_ATOM:
			// Self-evaluating values
			return ast;
	}

	// Invalid kind
	return NULL;
}

// For proper tail-call recursion, the next ast and env are both returned for EVAL to use.
// Null environment indicates that no further evaluation is needed
void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out)
{
	// Validate arguments
	if (!fn || !is_kind(args, CK_PAIR) || !env || !val_out || !env_out)
	{
		printf("apply : error : invalid arguments\n");
		return;
	}

	switch (fn->kind)
	{
		case CK_NATIVE_FUNC:
			// Apply a built-in native C function
			// Note: when n_params == 0, that means the function is variadic
			if (fn->as_native_fn.n_params && (list_length(args) != fn->as_native_fn.n_params))
			{
				printf("apply : error : native function requires %d parameters\n", fn->as_native_fn.n_params);
				return;
			}

			if (fn->as_native_fn.func == fn_eval)
			{
				// Special case for eval, because
				// it only needs to do what EVAL already does.
				// Don't even call eval because it's a dummy value.
				*val_out = args->as_pair.first;
				*env_out = repl_env; // Use the REPL environment instead of the current one?
				break;
			}

			// Call it
			fn->as_native_fn.func(args, env, val_out);
			*env_out = NULL;
			break;
		case CK_FUNC:
			// Apply lisp function created by fn*
			env = env_create(fn->as_fn.env, fn->as_fn.params, args);
			if (!env)
			{
				*env_out = NULL;
				*val_out = NULL;
				return;
			}
			*val_out = fn->as_fn.ast;
			*env_out = env;
			break;
		default:
			// Error: not a function
			printf("apply : error : first item in list is not a function\n");
			*env_out = NULL;
			*val_out = NULL;
			break;
	}
}

// The only false values are NULL, "nil" and "#f"
int truthy (Cell *x)
{
	if (!x)
		return 0;
	return !(is_kind(x, CK_SYMBOL) && (x->as_str == s_nil || x->as_str == s_false));
}

// Returns 1 or 0 for if it is a special form or not
int eval_special (Cell *head, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out)
{
	if (!ast_out || !env_out)
		return 0;

	// Default behavior is no tail-call, error
	*env_out = NULL;
	*ast_out = NULL;

	// Head must be a symbol
	if (!is_kind(head, CK_SYMBOL))
		return 0;

	// Make sure that ast is a list
	if (!ast)
		ast = make_empty_list();
	if (!ast)
		return 0;
	assert(is_kind(ast, CK_PAIR));

	const char *name = head->as_str;
	if (name == s_def_bang)
	{
		// [def! <symbol> value]
		if (list_length(ast) != 2)
		{
			printf("def! : error : requires 2 expressions\n");
			return 1;
		}

		Cell *sym = ast->as_pair.first;
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("def! : error : argument 1 must be a symbol\n");
			return 1;
		}

		Cell *val = EVAL(ast->as_pair.rest->as_pair.first, env);
		if (!val)
			return 1;

		if (env_set(env, sym, val))
			*ast_out = val;
		else
			printf("def! : error : cannot define symbol\n");

		return 1;
	}
	else if (name == s_let_star)
	{
		// [let* [sym1 expr1 sym2 expr2...] expr]
		if (list_length(ast) != 2)
		{
			// Error: invalid ast
			printf("let* : error : requires 2 expressions\n");
			return 1;
		}

		// Go through the bindings list and add the
		// bindings to the environment
		Cell *let_env = env_create(env, NULL, NULL);
		Cell *p = ast->as_pair.first; // pointer to bindings list
		while (is_kind(p, CK_PAIR))
		{
			if (!is_kind(p->as_pair.rest, CK_PAIR))
			{
				// Error: odd amount of arguments in first list
				printf("let* : error : requires an even amount of items in bindings list\n");
				return 1;
			}
			if (!is_kind(p->as_pair.first, CK_SYMBOL))
			{
				// Error: even element in bindings list not a symbol
				printf("let* : error : requires even elements in bindings list to be symbols\n");
				return 1;
			}

			env_set(let_env, p->as_pair.first, EVAL(p->as_pair.rest->as_pair.first, let_env));

			// Next pair (next twice)
			p = p->as_pair.rest->as_pair.rest;
		}

		// Tail call on the body expr
		*env_out = let_env;
		*ast_out = ast->as_pair.rest->as_pair.first;
		return 1;
	}
	else if (name == s_fn_star)
	{
		// [fn* [symbol1 symbol2 ...] expr]
		if (list_length(ast) != 2)
		{
			printf("fn* : error : requires 2 expressions\n");
			return 1;
		}

		// Check that the parameter list is only symbols
		Cell *p = ast->as_pair.first;
		while (is_kind(p, CK_PAIR) && !is_empty_list(p))
		{
			if (!is_kind(p->as_pair.first, CK_SYMBOL))
			{
				// Error: parameter list must be all symbols
				printf("fn* : error : items of parameter list must be symbols\n");
				return 1;
			}
			if (p->as_pair.rest && !is_kind(p->as_pair.rest, CK_PAIR))
			{
				// Something other than NULL or a list is next
				printf("fn* : error : parameter list is not a proper list\n");
				return 1;
			}

			// Next
			p = p->as_pair.rest;
		}

		// New function closure, no tail call
		*ast_out = make_fn(ast->as_pair.first, ast->as_pair.rest->as_pair.first, env);
		return 1;
	}
	else if (name == s_if)
	{
		// [if expr t-expr f-expr] OR [if expr t-expr]
		int len = list_length(ast);
		if (!is_kind(ast, CK_PAIR) || (len != 2 && len != 3))
		{
			// Error: too few or too many arguments
			printf("if : error : must have 2 or 3 arguments\n");
			return 1;
		}

		Cell *cond = EVAL(ast->as_pair.first, env);
		if (cond)
		{
			if (truthy(cond))
			{
				// Tail call with t-expr
				*env_out = env;
				*ast_out = ast->as_pair.rest->as_pair.first;
			}
			else if (ast->as_pair.rest->as_pair.rest)
			{
				// Tail call with f-expr
				*env_out = env;
				*ast_out = ast->as_pair.rest->as_pair.rest->as_pair.first;
			}
			else
			{
				// No f-expr, so return nil
				*ast_out = make_symbol(s_nil);
			}
		}

		return 1;
	}
	else if (name == s_do)
	{
		// [do exprs...]

		// Evaluate all but the last item
		while (is_kind(ast, CK_PAIR) && !is_empty_list(ast) && ast->as_pair.rest)
		{
			EVAL(ast->as_pair.first, env);
			ast = ast->as_pair.rest;
		}

		// Has a last item, so do a tail call to evaluate that
		if (is_kind(ast, CK_PAIR) && !is_empty_list(ast))
		{
			*ast_out = ast->as_pair.first;
			*env_out = env;
		}
		else
		{
			// If this point is reached, there were no items
			*ast_out = make_symbol(s_nil);
		}

		return 1;
	}
	else if (name == s_quote)
	{
		// [quote expr]
		if (list_length(ast) != 1)
		{
			printf("quote : error : requires 1 expression");
			return 1;
		}

		*ast_out = ast->as_pair.first;
		return 1;
	}
	else
	{
		// Not a known name for a special form
		return 0;
	}
}

Cell *EVAL (Cell *ast, Cell *env)
{
	if (!env)
	{
		printf("EVAL : error : environment is NULL\n");
		return NULL;
	}

	while (1)
	{
		if (!ast)
			break;

		if (!is_kind(ast, CK_PAIR))
			return eval_ast(ast, env);

		// Empty list -> itself
		if (is_empty_list(ast))
			return ast;

		// Special form evaluation...
		Cell *new_env = NULL;
		Cell *new_ast = NULL;
		if (eval_special(ast->as_pair.first, ast->as_pair.rest, env, &new_ast, &new_env))
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
		Cell *fn = ast->as_pair.first;
		Cell *args = ast->as_pair.rest;

		// Make sure that the arguments are a list
		if (!is_kind(args, CK_PAIR))
			args = make_empty_list();

		// Apply the function
		apply(fn, args, env, &ast, &env);

		// A null environment signals that a tail call is not necessary.
		// Otherwise, continue the loop with the new ast and the new env.
		if (!env)
			return ast;
	}

	printf("EVAL : error : ast is NULL\n");
	return NULL;
}

void PRINT (Cell *expr)
{
	char buffer[2 * 1024];
	int p_len = pr_str(expr, buffer, sizeof(buffer), 1);
	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Cell *env)
{
	// Allow passing C strings and to automatically get the length
	if (length < 0)
		length = strlen(start);

	Cell * form = READ(start, length);
	if (form)
	{
		Cell * value = EVAL(form, env);
		if (value)
		{
			PRINT(value);
			return;
		}
	}
	PRINT(make_string("no value"));
}

// For adding C functions to the environment
// Note: when n_params == 0, that means the function is variadic
void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func)
{
	if (!env || !name || n_params < 0 || !func)
		return;
	Cell *str = insert_string(name);
	env_set(env, make_symbol(str->as_str), make_native_fn(n_params, func));
}

// Returns global environment
Cell *init (int ncells, int nchars)
{
	init_cells(ncells);
	init_strings(nchars);

	// Setup the global environment now
	Cell *env = env_create(NULL, NULL, NULL);
	env_set_native_fn(env, "*",           2, fn_mul);
	env_set_native_fn(env, "+",           2, fn_add);
	env_set_native_fn(env, "-",           2, fn_sub);
	env_set_native_fn(env, "/",           2, fn_div);
	env_set_native_fn(env, "<",           2, fn_lt);
	env_set_native_fn(env, "<=",          2, fn_lte);
	env_set_native_fn(env, "=",           2, fn_eq);
	env_set_native_fn(env, ">",           2, fn_gt);
	env_set_native_fn(env, ">=",          2, fn_gte);
	env_set_native_fn(env, "atom",        1, fn_atom);
	env_set_native_fn(env, "atom?",       1, fn_atom_p);
	env_set_native_fn(env, "count",       1, fn_count);
	env_set_native_fn(env, "deref",       1, fn_deref);
	env_set_native_fn(env, "empty?",      1, fn_empty_p);
	env_set_native_fn(env, "eval",        1, fn_eval);
	env_set_native_fn(env, "int?",        1, fn_int_p);
	env_set_native_fn(env, "list",        0, fn_list);
	env_set_native_fn(env, "list?",       1, fn_list_p);
	env_set_native_fn(env, "pr-str",      0, fn_pr_str);
	env_set_native_fn(env, "println",     0, fn_println);
	env_set_native_fn(env, "prn",         0, fn_prn);
	env_set_native_fn(env, "read-string", 1, fn_read_str);
	env_set_native_fn(env, "reset!",      2, fn_reset_bang);
	env_set_native_fn(env, "slurp",       1, fn_slurp);
	env_set_native_fn(env, "str",         0, fn_str);
	env_set_native_fn(env, "swap!",       0, fn_swap_bang);
	env_set_native_fn(env, "pair",        2, fn_pair);
	env_set_native_fn(env, "concat",      0, fn_concat);
	env_set_native_fn(env, "assoc",       2, fn_assoc);
	return env;
}

