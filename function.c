#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "function.h"
#include "lizp.h"
#include "reader.h"
#include "lizp_string.h"

// [str a b c ...] -> "abc..." (prints non-readably)
void fn_str (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = string_join(args, 0, 0);
}

// [pr-str a b c ...] -> "a b c ..." (prints readably)
void fn_pr_str (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = string_join(args, ' ', 1);
}

// [prn a b c ...] -> nil (prints readably)
void fn_prn (Cell *args, Cell *env, Cell **val_out)
{
	Cell *s = string_join(args, ' ', 1);
	if (s)
		printf("%s", s->as_str);
	*val_out =  make_symbol(s_nil);
}

// [println a b c ...] -> nil (prints non-readably)
void fn_println (Cell *args, Cell *env, Cell **val_out)
{
	Cell *s = string_join(args, ' ', 0);
	if (s)
		printf("%s", s->as_str);
	*val_out = make_symbol(s_nil);
}

// [list a ...] -> [a ...] (variadic)
void fn_list (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = args;
}

// [deref atom]
void fn_deref (Cell *args, Cell *env, Cell **val_out)
{
	if (is_kind(args->as_pair.first, CK_ATOM))
		*val_out = args->as_pair.first->as_atom;
	else
		*val_out = NULL;
}

// [atom? x] -> bool
void fn_atom_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_ATOM));
}

// [atom x] -> #<atom x>
void fn_atom (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_atom(args->as_pair.first);
}

void fn_eval (Cell *args, Cell *env, Cell **val_out)
{
	assert(0 && "Not to be implemented. This function is only needed for its pointer value.");
}

// (slurp "file name") -> "file contents"
void fn_slurp (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;

	// Validate arguments
	if (!is_kind(a, CK_STRING))
		return;

	// Read all contents of the file...
	// Open file
	FILE *f = fopen(a->as_str, "r");
	if (!f) // failed
		return;

	// Get file length
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	// See if we have enough room for this string data
	if (!string_can_alloc(fsize + 1))
	{
		fclose(f);
		return;
	}

	// Read the char data and null-terminate it
	fread(char_free, fsize, 1, f);
	fclose(f);

	// Move the character allocation pointer
	*val_out = intern_string(char_free, fsize);
}

// [read-string "str"] -> any value
void fn_read_str (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	if (is_kind(a, CK_STRING))
		read_str(a->as_str, strlen(a->as_str), val_out);
}

// [empty? x]
void fn_empty_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_empty_list(args->as_pair.first));
}

// [count list]
void fn_count (Cell *args, Cell *env, Cell **val_out)
{
	if (!is_kind(args->as_pair.first, CK_PAIR))
	{
		printf("count : error : first argument must be a list\n");
		return;
	}
	*val_out = make_int(list_length(args->as_pair.first));
}

// [list? x]
void fn_list_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_PAIR));
}

// [int? x]
void fn_int_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_INT));
}


// [reset! atom value] -> value
void fn_reset_bang (Cell *args, Cell *env, Cell **val_out)
{
	Cell *atom = args->as_pair.first;
	Cell *value = args->as_pair.rest->as_pair.first;

	if (!is_kind(atom, CK_ATOM))
	{
		printf("reset! : error : first argument must be an atom\n");
		return;
	}

	atom->as_atom = value;
	*val_out = value;
}

// [= x y]
void fn_eq (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(cell_eq(args->as_pair.first,
				args->as_pair.rest->as_pair.first));
}

// [< n1 n2]
void fn_lt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int < b->as_int);
}

// [> n1 n2]
void fn_gt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int > b->as_int);
}

// [<= n1 n2]
void fn_lte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int <= b->as_int);
}

// [>= n1 n2]
void fn_gte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int >= b->as_int);
}

// [+ n1 n2]
void fn_add (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int + b->as_int);
}

// [- n1 n2]
void fn_sub (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int - b->as_int);
}

// [* n1 n2]
void fn_mul (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int * b->as_int);
}

// [/ n1 n2]
void fn_div (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int / b->as_int);
}

// [swap! atom fn args...] (variadic)
// The atom's val is modified to the result of applying the
// function with the atom's value as the first argument and the
// optionally given function arguments as the rest of the arguments.
// Return's the atom's new value
void fn_swap_bang (Cell *args, Cell *env, Cell **val_out)
{
	if (list_length(args) < 2)
	{
		printf("swap! : error : requires at least 2 arguments\n");
		return;
	}

	Cell *atom = args->as_pair.first;
	if (!is_kind(atom, CK_ATOM))
	{
		printf("swap! : error : argument 1 must be an atom\n");
		return;
	}

	Cell *fn = args->as_pair.rest->as_pair.first;
	if (!is_function(fn))
	{
		printf("swap! : error : argument 2 must be a function\n");
		return;
	}

	// Create a new arguments list for the function to be called
	Cell *args2 = args->as_pair.rest->as_pair.rest;
	if (!args2)
		args2 = make_empty_list();
	list_push(atom->as_atom, &args2);

	// Apply the function by evaluating a list as [fn args]
	*val_out = EVAL(make_pair(fn, args2), env);

	// Modify atom's value
	atom->as_atom = *val_out;
}

// [pair x y] -> [x | y]
void fn_pair (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_pair(args->as_pair.first, args->as_pair.rest->as_pair.first);
}

// [concat l1 l2 ...] -> list
void fn_concat (Cell *args, Cell *env, Cell **val_out)
{
	Cell *list = make_empty_list();
	Cell *p = list;

	while (is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		// Current list from arguments
		Cell *a = args->as_pair.first;
		if (!is_kind(a, CK_PAIR))
		{
			printf("fn_concat : error : arguments must be lists\n");
			return;
		}

		// Add all of the items from the current list
		while (is_kind(a, CK_PAIR) && !is_empty_list(a))
		{
			// Put item into the list
			p->as_pair.first = a->as_pair.first;
			p->as_pair.rest = make_empty_list();
			p = p->as_pair.rest;

			// Next argument
			a = a->as_pair.rest;
		}
		// The list should end with null instead of an empty list
		p->as_pair.rest = NULL;

		// Next argument
		args = args->as_pair.rest;
	}

	*val_out = list;
}

void fn_assoc (Cell *args, Cell *env, Cell **val_out)
{
	if (!is_kind(args->as_pair.rest->as_pair.first, CK_PAIR))
	{
		printf("assoc : error : second argument must be a list\n");
		*val_out = make_symbol(s_nil);
		return;
	}

	Cell *slot = alist_assoc(args->as_pair.first, args->as_pair.rest->as_pair.first);
	if (slot)
	{
		assert(is_kind(slot, CK_PAIR));
		*val_out = slot;
	}
	else
	{
		*val_out = make_symbol(s_nil);
	}
}

void fn_first (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	if (is_kind(a, CK_PAIR))
	{
		if (is_empty_list(a))
			*val_out = make_symbol(s_nil);
		else
			*val_out = a->as_pair.first;
	}
	else
	{
		printf("first : error : not a list\n");
		*val_out = NULL;
	}
}

void fn_rest (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	if (is_kind(a, CK_PAIR))
	{
		if (is_empty_list(a))
			*val_out = make_symbol(s_nil);
		else
			*val_out = a->as_pair.rest;
	}
	else
	{
		printf("rest : error : not a list\n");
		*val_out = NULL;
	}
}

