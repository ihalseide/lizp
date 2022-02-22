#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "cell.h"
#include "error.h"
#include "function.h"
#include "lizp.h"
#include "printer.h"
#include "reader.h"

void print_nonreadably (Cell *expr)
{
	static char buffer[2 * 1024];
	int p_len = pr_str(expr, buffer, sizeof(buffer), 0);
	printf("%.*s\n", p_len, buffer);
}

// [str a b c ...] -> "abc..." (prints non-readably)
Cell *fn_str (Cell *args)
{
	return string_join(args, 0, 0);
}

// [pr-str a b c ...] -> "a b c ..." (prints readably)
Cell *fn_pr_str (Cell *args)
{
	return string_join(args, ' ', 1);
}

// [prn a b c ...] -> nil (prints readably)
Cell *fn_prn (Cell *args)
{
	Cell *s = string_join(args, ' ', 1);
	if (stringp(s))
	{
		PRINT(s);
		cell_free_all(s);
	}
	return &sym_nil;
}

// [println a b c ...] -> nil (prints non-readably)
Cell *fn_println (Cell *args)
{
	Cell *s = string_join(args, ' ', 0);
	if (stringp(s))
	{
		print_nonreadably(s);
		cell_free_all(s);
	}
	return &sym_nil;
}

// [list ...] -> [...] (variadic)
Cell *fn_list (Cell *args)
{
	return args;
}

// [eval expr]
Cell *fn_eval (Cell *args)
{
	(void) args;
	assert(0 && "Not to be implemented. This function is only needed for its pointer value.");
}

// [slurp "file name"] -> "file contents"
Cell *fn_slurp (Cell *args)
{
	Cell *a = args->first;

	// Validate arguments
	if (!stringp(a))
		error_raise("slurp : 1st argument must be a string file name");

	// Get string of file name
	char path[1024];
	int len = pr_str(a, path, sizeof(path) - 1, 0);
	path[len] = '\0';

	FILE *f = fopen(path, "r");
	if (!f)
		error_raise("slurp : could not read file");

	// Get file length
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	// See if we have enough room for this string data
	if (!cell_can_alloc(fsize + 1))
	{
		fclose(f);
		error_raise("slurp : not enough memory to read file");
	}

	// Read the char data and null-terminate it.
	// Stack allocate the file contents because it's
	// getting converte to a lisp string anyways...
	char content[fsize + 1];
	fread(content, fsize, 1, f);
	fclose(f);
	content[fsize] = 0;

	// Convert to lisp string
	Cell *s;
	int parse = string_to_list(content, fsize, 0, &s);
	assert(parse == fsize);

	return s;
}

// [read-string "str"] -> any value
Cell *fn_read_str (Cell *args)
{
	Cell *a = args->first;
	if (stringp(a))
	{
		char buffer[4 * 1024];
		int len = pr_str(a, buffer, sizeof(buffer), 0);
		assert((unsigned) len < sizeof(buffer));
		Cell *b;
		read_str(buffer, len, &b);
		return b;
	}
	else
	{
		error_raise("read-string : argument must be a string");
	}
}

// [empty? x]
Cell *fn_empty_p (Cell *args)
{
	return get_bool_sym(emptyp(args->first));
}

// [count list]
Cell *fn_count (Cell *args)
{
	if (pairp(args->first))
		return make_int(list_length(args->first));
	else
		error_raise("count : first argument must be a list");
}

// [list? x]
Cell *fn_list_p (Cell *args)
{
	return get_bool_sym(pairp(args->first));
}

// [int? x]
Cell *fn_int_p (Cell *args)
{
	return get_bool_sym(intp(args->first));
}

// [= x y]
Cell *fn_eq (Cell *args)
{
	return get_bool_sym(cell_eq(args->first,
				args->rest->first));
}

// [< n1 n2]
Cell *fn_lt (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return get_bool_sym(a->integer < b->integer);
	else
		error_raise("< : both arguments must be integers");
}

// [> n1 n2]
Cell *fn_gt (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return get_bool_sym(a->integer > b->integer);
	else
		error_raise("> : both arguments must be integers");
}

// [<= n1 n2]
Cell *fn_lte (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return get_bool_sym(a->integer <= b->integer);
	else
		error_raise("<= : both arguments must be integers");
}

// [>= n1 n2]
Cell *fn_gte (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return get_bool_sym(a->integer >= b->integer);
	else
		error_raise(">= : both arguments must be integers");
}

// [+ n1 n2]
Cell *fn_add (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return make_int(a->integer + b->integer);
	else
		error_raise("+ : both arguments must be integers");
}

// [- n1 n2]
Cell *fn_sub (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return make_int(a->integer - b->integer);
	else
		error_raise("- : both arguments must be integers");
}

// [* n1 n2]
Cell *fn_mul (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		return make_int(a->integer * b->integer);
	else
		error_raise("* : both arguments must be integers");
}

// [/ n1 n2]
Cell *fn_div (Cell *args)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (intp(a) && intp(b))
		if (b->integer == 0)
			error_raise("/ : division by zero");
		else
			return make_int(a->integer / b->integer);
	else
		error_raise("/ : both arguments must be integers");
}

// [pair x y] -> [x | y]
Cell *fn_pair (Cell *args)
{
	return make_pair_valid(args->first, args->rest->first);
}

// [concat l1 l2 ...] -> list
Cell *fn_concat (Cell *args)
{
	Cell *list = make_empty_list();
	Cell *p = list;

	while (nonempty_listp(args))
	{
		// Current list from arguments
		Cell *a = args->first;
		if (!pairp(a) || functionp(a) || stringp(a))
			error_raise("fn_concat : arguments must be lists");

		// Add all of the items from the current list
		while (nonempty_listp(a))
		{
			// Put item into the list
			p->first = a->first;
			p->rest = make_empty_list();
			p = p->rest;

			// Next argument
			a = a->rest;
		}
		// The list should end with null instead of an empty list
		p->rest = NULL;

		// Next argument
		args = args->rest;
	}

	return list;
}

// [assoc item alist]
Cell *fn_assoc (Cell *args)
{
	if (pairp(args->rest->first))
	{
		Cell *slot = alist_assoc(args->first, args->rest->first);
		if (slot)
			return slot;
		else
			return &sym_nil;
	}
	else
	{
		error_raise("assoc : second argument must be a list");
	}
}

// [first pair]
Cell *fn_first (Cell *args)
{
	Cell *a = args->first;
	if (emptyp(a))
	{
		return &sym_nil;
	}
	else if (pairp(a))
	{
		return a->first;
	}
	else
	{
		error_raise("first : not a list");
		return NULL;
	}
}

// [rest pair]
Cell *fn_rest (Cell *args)
{
	Cell *a = args->first;
	if (emptyp(a))
		return &sym_nil;
	else if (pairp(a))
		return a->rest;
	else
		error_raise("first : not a list");
}

