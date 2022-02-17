#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "cell.h"
#include "function.h"
#include "lizp.h"
#include "reader.h"
#include "lizp_string.h"

/*
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
*/

// [list a ...] -> [a ...] (variadic)
void fn_list (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = args;
}

void fn_eval (Cell *args, Cell *env, Cell **val_out)
{
	assert(0 && "Not to be implemented. This function is only needed for its pointer value.");
}

// (slurp "file name") -> "file contents"
void fn_slurp (Cell *args, Cell *env, Cell **val_out)
{
	assert(0 && "NOT IMPLEMENTED");
	/*
	Cell *a = args->first;

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

	*/
}

// [read-string "str"] -> any value
void fn_read_str (Cell *args, Cell *env, Cell **val_out)
{
	assert(0 && "not implemented");
	/*
	Cell *a = args->first;
	if (is_kind(a, CK_STRING))
		read_str(a->as_str, strlen(a->as_str), val_out);
		*/
}

// [empty? x]
void fn_empty_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = get_bool_sym(emptyp(args->first));
}

// [count list]
void fn_count (Cell *args, Cell *env, Cell **val_out)
{
	if (!is_kind(args->first, CK_PAIR))
	{
		printf("count : error : first argument must be a list\n");
		return;
	}
	*val_out = make_int(list_length(args->first));
}

// [list? x]
void fn_list_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = get_bool_sym(is_kind(args->first, CK_PAIR));
}

// [int? x]
void fn_int_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = get_bool_sym(is_kind(args->first, CK_INTEGER));
}

// [= x y]
void fn_eq (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = get_bool_sym(cell_eq(args->first,
				args->rest->first));
}

// [< n1 n2]
void fn_lt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = get_bool_sym(a->integer < b->integer);
}

// [> n1 n2]
void fn_gt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = get_bool_sym(a->integer > b->integer);
}

// [<= n1 n2]
void fn_lte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = get_bool_sym(a->integer <= b->integer);
}

// [>= n1 n2]
void fn_gte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = get_bool_sym(a->integer >= b->integer);
}

// [+ n1 n2]
void fn_add (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = make_int(a->integer + b->integer);
}

// [- n1 n2]
void fn_sub (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = make_int(a->integer - b->integer);
}

// [* n1 n2]
void fn_mul (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = make_int(a->integer * b->integer);
}

// [/ n1 n2]
void fn_div (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	Cell *b = args->rest->first;
	if (!is_kind(a, CK_INTEGER) || !is_kind(b, CK_INTEGER))
		return;
	*val_out = make_int(a->integer / b->integer);
}

// [pair x y] -> [x | y]
void fn_pair (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_pair(args->first, args->rest->first);
}

// [concat l1 l2 ...] -> list
void fn_concat (Cell *args, Cell *env, Cell **val_out)
{
	Cell *list = make_empty_list();
	Cell *p = list;

	while (nonempty_listp(args))
	{
		// Current list from arguments
		Cell *a = args->first;
		if (!is_kind(a, CK_PAIR))
		{
			printf("fn_concat : error : arguments must be lists\n");
			return;
		}

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

	*val_out = list;
}

void fn_assoc (Cell *args, Cell *env, Cell **val_out)
{
	if (!is_kind(args->rest->first, CK_PAIR))
	{
		printf("assoc : error : second argument must be a list\n");
		*val_out = NULL;
		return;
	}

	Cell *slot = alist_assoc(args->first, args->rest->first);
	if (slot)
	{
		assert(is_kind(slot, CK_PAIR));
		*val_out = slot;
	}
	else
	{
		*val_out = NULL;
	}
}

void fn_first (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	if (is_kind(a, CK_PAIR))
	{
		if (emptyp(a))
			*val_out = NULL;
		else
			*val_out = a->first;
	}
	else
	{
		printf("first : error : not a list\n");
		*val_out = NULL;
	}
}

void fn_rest (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->first;
	if (is_kind(a, CK_PAIR))
	{
		if (emptyp(a))
			*val_out = NULL;
		else
			*val_out = a->rest;
	}
	else
	{
		printf("rest : error : not a list\n");
		*val_out = NULL;
	}
}

