#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "cell.h"
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

static Cell *apply_built_in_var (Native_fn_t id, Cell *args)
{
	assert(pairp(args));
	switch (id)
	{
		case FN_STR:
			// [str a b c ...] -> "abc..." (prints non-readably)
			return string_join(args, 0, 0);
		case FN_PR_STR:
			// [pr-str a b c ...] -> "a b c ..." (prints readably)
			return string_join(args, ' ', 1);
		case FN_PRN:
			// [prn a b c ...] -> nil (prints readably)
			{
				Cell *s = string_join(args, ' ', 1);
				if (stringp(s))
				{
					PRINT(s);
					cell_free_all(s);
				}
				return &sym_nil;
			}
		case FN_PRINTLN:
			// [println a b c ...] -> nil (prints non-readably)
			{
				Cell *s = string_join(args, ' ', 0);
				if (stringp(s))
				{
					print_nonreadably(s);
					cell_free_all(s);
				}
				return &sym_nil;
			}
		case FN_LIST:
			// [list ...] -> [...] (variadic)
			return args;
		case FN_CONCAT:
			// [concat l1 l2 ...] -> list
			{
				Cell *list = make_empty_list();
				Cell *p = list;

				while (nonempty_listp(args))
				{
					// Current list from arguments
					Cell *a = args->first;
					if (!pairp(a) || functionp(a) || stringp(a))
						//arguments must be lists
						return NULL;

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
		default:
			assert(0);
	}
}

static Cell *apply_built_in_1 (Native_fn_t id, Cell *args)
{
	if (1 != list_length(args))
		// Incorrect number of arguments
		return NULL;

	Cell *p1 = args->first;
	if (!cell_validp(p1))
		// Invalid argument
		return NULL;

	switch (id)
	{
		case FN_EVAL:
			assert(0 && "not implemented yet");
		case FN_SLURP:
			// [slurp "file name"] -> "file contents"
			{
				// Validate arguments
				if (!stringp(p1))
					//1st argument must be a string file name
					return NULL;

				// Get string of file name
				char path[1024];
				int len = pr_str(p1, path, sizeof(path) - 1, 0);
				path[len] = '\0';

				FILE *f = fopen(path, "r");
				if (!f)
					// could not read file
					return NULL;

				// Get file length
				fseek(f, 0, SEEK_END);
				long fsize = ftell(f);
				fseek(f, 0, SEEK_SET);

				// See if we have enough room for this string data
				if (!cell_can_alloc(fsize + 1))
				{
					// not enough memory to read file
					fclose(f);
					return NULL;
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
		case FN_READ_STR:
			// [read-string "str"] -> any value
			{
				if (stringp(p1))
				{
					char buffer[4 * 1024];
					int len = pr_str(p1, buffer, sizeof(buffer), 0);
					assert((unsigned) len < sizeof(buffer));
					Cell *b;
					read_str(buffer, len, &b);
					return b;
				}
				else
				{
					// argument must be a string
					return NULL;
				}
			}
		case FN_EMPTY_P:
			// [empty? x]
			return get_bool_sym(emptyp(p1));
		case FN_COUNT:
			// [count list]
			if (pairp(p1))
				return make_int(list_length(p1));
			else
				// first argument must be a list
				return NULL;
		case FN_LIST_P:
			// [list? x]
			return get_bool_sym(pairp(p1));
		case FN_INT_P:
			// [int? x]
			return get_bool_sym(intp(p1));
		case FN_FIRST:
			// [first pair]
			if (emptyp(p1))
				return &sym_nil;
			else if (pairp(p1))
				return p1->first;
			else
				return NULL;
		case FN_REST:
			// [rest pair]
			if (emptyp(p1))
				return &sym_nil;
			else if (pairp(p1))
				return p1->rest;
			else
				return NULL;
		default:
			assert(0);
	}
}

static Cell *apply_built_in_2int (Native_fn_t id, Cell *p1, Cell *p2)
{
	int n1, n2;

	if (intp(p1))
		n1 = p1->integer;
	else
		// p1 must be an int type
		return NULL;

	if (intp(p2))
		n2 = p2->integer;
	else
		// p2 must be an int type
		return NULL;

	switch (id)
	{
		case FN_LT: 
			return get_bool_sym(n1 < n2);
		case FN_GT: 
			return get_bool_sym(n1 > n2);
		case FN_LTE:
			return get_bool_sym(n1 <= n2);
		case FN_GTE:
			return get_bool_sym(n1 >= n2);
		case FN_ADD:
			return make_int(n1 + n2);
		case FN_SUB:
			return make_int(n1 - n2);
		case FN_MUL:
			return make_int(n1 * n2);
		case FN_DIV:
			if (n2 == 0)
				// Division by zero
				return NULL;
			else
				return make_int(n1 / n2);
		default:
			assert(0);
	}
}

static Cell *apply_built_in_2 (Native_fn_t id, Cell *args)
{
	assert(pairp(args));
	
	if (2 != list_length(args))
		// Incorrect number of arguments
		return NULL;

	Cell *p1 = args->first;
	if (!cell_validp(p1))
		// Invalid argument
		return NULL;

	Cell *p2 = args->rest->first;
	if (!cell_validp(p2))
		// Invalid argument
		return NULL;

	switch (id)
	{
		case FN_EQ:
			// [= x y]
			return get_bool_sym(cell_eq(p1, p2));
		case FN_LT:
		case FN_GT:
		case FN_LTE:
		case FN_GTE:
		case FN_ADD:
		case FN_SUB:
		case FN_MUL:
		case FN_DIV:
			return apply_built_in_2int(id, p1, p2);
		default:
			assert(0);
	}
}

Cell *apply_built_in (Native_fn_t id, Cell *args)
{
	assert(pairp(args));

	switch (id)
	{
		case FN_EVAL:
			assert(0 && "should not be handled here");
		case FN_SLURP:
		case FN_READ_STR:
		case FN_EMPTY_P:
		case FN_COUNT:
		case FN_LIST_P:
		case FN_INT_P:
		case FN_FIRST:
		case FN_REST:
			return apply_built_in_1(id, args);
		case FN_EQ:
		case FN_LT:
		case FN_GT:
		case FN_LTE:
		case FN_GTE:
		case FN_ADD:
		case FN_SUB:
		case FN_MUL:
		case FN_DIV:
			return apply_built_in_2(id, args);
		case FN_STR:
		case FN_PR_STR:
		case FN_PRN:
		case FN_PRINTLN:
		case FN_LIST:
		case FN_CONCAT:
			return apply_built_in_var(id, args);
		case FN_INVALID:
		default:
			assert(0 && "invalid built-in function id");
	}
}

