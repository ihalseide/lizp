#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cell.h"
#include "printer.h"
#include "lizp_string.h"

const char *s_nil = "nil",
	  *s_false    = "#f",
	  *s_true     = "#t",
	  *s_def_bang = "def!",
	  *s_let_star = "let*",
	  *s_if       = "if",
	  *s_fn_star  = "fn*",
	  *s_do       = "do",
	  *s_quote    = "quote";

Cell *string_join (Cell *items, char sep, int readable)
{
	Cell *p_items = items;

	Cell *result = make_empty_list();
	Cell *p_result = result;

	// Do the first item without separator
	if (is_kind(p_result, CK_PAIR) && nonempty_listp(p_items))
	{
		p_result->first = p_items->first;

		// Next
		p_items = p_items->rest;
		p_result = p_result->rest;
	}

	Cell *separator = make_int(sep);

	// Do the rest of the items with separator
	while (is_kind(p_result, CK_PAIR) && nonempty_listp(p_items))
	{
		// Add separator
		p_result->rest = make_pair(separator, NULL);
		p_result = p_result->rest;

		// Add item
		p_result->rest = make_pair(p_items->first, NULL);

		// Next
		p_items = p_items->rest;
		p_result = p_result->rest;
	}

	return result;
}

// For string reading and writing
int string_step (const char **stream, int *length, int n)
{
	if (n <= *length)
	{
		*stream += n;
		*length -= n;
		return n;
	}
	return 0;
}

void string_skip_white(const char **stream, int *length)
{
	if (!stream)
		return;

	const char *view = *stream;
	int rem = *length;
	while (isspace(*view) && (rem > 0))
		string_step(&view, &rem, 1);
	*stream = view;
	*length = rem;
}


