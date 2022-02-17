#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cell.h"
#include "lizp_string.h"

// Returns the number of chars parsed
int string_to_list (const char *start, int length, int escape, Cell **out)
{
	// Validate args
	if (!start || length <= 0 || !out)
		return 0;

	const char *view = start;
	Cell *list = make_string_start();
	Cell *p = list;

	while (is_kind(p, CK_PAIR) && *view && length)
	{
		// Get next char to add to end of string
		char c = *view;

		// Maybe process escape codes
		if (escape && length && (c == '\\'))
		{
			string_step(&view, &length, 1);
			if (!length)
			{
				*out = NULL;
				cell_free_all(list);
				return view - start;
			}

			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
			}
		}
		string_step(&view, &length, 1);

		// Put list with char in REST slot
		p->rest = make_single_list(make_int(c));
		p = p->rest;
	}

	// See if p being invalid is what caused the while loop to stop
	if (is_kind(p, CK_PAIR))
	{
		*out = list;
		return view - start;
	}
	else
	{
		*out = NULL;
		cell_free_all(list);
		return view - start;
	}
}

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


