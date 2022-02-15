#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#include "reader.h"
#include "lizp_string.h"

int char_is_symbol (char c)
{
	return (c > ' ')
		&& (c != '`') && (c != '\'')
		&& (c != ';')
		&& (c != '|') && (c != '[') && (c != ']');
}

int parse_int (const char *start, int length, int *out)
{
	// Validate inputs
	if (!out)
		return 0;
	if (!start || !out || length <= 0)
	{
		*out = 0;
		return 0;
	}

	const char *view = start;

	// Read optional minus sign
	int sign = 1;
	if (*view == '-')
	{
		sign = -sign;
		string_step(&view, &length, 1);
	}

	// Result n from reading digits
	int n = 0;
	while (isdigit(*view) && (length > 0))
	{
		n = (n * 10) + ((*view) - '0');
		string_step(&view, &length, 1);
	}

	*out = n * sign;
	return view - start;
}

// Create a string cell that has the string value from reading a quoted string
// Strings are delimited by 2 different characters, example: `hello, world'
int read_string_literal (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!out)
		return 0;
	if (!start || length <= 0 || !char_free)
	{
		*out = NULL;
		return 0;
	}

	// Is the current part of input string
	const char *view = start;

	// Is the current part of the output buffer (char pool)
	// Write to the char_pool, but do not move the char_free pointer.
	char *pad = char_free;
	int pad_len = char_pool_cap - (char_free - char_pool);

	// Read opening quote
	string_step(&view, &length, 1);

	// Read contents and process escape codes
	while (*view && (*view != '\'') && (length > 0) && (pad_len > 0))
	{
		char c = *view;
		if (*view == '\\')
		{
			string_step(&view, &length, 1);
			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
			}
		}
		*pad = c;
		string_step((const char**) &pad, &pad_len, 1);
		string_step(&view, &length, 1);
	}

	// Read closing quote
	string_step(&view, &length, 1);

	// Intern the string, which will handle the case that
	// this string is already in the char_pool.
	// May or may not cause string allocation.
	int result_length = pad - char_free;
	*out = intern_string(char_free, result_length);

	// Return the number of chars written
	return view - start;
}

// Read in a symbol or number
int read_item (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	if (!start || (length <= 0))
	{
		*out = NULL;
		return 0;
	}

	const char *p = start;

	// Symbol or number
	while (length > 0 && char_is_symbol(*p))
		string_step(&p, &length, 1);

	// Symbols or numbers can start with a '-', so we need to distinguish them by 
	// what the next character is.
	if (isdigit(*start) || ((length > 1) && (*start == '-') && isdigit(*(start + 1))))
	{
		// Number
		int x;
		int n = parse_int(start, p - start, &x);
		if (!n)
		{
			*out = NULL;
			return p - start;
		}
		*out = make_int(x);
	}
	else
	{
		// Symbol
		Cell *name = intern_string(start, p - start);
		if (!name)
			*out = NULL;

		assert(is_kind(name, CK_STRING));
		*out = make_symbol(name->as_str);
	}

	return p - start;
}

int read_list (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	if (!start || length <= 0)
	{
		*out = NULL;
		return 0;
	}

	const char *view = start;
	int rem = length;

	// Consume the opening paren
	string_step(&view, &rem, 1);
	string_skip_white(&view, &rem);

	Cell *list = make_empty_list();
	if (!list)
		return view - start;

	if (*view != ']')
	{
		Cell *p = list;

		// Read the first element
		if (!string_step(&view, &rem, read_str(view, rem, &(p->as_pair.first))))
		{
			*out = NULL;
			return view - start;
		}

		// Read the rest of the normal elements (don't handle the "dot")
		while ((rem > 0) && *view && (*view != ']') && (*view != '|'))
		{
			// Read an element
			Cell *e;
			if (!string_step(&view, &rem, read_str(view, rem, &e)) || !e)
				break;

			p->as_pair.rest = make_pair(e, NULL);
			p = p->as_pair.rest;
		}

		if (*view == '|')
		{
			// Dotted list
			Cell *e;
			string_step(&view, &rem, 1);
			string_step(&view, &rem, read_str(view, rem, &e));
			p->as_pair.rest = e;
		}
	}

	if (*view == ']')
	{
		// Consume the final character
		string_step(&view, &rem, 1);
		*out = list;
	}
	else
	{
		// Error: unexpected end of input
		*out = NULL;
		printf("read_list: error : error reading item or unexpected end of input\n");
	}

	return view - start;
}

int read_until (const char *start, int length, char sentinel)
{
	int i = 0;
	while (start[i] && (i < length) && (start[i] != sentinel))
		i++;
	return i;
}

// Read a form from an input stream/string
// Returns: the number of characters read
int read_str (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	if (!start || length <= 0)
	{
		*out = NULL;
		return 0;
	}

	const char *view = start;
	int rem = length;

	string_skip_white(&view, &rem);
	switch (*view)
	{
		case '\0':
			// End of input
			*out = make_symbol(s_nil);
			break;
		case ';':
			// Line comment
			*out = make_symbol(s_nil);
			string_step(&view, &rem, read_until(view, rem, '\n'));
			break;
		case '\'':
			*out = NULL;
			printf("read error : unmatched closing delimiter `\\%c'\n", *view);
			break;
		case ']':
			*out = NULL;
			printf("read error : unmatched closing delimiter `%c'\n", *view);
			break;
		case '|':
			*out = NULL;
			printf("read error : pair delimiter `|' should only be inside a list\n");
			break;
		case '[':
			// Opening paren, for lists
			string_step(&view, &rem, read_list(view, rem, out));
			break;
		case '`': 
			// Quoted string literal
			string_step(&view, &rem, read_string_literal(view, rem, out));
			break;
		default:
			// Symbol or number
			string_step(&view, &rem, read_item(view, rem, out));
			break;
	}
	string_skip_white(&view, &rem);

	return view - start;
}

