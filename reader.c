#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <ctype.h>

#include "reader.h"
#include "lizp.h"

int char_symbolp (char c)
{
	return (c > ' ')
		&& (c != '"')
		&& (c != ';')
		&& (c != '|') && (c != '[') && (c != ']');
}

// Returns the number of characters read
int read_int (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || !out || length <= 0)
		return 0;

	const char *view = start;

	// Result n from reading digits
	int n = 0;
	while (isdigit(*view) && (length > 0))
	{
		n = (n * 10) + ((*view) - '0');
		string_step(&view, &length, 1);
	}

	*out = make_int(n);
	return view - start;
}

// Read and intern symbol
// Returns number of chars read
int read_sym (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || !out || length <= 0)
		return 0;

	const char *view = start;

	while (char_symbolp(*view) && (length > 0))
		string_step(&view, &length, 1);

	Cell *name;
	int symbol_len = view - start;
	int parse_len = string_to_list(start, view - start, 0, &name);
	assert(stringp(name));
	assert(symbol_len == parse_len);

	Cell *interned = intern_symbol(name);
	assert(symbolp(interned));

	// Free name if it was already interned before
	if (interned->sym_name != name)
		cell_free_all(name);

	*out = interned;
	return symbol_len;
}

// Convert a string to a list of characters
int read_quoted_string (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!out || !start || length <= 0 || *start != '"')
		return 0;

	const char *view = start;

	// Read opening quote
	string_step(&view, &length, 1);
	
	// Do a pass to find the end quote
	int quoted_len = read_until(view, length, '"');
	string_step(&view, &length, quoted_len);

	if (length && *view == '"')
	{
		// Read closing quote
		string_step(&view, &length, 1);

		Cell *string = NULL;
		int parsed_len = string_to_list(start + 1, quoted_len, 1, &string);
		if (parsed_len == 0 && quoted_len == 0)
		{
			// Empty string
			*out = make_string_start();
			return view - start;
		}
		else if (parsed_len == quoted_len && cell_validp(string))
		{
			// Successfully processed non-empty string
			*out = string;
			return view - start;
		}
		else
		{
			*out = make_error_c("read_quoted_string : error : could not parse string contents", &sym_nil);
			return view - start;
		}
	}
	else
	{
		*out = make_error_c("read_quoted_string : error : unexpected end of input in quoted string", &sym_nil);
		return view - start;
	}
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

	if (*view && *view != ']')
	{
		Cell *p = list;

		// Read the first element
		int n = string_step(&view, &rem, read_str(view, rem, &(p->first)));
		if (!n)
		{
			*out = make_error_c("no items", &sym_nil);
			return view - start;
		}

		// Read the rest of the normal elements (don't handle the "dot")
		while ((rem > 0) && *view && (*view != ']') && (*view != '|'))
		{
			// Read an element
			Cell *e = NULL;
			if (!string_step(&view, &rem, read_str(view, rem, &e)) || !e)
			{
				*out = make_error_c("read_list : could not read item", &sym_nil);
				return view - start;
			}

			p->rest = make_pair(e, NULL);
			p = p->rest;
		}

		if (*view == '|')
		{
			// Dotted list
			Cell *e;
			string_step(&view, &rem, 1);
			string_step(&view, &rem, read_str(view, rem, &e));
			p->rest = e;
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
		*out = make_error_c("read_list: unexpected end of input", &sym_nil);
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
// Returns: the number of characters read, and writes the result to "out"
int read_str (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out || !start || length <= 0)
		return 0;

	const char *view = start;
	int rem = length;

	// Loop is for allowing comments to restart the read
	int loop = 1;
	while (loop)
	{
		loop = 0;
		string_skip_white(&view, &rem);
		switch (*view)
		{
			case '\0':
				// End of input
				*out = NULL;
				break;
			case ';':
				// Line comment
				*out = NULL;
				string_step(&view, &rem, read_until(view, rem, '\n'));
				loop = 1;
				break;
			case ']':
				*out = make_error_c("read : unmatched closing ']'", &sym_nil);
				break;
			case '|':
				*out = make_error_c("read : the pair delimiter '|' should only be inside a list", &sym_nil);
				break;
			case '[':
				// Opening paren, for lists
				string_step(&view, &rem, read_list(view, rem, out));
				break;
			case '"': 
				// Quoted string literal
				string_step(&view, &rem, read_quoted_string(view, rem, out));
				break;
			default:
				// Symbol or number
				if (isdigit(*view))
					string_step(&view, &rem, read_int(view, rem, out));
				else
					string_step(&view, &rem, read_sym(view, rem, out));
				break;
		}
	}
	string_skip_white(&view, &rem);

	return view - start;
}

