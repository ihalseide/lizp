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
		&& (c != '[') && (c != ']');
}

// Returns the number of characters read
// number read -> out
int read_int (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0)
		return 0;

	int n = 0;
	int i;
	for (i = 0; (i < length) && isdigit(start[i]); i++)
		n = (n * 10) + (start[i] - '0');

	if (out)
		*out = make_int(n);

	return i;
}

// Read and intern symbol
// Returns number of chars read
int read_sym (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0)
		return 0;

	// Find symbol length
	int i;
	for (i = 0; (i < length) && char_symbolp(start[i]); i++)
		;
	int symbol_len = i;

	// Parse the symbol name
	Cell *name;
	int parse_len = string_to_list(start, symbol_len, 0, &name);
	assert(stringp(name));
	assert(symbol_len == parse_len);

	// Intern the symbol name
	Cell *interned = intern_symbol(name);
	assert(symbolp(interned));

	// Free name if it was already interned before
	if (interned->sym_name != name)
		cell_free_all(name);

	if (out)
		*out = interned;
	return symbol_len;
}

// Convert a string to a list of characters
int read_quoted_string (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!start || length <= 0 || *start != '"')
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
			if (out)
				*out = make_string_start();
			return view - start;
		}
		else if (parsed_len == quoted_len && cell_validp(string))
		{
			// Successfully processed non-empty string
			if (out)
				*out = string;
			return view - start;
		}
		else
		{
			// could not parse string contents
			if (out)
				*out = NULL;
			return view - start;
		}
	}
	else
	{
		// unexpected end of input in quoted string
		if (out)
			*out = NULL;
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

	// Consume the opening paren
	string_step(&view, &length, 1);
	string_skip_white(&view, &length);

	Cell *list = make_empty_list();
	if (!list)
		return view - start;

	if (*view && *view != ']')
	{
		Cell *p = list;

		// Read the first element
		int n = string_step(&view, &length, read_str(view, length, &(p->first)));
		if (!n)
		{
			// no items
			*out = NULL;
			return view - start;
		}

		// Read the rest of the normal elements (don't handle the "dot")
		while ((length > 0) && *view && (*view != ']'))
		{
			// Read an element
			Cell *e = NULL;
			if (!string_step(&view, &length, read_str(view, length, &e)) || !e)
			{
				// could not read item
				*out = NULL;
				return view - start;
			}

			p->rest = make_single_list(e);
			p = p->rest;
		}
	}

	if (*view == ']')
	{
		// Consume the final character
		string_step(&view, &length, 1);
		*out = list;
	}
	else
	{
		// unexpected end of input
		*out = NULL;
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
				// unmatched closing ']'
				*out = NULL; 
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

