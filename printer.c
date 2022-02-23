#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "cell.h"
#include "printer.h"

// Returns: number of chars written
int print_char(char c, char *out, int length)
{
	// Validate arguments
	if (!out || length <= 0)
		return 0;

	*out = c;
	return 1;
}

// Returns: number of chars written
int print_cstr(const char *s, char *out, int len)
{
	// Validate inputs
	if (!s || !out)
		return 0;

	int i;
	for (i = 0; s[i] && i < len; i++)
		out[i] = s[i];

	return i;
}

// Returns: number of chars written
int print_int(int n, char *out, int len)
{
	assert(out);

	// Zero -> special case
	if (len >= 0 && n == 0)
	{
		*out = '0';
		return 1;
	}

	char buf[20];
	const int sz = sizeof(buf);

	// U = magnitude of N
	int u = (n >= 0)? n : -n;

	int i;
	for (i = 0; (u > 0) && (i < len) && (i < sz); i++)
	{
		buf[sz - i - 1] = '0' + (u % 10);
		u /= 10;
	}

	// Loop should run at least once, even for n == 0
	assert(i >= 1);

	// Minus sign for negative numbers
	if (n < 0)
		buf[--i] = '-';

	memcpy(out, buf + sz - i, i);
	return i;
}

int print_list_as_string(const Cell *list, char *out, int length, int readable)
{
	// Validate inputs
	if (!(pairp(list) || cell_eq(list, &sym_nil)) || !out || length <= 0)
		return 0;

	char *view = out;

	// Opening quote
	if (readable)
		string_step((const char**) &view, &length, print_char('"', view, length));

	// String contents
	const Cell *p = list;
	while ((length > 1) && nonempty_listp(p))
	{
		// Get character value in list
		Cell *e = p->first;
		// If an element is not an integer, the string stops printing out
		if (!intp(e))
			break;
		char c = (char) e->integer;

		if (readable)
		{
			// Do string escaping...
			char esc;
			switch (c)
			{
				case '\n': esc = 'n'; break;
				case '\t': esc = 't'; break;
				case '\0': esc = '0'; break;
				case '\\': esc = '\\'; break;
				default: esc = 0; break;
			}

			if (esc)
			{
				// Print a slash and get ready to print the escape char next
				string_step((const char**) &view, &length, print_char('\\', view, length));
				c = esc;
			}
		}

		// Write char
		string_step((const char**) &view, &length, print_char(c, view, length));

		// Next list item
		p = p->rest;
	}

	// Closing quote
	if (readable)
		string_step((const char**) &view, &length, print_char('"', view, length));

	// Return length, including quotes that were written
	return view - out;
}

int print_list(Cell *list, char *out, int length, int readable)
{
	// Validate arguments
	assert(pairp(list));
	if (!out || (length <= 0))
		return 0;

	char *view = out;

	// Print opening '['
	string_step(&view, &length, print_char('[', view, length));

	if (nonempty_listp(list))
	{
		// Print the first item with no leading space
		string_step(&view, &length, pr_str(list->first, view, length, readable));
		// Next item
		list = list->rest;

		// Print the rest of the items
		while (nonempty_listp(list))
		{
			// Print item with leading space
			string_step(&view, &length, print_char(' ', view, length));
			string_step(&view, &length, pr_str(list->first, view, length, readable));
			// Next item
			list = list->rest;
		}
		assert(cell_eq(list, &sym_nil));
	}

	// Print closing ']'
	string_step(&view, &length, print_char(']', view, length));

	return view - out;
}

int print_symbol(Cell *sym, char *out, int length)
{
	assert(symbolp(sym));
	assert(stringp(sym->sym_name));
	return print_list_as_string(sym->sym_name->rest, out, length, 0);
}

// This function is necessary because there are a bunch of special types of pairs
int print_pair(Cell *p, char *out, int length, int readable)
{
	assert(out);
	assert(pairp(p));
	if (length <= 0)
		return 0;

	if (stringp(p))
		return print_list_as_string(p->rest, out, length, readable);
	else if (functionp(p))
		return print_cstr("#<function>", out, length);
	else
		return print_list(p, out, length, readable);
}

// Does: Prints p to the given output stream
// Returns: number of chars written
int pr_str(Cell *p, char *out, int length, int readable)
{
	// Validate arguments
	assert(out);
	if (length <= 0)
		return 0;

	if (!cell_validp(p))
		return print_cstr("#<invalid>", out, length);
	switch (p->kind)
	{
		case CK_INTEGER:
			return print_int(p->integer, out, length);
		case CK_SYMBOL:
			return print_symbol(p, out, length);
		case CK_PAIR:
			return print_pair(p, out, length, readable);
		default:
			assert(0);
	}
}

