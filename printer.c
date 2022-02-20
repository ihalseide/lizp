#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "cell.h"
#include "printer.h"

int print_char (char c, char *out, int length)
{
	if (out && (length > 0))
	{
		*out = c;
		return 1;
	}
	return 0;
}

// returns number of chars written
int print_cstr (const char *s, char *out, int length)
{
	// Validate inputs
	if ((s == NULL) || (out == NULL) || (length <= 0))
		return 0;

	int i;
	for (i = 0; s[i] && i < length; i++)
		out[i] = s[i];
	return i;
}

// returns number of chars written
int print_int (int n, char *out, int length)
{
	char buf[20];
	int u = (n >= 0)? n : -n;

	// Remaining length
	int rem = length;

	int i = sizeof(buf) - 1;
	while ((rem > 0) && (i >= 0))
	{
		buf[i] = '0' + (u % 10);
		u /= 10;
		if (u <= 0) { break; }
		rem--;
		i--;
	}

	// Add minus sign
	if (n < 0)
	{
		buf[--i] = '-';
	}

	int len = sizeof(buf) - i;
	memcpy(out, buf + i, len);
	return len;
}

int print_list_as_string (const Cell *list, char *out, int length, int readable)
{
	// Validate inputs
	if (!(is_kind(list, CK_PAIR) || cell_eq(list, &sym_nil)) || !out || length <= 0)
		return 0;

	char *view = out;
	int rem = length;

	// Opening quote
	if (readable)
		string_step((const char**) &view, &rem, print_char('"', view, rem));

	// String contents
	const Cell *p = list;
	while ((rem > 1) && nonempty_listp(p))
	{
		// Get character value in list
		Cell *e = p->first;
		if (!is_kind(e, CK_INTEGER))
			break;
		char c = (char) e->integer;

		if (readable)
		{
			// Do string escaping...
			if (rem < 2)
				break;

			char esc = 0;
			switch (c)
			{
				case '\n': esc = 'n'; break;
				case '\t': esc = 't'; break;
				case '\\': esc = '\\'; break;
			}

			if (esc)
			{
				string_step((const char**) &view, &rem, print_char('\\', view, rem));
				c = esc;
			}
		}

		// Write char
		string_step((const char**) &view, &rem, print_char(c, view, rem));

		// Next list item
		p = p->rest;
	}

	// Closing quote
	if (readable)
		string_step((const char**) &view, &rem, print_char('"', view, rem));

	// Return length, including quotes that were written
	return view - out;
}

int print_list (Cell *list, char *out, int length, int readable)
{
	// Validate arguments
	if (!is_kind(list, CK_PAIR) || !out || (length <= 0))
		return 0;

	char *view = out;
	int rem = length;

	// Print opening char
	string_step((const char**)&view, &rem, print_char('[', view, rem));

	// Print the first item with no leading space
	if (nonempty_listp(list))
	{
		string_step((const char**)&view, &rem, pr_str(list->first, view, rem, readable));
		list = list->rest;

		// Print the rest of the normal list elements
		while (nonempty_listp(list))
		{
			string_step((const char**)&view, &rem, print_char(' ', view, rem));
			string_step((const char**)&view, &rem, pr_str(list->first, view, rem, readable));

			// Next
			list = list->rest;
		}

		// List will be the last item in the 'rest' slot of the list at this point
		// If there is a value (except nil) in the final rest slot, then print it dotted
		if (cell_validp(list) && !cell_eq(list, &sym_nil))
		{
			string_step((const char**)&view, &rem, print_cstr(" | ", view, rem));
			string_step((const char**)&view, &rem, pr_str(list, view, rem, readable));
		}
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(']', view, rem));

	return length - rem;
}

int print_symbol (Cell *sym, char *out, int length)
{
	assert(is_kind(sym, CK_SYMBOL));
	assert(stringp(sym->sym_name));
	return print_list_as_string(sym->sym_name->rest, out, length, 0);
}

int print_pair (Cell *p, char *out, int length, int readable)
{
	assert(is_kind(p, CK_PAIR));
	if (stringp(p))
		return print_list_as_string(p->rest, out, length, readable);
	else
		return print_list(p, out, length, readable);
}

// Does: Prints form X to output stream
// Returns: number of chars written
int pr_str (Cell *x, char *out, int length, int readable)
{
	// Validate inputs
	if (!out || (length <= 0))
		return 0;

	if (!cell_validp(x))
		return print_cstr("#<invalid>", out, length);
	switch (x->kind)
	{
		case CK_INTEGER:
			return print_int(x->integer, out, length);
		case CK_SYMBOL:
			return print_symbol(x, out, length);
		case CK_PAIR:
			return print_pair(x, out, length, readable);
		case CK_FUNCTION:
			return print_cstr("#<code>", out, length);
		default:
			return print_cstr("#<invalid>", out, length);
	}
}

