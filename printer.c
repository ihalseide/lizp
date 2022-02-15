#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "printer.h"
#include "lizp_string.h"

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
int print_cstr (char *s, char *out, int length)
{
	// Validate inputs
	if ((s == NULL) || (out == NULL) || (length <= 0))
		return 0;

	int i;
	for (i = 0; s[i] && i < length; i++)
		out[i] = s[i];
	return i;
}

// Print out a string
// returns number of chars written
int print_string (const char *str, char *out, int length, int readable)
{
	// Validate inputs
	if (!str || !out || (length < 0))
		return 0;

	char *view = out;
	int rem = length;

	// Opening quote
	if (readable)
		string_step((const char**) &view, &rem, print_char('"', view, rem));

	// String contents
	while (*str && (rem > 0))
	{
		char c = *str++;
		if (readable)
		{
			// Do string escaping...

			// We may need room for 2 characters
			if (rem < 2)
				break;

			char next_c = 0;
			switch (c)
			{
				case '\n': next_c = 'n';  break;
				case '\t': next_c = 't';  break;
				case '"':  next_c = '"';  break;
				case '\\': next_c = '\\'; break;
			}
			if (next_c)
			{
				string_step((const char**) &view, &rem, print_char('\\', view, rem));
				c = next_c;
			}
		}
		string_step((const char**) &view, &rem, print_char(c, view, rem));
	}

	// Closing quote
	if (readable)
		string_step((const char**) &view, &rem, print_char('"', view, rem));

	// Return length, including quotes that were written
	return view - out;
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

int print_list (const Cell *list, char *out, int length, int readable)
{
	// Validate arguments
	if (!is_kind(list, CK_PAIR) || !out || (length <= 0))
		return 0;

	char *view = out;
	int rem = length;

	// Print opening char
	string_step((const char**)&view, &rem, print_char('[', view, rem));

	// Print the first item with no leading space
	if (is_nonempty_list(list))
	{
		string_step((const char**)&view, &rem, pr_str(list->as_pair.first, view, rem, readable));
		list = list->as_pair.rest;

		// Print the rest of the normal list elements
		while (is_nonempty_list(list))
		{
			string_step((const char**)&view, &rem, print_char(' ', view, rem));
			string_step((const char**)&view, &rem, pr_str(list->as_pair.first, view, rem, readable));

			// Next
			list = list->as_pair.rest;
		}

		// List will be the last item in the 'rest' slot of the list at this point
		// If there is a value (except nil) in the final rest slot, then print it dotted
		if (list && !(is_kind(list, CK_SYMBOL) && list->as_str == s_nil))
		{
			string_step((const char**)&view, &rem, print_string(" | ", view, rem, 0));
			string_step((const char**)&view, &rem, pr_str(list, view, rem, readable));
		}
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(']', view, rem));

	return length - rem;
}

// Does: Prints form X to output stream
// Returns: number of chars written
int pr_str (const Cell *x, char *out, int length, int readable)
{
	// Validate inputs
	if (!out || !x || (length <= 0))
		return 0;

	switch (x->kind)
	{
		case CK_INT:
			return print_int(x->as_int, out, length);
		case CK_STRING:
			return print_string(x->as_str, out, length, readable);
		case CK_SYMBOL:
			return print_string(x->as_str, out, length, 0);
		case CK_PAIR:
			return print_list(x, out, length, readable);
		case CK_FUNC:
			return print_cstr("#<function>", out, length);
		case CK_NATIVE_FUNC:
			return print_cstr("#<code>", out, length);
		case CK_ATOM:
			return print_cstr("#<atom>", out, length);
		default:
			// Error: invalid cell kind
			printf("pr_str : error : invalid cell kind\n");
			return 0;
	}
}
