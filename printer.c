#include <assert.h>
#include <string.h>
#include <stdbool.h>

#include "printer.h"

static int printNumBase = 10;
static bool printNumUpper = false;

int PrinterGetBase(void)
{
	return printNumBase;
}

void PrinterSetBase(int b)
{
	switch (b)
	{
		case 2:
		case 10:
		case 16:
		case 36:
			printNumBase = b;
			break;
		default:
			break;
	}
}

bool PrinterGetUpper(void)
{
	return printNumUpper;
}

void PrinterSetUpper(bool b)
{
	printNumUpper = b? true : false;
}

// Returns: number of chars written
int PrintChar(char c, char *out, int length)
{
	// Validate arguments
	if (!out || length <= 0)
		return 0;

	*out = c;
	return 1;
}

// Returns: number of chars written
int PrintCStr(const char *s, char *out, int len)
{
	// Validate inputs
	if (!s || !out)
		return 0;

	int i;
	for (i = 0; s[i] && i < len; i++)
		out[i] = s[i];

	return i;
}

char ValueToDigit(int d, bool upper)
{
	if (0 <= d && d <= 9)
	{
		return '0' + d;
	}
	else if (10 <= d && d <= 35)
	{
		if (upper)
		{
			return 'A' + d - 10;
		}
		else
		{
			return 'a' + d - 10;
		}
	}
	else
	{
		return '?';
	}
}

// TODO: print out full representation of binary numbers
// Returns: number of chars written
int PrintInt(int n, char *out, int len, int readable, int base, bool upper)
{
	assert(base > 1);
	assert(out);

	// Zero -> special case
	if (len >= 0 && n == 0)
	{
		*out = '0';
		return 1;
	}

	char buf[32];
	const int sz = sizeof(buf);

	// U = magnitude of N
	int u = (n >= 0)? n : -n;

	int i;
	for (i = 0; (u > 0) && (i < len); i++)
	{
		assert(i < sz);
		buf[sz - i - 1] = ValueToDigit(u % base, upper);
		u /= base;
	}

	// Loop should run at least once, even for n == 0
	assert(i >= 1);

	// Sigil for base
	if (readable)
	{
		switch (base)
		{
			case 2:
				buf[sz - i - 1] = '+';
				i++;
				break;
			case 10:
				buf[sz - i - 1] = '#';
				i++;
				break;
			case 16:
				buf[sz - i - 1] = '$';
				i++;
				break;
			case 36:
				break;
			default:
				assert(0 && "not implemented");
		}
	}

	// Minus sign for negative numbers
	if (base != 2 && n < 0)
	{
		assert(i < sz);
		buf[sz - i - 1] = '-';
		i++;
	}

	memcpy(out, buf + sz - i, i);
	return i;
}

/*
int print_list_as_string(const Val *list, char *out, int length, int readable)
{
	// Validate inputs
	if (!(pairp(list) || cell_eq(list, &sym_nil)) || !out || length <= 0)
		return 0;

	char *view = out;

	// Opening quote
	if (readable)
		string_step((const char**) &view, &length, PrintChar('"', view, length));

	// String contents
	const Val *p = list;
	while ((length > 1) && nonempty_listp(p))
	{
		// Get character value in list
		Val *e = p->first;
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
				string_step((const char**) &view, &length, PrintChar('\\', view, length));
				c = esc;
			}
		}

		// Write char
		string_step((const char**) &view, &length, PrintChar(c, view, length));

		// Next list item
		p = p->rest;
	}

	// Closing quote
	if (readable)
		string_step((const char**) &view, &length, PrintChar('"', view, length));

	// Return length, including quotes that were written
	return view - out;
}
*/

int PrintSeq(Seq *list, char *out, int length, int readable)
{
	// Validate arguments
	if (!list || !out || (length <= 0))
		return 0;

	char *view = out;

	// Print opening '['
	view += PrintChar('[', view, length);

	// Print 1st without a space
	if (SeqLength(list) && view < (out+length))
	{
		view += PrintVal(SeqGet(list, 0), view, length-(view-out), readable);
	}

	// Print list contents
	for (int i = 1; i < SeqLength(list) && view < (out+length); i++)
	{
		view += PrintChar(' ', view, length-(view-out));
		view += PrintVal(SeqGet(list, i), view, length-(view-out), readable);
	}

	// Print closing ']'
	view += PrintChar(']', view, length-(view-out));

	return view - out;
}

// Does: Prints p to the given output stream
// Returns: number of chars written
int PrintVal(Val *p, char *out, int length, int readable)
{
	// Validate arguments
	if (length <= 0)
		return 0;

	if (p)
	{
		switch (p->kind)
		{
			case CK_INT:
				return PrintInt(p->integer, out, length, readable, printNumBase, printNumUpper);
			case CK_SEQ:
				return PrintSeq(p->sequence, out, length, readable);
			default:
				assert(0);
		}
	}
	else
	{
		return PrintCStr("(NULL)", out, length);
	}
}
