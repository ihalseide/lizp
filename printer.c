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

// Set base to 2, 10, 16, or 36
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
			// Do not set to any other base
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
	if (out && length > 0)
	{
		*out = c;
		return 1;
	}
	else
	{
		return 0;
	}
}

// Returns: number of chars written
int PrintCStr(const char *s, char *out, int len)
{
	// Validate inputs
	if (!s || !out)
	{
		return 0;
	}
	int i;
	for (i = 0; s[i] && i < len; i++)
	{
		out[i] = s[i];
	}
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

// Print a seq/list of numbers as a string.
// String is escaped and quoted if readable is true.
int PrintListAsChars(Seq *p, char *out, int length, int readable)
{
	// Validate inputs
	if (!out || length <= 0)
	{
		return 0;
	}
	char *view = out;
	// Opening quote
	if (readable)
	{
		view += PrintChar('"', view, length-(view-out));
	}
	// String contents
	const int len = SeqLength(p);
	for (int i = 0; i < len && (view-out) < length; i++)
	{
		// Get character value in list
		Val *e = SeqGet(p, i);
		char c;
		if (ValIsInt(e))
		{
			c = (char) e->integer;
		}
		else
		{
			// If an element is not an integer, the string stops printing out
			break;
		}

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

			// Write escaped code
			if (esc)
			{
				// Print a slash and get ready to print the escape char next
				view += PrintChar('\\', view, length-(view-out));
				c = esc;
			}
		}

		// Write char
		view += PrintChar(c, view, length-(view-out));
	}
	// Closing quote
	if (readable)
	{
		view += PrintChar('"', view, length-(view-out));
	}
	// Return length, including quotes that were written
	return view - out;
}

int PrintSeq(Seq *list, char *out, int length, int readable)
{
	// Validate arguments
	if (!out || (length <= 0))
	{
		return 0;
	}
	char *view = out;
	const int seqLen   = SeqLength(list);
	// Print opening '['
	view += PrintChar('[', view, length);
	// Print 1st without a space
	if (seqLen && view < (out+length))
	{
		view += PrintVal(SeqGet(list, 0), view, length-(view-out), readable);
	}
	// Print list contents
	for (int i = 1; i < seqLen && view < (out+length); i++)
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
		return PrintCStr("(none)", out, length);
	}
}

static void ValueToDigitTest(void)
{
	assert(ValueToDigit(1, false) == '1');
	assert(ValueToDigit(1, true) == '1');

	assert(ValueToDigit(9, true) == '9');
	assert(ValueToDigit(9, false) == '9');

	assert(ValueToDigit(10, true) == 'A');
	assert(ValueToDigit(10, false) == 'a');

	assert(ValueToDigit(15, true) == 'F');
	assert(ValueToDigit(15, false) == 'f');

	assert(ValueToDigit(16, true) == 'G');
	assert(ValueToDigit(16, false) == 'g');

	assert(ValueToDigit(35, true) == 'Z');
	assert(ValueToDigit(35, false) == 'z');

	assert(ValueToDigit(36, true) == '?');
	assert(ValueToDigit(36, false) == '?');

	assert(ValueToDigit(-1, true) == '?');
	assert(ValueToDigit(-1, false) == '?');
}

void PrinterTest(void)
{
	ValueToDigitTest();
}

