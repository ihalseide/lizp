#include <ctype.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "reader.h"
#include "sequence.h"
#include "value.h"

int ReadSpace(const char *s, int len)
{
	const char *view = s;
	while ((view - s) < len && isspace(*view))
	{
		view++;
	}
	return view - s;
}

// Digit to integer value
// Returns -1 upon error
int DigitValue(char d)
{
	if ('0' <= d && d <= '9')
	{
		return d - '0';
	}
	else if ('a' <= d && d <= 'z')
	{
		return d - 'a' + 10;
	}
	else if ('A' <= d && d <= 'Z')
	{
		return d - 'A' + 10;
	}
	else
	{
		return -1;
	}
}

// TODO: check for overflow
// Returns the number of characters read
// number read -> out
int ReadInt(const char *start, int length, int *valOut)
{
	// Validate inputs
	if (!start || length <= 0)
	{
		if (valOut)
		{
			*valOut = 0;
		}
		return 0;
	}

	const char *view = start;

	// Read prefix sigil(s)
	bool neg = false;
	int base = 0;
	while (1)
	{
		switch (*view)
		{
			case '#':
				base = 10;
				break;
			case '$':
				base = 16;
				break;
			case '+':
				if (neg)
				{
					// Cannot have negative base-2 numbers
					fprintf(stderr, "Cannot have negative base-2 numbers\n");
					if (valOut)
					{
						*valOut = 0;
					}
					return 0;
				}
				else
				{
					base = 2;
				}
				break;
			case '-':
				neg = !neg;
				view++;
				continue;
			default:
				if (isalnum(*view))
				{
					base = 36;
					view--;
				}
				else
				{
					// Invalid beginning of integer
					printf("Invalid beginning of integer\n");
					if (valOut)
					{
						*valOut = 0;
					}
					return 0;
				}
				break;
		}

		view++;
		break;
	}

	const char *viewDigits = view;

	int n = 0;
	int d;
	while (*view && (view - start < length))
	{
		if (isalnum(*view))
		{
			d = DigitValue(*view);
			if (0 <= d && d < base)
			{
				n = (n * base) + d;
			}
			else
			{
				// Invalid digit for base
				fprintf(stderr, "Invalid digit '%c' for base %d", *view, base);
				if (valOut)
				{
					*valOut = 0;
				}
				return 0;
			}
		}
		else if (*view != '_')
		{
			// Allow underscore to separate digits.
			// All other characters are invalid, so
			// this must be the end of the number.
			break;
		}
		view++;
	}

	// Check if there were any valid digits
	if (view == viewDigits)
	{
		// No valid digits were read after the sigils
		fprintf(stderr, "No valid digits were read after the sigils\n");
		if (valOut)
		{
			*valOut = 0;
		}
		return 0;
	}

	// Apply sign
	if (neg)
	{
		n = -n;
	}

	// Return results
	if (valOut)
	{
		*valOut = n;
	}
	return view - start;
}

// Returns number of chars read
int ReadSeq(const char *start, int length, Seq *toList)
{
	// Validate arguments
	if (!start || length <= 0)
	{
		return 0;
	}

	const char *view = start;

	// Consume the opening paren
	assert(*view == '[');
	view++;

	// Skip whitespace
	view += ReadSpace(view, (start+length)-view);
	if (*view != ']')
	{
		// Non-empty list
		Val *e; // element of list
		while (*view && *view != ']' && view < start+length)
		{
			int len = ReadVal(view, (start+length)-view, &e);
			if (!len)
			{
				// Error reading element
				return 0;
			}
			SeqAppend(toList, e);
			view += len;
		}
	}

	if (*view == ']')
	{
		// Consume the closing paren
		view++;
		return view - start;
	}
	else
	{
		// Reading error
		fprintf(stderr, "error reading list: unexpected end of input");
		return 0;
	}
}

int ReadVal(const char *start, int length, Val **out)
{
	// Validate arguments
	if (!out || !start || length <= 0)
		return 0;

	const char *view = start;

	// Loop is for allowing comments to restart the read
	while (1)
	{
		view += ReadSpace(view, start+length-view);
		switch (*view)
		{
			case '\0':
				// End of input
				*out = NULL;
				break;
			case ']':
				// Unmatched list
				fprintf(stderr, "error unmatched closing ']'\n");
				*out = NULL;
				break;
			case '[':
				// Read sequence / list
				{
					Seq *s = SeqInit(0);
					int len = ReadSeq(view, start+length-view, s);
					if (len)
					{
						view += len;
						*out = ValMakeSeq(s);
					}
					else
					{
						*out = NULL;
					}
				}
				break;
			default:
				// Read integer
				{
					int n;
					int len = ReadInt(view, start+length-view, &n);
					if (len)
					{
						view += len;
						*out = ValMakeInt(n);
					}
					else
					{
						*out = NULL;
					}
				}
				break;
		}
		break;
	}
	view += ReadSpace(view, start+length-view);

	return view - start;
}

/*
// Convert a string to a list of characters
int read_quoted_string (const char *start, int length, Val **out)
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

		Val *string = NULL;
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
		error("unexpected end of input in string literal");
	}
}
*/

void DigitValueTest(void)
{
	printf("<%s>\n", __func__);

	assert(DigitValue('\0') < 0);
	assert(DigitValue('+') < 0);
	assert(DigitValue('[') < 0);
	assert(DigitValue(']') < 0);
	assert(DigitValue('_') < 0);

	assert(DigitValue('0') == 0);
	assert(DigitValue('1') == 1);
	assert(DigitValue('9') == 9);
	assert(DigitValue('a') == 10);
	assert(DigitValue('A') == 10);
	assert(DigitValue('f') == 15);
	assert(DigitValue('F') == 15);
	assert(DigitValue('z') == 35);
	assert(DigitValue('Z') == 35);

	printf("</%s>\n", __func__);
}

void ReadIntTest(void)
{
	printf("<%s>\n", __func__);

	const char *s;
	int n;

	s = "";
	assert(ReadInt(s, strlen(s), &n) == 0);
	assert(n == 0);

	s = "!";
	assert(ReadInt(s, strlen(s), &n) == 0);
	assert(n == 0);

	s = "-";
	assert(ReadInt(s, strlen(s), &n) == 0);
	assert(n == 0);

	s = "+";
	assert(ReadInt(s, strlen(s), &n) == 0);
	assert(n == 0);

	s = "0]";
	assert(ReadInt(s, strlen(s), &n) == 1);
	assert(n == 0);

	s = "0";
	assert(ReadInt(s, strlen(s), &n) == 1);
	assert(n == 0);

	s = "1";
	assert(ReadInt(s, strlen(s), &n) == 1);
	assert(n == 1);

	s = "#1";
	assert(ReadInt(s, strlen(s), &n) == 2);
	assert(n == 1);

	s = "#12";
	assert(ReadInt(s, strlen(s), &n) == 3);
	assert(n == 12);

	s = "10";
	assert(ReadInt(s, strlen(s), &n) == 2);
	assert(n == 36);

	s = "-4";
	assert(ReadInt(s, strlen(s), &n) == 2);
	assert(n == -4);

	s = "$FF";
	assert(ReadInt(s, strlen(s), &n) == 3);
	assert(n == 0xFF);

	s = "$B3";
	assert(ReadInt(s, strlen(s), &n) == 3);
	assert(n == 0xB3);

	// 1011 0011 == $B3
	s = "+1011_0011 ";
	assert(ReadInt(s, strlen(s), &n) == 10);
	assert(n == 0xB3);

	s = "#2_022 ";
	assert(ReadInt(s, strlen(s), &n) == 6);
	assert(n == 2022);

	s = "z ";
	assert(ReadInt(s, strlen(s), &n) == 1);
	assert(n == 35);

	s = "-z";
	assert(ReadInt(s, strlen(s), &n) == 2);
	assert(n == -35);

	s = "-$a";
	assert(ReadInt(s, strlen(s), &n) == 3);
	assert(n == -0xa);

	s = "-#9";
	assert(ReadInt(s, strlen(s), &n) == 3);
	assert(n == -9);

	s = "+1001";
	assert(ReadInt(s, strlen(s), &n) == 5);
	assert(n == 9);

	// This is a failure condition
	s = "-+1001";
	assert(ReadInt(s, strlen(s), &n) == 0);
	assert(n == 0);

	printf("</%s>\n", __func__);
}

void ReadSeqTest(void)
{
	printf("<%s>\n", __func__);
	
	const char *s;
	int len;
	Seq *seq;

	s = "[]";
	seq = SeqInit(0);
	len = ReadSeq(s, strlen(s), seq);
	assert(len == 2);
	assert(SeqLength(seq) == 0);
	SeqFree(seq);

	s = "[ ]";
	seq = SeqInit(0);
	len = ReadSeq(s, strlen(s), seq);
	assert(len == 3);
	assert(SeqLength(seq) == 0);
	SeqFree(seq);

	s = "[0]";
	seq = SeqInit(0);
	len = ReadSeq(s, strlen(s), seq);
	assert(len == 3);
	assert(SeqLength(seq) == 1);
	assert(((Val*)SeqGet(seq, 0))->integer == 0);
	SeqFree(seq);

	s = "[1 2 3]";
	seq = SeqInit(0);
	len = ReadSeq(s, strlen(s), seq);
	assert(len == 7);
	assert(SeqLength(seq) == 3);
	assert(((Val*)SeqGet(seq, 0))->integer == 1);
	assert(((Val*)SeqGet(seq, 1))->integer == 2);
	assert(((Val*)SeqGet(seq, 2))->integer == 3);
	SeqFree(seq);

	s = "[ 1 2 3 ]";
	seq = SeqInit(0);
	len = ReadSeq(s, strlen(s), seq);
	assert(len == 9);
	assert(SeqLength(seq) == 3);
	assert(((Val*)SeqGet(seq, 0))->integer == 1);
	assert(((Val*)SeqGet(seq, 1))->integer == 2);
	assert(((Val*)SeqGet(seq, 2))->integer == 3);
	SeqFree(seq);

	printf("</%s>\n", __func__);
}

void ReadValTest(void)
{
	printf("<%s>\n", __func__);

	const char *s;
	int len;
	Val *v;

	v = NULL;
	s = " 5";
	len = ReadVal(s, strlen(s), &v);
	assert(len == 2);
	assert(ValIsInt(v));
	assert(v->integer == 5);
	ValFree(v);

	v = NULL;
	s = "[[1]2 3]";
	len = ReadVal(s, strlen(s), &v);
	assert(len == 8);
	assert(ValIsSeq(v));
	assert(SeqLength(v->sequence) == 3);
	assert(ValIsSeq(SeqGet(v->sequence, 0)));
	assert(ValIsInt(SeqGet(v->sequence, 1)));
	assert(ValIsInt(SeqGet(v->sequence, 2)));
	assert(SeqLength(((Val*)SeqGet(v->sequence, 0))->sequence) == 1);
	SeqFree(((Val*)SeqGet(v->sequence, 0))->sequence);
	ValFree(SeqGet(v->sequence, 0));
	SeqFree(v->sequence);
	ValFree(v);

	printf("</%s>\n", __func__);
}

void ReaderTest(void)
{
	printf("<%s>\n", __func__);

	DigitValueTest();
	ReadIntTest();
	ReadSeqTest();
	ReadValTest();

	printf("</%s>\n", __func__);
}

