#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

enum Cell_kind
{
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_PAIR,
	_CELL_KIND_COUNT,
};

typedef struct string String;
struct string
{
	int length;
	char *start;
};

typedef struct cell Cell;
struct cell
{
	enum Cell_kind flag;
	union
	{
		int num;
		String str;
		struct
		{
			Cell * first;
			Cell * rest;
			char variant;
		};
	};
};

// Cell memory:
Cell *cell_pool = NULL;
int cell_pool_cap = 0;

// String memory:
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;
Cell *string_list = NULL;

// (forward declare)
Cell *cell_string (String s);
Cell *cell_pair (Cell *first, Cell *rest);

// How to set up the cell memory
// SHOULD ONLY BE CALLED ONCE
// returns 0 on success
int pools_init (int ncells, int nchars) {
	// Allocate the arrays
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	char_pool = malloc(nchars);
	char_free = char_pool;

	// Check the malloc'd pointers
	if (!cell_pool || !char_pool) {
		fprintf(stderr, "pools_init: malloc failed\n");
		return 1;
	}

	// Set the capacities
	cell_pool_cap = ncells;
	char_pool_cap = nchars;

	// Link the free cells together in a circular list
	for (int i = 0; i < (cell_pool_cap - 1); i++)
	{
		cell_pool[i].rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].rest = cell_pool;

	// Set up the internal string circular list with one item
	Cell *empty_s = cell_string((String) { .start = NULL, .length = 0 });
	string_list = cell_pair(empty_s, NULL);
	string_list->rest = string_list;

	return 0;
}

void string_step (String *s, int n)
{
	s->length -= n;
	s->start += n;
}

void string_skip_white (String *s)
{
	String v = *s;
	while (isspace(*v.start) && v.length > 0)
	{
		v.start++;
		v.length--;
	}
	s->start = v.start;
	s->length = v.length;
}

bool string_validp (String *s)
{
	return (s != NULL) && (s->start != NULL) && (s->length >= 0);
}

// returns a cell whose first = the interned string
const Cell *string_create (String s) {
	// Validate inputs
	if (s.start == NULL || s.length <= 0) {
		return NULL;
	}

	// Check memory space
	if ((char_free - char_pool) >= char_pool_cap) {
		fprintf(stderr, "string_create: out of string character memory\n");
		exit(1);
	}

	// Copy string
	char *res = char_free;
	for (int i = 0; i < s.length; i++) {
		*char_free++ = s.start[i];
	}

	// Add string terminator for easy use with C
	*char_free++ = '\0';

	// Insert the new string cell into the string list and return it
	Cell *x = cell_pair(cell_string((String){ .start = res, .length = s.length}),
				        string_list->rest);
	string_list->rest = x;
	return x;
}

// Removes a cell from the cell pool a.k.a. free list
Cell *cell_get ()
{
	if (!cell_pool)
	{
		return NULL;
	}

	Cell *x = cell_pool->rest;
	if (x)
	{
		cell_pool->rest = x->rest;
	}
	return x;
}

// Inserts a cell back into the free list
void cell_free (Cell *x)
{
	if (!x || !cell_pool)
	{
		return;
	}

	x->rest = cell_pool->rest;
	cell_pool->rest = x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
	{
		_Static_assert(_CELL_KIND_COUNT == 4, "exhaustive handling of all cell kinds");
		switch (k)
		{
			case T_INT:
			case T_SYMBOL:
			case T_STRING:
			case T_PAIR:
				x->flag = k;
				break;
			default:
				// error
				fprintf(stderr, "cell_init: invalid cell kind\n");
				exit(1);
				break;
		}
	}
	return x;
}

Cell *cell_int (int n)
{
	Cell *x = cell_init(T_INT);
	if (x)
	{
		x->num = n;
	}
	return x;
}

Cell *cell_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(T_PAIR);
	if (x)
	{
		x->first = first;
		x->rest = rest;
	}
	return x;
}

const Cell *string_intern (String s);

Cell *cell_symbol (String s)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
	{
		x->str.start = s.start;
		x->str.length = s.length;
	}
	return x;
}

Cell *cell_string (String s)
{
	Cell *x = cell_init(T_STRING);
	if (x)
	{
		x->str.start = s.start;
		x->str.length = s.length;
	}
	return x;
}

// Can compare length encoded string and c-style strings
int stream_eq (const char *s1, const char *s2, int len) {
	// Validate arguments
	if (len < 0) {
		return 0;
	}

	if (len) {
		// Compare length strings
		for (int i = 0; i < len; i++) {
			if (s1[i] != s2[i]) {
				return 0;
			}
		}
		return 1;
	}
	else {
		// Compare c-style string
		while (*s1 && *s2 && *s1 == *s2) {
			s1++;
			s2++;
		}
		return *s1 == *s2;
	}
}

bool string_eq (String s1, String s2)
{
	return (s1.length == s2.length) && stream_eq(s1.start, s2.start, s1.length);
}

int string_find (const char *s, char x) {
	const char *p = s;
	while (*p != x) {
		p++;
	}
	return p - s;
}

String cstring (char *str)
{
	return (String)
	{
		.length = string_find(str, '\0'),
		.start = str,
	};
}

// Use this when creating new strings from short-lived char pointers
const Cell *string_intern (String s)
{
	if (!s.start || s.length < 0)
	{
		return NULL;
	}

	// Linear search through the (circular) string list
	Cell *p = string_list;
	do
	{
		if (string_eq(p->first->str, s))
		{
			// Found an internal string, so return that
			return p;
		}

		p = p->rest;
	}
	while (p != string_list);

	// Did not find an internal string,
	return string_create(s);
}

int read_symbol (String s, Cell **out)
{
	// Get how much of `s` is alphabetical chars
	int i;
	for (i = 0; (i < s.length) && isalpha(s.start[i]); i++)
	{
		continue;
	}

	const Cell *internal_string = string_intern((String){ .start = s.start, .length = i});
	*out = cell_symbol(internal_string->first->str);
	return i;
}

int read_int (String s, Cell **out)
{
	char *p = s.start;
	int len = s.length;

	int n = 0;
	while (isdigit(*p) && len)
	{
		n = (n * 10) + (*p - '0');
		p++;
		len--;
	}
	int num_len = s.length - len;

	*out = cell_int(n);
	return num_len;
}

char char_end (char c)
{
	switch (c)
	{
		case '(': return ')';
		case '[': return ']';
		case '{': return '}';
		case '|': return '|';
		default: return 0;
	}
}

// Modifies the given list and returns the pointer to the new head
void reverse_list (Cell **list)
{
	Cell *p = NULL; // previous
	Cell *c = *list; // current
	Cell *n = NULL; // next

	while (c)
	{
		// Get the next node
		n = c->rest;
		// Set current node's next to the previous
		c->rest = p;
		// Advance down the list
		p = c;
		c = n;
	}

	// Update the reference to the start of the list
	*list = p;
}

// (forward declare)
int read_form (String s, Cell **out);

int read_list (String s, Cell **out)
{
	String view = s;

	// Consume the opening character
	char opener = view.start[0];
	char closer = char_end(opener);
	string_step(&view, 1);

	Cell *e;
	Cell *list, *p;

	// Check if there are no elements
	string_skip_white(&view);
	if (*view.start == closer)
	{
		// empty list
		// consume the final character
		string_step(&view, 1);
		*out = cell_pair(NULL, NULL);
		(*out)->variant = opener;
		int len = s.length - view.length;
		return len;
	}

	// Read the first element
	string_step(&view, read_form(view, &e));
	string_skip_white(&view);

	// Read the rest of the normal elements (don't handle the dot)
	p = list = cell_pair(e, NULL);
	while (view.length && *view.start != closer && *view.start != '.')
	{
		string_step(&view, read_form(view, &e));
		p->rest = cell_pair(e, NULL);
		p = p->rest;
		string_skip_white(&view);
	}

	// Handle either the optionally dotted end of the list
	bool has_dot = false;

	if (*view.start == '.')
	{
		// Dotted end of the list:
		has_dot = true;
		// consume the '.' dot
		string_step(&view, 1);
		// read what should be the final element
		string_step(&view, read_form(view, &e));
		p->rest = e;
		string_skip_white(&view);
	}

	if (*view.start == closer)
	{
		// The actual end of list:
		// consume the final character
		string_step(&view, 1);
		*out = list;
		(*out)->variant = opener;
		int len = s.length - view.length;
		return len;
	}
	else
	{
		// Unexpected end of list, or multiple items after the '.' dot
		if (has_dot)
		{
			fprintf(stderr, "expected the form directly after the '.' (dot) to be the final form of the enclosing list\n");
		}
		else
		{
			fprintf(stderr, "unexpected end of list\n");
		}
		exit(1);
	}
}

int read_form (String s, Cell **out)
{
	char *start = s.start;

	// Skip whitespace
	while (isspace(*s.start))
	{
		s.start++;
		s.length--;
	}

	int offset;
	char c = *s.start;
	switch (c)
	{
		case '(':
		case '{':
		case '[':
		case '|':
			// Opening paren, for lists
			offset = read_list(s, out);
			break;
		case ')':
		case '}':
		case ']':
			// Shouldn't appear in valid text with matched parens
			fprintf(stderr, "read_form: unmatched closing '%c' character\n", c);
			exit(1);
		case '.':
			// Should only be inside when reading a list
			fprintf(stderr, "read_form: unexpected '.' (dot) character\n");
			exit(1);
		case '-':
			// Number
			offset = read_int(s, out);
			break;
		case '\0':
			// Null terminator for strings
			fprintf(stderr, "read_form: unexpected end of string");
			exit(1);
		default:
			if (isalpha(c))
			{
				offset = read_symbol(s, out);
			}
			else if (isdigit(c))
			{
				offset = read_int(s, out);
			}
			else
			{
				fprintf(stderr, "read_form: unexpected character (code = 0x%x)\n", c);
				exit(1);
			}
	}
	return (s.start + offset) - start;
}

int print_char (char c, String out)
{
	if (out.start && (out.length > 0))
	{
		*out.start = c;
		return 1;
	}
	return 0;
}

// returns number of chars written
int print_cstr (char *s, String out)
{
	if (!out.start)
	{
		return 0;
	}

	int i;
	for (i = 0; s[i] && i < out.length; i++)
	{
		out.start[i] = s[i];
	}
	return i;
}

// returns number of chars written
int print_symbol (String sym, String out)
{
	int i;
	for (i = 0; i < sym.length && i < out.length; i++)
	{
		out.start[i] = sym.start[i];
	}
	return i;
}

// returns number of chars written
int print_int (int n, String out)
{
	char buf[20];

	int out_len = out.length;
	int i = sizeof(buf);

	do
	{
		i--;
		buf[i] = '0' + (n % 10);
		n /= 10;
		out_len--;
	}
	while ((out_len > 0) && (n > 0) && (i > 0));

	int len = sizeof(buf) - i;
	memcpy(out.start, buf + i, len);
	return len;
}

// (forward declare)
int print_form (Cell *x, String out);

int print_pair (Cell *x, String out)
{
	String view = out;

	// Remember the type of parens used to create the list (paren is default)
	char opener = (x->variant)? x->variant : '(';
	char closer = char_end(opener);

	string_step(&view, print_char(opener, view));
	while (x != NULL && x->first != NULL)
	{
		string_step(&view, print_form(x->first, view));

		if (x->rest == NULL)
		{
			break;
		}

		// See if the list continues with more pairs...
		if (x->rest->flag == T_PAIR)
		{
			// Step into the 'rest' pair
			if (x->rest->first != NULL)
			{
				string_step(&view, print_cstr(" ", view));
			}
			x = x->rest;
		}
		else
		{
			// Dotted list because the rest of this pair is not a pair
			string_step(&view, print_cstr(" . ", view));
			string_step(&view, print_form(x->rest, view));
			break;
		}
	}
	string_step(&view, print_char(closer, view));

	int len = out.length - view.length;
	return len;
}

// returns number of chars written
int print_form (Cell *x, String out)
{
	if (x == NULL)
	{
		return print_cstr("NULL", out);
	}

	_Static_assert(_CELL_KIND_COUNT == 4, "exhaustive handling of all cell kinds");
	switch (x->flag)
	{
		case T_INT:
			return print_int(x->num, out);
		case T_STRING:
		case T_SYMBOL:
			return print_symbol(x->str, out);
		case T_PAIR:
			return print_pair(x, out);
		default:
			// error
			fprintf(stderr, "cell_print: invalid cell kind\n");
			exit(1);
			break;
	}
}

// DEBUG
void print_intern_strings ()
{
	printf("Interned strings:\n---\n");
	Cell *p = string_list;
	do
	{
		Cell *s = p->first;
		if (s->flag == T_STRING)
		{
			printf("%.*s\n", s->str.length, s->str.start);
		}
		else
		{
			printf("NOT A STRING\n");
		}

		p = p->rest;
	}
	while(p != string_list);
	printf("---\n");
}

Cell *READ ()
{
	char buffer[1000];
	fgets(buffer, sizeof(buffer), stdin);
	String in = (String) { .length = strlen(buffer), .start = buffer };

	Cell *x;

	if (*(in.start) == 'i')
	{
		print_intern_strings();
		x = cell_pair(NULL, NULL);
	}
	else if (*(in.start) == 'q')
	{
		exit(1);
	}
	else
	{
		read_form(in, &x);
	}

	return x;
}

Cell *EVAL (Cell *expr)
{
	return expr;
}

void PRINT (Cell *expr)
{
	char buffer[1000];
	String out = (String) { .length = sizeof(buffer), .start = buffer };

	int p_len = print_form(expr, out);

	printf("%.*s\n", p_len, out.start);
}

void rep ()
{

	Cell * form = READ();
	Cell * value = EVAL(form);
	PRINT(value);
}

int main (int argc, char **argv)
{
	pools_init(1024, 2048);

	while(1)
	{
		printf("user> ");
		rep();
	}
	return 0;
}
