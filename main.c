#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <ctype.h>

#include "types.h"
#include "main.h"

// Cell memory:
Cell *cell_pool = NULL;
int cell_pool_cap = 0;

// String memory:
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;
Cell *string_list = NULL;

// Constant symbols
Cell * c_nil;
Cell * c_true;
Cell * c_false;

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

bool string_eq (String s1, String s2)
{
	return (s1.length == s2.length) && stream_eq(s1.start, s2.start, s1.length);
}

// An environment is a list starting with the outer environment,
// and then the rest of the list is a list of pairs which represent
// the defined symbols.
// Each pair is of the form (symbol . value).
Cell *env_create (Cell *env_outer)
{
	return make_pair(
			env_outer,
			make_pair(NULL, NULL));
}

// Search only the current env for the symbol
Cell *env_get_self (Cell *env, const Cell *sym)
{
	Cell *p = env->rest;
	while (p != NULL && p->first != NULL)
	{
		Cell *slot = p->first;
		if (string_eq(slot->first->str, sym->str))
		{
			return slot;
		}
		p = p->rest;
	}
	return NULL;
}

// Find the innermost env which contains symbol
Cell *env_find (Cell *env, const Cell *sym)
{
	while (env)
	{
		Cell *slot = env_get_self(env, sym);
		if (slot)
		{
			return env;
		}

		// Note: the first slot of an env
		// is its outer env.
		env = env->first;
	}
	return NULL;
}

// Get the innermost definition for the symbol
Cell *env_get (Cell *env, Cell *sym)
{
	// Validate inputs
	if ((env == NULL) || (sym == NULL))
	{
		return NULL;
	}

	Cell *containing_env = env_find(env, sym);
	if (containing_env != NULL)
	{
		Cell *slot = env_get_self(containing_env, sym);
		return slot->rest;
	}

	// Symbol not found
	return NULL;
}

void env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs. Val is allowed to be null to "undefine" things.
	if ((env == NULL) || (sym == NULL))
	{
		return;
	}

	Cell *slot = env_get_self(env, sym);
	if (slot != NULL)
	{
		// Change the value of the already present slot
		slot->rest = val;
	}
	else
	{
		// Push the new (symbol . value) pair to the env
		env->rest = make_pair(
				make_pair(sym, val),
				env->rest);
	}
}

void env_set_c (Cell *env, char *cstr, Cell *val)
{
	if ((env == NULL) || (cstr == NULL) || (val == NULL))
	{
		return;
	}

	Cell *sym = make_symbol(string_intern(cstring(cstr))->str);
	env_set(env, sym, val);
}


int string_find (const char *s, char x)
{
	const char *p = s;
	while (*p != x)
	{
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

// How to set up the cell memory
// SHOULD ONLY BE CALLED ONCE
void pools_init (int ncells, int nchars)
{
	// Allocate the arrays
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	char_pool = malloc(nchars);
	char_free = char_pool;

	// Check the malloc'd pointers
	if (!cell_pool || !char_pool)
	{
		fprintf(stderr, "pools_init: malloc failed\n");
		exit(1);
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
	Cell *empty_s = make_string((String) { .start = NULL, .length = 0 });
	string_list = make_pair(empty_s, NULL);
	string_list->rest = string_list;

	// Create the constant symbols
	c_nil = make_symbol(string_intern(cstring("#nil"))->str);
	c_true = make_symbol(string_intern(cstring("#true"))->str);
	c_false = make_symbol(string_intern(cstring("#false"))->str);
}

// only for use by string_intern
Cell *string_create (String s)
{
	// Validate inputs
	if (s.start == NULL || s.length <= 0)
	{
		return NULL;
	}

	// Check memory space
	if ((char_free - char_pool) >= char_pool_cap)
	{
		fprintf(stderr, "string_create: out of string character memory\n");
		exit(1);
	}

	// Copy string
	char *res = char_free;
	for (int i = 0; i < s.length; i++)
	{
		*char_free++ = s.start[i];
	}

	// Add string terminator for easy use with C
	*char_free++ = '\0';

	// Create a (string-cell . next) pair that is inside the string list
	Cell *x = make_pair(make_string((String){ .start = res, .length = s.length}),
				        string_list->rest);
	string_list->rest = x;

	// Return the string cell
	return x->first;
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

Cell *make_int (int n)
{
	Cell *x = cell_init(T_INT);
	if (x)
	{
		x->num = n;
	}
	return x;
}

Cell *make_cfunc (Cell *(*c_func)(Cell*))
{
	Cell *x = cell_init(T_C_FUNCTION);
	if (x)
	{
		x ->c_func = c_func;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(T_PAIR);
	if (x)
	{
		x->first = first;
		x->rest = rest;
	}
	return x;
}

Cell *make_symbol (String s)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
	{
		x->str.start = s.start;
		x->str.length = s.length;
	}
	return x;
}

Cell *make_string (String s)
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
int stream_eq (const char *s1, const char *s2, int len)
{
	// Validate arguments
	if ((s1 == NULL) || (s2 == NULL) || (len < 0))
	{
		return 0;
	}

	if (len)
	{
		// Compare length strings
		for (int i = 0; i < len; i++)
		{
			if (s1[i] != s2[i])
			{
				return 0;
			}
		}
		return 1;
	}
	else
	{
		// Compare c-style string
		while (*s1 && *s2 && *s1 == *s2)
		{
			s1++;
			s2++;
		}
		return *s1 == *s2;
	}
}

// Use this when creating new strings from short-lived char pointers
// returns a string cell
Cell *string_intern (String s)
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
			return p->first;
		}

		p = p->rest;
	}
	while (p != string_list);

	// Did not find an internal string,
	return string_create(s);
}

char isparen (char c)
{
	return ((c == '(')
			|| (c == ')')
			|| (c == '[')
			|| (c == ']')
			|| (c == '{')
			|| (c == '}')
			|| (c == '|'));
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

int is_symbol_char (char c)
{
	return !isparen(c) && !isspace(c);
}

int read_symbol (String s, Cell **out)
{
	// Get how much of `s` is alphabetical chars
	int i;
	for (i = 0; (i < s.length) && is_symbol_char(s.start[i]); i++)
	{
		continue;
	}

	Cell *str = string_intern((String){ .start = s.start, .length = i});
	return i;
}

int read_int (String s, Cell **out)
{
	String view = s;

	int sign = 1;
	if (*view.start == '-')
	{
		sign = -sign;
		string_step(&view, 1);
	}

	int n = 0;
	while (isdigit(*view.start) && view.length)
	{
		n = (n * 10) + ((*view.start) - '0');
		string_step(&view, 1);
	}

	int num_len = s.length - view.length;
	*out = make_int(n * sign);
	return num_len;
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
		// (empty list = pair where first = NULL)
		// consume the final character
		string_step(&view, 1);
		*out = make_pair(NULL, c_nil);
		(*out)->variant = opener;
		int len = s.length - view.length;
		return len;
	}

	// Read the first element
	string_step(&view, read_form(view, &e));
	string_skip_white(&view);

	// Read the rest of the normal elements (don't handle the dot)
	p = list = make_pair(e, c_nil);
	while ((view.length >= 0) && (*view.start != closer) && (*view.start != '.'))
	{
		string_step(&view, read_form(view, &e));
		p->rest = make_pair(e, c_nil);
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
	String view = s;
	string_skip_white(&view);
	switch (*view.start)
	{
		case '(':
		case '{':
		case '[':
		case '|':
			// Opening paren, for lists
			string_step(&view, read_list(view, out));
			break;
		case ')':
		case '}':
		case ']':
			// Shouldn't appear in valid text with matched parens
			fprintf(stderr, "read_form: unmatched closing '%c' character\n", *view.start);
			exit(1);
		case '.':
			// Should only be inside when reading a list
			fprintf(stderr, "read_form: unexpected '.' (dot) character\n");
			exit(1);
		case '-':
			// Number
			string_step(&view, read_int(view, out));
			break;
		case '\0':
			// Null terminator for strings
			fprintf(stderr, "read_form: unexpected end of string");
			exit(1);
		default:
			if (isdigit(*view.start))
			{
				string_step(&view, read_int(view, out));
			}
			else
			{
				string_step(&view, read_symbol(view, out));
			}
	}
	return view.start - s.start;
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
	int u = (n >= 0)? n : -n;

	// Remaining length
	int rem = out.length;

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
	memcpy(out.start, buf + i, len);
	return len;
}

int print_pair (Cell *x, String out)
{
	String view = out;

	// Remember the type of parens used to create the list (paren is default)
	char opener = (x->variant)? x->variant : '(';
	char closer = char_end(opener);

	string_step(&view, print_char(opener, view));
	if (x->first != NULL)
	{
		while (x != c_nil)
		{
			string_step(&view, print_form(x->first, view));

			if (x->rest == c_nil)
			{
				break;
			}

			// See if the list continues with more pairs...
			if (x->rest->kind == T_PAIR)
			{
				// Step into the 'rest' pair
				string_step(&view, print_cstr(" ", view));
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
	}
	string_step(&view, print_char(closer, view));

	int len = out.length - view.length;
	return len;
}

// returns number of chars written
int print_form (Cell *x, String out)
{
	if ((x == NULL) || (out.length == 0))
	{
		return 0;
	}

	_Static_assert(_CELL_KIND_COUNT == 5, "exhaustive handling of all cell kinds");
	switch (x->kind)
	{
		case T_INT:
			return print_int(x->num, out);
		case T_STRING:
		case T_SYMBOL:
			return print_symbol(x->str, out);
		case T_C_FUNCTION:
			return print_cstr("#<built-in function>", out);
		case T_PAIR:
			return print_pair(x, out);
		default:
			// error
			fprintf(stderr, "cell_print: invalid cell kind\n");
			exit(1);
			break;
	}
}

void PRINT (Cell *x);

Cell *READ ()
{
	char buffer[1000];
	if(!fgets(buffer, sizeof(buffer), stdin))
	{
		fprintf(stderr, "READ: fgets failed\n");
		exit(1);
	}
	String in = (String) { .length = strlen(buffer), .start = buffer };

	Cell *x;
	read_form(in, &x);

	return x;
}

// Evaluate each item of list x
// does not modify x
Cell *eval_list (Cell *x, Cell *env)
{
	Cell *y = make_pair(NULL, c_nil);
	Cell *p_y = y;
	Cell *p_x = x;

	// Turn x into y by evaluating each item of x:

	// eval the first element
	p_y->first = EVAL(p_x->first, env);
	p_x = p_x->rest;

	// eval the rest of the elements
	while (p_x != c_nil && p_x->first)
	{
		p_y->rest = make_pair(EVAL(p_x->first, env), c_nil);

		// next
		p_x = p_x->rest;
		p_y = p_y->rest;
	}

	// Copy the variant kind
	y->variant = x->variant;

	return y;
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

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
	{
		_Static_assert(_CELL_KIND_COUNT == 5, "handle all cell kinds");
		switch (k)
		{
			case T_INT:
			case T_STRING:
			case T_SYMBOL:
			case T_C_FUNCTION:
			case T_PAIR:
				x->kind = k;
				break;
			default:
				fprintf(stderr, "cell_init: invalid cell kind\n");
				exit(1);
		}
	}
	return x;
}


Cell *symbol_lookup (Cell *x, Cell *env)
{
	Cell *val = env_get(env, x);
	if (val)
	{
		return val;
	}
	else
	{
		return make_string(string_intern(cstring("<undefined>"))->str);
	}
}

bool is_self_evaluating (Cell* x)
{
	// Validate inputs
	if (x == NULL)
	{
		return false;
	}

	return ((x == c_nil)
			|| (x == c_true)
			|| (x == c_false)
			|| (x->kind == T_INT)
			|| (x->kind == T_STRING)
			// Empty list
			|| ((x->kind == T_PAIR) && x->first == NULL));
}

Cell *EVAL (Cell *x, Cell *env)
{
	if (is_self_evaluating(x))
	{
		printf("  is self-evaluating...");
		return x;
	}

	switch (x->kind)
	{
		case T_SYMBOL:
			printf("  lookup symbol...");
			return symbol_lookup(x, env);
		case T_PAIR:
			if (x->first == NULL)
			{
				// Is self-evaluating
				assert(0);
			}

			// List application
			Cell *y = eval_list(x, env);
			// TODO: call y[0] with args y[1...]
			return y;
		case T_INT:
		case T_STRING:
			// Are self-evaluating
			assert(0);
		default:
			// error
			fprintf(stderr, "cell_init: invalid cell kind\n");
			exit(1);
	}
}

void PRINT (Cell *expr)
{
	char buffer[1000];
	String out = (String) { .length = sizeof(buffer), .start = buffer };

	int p_len = print_form(expr, out);

	printf("%.*s\n", p_len, out.start);
}

void rep (Cell *env)
{
	Cell * form = READ();
	Cell * value = EVAL(form, env);
	PRINT(value);
}

// Built-in functions:

Cell *bi_plus (Cell *args)
{
	int a = args->first->num;
	int b = args->rest->first->num;
	return make_int(a + b);
}

Cell *bi_minus (Cell *args)
{
	int a = args->first->num;
	int b = args->rest->first->num;
	return make_int(a - b);
}

Cell *bi_star (Cell *args)
{
	int a = args->first->num;
	int b = args->rest->first->num;
	return make_int(a * b);
}

Cell *bi_slash (Cell *args)
{
	int a = args->first->num;
	int b = args->rest->first->num;
	return make_int(a / b);
}

int main (int argc, char **argv)
{
	// Initialize memories
	pools_init(1024, 2048);

	// Initialize the REPL environment
	Cell *repl_env = env_create(NULL);
	env_set_c(repl_env, "+", make_cfunc(bi_plus));
	env_set_c(repl_env, "-", make_cfunc(bi_minus));
	env_set_c(repl_env, "*", make_cfunc(bi_star));
	env_set_c(repl_env, "/", make_cfunc(bi_slash));
	env_set_c(repl_env, "one", make_int(1));

	while(1)
	{
		printf("user> ");
		rep(repl_env);
	}
	return 0;
}
