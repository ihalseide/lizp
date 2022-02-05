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
	T_FUNC,
	T_NATIVE_FUNC,
	T_PAIR,
	_CELL_KIND_COUNT,
};

enum Special_form
{
	SF_DEF_BANG,
	SF_LET_STAR,
	SF_IF,
	SF_DO,
	SF_FN_STAR,
	_SPECIAL_FORM_COUNT,
};

// Pairs & Lists:
// Pair = (first . rest)
// List = () = (NULL . NIL)
//      | (cell . List)
//      | (cell . NIL)


// Note on Cell pointers
// By convention, the only time NULL should ever appear is in 2 cases:
//   1. in the first slot of an empty list
//   2. when a symbol's value is undefined in an environment
//   3. cell returned by cell_get(), since it must be a pointer from the
//      cell pool and thus cannot be nil
typedef struct cell Cell;
struct cell
{
	enum Cell_kind kind;           // Type of cell
	union
	{
		int as_int;                // Integer value
		Cell *(*as_c_func)(Cell*); // C function value
		Cell * as_symbol;          // Symbol name value (pointer to string cell)
		struct                     // String value
		{
			const char *start;
			int length;
		} as_str;
		struct                     // Very similar to pair, could be combined,
		{                          // but it adds clarity.
			Cell *params;
			Cell *body;
		} as_func;
		struct                     // Pair/list value
		{
			Cell * first;
			Cell * rest;
			char variant;          // Type of parens used to write the list
		} as_pair;
	};
};


// Cell memory (holds the actual cell values):
Cell *cell_pool = NULL;
int cell_pool_cap = 0;

// String memory (holds the actual characters):
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;

// Strings interned
Cell *string_list = NULL;

// Symbols interned
Cell *symbol_list = NULL;

// Cell constant values (symbols)
Cell c_nil;
Cell c_true;
Cell c_false;

// Special form constant cells (symbols)
Cell c_sf_def_bang;
Cell c_sf_let_star;
Cell c_sf_fn_star;
Cell c_sf_do;
Cell c_sf_if;

// Forward-declare
void PRINT (Cell *x);

char char_end (char c)
{
	switch (c)
	{
		case '(': return ')';
		case '[': return ']';
		case '{': return '}';
		case '|': return '|';
		case '"': return '"';
		default: return '\0';
	}
}

char char_is_paren (char c)
{
	return (c == '(') || (c == ')')
		|| (c == '[') || (c == ']')
		|| (c == '{') || (c == '}')
		|| (c == '|');
}

int char_is_symbol (char c)
{
	return !char_is_paren(c) && !isspace(c);
}

// Checks two character streams for equality.
// If len == 0, then this is equivalent to strcmp(s1,s2) == 0
// Otherwise, this only returns true if s1 and s2 are equal up to
// the `len`th character.
bool stream_eq (const char *s1, int len1, const char *s2, int len2)
{
	// Validate arguments
	if ((s1 == NULL) || (s2 == NULL) || (len1 < 0) || (len2 < 0))
	{
		return false;
	}

	if (len1 != len2)
	{
		return false;
	}

	if (s1 == s2)
	{
		return true;
	}

	// Compare length strings
	for (int i = 0; i < len1; i++)
	{
		if (s1[i] != s2[i])
		{
			return false;
		}
	}

	return true;
}

// Removes a cell from the cell pool a.k.a. free list
// Can return NULL
Cell *cell_get ()
{
	if (!cell_pool)
	{
		return NULL;
	}

	Cell *x = cell_pool->as_pair.rest;
	if (x)
	{
		cell_pool->as_pair.rest = x->as_pair.rest;
	}
	return x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
	{
		_Static_assert(_CELL_KIND_COUNT == 6, "handle all cell kinds");
		switch (k)
		{
			case T_INT:
			case T_STRING:
			case T_SYMBOL:
			case T_FUNC:
			case T_NATIVE_FUNC:
			case T_PAIR:
				x->kind = k;
				break;
			default:
				fprintf(stderr, "cell_init : error : invalid cell kind\n");
				exit(1);
		}
	}
	return x;
}

Cell *make_fn (Cell *args, Cell *body)
{
	Cell *x = cell_init(T_FUNC);
	if (x)
	{
		x->as_func.params = args;
		x->as_func.body = body;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(T_PAIR);
	if (x)
	{
		x->as_pair.first = first;
		x->as_pair.rest = rest;
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, &c_nil);
}

bool is_list (Cell *x)
{
	return x && (x->kind == T_PAIR);
}

bool is_empty_list (Cell *x)
{
	return is_list(x) && (x->as_pair.first == NULL);
}

int list_length (Cell *list)
{
	assert(is_list(list));

	int i;
	for (i = 0; (list != &c_nil) && !is_empty_list(list); i++)
	{
		list = list->as_pair.rest;
	}

	return i;
}

// Define what cell values count as True or False
bool truthy (Cell *x)
{
	return ((x != NULL)
			&& (x != &c_nil)
			&& (x != &c_false));
}

Cell *make_symbol (Cell *name)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
	{
		x->as_symbol = name;
	}
	return x;
}

Cell *make_string (const char *start, int length)
{
	Cell *x = cell_init(T_STRING);
	if (x)
	{
		x->as_str.start = start;
		x->as_str.length = length;
	}
	return x;
}

// Insert a new string into the string list and return it
Cell *string_add (const char *start, int length)
{
	Cell *new_string = make_string(start, length);
	Cell *x = make_pair(new_string, string_list->as_pair.rest);
	string_list->as_pair.rest = x;
	return new_string;
}

// Helper function for use by string_intern and string_intern_cstring.
Cell *string_create (const char *start, int length)
{
	// Validate inputs
	if (start == NULL || length <= 0)
	{
		return &c_nil;
	}

	// Check memory space
	if ((char_free - char_pool) >= char_pool_cap)
	{
		fprintf(stderr, "string_create: out of string character memory\n");
		exit(1);
	}

	// Copy string
	char *res = char_free;
	for (int i = 0; i < length; i++)
	{
		*char_free++ = start[i];
	}

	// Add string terminator for easy use with C
	// (Even though every string is length encoded,
	//  it's a hassle to add a null terminator if it isn't already there)
	*char_free++ = '\0';

	return string_add(res, length);
}

// Use this when creating new strings from short-lived char pointers
// returns a string cell
Cell *string_intern (const char *start, int length)
{
	if ((start == NULL) || (length < 0))
	{
		return &c_nil;
	}

	// Length 0 => empty string
	if (length == 0)
	{
		return string_list->as_pair.first;
	}

	// Linear search through the string list
	Cell *p = string_list;
	while (p != &c_nil)
	{
		Cell *s = p->as_pair.first;
		if (stream_eq(s->as_str.start, s->as_str.length, start, length))
		{
			// Found an internal string, so return that
			return s;
		}

		// Next
		p = p->as_pair.rest;
	}

	// Did not find an internal string, so make new one
	return string_create(start, length);
}

// For interning literal constant C-strings in this source code
Cell *string_intern_cstring (const char *str)
{
	assert(str != NULL);
	return string_intern(str, strlen(str));
}

// Get a canonical symbol with a given name
// Name should be an canonical interned string!
Cell *symbol_intern (Cell *name)
{
	assert(name->kind == T_STRING);
	
	// Search through the circular symbol list
	Cell *p = symbol_list;
	while (p != &c_nil)
	{
		if (p->as_pair.first->as_symbol == name)
		{
			// Found an internal string, so return that
			return p->as_pair.first;
		}

		// Next
		p = p->as_pair.rest;
	}

	// Not found internally, so create new symbol
	// add to list
	Cell *sym_node = make_pair(make_symbol(name), symbol_list->as_pair.rest);
	symbol_list->as_pair.rest = sym_node;
	return sym_node->as_pair.first;
}

// Inserts a cell back into the free list
void cell_free (Cell *x)
{
	if ((x == NULL) || (cell_pool == NULL))
	{
		return;
	}

	x->as_pair.rest = cell_pool->as_pair.rest;
	cell_pool->as_pair.rest = x;
}

void cell_free_all (Cell *x)
{
	if ((x == NULL))
	{
		return;
	}

	if (x->kind == T_PAIR)
	{
		cell_free_all(x->as_pair.first);
		cell_free_all(x->as_pair.rest);
	}

	cell_free(x);
}

Cell *make_int (int n)
{
	Cell *x = cell_init(T_INT);
	if (x)
	{
		x->as_int = n;
	}
	return x;
}

Cell *make_cfunc (Cell *(*c_func)(Cell*))
{
	Cell *x = cell_init(T_NATIVE_FUNC);
	if (x)
	{
		x ->as_c_func = c_func;
	}
	return x;
}

void string_skip_white(const char **stream, int *length)
{
	if (!stream)
	{
		return;
	}

	const char *view = *stream;
	int rem = *length;
	while (isspace(*view) && (rem > 0))
	{
		view++;
		rem--;
	}
	*stream = view;
	*length = rem;
}

void string_step (const char **stream, int *length, int n)
{
	*stream += n;
	*length -= n;
}

bool symbol_eq (const Cell *s1, const Cell *s2)
{
	// Validate inputs
	if (!s1 || !s2
			|| (s1->kind != T_SYMBOL) || (s2->kind != T_SYMBOL)
			|| !s1->as_symbol || !s2->as_symbol)
	{
		return false;
	}

	return (s1 == s2)
		|| (s1->as_symbol == s2->as_symbol)
		|| stream_eq(s1->as_symbol->as_str.start, s1->as_symbol->as_str.length,
				s2->as_symbol->as_str.start, s2->as_symbol->as_str.length);
}

int read_symbol (const char *start, int length, Cell **out)
{
	// Validate inputs
	if ((start == NULL) || out == NULL)
	{
		return 0;
	}

	// Read up to the next non-symbol character
	int i;
	for (i = 0; (i < length) && char_is_symbol(start[i]); i++)
	{
		continue;
	}

	*out = symbol_intern(string_intern(start, i));
	return i;
}

// Read a quoted string
int read_string (const char *start, int length, Cell **out)
{
	// Validate inputs
	if ((start == NULL) || out == NULL)
	{
		return 0;
	}

	const char *p = start;
	int rem = length;

	// Consume first quote
	string_step(&p, &rem, 1);

	// Read chars until next quote
	while ((rem > 0) && (*p != '"'))
	{
		string_step(&p, &rem, 1);
	}
	// Consume the last quote
	if (*p == '"')
	{
		string_step(&p, &rem, 1);
	}

	int len = length - rem;
	// Don't include the quotes
	*out = string_intern(start + 1, len - 2);
	return len;
}

int read_int (const char *start, int length, Cell **out)
{
	// Validate inputs
	if ((start == NULL) || out == NULL)
	{
		return 0;
	}

	int start_length = length;

	int sign = 1;
	if (*start == '-')
	{
		sign = -sign;
		start++;
		length--;
	}

	int n = 0;
	while (isdigit(*start) && (length > 0))
	{
		n = (n * 10) + ((*start) - '0');
		start++;
		length--;
	}

	int num_len = start_length - length;
	*out = make_int(n * sign);
	return num_len;
}

int read_str (const char *start, int length, Cell **out);

int read_list (const char *start, int length, Cell **out)
{
	// Validate inputs
	if ((start == NULL) || out == NULL)
	{
		return 0;
	}

	const char *view = start;
	int rem = length;

	// Consume the opening character
	char opener = *view;
	char closer = char_end(opener);
	string_step(&view, &rem, 1);

	// Check if there are no elements
	string_skip_white(&view, &rem);
	if (*view == closer)
	{
		// empty list
		// (empty list = pair where first = NULL)
		// consume the final character
		string_step(&view, &rem, 1);

		*out = make_empty_list();
		(*out)->as_pair.variant = opener;

		int len = view - start;
		return len;
	}

	// Read the first element
	Cell *e;
	string_step(&view, &rem, read_str(view, rem, &e));
	string_skip_white(&view, &rem);

	// Read the rest of the normal elements (don't handle the dot)
	Cell *list, *p;
	p = list = make_pair(e, &c_nil);
	while ((rem > 0) && (*view != closer) && (*view != '.'))
	{
		string_step(&view, &rem, read_str(view, rem, &e));
		p->as_pair.rest = make_pair(e, &c_nil);
		p = p->as_pair.rest;
		string_skip_white(&view, &rem);
	}

	// Handle either the optionally dotted end of the list
	bool has_dot = false;

	if (*view == '.')
	{
		// Dotted end of the list:
		has_dot = true;
		// consume the '.' dot
		string_step(&view, &rem, 1);
		// read what should be the final element
		string_step(&view, &rem, read_str(view, rem, &e));
		p->as_pair.rest = e;
		string_skip_white(&view, &rem);
	}

	if (*view == closer)
	{
		// The actual end of list:
		// consume the final character
		string_step(&view, &rem, 1);
		*out = list;
		(*out)->as_pair.variant = opener;
		int len = length - rem;
		return len;
	}
	else
	{
		// Unexpected end of list, or multiple items after the '.' dot
		if (has_dot)
		{
			*out = string_intern_cstring("read_list : error : expected the form directly after the '.' (dot) to be the final form of the enclosing list");
			return (*out)->as_str.length;
		}
		else
		{
			*out = string_intern_cstring("read_list : error : unexpected end of list");
			return (*out)->as_str.length;
		}
	}
}

// Env = (alist . outer)
Cell *env_create (Cell *env_outer)
{
	assert(env_outer);
	return make_pair(make_empty_list(), env_outer);
}

// Find a symbol in an alist.
// ( an alist is a list of the form ((symbol . value) (symbol . value) etc.) )
Cell *alist_assoc (const Cell *sym, Cell *alist)
{
	// Validate inputs
	if (!sym || !alist || (sym == &c_nil) || (sym->kind != T_SYMBOL) || (alist->kind != T_PAIR))
	{
		return &c_nil;
	}

	// Iterate through the list
	while (!is_empty_list(alist) && (alist != &c_nil))
	{
		if (alist->as_pair.first->as_pair.first == sym)
		{
			// Found a a slot with the symbol, return the slot
			return alist->as_pair.first;
		}

		// Next
		alist = alist->as_pair.rest;
	}

	return &c_nil;
}

// Search only the current env for the symbol
// Returns:
//   when found -> the (symbol . value) "slot"
//   not found -> nil
Cell *env_get_self (Cell *env, const Cell *sym)
{
	// Search through the slots alist in the environment
	return alist_assoc(sym, env->as_pair.first);
}

// Find the innermost env which contains symbol
// Returns:
//   if found -> environment Cell 
//   not found -> nil
Cell *env_find (Cell *env, const Cell *sym)
{
	// Validate inputs
	if ((env == NULL) || (env == &c_nil) || (env->kind != T_PAIR)
			|| (sym == NULL) || (sym->kind != T_SYMBOL))
	{
		return &c_nil;
	}

	// Search up the environment hierarchy
	while (is_list(env) && !is_empty_list(env))
	{
		Cell *slot = env_get_self(env, sym);
		if (slot != &c_nil)
		{
			return env;
		}

		// Move on to the outer environment
		env = env->as_pair.rest;
	}

	// Not found
	return &c_nil;
}

// Get the innermost definition for the symbol.
// Returns:
//   when found -> the slot containing (symbol . value)
//   when not found -> nil
Cell *env_get (Cell *env, Cell *sym)
{
	// Validate inputs
	if (!env || (env == &c_nil) || (env->kind != T_PAIR)
			|| !sym || (sym->kind != T_SYMBOL))
	{
		return &c_nil;
	}

	// Find the environment which contains the symbol
	Cell *containing_env = env_find(env, sym);
	if (containing_env == &c_nil)
	{
		// Symbol not found
		return &c_nil;
	}
	else
	{
		// Fetch the symbol from the environment it's in
		return env_get_self(containing_env, sym);
	}
}

// Push an item to the front of a list
// Returns a new list and does not modify the original
Cell *list_push (Cell *item, Cell *list)
{
	// Validate inputs
	if (list->kind != T_PAIR)
	{
		return &c_nil;
	}

	if (is_empty_list(list))
	{
		list->as_pair.first = item;
	}
	else
	{
		list = make_pair(item, list);
	}
	return list;
}

void env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	// Val can be nil and of any type.
	if (!env || (env == &c_nil) || (env->kind != T_PAIR)
			|| !sym || (sym == &c_nil) || (sym->kind != T_SYMBOL)
			|| !val)
	{
		return;
	}

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = env_get_self(env, sym);
	if (slot == &c_nil)
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		env->as_pair.first = list_push(make_pair(sym, val), env->as_pair.first);
	}
	else
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->as_pair.rest = val;
	}
}

void env_set_c (Cell *env, const char *cstr, Cell *val)
{
	if (!env || !cstr || !val || (env == &c_nil))
	{
		return;
	}
	Cell *name = string_intern_cstring(cstr);
	Cell *sym = symbol_intern(name);
	env_set(env, sym, val);
}

// Read a form from an input stream/string
// Returns: the number of characters read
int read_str (const char *start, int length, Cell **out)
{
	const char *view = start;
	int rem = length;

	string_skip_white(&view, &rem);

	switch (*view)
	{
		case '"':
			// Read quoted string
			string_step(&view, &rem, read_string(view, rem, out));
			break;
		case '(':
		case '{':
		case '[':
		case '|':
			// Opening paren, for lists
			string_step(&view, &rem, read_list(view, rem, out));
			break;
		case ')':
		case '}':
		case ']':
			// Closing paren, shouldn't appear in valid text with matched parens
			*out = string_intern_cstring("read_str : error : unmatched list closing character");
			return (*out)->as_str.length;
		case '.':
			// Should only be inside when reading a list
			*out = string_intern_cstring("read_str : error : unexpected '.' (dot) character");
			return (*out)->as_str.length;
		case '\0':
			// Null terminator for strings
			*out = string_intern_cstring("read_str : error : unexpected end of input string");
			return (*out)->as_str.length;
		case '-':
			// Symbol or Number?
			if ((rem > 0) && isdigit(*(view + 1)))
			{
				// Read number because next char is a digit
				string_step(&view, &rem, read_int(view, rem, out));
			}
			else
			{
				// Otherwise read it as a symbol
				string_step(&view, &rem, read_symbol(view, rem, out));
			}
			break;
		default:
			if (isdigit(*view))
			{
				string_step(&view, &rem, read_int(view, rem, out));
			}
			else
			{
				string_step(&view, &rem, read_symbol(view, rem, out));
			}
	}
	return view - start;
}

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
	{
		return 0;
	}

	int i;
	for (i = 0; s[i] && i < length; i++)
	{
		out[i] = s[i];
	}
	return i;
}

// Print out a string cell
// returns number of chars written
int print_string (const char *start, int ilength, char *out, int length, bool quoted)
{
	// Validate inputs
	if ((start == NULL) || (out == NULL) || (ilength < 0) || (length < 0))
	{
		return 0;
	}

	char *p_out = out;

	// Opening quote
	if (quoted)
	{
		*p_out++ = '"';
	}

	// String contents
	for (int i = 0; (i < ilength) && (i < length); i++)
	{
		*p_out++ = *start++;
	}

	// Closing quote
	if (quoted)
	{
		*p_out++ = '"';
	}

	// Return length, including quotes that were written
	return p_out - out;
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

int pr_str (Cell *x, char *out, int length);

int print_list (Cell *x, char *out, int length)
{
	char *view = out;
	int rem = length;

	// Remember the type of parens used to create the list (paren is default)
	char opener = (x->as_pair.variant)? x->as_pair.variant : '(';
	char closer = char_end(opener);

	// Print opening char
	string_step((const char**)&view, &rem, print_char(opener, view, rem));

	// Print contents
	while ((rem > 0) && x->as_pair.first)
	{
		string_step((const char**)&view, &rem, pr_str(x->as_pair.first, view, rem));

		// See if the list continues with more pairs...
		if (x->as_pair.rest == &c_nil)
		{
			break;
		}
		else if (x->as_pair.rest->kind == T_PAIR)
		{
			// Next pair in the list
			string_step((const char**)&view, &rem, print_char(' ', view, rem));
			x = x->as_pair.rest;
		}
		else
		{
			// Dotted list because the rest of this pair is not a pair
			string_step((const char**)&view, &rem, print_cstr(" . ", view, rem));
			string_step((const char**)&view, &rem, pr_str(x->as_pair.rest, view, rem));
			break;
		}
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(closer, view, rem));

	int len = length - rem;
	return len;
}

// Prints form X to output stream
// Returns: number of chars written
int pr_str (Cell *x, char *out, int length)
{
	// Validate inputs
	if ((out == NULL))
	{
		return 0;
	}

	if (length == 0)
	{
		// No room to print anything
		return 0;
	}

	// Debug only, should be an error
	if (!x)
	{
		return print_cstr("<NULL>", out, length);
	}

	_Static_assert(_CELL_KIND_COUNT == 6, "exhaustive handling of all cell kinds");
	switch (x->kind)
	{
		case T_INT:
			return print_int(x->as_int, out, length);
		case T_STRING:
			return print_string(x->as_str.start, x->as_str.length, out, length, true);
		case T_SYMBOL:
			{
				Cell *name = x->as_symbol;
				return print_string(name->as_str.start, name->as_str.length, out, length, false);
			}
		case T_FUNC:
			return print_cstr("#<fn>", out, length);
		case T_NATIVE_FUNC:
			return print_cstr("#<code>", out, length);
		case T_PAIR:
			return print_list(x, out, length);
		default:
			// Error: invalid cell kind
			return 0;
	}
}

Cell *READ (const char *start, int length)
{
	// Validate inputs
	if (!start || (length < 0))
	{
		return &c_nil;
	}

	Cell *x;
	read_str(start, length, &x);
	return x;
}

Cell *EVAL (Cell *, Cell *env);

// Evaluate each item of list x.
// Does not modify x.
// Returns: new list.
Cell *eval_each (Cell *x, Cell *env)
{
	assert(is_list(x));
	assert(is_list(env));

	// Eval the first element
	Cell *y = make_pair(EVAL(x->as_pair.first, env), &c_nil);

	// eval the rest of the elements
	Cell *p_y = y;
	Cell *p_x = x->as_pair.rest;
	while (p_x != &c_nil)
	{
		if (p_x->kind != T_PAIR)
		{
			// dotted list
			p_y->as_pair.rest = EVAL(p_x, env);
			break;
		}

		// Fill in next slot of y
		p_y->as_pair.rest = make_pair(EVAL(p_x->as_pair.first, env), &c_nil);

		// next
		p_x = p_x->as_pair.rest;
		p_y = p_y->as_pair.rest;
	}

	// Copy the variant kind
	y->as_pair.variant = x->as_pair.variant;

	return y;
}

Cell *apply (Cell *func, Cell *args, Cell *env)
{
	assert(func);
	assert(args);
	assert(env);
	assert((args->kind == T_PAIR) || (args == &c_nil));
	assert(env->kind == T_PAIR);

	// Handle the types of functions
	switch (func->kind)
	{
		case T_NATIVE_FUNC:
			// Run C function with args
			return func->as_c_func(args);
		case T_FUNC:
			{
				// Run lisp function (created by fn* )
				
				// Check length of formal parameters against actual arguments
				int n_params = list_length(func->as_func.params);
				int n_args = list_length(args);
				if (n_params != n_args)
				{
					return make_pair(string_intern_cstring("error : apply : arguments do not match formal parameters"),
							make_pair(make_int(n_args),
								make_pair(make_int(n_params), &c_nil)));
				}

				// Bind the arguments to a temporary environment
				Cell *fn_env = env_create(env);
				Cell *p_param = func->as_func.params;
				Cell *p_arg = args;
				while ((p_arg != NULL) && (p_param != NULL))
				{
					// Bind 1
					env_set(fn_env, p_param->as_pair.first, p_arg->as_pair.first);
					// next args
					p_param = p_param->as_pair.rest;
					p_arg = p_arg->as_pair.rest;
				}

				// Evaluate the body
				Cell *result = EVAL(func->as_func.body, fn_env);

				// TODO: discard the environment
				return result;
			}
		default:
			// Error
			return string_intern_cstring("error : apply : 1st item of expression is not a function");
	}
}

Cell *symbol_lookup (Cell *env, Cell *sym)
{
	assert(env != NULL);
	assert(sym != NULL);
	assert(sym->kind == T_SYMBOL);
	assert(env->kind == T_PAIR);

	Cell *slot = env_get(env, sym);

	if (slot == &c_nil)
	{
		// Symbol undefined.
		// TODO: raise better error
		return string_intern_cstring("error: undefined symbol");
	}

	// Return the slot's value
	return slot->as_pair.rest;
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	assert(ast);
	assert(env);
	assert(env->kind == T_PAIR);

	_Static_assert(_CELL_KIND_COUNT == 6, "handle all cell kinds");
	switch (ast->kind)
	{
		case T_SYMBOL:
			return symbol_lookup(env, ast);
		case T_PAIR:
			return eval_each(ast, env);
		case T_INT:
		case T_STRING:
		case T_FUNC:
		case T_NATIVE_FUNC:
		default:
			return ast;
	}
}

Cell *EVAL (Cell *x, Cell *env)
{
	assert(x != NULL);
	assert(env != NULL);
	assert(env->kind == T_PAIR);

	// Special constant symbols
	if ((x == &c_nil) || (x == &c_true) || (x == &c_false))
	{
		return x;
	}

	if (!is_list(x))
	{
		return eval_ast(x, env);
	}

	// Empty list evals to itself
	if (is_empty_list(x))
	{
		return x;
	}

	// "Apply" list as a special form or a function...
	Cell *head = x->as_pair.first;
	Cell *args = x->as_pair.rest;
	if (args == &c_nil)
	{
		args = make_empty_list();
	}

	// Check special forms
	if (head == &c_sf_def_bang)
	{
		// (def! symbol val)
		if (list_length(args) != 2)
		{
			return string_intern_cstring("def! : error : requires 2 operands");
		}
		Cell *sym = args->as_pair.first;
		// note that there is no need to check for trying to define nil and other special
		// symbols because EVAL always evaluates those to themselves
		if (sym->kind != T_SYMBOL)
		{
			return string_intern_cstring("def! : error : 1st argument must be a symbol");
		}
		Cell *val = EVAL(args->as_pair.rest->as_pair.first, env);
		env_set(env, sym, val);
		return symbol_lookup(env, sym);
	}
	else if (head == &c_sf_let_star)
	{
		// (let* (symbol1 value1 symbol2 value2 ...) expr)
		if (list_length(args) != 2)
		{
			return string_intern_cstring("let* : error : requires 2 operands");
		}
		if ((args->as_pair.first->kind != T_PAIR) || (list_length(args->as_pair.first) % 2 != 0))
		{
			return string_intern_cstring("let* : error : 1st operand not a list with an even count");
		}

		// Create env from 1st arg
		Cell *let_env = env_create(env);
		Cell *p = args->as_pair.first;
		while (is_list(p))
		{
			Cell *sym = p->as_pair.first;
			// note that there is no need to check for trying to define nil and other special
			// symbols because EVAL always evaluates those to themselves
			if (sym->kind != T_SYMBOL)
			{
				return string_intern_cstring("let* : error : every other item within 1st operand must be a symbol");
			}
			env_set(let_env, sym, EVAL(p->as_pair.rest->as_pair.first, let_env));
			p = p->as_pair.rest->as_pair.rest;
		}

		// Eval 2nd arg with env
		Cell *result = EVAL(args->as_pair.rest->as_pair.first, let_env);

		// TODO: discard let_env

		return result;
	}
	else if (head == &c_sf_fn_star)
	{
		if (list_length(args) != 2)
		{
			return string_intern_cstring("fn* : error : requires 2 operands");
		}
		if (args->as_pair.first->kind != T_PAIR)
		{
			return string_intern_cstring("fn* : error : 1st operand not a list");
		}
		Cell *p = args->as_pair.first;
		while (is_list(p) && !is_empty_list(p))
		{
			if (p->as_pair.first->kind != T_SYMBOL)
			{
				return string_intern_cstring("fn* : error : 1st operand must be a list of symbols");
			}
			p = p->as_pair.rest;
		}

		return make_fn(args->as_pair.first, args->as_pair.rest->as_pair.first);
	}
	/*
	else if (head == &c_sf_XXX)
	{

	}
	*/
	else
	{
		// Normal function application
		Cell *e_list = eval_ast(x, env);
		head = e_list->as_pair.first;
		args = e_list->as_pair.rest;
		if (args == &c_nil)
		{
			args = make_empty_list();
		}
		return apply(head, args, env);
	}
}

void PRINT (Cell *expr)
{
	char buffer[1000];

	int p_len = pr_str(expr, buffer, sizeof(buffer));

	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Cell *env)
{
	Cell * form = READ(start, length);
	Cell * value = EVAL(form, env);
	PRINT(value);
}

// Built-in functions:
// ---
Cell *bi_plus (Cell *args)
{
	int a = args->as_pair.first->as_int;
	int b = args->as_pair.rest->as_pair.first->as_int;
	return make_int(a + b);
}

Cell *bi_minus (Cell *args)
{
	int a = args->as_pair.first->as_int;
	int b = args->as_pair.rest->as_pair.first->as_int;
	return make_int(a - b);
}

Cell *bi_star (Cell *args)
{
	int a = args->as_pair.first->as_int;
	int b = args->as_pair.rest->as_pair.first->as_int;
	return make_int(a * b);
}

Cell *bi_slash (Cell *args)
{
	int a = args->as_pair.first->as_int;
	int b = args->as_pair.rest->as_pair.first->as_int;
	return make_int(a / b);
}

Cell *bi_list (Cell *args)
{
	return args;
}

Cell *bi_pair (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring("pair : error : requires 2 arguments");
	}
	return make_pair(args->as_pair.first, args->as_pair.rest->as_pair.first);
}

Cell *bi_first (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("first : error : requires 1 arguments");
	}
	if (args->as_pair.first->kind != T_PAIR)
	{
		return string_intern_cstring("first : error : 1st argument must be a pair");
	}

	// If the argument happends to be the empty list,
	// then return nil instead of NULL
	Cell *result = args->as_pair.first->as_pair.first;
	if (!result)
	{
		result = &c_nil;
	}
	return result;
}

Cell *bi_rest (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("rest : error : requires 1 arguments");
	}
	if (args->as_pair.first->kind != T_PAIR)
	{
		return string_intern_cstring("rest : error : 1st argument must be a pair");
	}
	return args->as_pair.first->as_pair.rest;
}

Cell *bi_list_p (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("list? : error : requires 1 argument");
	}
	return (args->as_pair.first->kind == T_PAIR)? &c_true : &c_false;
}

Cell *bi_empty_p (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("empty? : error : requires 1 argument");
	}
	if (args->as_pair.first->kind != T_PAIR)
	{
		return string_intern_cstring("empty? : error : requires list argument");
	}
	return (args->as_pair.first->as_pair.first == NULL)? &c_true : &c_false;
}

Cell *bi_count (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("count : error : requires 1 argument");
	}
	if (args->as_pair.first->kind != T_PAIR)
	{
		return string_intern_cstring("count : error : requires list argument");
	}
	return make_int(list_length(args->as_pair.first));
}

bool cell_equal (Cell *x, Cell *y)
{
	if ((x == NULL) || (y == NULL))
	{
		return x == y;
	}
	if (x == y)
	{
		return true;
	}
	if (x->kind != y->kind)
	{
		return false;
	}
	_Static_assert(_CELL_KIND_COUNT == 6, "handle all cell kinds");
	switch (x->kind)
	{
		case T_INT:
			return x->as_int == y->as_int;
		case T_SYMBOL:
			return x->as_symbol == y->as_symbol;
		case T_NATIVE_FUNC:
			return x->as_c_func == y->as_c_func;
		case T_PAIR:
			return (cell_equal(x->as_pair.first, y->as_pair.first)
					&& cell_equal(x->as_pair.rest, y->as_pair.rest));
		case T_FUNC:
		case T_STRING:
			// TODO: Not implemetned, default false
			return false;
		default:
			assert(0);
	}
}

Cell *bi_number_p (Cell *args)
{
	if (list_length(args) != 1)
	{
		return string_intern_cstring("number? : error : requires 1 argument");
	}
	return (args->as_pair.first->kind == T_SYMBOL)? &c_true : &c_false;
}

Cell *bi_equal (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring("= : error : requires 2 arguments");
	}
	return (cell_equal(args->as_pair.first, args->as_pair.rest->as_pair.first))? &c_true : &c_false;
}

Cell *bi_less_than (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring("< : error : requires 2 argument");
	}
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (a->kind != T_INT)
	{
		return string_intern_cstring("< : error : 1st argument must be an integer");
	}
	if (b->kind != T_INT)
	{
		return string_intern_cstring("< : error : 2nd argument must be an integer");
	}
	return (a->as_int < b->as_int)? &c_true : &c_false;
}

Cell *bi_more_than (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring("> : error : requires 2 argument");
	}
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (a->kind != T_INT)
	{
		return string_intern_cstring("> : error : 1st argument must be an integer");
	}
	if (b->kind != T_INT)
	{
		return string_intern_cstring("> : error : 2nd argument must be an integer");
	}
	return (a->as_int > b->as_int)? &c_true : &c_false;
}

Cell *bi_less_than_equal (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring("<= : error : requires 2 argument");
	}
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (a->kind != T_INT)
	{
		return string_intern_cstring("<= : error : 1st argument must be an integer");
	}
	if (b->kind != T_INT)
	{
		return string_intern_cstring("<= : error : 2nd argument must be an integer");
	}
	return (a->as_int <= b->as_int)? &c_true : &c_false;
}

Cell *bi_more_than_equal (Cell *args)
{
	if (list_length(args) != 2)
	{
		return string_intern_cstring(">= : error : requires 2 argument");
	}
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (a->kind != T_INT)
	{
		return string_intern_cstring(">= : error : 1st argument must be an integer");
	}
	if (b->kind != T_INT)
	{
		return string_intern_cstring(">= : error : 2nd argument must be an integer");
	}
	return (a->as_int >= b->as_int)? &c_true : &c_false;
}

Cell *bi_prn (Cell *args)
{
	assert(0 && "not implemented");
	return &c_nil;
}

// --- end of built-in functions

void init_cell_const (Cell *x, char *name)
{
	assert(x != NULL);
	assert(name != NULL);

	x->kind = T_SYMBOL;
	x->as_symbol = string_intern_cstring(name);
}

int init_cells (int ncells)
{
	// Allocate the arrays
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
	{
		return 1;
	}

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	for (int i = 0; i < (cell_pool_cap - 1); i++)
	{
		cell_pool[i].as_pair.rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].as_pair.rest = &c_nil;

	return 0;
}

int init_strings (int nchars)
{
	char_pool = malloc(nchars);
	if (!char_pool)
	{
		return 1;
	}

	char_free = char_pool;
	char_pool_cap = nchars;

	// Set up the internal string list with one item,
	// the empty string.
	string_list = make_pair(make_string("", 0), &c_nil);

	return 0;
}

int init_symbols (void)
{
	// Set up the internal symbol list
	Cell *to_add[] = {
		&c_true,
		&c_false,
		&c_sf_def_bang,
		&c_sf_let_star,
		&c_sf_fn_star,
		&c_sf_do,
		&c_sf_if,
		&c_nil,
	};
	symbol_list = &c_nil;
	for (int i = 0; i < sizeof(to_add)/sizeof(to_add[0]); i++)
	{
		symbol_list = make_pair(to_add[i], symbol_list);
	}

	// Set up the constant cell values
	init_cell_const(&c_nil, "nil");
	init_cell_const(&c_true, "true");
	init_cell_const(&c_false, "false");

	// Set up the constant cell special forms
	init_cell_const(&c_sf_def_bang, "def!");
	init_cell_const(&c_sf_let_star, "let*");
	init_cell_const(&c_sf_fn_star, "fn*");
	init_cell_const(&c_sf_do, "do");
	init_cell_const(&c_sf_if, "if");

	return 0;
}

int init (int ncells, int nchars)
{
	if (init_cells(ncells) || init_strings(nchars) || init_symbols())
	{
		return 1;
	}
	return 0;
}

Cell *init_env (void)
{
	Cell *env = env_create(&c_nil);
	if (env)
	{
		// Define these just because they might somehow be looked up
		// with out eval'ing them
		env_set_c(env, "nil", &c_nil);
		env_set_c(env, "true", &c_true);
		env_set_c(env, "false", &c_false);

		// Built-in Functions
		env_set_c(env, "+", make_cfunc(bi_plus));
		env_set_c(env, "-", make_cfunc(bi_minus));
		env_set_c(env, "*", make_cfunc(bi_star));
		env_set_c(env, "/", make_cfunc(bi_slash));
		env_set_c(env, "list", make_cfunc(bi_list));
		env_set_c(env, "pair", make_cfunc(bi_pair));
		env_set_c(env, "first", make_cfunc(bi_first));
		env_set_c(env, "rest", make_cfunc(bi_rest));
		env_set_c(env, "list?", make_cfunc(bi_list_p));
		env_set_c(env, "empty?", make_cfunc(bi_empty_p));
		env_set_c(env, "count", make_cfunc(bi_count));
		env_set_c(env, "number?", make_cfunc(bi_number_p));
		env_set_c(env, "=", make_cfunc(bi_equal));
		env_set_c(env, "<", make_cfunc(bi_less_than));
		env_set_c(env, ">", make_cfunc(bi_more_than));
		env_set_c(env, "<=", make_cfunc(bi_less_than_equal));
		env_set_c(env, ">=", make_cfunc(bi_more_than_equal));
		env_set_c(env, "prn", make_cfunc(bi_prn));
	}
	return env;
}

int main (int argc, char **argv)
{
	// Initialize memories
	if(init(1024, 2048))
	{
		return 1;
	}

	// Initialize the REPL environment symbols
	Cell *repl_env = init_env();
	if (!repl_env)
	{
		return 1;
	}

	// Debug: log the present symbols
	printf("symbols: ");
	PRINT(symbol_list);

	// Debug: log the REPL environment
	//printf("environment: ");
	//PRINT(repl_env);

	// REPL
	char buffer[1024];
	while(1)
	{
		printf("user> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
		{
			break;
		}
		rep(buffer, strlen(buffer), repl_env);
	}
	// Always add a newline at the end for nice command-line usage
	printf("\n");

	return 0;
}
