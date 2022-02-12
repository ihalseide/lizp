#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

enum Cell_kind
{
	CK_INT,
	CK_STRING,
	CK_SYMBOL,
	CK_FUNC,
	CK_NATIVE_FUNC,
	CK_PAIR,
	CK_ATOM,
};

typedef struct cell Cell;
typedef void (*Native_fn)(Cell* args, Cell *env, Cell **out);

struct cell
{
	enum Cell_kind kind;
	union
	{
		int as_int;
		const char *as_str;
		Cell *as_atom;
		struct
		{
			Cell *first;
			Cell *rest;
		} as_pair;
		struct
		{
			int n_params;
			Native_fn func;
		} as_native_fn;
		struct
		{
			const Cell *params;
			const Cell *ast;
			Cell *env;
		} as_fn;
	};
};

// Forward declarations
Cell *EVAL (Cell *, Cell *env); 
void PRINT (const Cell *expr);
int pr_str (const Cell *x, char *out, int length, int readable);
void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out);

// Cell memory (holds the actual cell values):
Cell *cell_pool = NULL;
int cell_pool_cap = 0;

// String memory (holds the actual characters):
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;

// Strings interned
Cell *string_list = NULL;

// REPL environment
Cell *repl_env = NULL;

// Useful string values
const char *s_nil = "nil",
	  *s_false    = "#f",
	  *s_true     = "#t",
	  *s_def_bang = "def!",
	  *s_let_star = "let*",
	  *s_if       = "if",
	  *s_fn_star  = "fn*",
	  *s_do       = "do";

int char_is_symbol (char c)
{
	return (c > ' ')
		&& (c != '"') && (c != '|') && (c != '[') && (c != ']')
		&& (c != '(') && (c != ')') && (c != '{') && (c != '}');
}

// Get a new cell
Cell *cell_alloc ()
{
	if (!cell_pool)
		return NULL;

	Cell *x = cell_pool->as_pair.rest;

	// Remove the cell from the free list
	if (x)
		cell_pool->as_pair.rest = x->as_pair.rest;

	return x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_alloc();
	if (x)
		x->kind = k;

	return x;
}

int is_kind (const Cell *x, enum Cell_kind kind)
{
	return x && (x->kind == kind);
}

int is_function (const Cell *x)
{
	return is_kind(x, CK_FUNC) || is_kind(x, CK_NATIVE_FUNC);
}

Cell *make_int (int n)
{
	Cell *x = cell_init(CK_INT);
	if (x)
		x->as_int = n;
	return x;
}

Cell *make_native_fn (int n_params, Native_fn func)
{
	if (n_params < 0 || !func)
		return NULL;

	Cell *x = cell_init(CK_NATIVE_FUNC);
	if (x)
	{
		x->as_native_fn.n_params = n_params;
		x->as_native_fn.func = func;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(CK_PAIR);
	if (x)
	{
		x->as_pair.first = first;
		x->as_pair.rest = rest;
	}
	return x;
}

Cell *make_atom (Cell *ref)
{
	Cell *x = cell_init(CK_ATOM);
	if (x)
	{
		x->as_atom = ref;
	}
	return x;
}

Cell *make_symbol (const char *str)
{
	Cell *x = cell_init(CK_SYMBOL);
	if (x)
		x->as_str = str;
	return x;
}

Cell *make_string (const char *str)
{
	Cell *x = cell_init(CK_STRING);
	if (x)
		x->as_str = str;
	return x;
}

// A custom lisp function is a really just a list.
// The form of that list is: (params env . body)
Cell *make_fn (const Cell *params, const Cell *body, Cell *outer_env)
{
	if (!is_kind(params, CK_PAIR) || !body || !is_kind(outer_env, CK_PAIR))
		return NULL;

	Cell *x = cell_init(CK_FUNC);
	if (x)
	{
		x->as_fn.params = params;
		x->as_fn.ast = body;
		x->as_fn.env = outer_env;
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, NULL);
}

int is_empty_list (const Cell *x)
{
	return is_kind(x, CK_PAIR) && (x->as_pair.first == NULL);
}

int list_length (const Cell *list)
{
	int i;
	for (i = 0; is_kind(list, CK_PAIR) && !is_empty_list(list); i++)
		list = list->as_pair.rest;
	return i;
}

int string_can_alloc (int length)
{
	return char_free && (char_free + length) < (char_pool + char_pool_cap);
}

// Get space for a string of certain length
char *string_alloc (int length)
{
	// Make sure there is enough memory
	if (!string_can_alloc(length))
		return NULL;

	char *v = char_free;
	char_free += length;
	return v;
}

// Add string to the char_pool.
// Does not modify the char_free pointer.
char *string_pool_write (const char *start, int length)
{
	// Validate inputs
	if (!start || length < 0 || !char_free)
		return NULL;

	// +1 becuase there needs to be room for
	// the extra null terminator char too
	char *new = string_alloc(length + 1);
	if (!new)
		return NULL;

	// Copy string unless its already in the pool
	// because the start already points there.
	if (start != char_free)
		// Only return the new string if memcpy succeeds
		if (!memcpy(new, start, length))
			return NULL;

	// Add null terminator
	new[length] = '\0';

	return new;
}

// For writing C strings
char *string_pool_write_c (const char *str)
{
	return string_pool_write(str, strlen(str));
}

// See if a string cells string is equal to the given str
int string_equal (const Cell *string_cell, const char *str, int length)
{
	// Validate arguments
	if (!string_cell || !str || length < 0)
		return 0;

	// Compare pointer addresses
	const char *str2 = string_cell->as_str;
	if (str2 == str)
		return 1;

	// Compare lengths
	int cs_length = strlen(str2);
	if (length != cs_length)
		return 0;

	// Compare contents
	return strncmp(string_cell->as_str, str, length) == 0;
}

Cell *find_string (const char *start, int length)
{
	// Search whole string list for equivalent string
	Cell *p = string_list;
	while (is_kind(p, CK_PAIR) && !is_empty_list(p))
	{
		Cell *string = p->as_pair.first;
		assert(is_kind(string, CK_STRING));

		// Found?
		if (string_equal(string, start, length))
			return string;

		// Next
		p = p->as_pair.rest;
	}

	// Not found
	return NULL;
}

// Add a C string to string list
Cell *insert_string (const char *str)
{
	// Validate arguments
	if (!str)
		return NULL;

	// Make string cell
	Cell *s = make_string(str);
	if (!s)
		return NULL;

	// Insert new node to string list
	Cell *node = make_pair(s, string_list->as_pair.rest);
	if (!node)
		return NULL;
	string_list->as_pair.rest = node;

	return s;
}

// Returns internal string cell
Cell *intern_string (const char *start, int length)
{
	// If string is already interned, return that value
	Cell *result = find_string(start, length);
	if (result)
		return result;

	// Make (C string) copy of the string
	char *i_string = string_pool_write(start, length);
	if (!i_string)
		return NULL;

	return insert_string(i_string);
}

// For string reading and writing
void string_step (const char **stream, int *length, int n)
{
	*stream += n;
	*length -= n;
}

void string_skip_white(const char **stream, int *length)
{
	if (!stream)
		return;

	const char *view = *stream;
	int rem = *length;
	while (isspace(*view) && (rem > 0))
		string_step(&view, &rem, 1);
	*stream = view;
	*length = rem;
}

int symbol_eq (const Cell *s1, const Cell *s2)
{
	return s1 && (s1->kind == CK_SYMBOL) && s1->as_str
		&& s2 && (s2->kind == CK_SYMBOL) && s2->as_str
		&& (strcmp(s1->as_str, s2->as_str) == 0);
}

// Find a symbol in an alist.
// An alist is a list of the form ((symbol . value) (symbol . value) ....)
Cell *alist_assoc (const Cell *sym, Cell *alist)
{
	// Validate inputs
	if (!is_kind(sym, CK_SYMBOL) || !is_kind(alist, CK_PAIR))
		return NULL;

	// Iterate through the list
	while (alist && !is_empty_list(alist))
	{
		// Check if it found a a slot with the symbol, and if so return the slot
		if (alist->as_pair.first && symbol_eq(alist->as_pair.first->as_pair.first, sym))
			return alist->as_pair.first;

		// Next
		alist = alist->as_pair.rest;
	}

	return NULL;
}

// Find the innermost env which contains symbol
// Returns:
//   if found -> environment Cell 
//   not found -> nil
Cell *env_find (Cell *env, const Cell *sym)
{
	// Validate inputs
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
		return NULL;

	// Search up the environment hierarchy
	while (env && is_kind(env, CK_PAIR) && !is_empty_list(env))
	{
		// Search current environment
		if (alist_assoc(sym, env->as_pair.first))
			return env;

		// Move on to the outer environment
		env = env->as_pair.rest;
	}

	// Not found
	return NULL;
}

// Get the innermost definition for the symbol.
// Returns:
//   when found -> the slot containing (symbol . value)
//   when not found -> nil
Cell *env_get (Cell *env, Cell *sym)
{
	// Validate inputs
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
		return NULL;

	// Do not allow nil, true, or false to be defined
	if (sym->as_str == s_nil || sym->as_str == s_false || sym->as_str == s_true)
		return NULL;

	// Find the environment which contains the symbol
	Cell *containing_env = env_find(env, sym);
	// Return the slot if environment was found
	if (containing_env)
		return alist_assoc(sym, containing_env->as_pair.first);

	// Symbol not found
	printf("env_get : error : symbol undefined\n");
	return NULL;
}

// Push an item in front of a list (which may be empty)
void push_list (Cell *item, Cell **list)
{
	// Validate arguments
	if (!item || !is_kind(*list, CK_PAIR))
		return;

	if (is_empty_list(*list))
		// An empty list has nothing in the first slot yet
		(*list)->as_pair.first = item;
	else
		*list = make_pair(item, *list);
}

// Returns whether it succeeded (1) or not (0)
int env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
		return 0;

	// Do not allow nil, true, or false to be defined
	if (sym->as_str == s_nil || sym->as_str == s_false || sym->as_str == s_true)
		return 0;

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = alist_assoc(sym, env->as_pair.first);
	if (slot)
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->as_pair.rest = val;
	}
	else
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		slot = make_pair(sym, val);
		push_list(slot, &(env->as_pair.first));
	}

	return 1;
}

// An environment is a list of the form (alist . outer-env).
// Create an environment with each item of the
// "binds" list set to the corresponding item in the "exprs" list.
Cell *env_create (Cell *env_outer, const Cell *binds, Cell *exprs)
{
	// Validate args (env_outer is allowed to be NULL)
	if (env_outer && !is_kind(env_outer, CK_PAIR))
	{
		printf("env_create : error : invalid outer environment\n");
		return NULL;
	}

	Cell *env = make_pair(make_empty_list(), env_outer);
	if (!env)
		return NULL;

	// Create the bindings by iterating both lists
	while (is_kind(binds, CK_PAIR) && is_kind(exprs, CK_PAIR) && !is_empty_list(binds) && !is_empty_list(exprs))
	{
		// Bind 1 pair
		Cell *sym = binds->as_pair.first;
		Cell *val = exprs->as_pair.first;

		// Make sure it only binds symbols
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("env_create : error : a member of the bindings list is not a symbol\n");
			return NULL;
		}
		env_set(env, sym, val);

		// Next
		binds = binds->as_pair.rest;
		exprs = exprs->as_pair.rest;
	}

	// Left over symbols in the bindings list
	if (binds && !is_empty_list(binds))
	{
		printf("env_create : error : not enough values to bind to symbols list\n");
		return NULL;
	}

	// Left over values in the exprs list
	if (exprs && !is_empty_list(exprs))
	{
		printf("env_create : error : too many values to bind to symbols list\n");
		return NULL;
	}

	return env;
}

int parse_int (const char *start, int length, int *out)
{
	// Validate inputs
	if (!out)
		return 0;
	*out = 0;
	if (!start || !out || length <= 0)
		return 0;

	const char *view = start;

	// Read optional minus sign
	int sign = 1;
	if (*view == '-')
	{
		sign = -sign;
		string_step(&view, &length, 1);
	}

	// Result n
	int n = 0;

	// Read all the digits
	while (isdigit(*view) && (length > 0))
	{
		n = (n * 10) + ((*view) - '0');
		string_step(&view, &length, 1);
	}

	int num_len = view - start;
	*out = n * sign;
	return num_len;
}

// Create a string cell that has the string value from reading a quoted string
int read_string_literal (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!out)
		return 0;
	if (!start || length <= 0 || !char_free)
	{
		*out = NULL;
		return 0;
	}

	// Is the current part of input string
	const char *view = start;

	// Is the current part of the output buffer (char pool)
	// Write to the char_pool, but do not move the char_free pointer.
	char *pad = char_free;
	int pad_len = char_pool_cap - (char_free - char_pool);

	// Read opening quote
	string_step(&view, &length, 1);

	// Read contents and process escape codes
	while (*view && (*view != '"') && (length > 0) && (pad_len > 0))
	{
		char c = *view;
		if (*view == '\\')
		{
			string_step(&view, &length, 1);
			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
			}
		}
		*pad = c;
		string_step((const char**) &pad, &pad_len, 1);
		string_step(&view, &length, 1);
	}

	// Read closing quote
	string_step(&view, &length, 1);

	// Intern the string, which will handle the case that
	// this string is already in the char_pool.
	// May or may not cause string allocation.
	int result_length = pad - char_free;
	*out = intern_string(char_free, result_length);

	// Return the number of chars written
	return view - start;
}

// Read in a symbol or number
int read_item (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	if (!start || (length <= 0))
	{
		*out = NULL;
		return 0;
	}

	const char *p = start;

	// Symbol or number
	while (length > 0 && char_is_symbol(*p))
		string_step(&p, &length, 1);

	// Symbols or numbers can start with a '-', so we need to distinguish them by 
	// what the next character is.
	if (isdigit(*start) || ((length > 1) && (*start == '-') && isdigit(*(start + 1))))
	{
		// Number
		int x;
		int n = parse_int(start, p - start, &x);
		if (!n)
		{
			*out = NULL;
			return p - start;
		}
		*out = make_int(x);
	}
	else
	{
		// Symbol
		Cell *name = intern_string(start, p - start);
		if (!name)
			*out = NULL;

		assert(is_kind(name, CK_STRING));
		*out = make_symbol(name->as_str);
	}

	return p - start;
}

int read_str (const char *start, int length, Cell **out);

char char_end_brace (char x)
{
	switch (x)
	{
		case '[': return ']';
		case '(': return ')';
		case '{': return '}';
		default:  return 0;
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
	int rem = length;

	// Consume the opening paren
	char end = char_end_brace(*view);
	string_step(&view, &rem, 1);
	string_skip_white(&view, &rem);

	Cell *list = make_empty_list();

	if (*view != end)
	{
		Cell *p = list;

		// Read the first element
		string_step(&view, &rem, read_str(view, rem, &p->as_pair.first));

		// Read the rest of the normal elements (don't handle the "dot")
		while ((rem > 0) && (*view != end) && (*view != '|'))
		{
			// Read an element
			Cell *e;
			string_step(&view, &rem, read_str(view, rem, &e));
			if (!e)
				break;

			p->as_pair.rest = make_pair(e, NULL);
			p = p->as_pair.rest;
		}

		if (*view == '|')
		{
			// Dotted list
			Cell *e;
			string_step(&view, &rem, 1);
			string_step(&view, &rem, read_str(view, rem, &e));
			p->as_pair.rest = e;
		}
	}

	if (*view == end)
	{
		// Consume the final character
		string_step(&view, &rem, 1);
		*out = list;
	}
	else
	{
		// Error: unexpected end of input
		*out = NULL;
		printf("read_list: error : error reading item or unexpected end of input\n");
	}

	return view - start;
}

int read_until (const char *start, int length, char sentinel)
{
	int i;
	for (i = 0; start[i] && (i < length) && (start[i] != sentinel); i++)
		;
	return i;
}

// Read a form from an input stream/string
// Returns: the number of characters read
int read_str (const char *start, int length, Cell **out)
{
	// Validate arguments
	if (!out)
		return 0;
	else
		*out = NULL;
	if (!start || length <= 0)
	{
		return 0;
	}

	const char *view = start;
	int rem = length;

	string_skip_white(&view, &rem);
	switch (*view)
	{
		case '\0': // End of input
			break;
		case ';': // Line comment
			string_step(&view, &rem, read_until(view, rem, '\n'));
			break;
		case ']':
		case ')':
		case '}': // Error, unmatched closing paren
			printf("read_str : error : unmatched closing paren\n");
			break;
		case '|': // Error, '|' for cons pairs should only be inside a list
			printf("read_str : error : '|' should only be inside a list\n");
			break;
		case '[':
		case '(':
		case '{': // Opening paren, for lists
			string_step(&view, &rem, read_list(view, rem, out));
			break;
		case '"': // Quoted string literal
			string_step(&view, &rem, read_string_literal(view, rem, out));
			break;
		default:
			// Read symbol or number
			string_step(&view, &rem, read_item(view, rem, out));
			break;
	}
	string_skip_white(&view, &rem);

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
	if (!is_empty_list(list))
	{
		string_step((const char**)&view, &rem, pr_str(list->as_pair.first, view, rem, readable));
		// Next item
		list = list->as_pair.rest;
	}

	// Print normal list elements
	while (is_kind(list, CK_PAIR) && !is_empty_list(list))
	{
		string_step((const char**)&view, &rem, print_char(' ', view, rem));
		string_step((const char**)&view, &rem, pr_str(list->as_pair.first, view, rem, readable));
		// Next item
		list = list->as_pair.rest;
	}

	// If there is a value (except nil) in the final rest slot, then print it dotted
	if (list && !is_empty_list(list) && !(is_kind(list, CK_SYMBOL) && list->as_str == s_nil))
	{
		string_step((const char**)&view, &rem, print_string(" | ", view, rem, 0));
		string_step((const char**)&view, &rem, pr_str(list, view, rem, readable));
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(']', view, rem));

	int len = length - rem;
	return len;
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

// Does: Read a form from the stream
// Returns: the form, which may be NULL
Cell *READ (const char *start, int length)
{
	// Validate inputs
	if (!start || (length < 0))
		return NULL;

	Cell *x;
	read_str(start, length, &x);
	return x;
}

// Does: evaluate each item of list x, without modifying x.
// Returns: a new list.
Cell *eval_each (Cell *list, Cell *env)
{
	// Validate arguments
	if (!is_kind(list, CK_PAIR) || !is_kind(env, CK_PAIR))
		return NULL;

	if (is_empty_list(list))
		return list;

	// Eval the first element
	Cell *y = make_pair(EVAL(list->as_pair.first, env), NULL);
	Cell *p_y = y;

	// eval the rest of the elements
	list = list->as_pair.rest;
	while (list && p_y)
	{
		if (!is_kind(list, CK_PAIR))
		{
			// dotted list
			p_y->as_pair.rest = EVAL(list, env);
			break;
		}

		// Fill in next slot of y
		p_y->as_pair.rest = make_pair(EVAL(list->as_pair.first, env), NULL);

		// next
		list = list->as_pair.rest;
		p_y = p_y->as_pair.rest;
	}

	return y;
}

int cell_eq (Cell *a, Cell *b)
{
	// Nothing equals NULL
	if (!a || !b)
		return 0;

	// Compare pointers
	if (a == b)
		return 1;

	// Must be the same kind
	if (a->kind != b->kind)
		return 0;

	switch (a->kind)
	{
		case CK_INT:
			return a->as_int == b->as_int;
		case CK_SYMBOL:
		case CK_STRING:
			return a->as_str == b->as_str;
		case CK_FUNC:
			return 0;
		case CK_NATIVE_FUNC:
			return (a->as_native_fn.func == b->as_native_fn.func)
				&& (a->as_native_fn.n_params == b->as_native_fn.n_params);
		case CK_PAIR:
			if (is_empty_list(a))
				return is_empty_list(b);
			else
				return cell_eq(a->as_pair.first, b->as_pair.first)
					&& cell_eq(a->as_pair.rest, b->as_pair.rest);
		default:
			assert(0 && "invalid cell kind");
			return 0;
	}
}

Cell *make_bool_sym (int val)
{
	if (val)
		return make_symbol(s_true);
	else
		return make_symbol(s_false);
}

// Create new string by joining together the
// string representation of all of the arguments
//   sep = character separator (no separator if it is 0)
//   readable = whether to print readably
Cell *string_join (const Cell *args, char sep, int readable)
{
	// Validate arguments
	if (!args)
		return 0;

	// Use the char pool as a space to write in (like reading string literals)
	char *pad = char_free;
	int len = char_pool_cap - (char_free - char_pool);

	// Print first item with no separator
	if ((len > 1) && is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		string_step((const char**) &pad, &len, pr_str(args->as_pair.first, pad, len, readable));
		args = args->as_pair.rest;
	}

	// Print the remaining items with separators
	while ((len > 1) && is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		if (sep)
			string_step((const char**) &pad, &len, print_char(sep, pad, len));
		string_step((const char**) &pad, &len, pr_str(args->as_pair.first, pad, len, readable));
		args = args->as_pair.rest;
	}

	// This will use the characters we already
	// wrote without re-copying them
	return intern_string(char_free, pad - char_free);
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	// Validate args
	if (!ast || !env)
		return NULL;

	switch (ast->kind)
	{
		case CK_SYMBOL: // Look up symbol's value
			{
				// These special symbols are self-evaluating
				if (ast->as_str == s_nil || ast->as_str == s_true || ast->as_str == s_false)
					return ast;

				// Get the value out of the environment's slot
				Cell *slot = env_get(env, ast);
				if (slot)
					return slot->as_pair.rest;

				printf("eval_ast : error : undefined symbol '%s'\n", ast->as_str);
				return NULL;
			}
		case CK_PAIR:
			return eval_each(ast, env);
		case CK_INT: // Self-evaluating values
		case CK_STRING:
		case CK_FUNC:
		case CK_NATIVE_FUNC:
		case CK_ATOM:
			return ast;
	}

	// Invalid kind
	return NULL;
}

// [str a b c ...] -> "abc..." (prints non-readably)
void fn_str (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = string_join(args, 0, 0);
}

// [pr-str a b c ...] -> "a b c ..." (prints readably)
void fn_pr_str (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = string_join(args, ' ', 1);
}

// [prn a b c ...] -> nil (prints readably)
void fn_prn (Cell *args, Cell *env, Cell **val_out)
{
	Cell *s = string_join(args, ' ', 1);
	if (s)
		printf("%s", s->as_str);
	*val_out =  make_symbol(s_nil);
}

// [println a b c ...] -> nil (prints non-readably)
void fn_println (Cell *args, Cell *env, Cell **val_out)
{
	Cell *s = string_join(args, ' ', 0);
	if (s)
		printf("%s", s->as_str);
	*val_out = make_symbol(s_nil);
}

// [list a ...] -> [a ...] (variadic)
void fn_list (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = args;
}

// [deref atom]
void fn_deref (Cell *args, Cell *env, Cell **val_out)
{
	if (is_kind(args->as_pair.first, CK_ATOM))
		*val_out = args->as_pair.first->as_atom;
	else
		*val_out = NULL;
}

// [atom? x] -> bool
void fn_atom_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_ATOM));
}

// [atom x] -> #<atom x>
void fn_atom (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_atom(args->as_pair.first);
}

void fn_eval (Cell *args, Cell *env, Cell **val_out)
{
	assert(0 && "Not to be implemented. This function is only needed for its pointer value.");
}

// (slurp "file name") -> "file contents"
void fn_slurp (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;

	// Validate arguments
	if (!is_kind(a, CK_STRING))
		return;

	// Read all contents of the file...
	// Open file
	FILE *f = fopen(a->as_str, "r");
	if (!f) // failed
		return;

	// Get file length
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	// See if we have enough room for this string data
	if (!string_can_alloc(fsize + 1))
	{
		fclose(f);
		return;
	}

	// Read the char data and null-terminate it
	fread(char_free, fsize, 1, f);
	fclose(f);

	// Move the character allocation pointer
	*val_out = intern_string(char_free, fsize);
}

// [read-string "str"] -> any value
void fn_read_str (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	if (is_kind(a, CK_STRING))
		read_str(a->as_str, strlen(a->as_str), val_out);
}

// [empty? x]
void fn_empty_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_empty_list(args->as_pair.first));
}

// [count list]
void fn_count (Cell *args, Cell *env, Cell **val_out)
{
	if (!is_kind(args->as_pair.first, CK_PAIR))
	{
		printf("count : error : first argument must be a list\n");
		return;
	}
	*val_out = make_int(list_length(args->as_pair.first));
}

// [list? x]
void fn_list_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_PAIR));
}

// [int? x]
void fn_int_p (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(is_kind(args->as_pair.first, CK_INT));
}


// [reset! atom value] -> value
void fn_reset_bang (Cell *args, Cell *env, Cell **val_out)
{
	Cell *atom = args->as_pair.first;
	Cell *value = args->as_pair.rest->as_pair.first;

	if (!is_kind(atom, CK_ATOM))
	{
		printf("reset! : error : first argument must be an atom\n");
		return;
	}

	atom->as_atom = value;
	*val_out = value;
}

// [= x y]
void fn_eq (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_bool_sym(cell_eq(args->as_pair.first,
				args->as_pair.rest->as_pair.first));
}

// [< n1 n2]
void fn_lt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int < b->as_int);
}

// [> n1 n2]
void fn_gt (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int > b->as_int);
}

// [<= n1 n2]
void fn_lte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int <= b->as_int);
}

// [>= n1 n2]
void fn_gte (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_bool_sym(a->as_int >= b->as_int);
}

// [+ n1 n2]
void fn_add (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int + b->as_int);
}

// [- n1 n2]
void fn_sub (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int - b->as_int);
}

// [* n1 n2]
void fn_mul (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int * b->as_int);
}

// [/ n1 n2]
void fn_div (Cell *args, Cell *env, Cell **val_out)
{
	Cell *a = args->as_pair.first;
	Cell *b = args->as_pair.rest->as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return;
	*val_out = make_int(a->as_int / b->as_int);
}

// [swap! atom fn args...] (variadic)
// The atom's val is modified to the result of applying the
// function with the atom's value as the first argument and the
// optionally given function arguments as the rest of the arguments.
// Return's the atom's new value
void fn_swap_bang (Cell *args, Cell *env, Cell **val_out)
{
	if (list_length(args) < 2)
	{
		printf("swap! : error : requires at least 2 arguments\n");
		return;
	}

	Cell *atom = args->as_pair.first;
	if (!is_kind(atom, CK_ATOM))
	{
		printf("swap! : error : argument 1 must be an atom\n");
		return;
	}

	Cell *fn = args->as_pair.rest->as_pair.first;
	if (!is_function(fn))
	{
		printf("swap! : error : argument 2 must be a function\n");
		return;
	}

	// Create a new arguments list for the function to be called
	Cell *args2 = args->as_pair.rest->as_pair.rest;
	if (!args2)
		args2 = make_empty_list();
	push_list(atom->as_atom, &args2);

	// Apply the function by evaluating a list as [fn args]
	*val_out = EVAL(make_pair(fn, args2), env);

	// Modify atom's value
	atom->as_atom = *val_out;
}

// [pair x y] -> [x | y]
void fn_pair (Cell *args, Cell *env, Cell **val_out)
{
	*val_out = make_pair(args->as_pair.first, args->as_pair.rest->as_pair.first);
}

// [concat l1 l2 ...] -> list
void fn_concat (Cell *args, Cell *env, Cell **val_out)
{
	Cell *list = make_empty_list();
	Cell *p = list;

	while (is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		// Current list from arguments
		Cell *a = args->as_pair.first;
		if (!is_kind(a, CK_PAIR))
		{
			printf("fn_concat : error : arguments must be lists\n");
			return;
		}

		// Add all of the items from the current list
		while (is_kind(a, CK_PAIR) && !is_empty_list(a))
		{
			// Put item into the list
			p->as_pair.first = a->as_pair.first;
			p->as_pair.rest = make_empty_list();
			p = p->as_pair.rest;

			// Next argument
			a = a->as_pair.rest;
		}
		// The list should end with null instead of an empty list
		p->as_pair.rest = NULL;

		// Next argument
		args = args->as_pair.rest;
	}

	*val_out = list;
}

// For proper tail-call recursion, the next ast and env are both returned for EVAL to use.
// Null environment indicates that no further evaluation is needed
void apply (Cell *fn, Cell *args, Cell *env, Cell **val_out, Cell **env_out)
{
	// Validate arguments
	if (!fn || !is_kind(args, CK_PAIR) || !env || !val_out || !env_out)
	{
		printf("apply : error : invalid arguments\n");
		return;
	}

	// If the value is NULL, there was an error
	*val_out = NULL;
	// If the env is not NULL, evaluation needs to be continued
	// with the env
	*env_out = NULL;

	switch (fn->kind)
	{
		case CK_NATIVE_FUNC:
			// Apply a built-in native C function
			if (fn->as_native_fn.n_params && (list_length(args) != fn->as_native_fn.n_params))
			{
				printf("apply : error : native function is defined only with %d parameters\n", fn->as_native_fn.n_params);
				return;
			}

			if (fn->as_native_fn.func == fn_eval)
			{
				// Special case for eval, because
				// it only needs to do what EVAL already does.
				// Don't even call eval because it's a dummy value.
				*val_out = args->as_pair.first;
				*env_out = repl_env; // Use the REPL environment instead of the current one?
				break;
			}

			fn->as_native_fn.func(args, env, val_out);
			break;
		case CK_FUNC:
			// Apply lisp function created by fn*
			env = env_create(fn->as_fn.env, fn->as_fn.params, args);
			if (!env)
				return;
			*val_out = fn->as_fn.ast;
			*env_out = env;
			break;
		default: // Error: not a function
			printf("apply : error : first item in list is not a function\n");
			break;
	}
}

int truthy (Cell *x)
{
	if (!x)
		return 0;

	if (!is_kind(x, CK_SYMBOL))
		return 1;

	const char *name = x->as_str;
	return name != s_nil && name != s_false;
}

// Returns 1 or 0 for if it is a special form or not
int eval_special (Cell *head, Cell *ast, Cell *env, Cell **ast_out, Cell **env_out)
{
	if (!ast_out || !env_out)
		return 0;

	// Default behavior is no tail-call, error
	*env_out = NULL;
	*ast_out = NULL;

	// Head must be a symbol
	if (!is_kind(head, CK_SYMBOL))
		return 0;

	// Make sure that ast is a list
	if (!ast)
		ast = make_empty_list();
	if (!ast)
		return 0;
	assert(is_kind(ast, CK_PAIR));

	const char *name = head->as_str;
	if (name == s_def_bang)
	{
		// [def! <symbol> value]
		if (list_length(ast) != 2)
		{
			printf("def! : error : requires 2 expressions\n");
			return 1;
		}

		Cell *sym = ast->as_pair.first;
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("def! : error : argument 1 must be a symbol\n");
			return 1;
		}

		Cell *val = EVAL(ast->as_pair.rest->as_pair.first, env);
		if (!val)
			return 1;

		if (env_set(env, sym, val))
			*ast_out = val;
		else
			printf("def! : error : cannot define symbol\n");

		return 1;
	}
	else if (name == s_let_star)
	{
		// [let* [sym1 expr1 sym2 expr2...] expr]
		if (list_length(ast) != 2)
		{
			// Error: invalid ast
			printf("let* : error : requires 2 expressions\n");
			return 1;
		}

		// Go through the bindings list and add the
		// bindings to the environment
		Cell *let_env = env_create(env, NULL, NULL);
		Cell *p = ast->as_pair.first; // pointer to bindings list
		while (is_kind(p, CK_PAIR))
		{
			if (!is_kind(p->as_pair.rest, CK_PAIR))
			{
				// Error: odd amount of arguments in first list
				printf("let* : error : requires an even amount of items in bindings list\n");
				return 1;
			}
			if (!is_kind(p->as_pair.first, CK_SYMBOL))
			{
				// Error: even element in bindings list not a symbol
				printf("let* : error : requires even elements in bindings list to be symbols\n");
				return 1;
			}

			env_set(let_env, p->as_pair.first, EVAL(p->as_pair.rest->as_pair.first, let_env));

			// Next pair (next twice)
			p = p->as_pair.rest->as_pair.rest;
		}

		// Tail call on the body expr
		*env_out = let_env;
		*ast_out = ast->as_pair.rest->as_pair.first;
		return 1;
	}
	else if (name == s_fn_star)
	{
		// [fn* [symbol1 symbol2 ...] expr]
		if (list_length(ast) != 2)
		{
			printf("fn* : error : requires 2 expressions\n");
			return 1;
		}

		// Check that the parameter list is only symbols
		Cell *p = ast->as_pair.first;
		while (is_kind(p, CK_PAIR) && !is_empty_list(p))
		{
			if (!is_kind(p->as_pair.first, CK_SYMBOL))
			{
				// Error: parameter list must be all symbols
				printf("fn* : error : items of parameter list must be symbols\n");
				return 1;
			}
			if (p->as_pair.rest && !is_kind(p->as_pair.rest, CK_PAIR))
			{
				// Something other than NULL or a list is next
				printf("fn* : error : parameter list is not a proper list\n");
				return 1;
			}

			// Next
			p = p->as_pair.rest;
		}

		// New function closure, no tail call
		*ast_out = make_fn(ast->as_pair.first, ast->as_pair.rest->as_pair.first, env);
		return 1;
	}
	else if (name == s_if)
	{
		// [if expr t-expr f-expr] OR [if expr t-expr]
		int len = list_length(ast);
		if (!is_kind(ast, CK_PAIR) || (len != 2 && len != 3))
		{
			// Error: too few or too many arguments
			printf("if : error : must have 2 or 3 arguments\n");
			return 1;
		}

		Cell *cond = EVAL(ast->as_pair.first, env);
		if (cond)
		{
			if (truthy(cond))
			{
				// Tail call with t-expr
				*env_out = env;
				*ast_out = ast->as_pair.rest->as_pair.first;
			}
			else if (ast->as_pair.rest->as_pair.rest)
			{
				// Tail call with f-expr
				*env_out = env;
				*ast_out = ast->as_pair.rest->as_pair.rest->as_pair.first;
			}
			else
			{
				// No f-expr, so return nil
				*ast_out = make_symbol(s_nil);
			}
		}

		return 1;
	}
	else if (name == s_do)
	{
		// [do exprs...]

		// Evaluate all but the last item
		while (is_kind(ast, CK_PAIR) && !is_empty_list(ast) && ast->as_pair.rest)
		{
			EVAL(ast->as_pair.first, env);
			ast = ast->as_pair.rest;
		}

		// Has a last item, so do a tail call to evaluate that
		if (is_kind(ast, CK_PAIR) && !is_empty_list(ast))
		{
			*ast_out = ast->as_pair.first;
			*env_out = env;
		}
		else
		{
			// If this point is reached, there were no items
			*ast_out = make_symbol(s_nil);
		}

		return 1;
	}

	return 0;
}

Cell *EVAL (Cell *ast, Cell *env)
{
	if (!env)
	{
		printf("EVAL : error : environment is NULL\n");
		return NULL;
	}

	while (1)
	{
		if (!ast)
			break;

		if (!is_kind(ast, CK_PAIR))
			return eval_ast(ast, env);

		// Empty list -> itself
		if (is_empty_list(ast))
			return ast;

		// Special form evaluation...
		Cell *new_env = NULL;
		Cell *new_ast = NULL;
		if (eval_special(ast->as_pair.first, ast->as_pair.rest, env, &new_ast, &new_env))
		{
			if (!new_ast)
				// Error in special form
				return NULL;

			if (!new_env)
				// Value is final, no tail call
				return new_ast;

			// Tail call loop if new_env is a valid one
			env = new_env;
			ast = new_ast;
			continue;
		}

		// Normal function application...

		// Evaluate all list items
		ast = eval_ast(ast, env);
		if (!ast)
			break;
		Cell *fn = ast->as_pair.first;
		Cell *args = ast->as_pair.rest;

		// Make sure that the arguments are a list
		if (!is_kind(args, CK_PAIR))
			args = make_empty_list();

		// Apply the function
		apply(fn, args, env, &ast, &env);

		// A null environment signals that a tail call is not necessary.
		// Otherwise, continue the loop with the new ast and the new env.
		if (!env)
			return ast;
	}

	printf("EVAL : error : ast is NULL\n");
	return NULL;
}

void PRINT (const Cell *expr)
{
	char buffer[2 * 1024];
	int p_len = pr_str(expr, buffer, sizeof(buffer), 1);
	printf("%.*s\n", p_len, buffer);
}

// Do one read, eval, and print cycle on a string.
void rep (const char *start, int length, Cell *env)
{
	// Allow passing C strings and to automatically get the length
	if (length < 0)
		length = strlen(start);

	Cell * form = READ(start, length);
	if (form)
	{
		Cell * value = EVAL(form, env);
		if (value)
		{
			PRINT(value);
			return;
		}
	}
	PRINT(make_string("no value"));
}

// For adding C functions to the environment
void env_set_native_fn (Cell *env, const char *name, int n_params, Native_fn func)
{
	if (!env || !name || n_params < 0 || !func)
		return;
	Cell *str = insert_string(name);
	env_set(env, make_symbol(str->as_str), make_native_fn(n_params, func));
}

// Returns global environment
Cell *init (int ncells, int nchars)
{
	// Init cell pool...

	// Allocate the cell array
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
		return NULL;

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	for (int i = 0; i < (cell_pool_cap - 1); i++)
		cell_pool[i].as_pair.rest = &cell_pool[i + 1];
	cell_pool[cell_pool_cap - 1].as_pair.rest = NULL;

	// Init strings/chars...
	char_pool = malloc(nchars);
	if (!char_pool)
		return NULL;

	char_free = char_pool;
	char_pool_cap = nchars;

	// Set up the internal string list
	string_list = make_pair(make_string(""), NULL);
	insert_string(s_nil);
	insert_string(s_true);
	insert_string(s_false);
	insert_string(s_def_bang);
	insert_string(s_fn_star);
	insert_string(s_let_star);
	insert_string(s_do);
	insert_string(s_if);

	// Setup the global environment now
	Cell *env = env_create(NULL, NULL, NULL);
	env_set_native_fn(env, "*",           2, fn_mul);
	env_set_native_fn(env, "+",           2, fn_add);
	env_set_native_fn(env, "-",           2, fn_sub);
	env_set_native_fn(env, "/",           2, fn_div);
	env_set_native_fn(env, "<",           2, fn_lt);
	env_set_native_fn(env, "<=",          2, fn_lte);
	env_set_native_fn(env, "=",           2, fn_eq);
	env_set_native_fn(env, ">",           2, fn_gt);
	env_set_native_fn(env, ">=",          2, fn_gte);
	env_set_native_fn(env, "atom",        1, fn_atom);
	env_set_native_fn(env, "atom?",       1, fn_atom_p);
	env_set_native_fn(env, "count",       1, fn_count);
	env_set_native_fn(env, "deref",       1, fn_deref);
	env_set_native_fn(env, "empty?",      1, fn_empty_p);
	env_set_native_fn(env, "eval",        1, fn_eval);
	env_set_native_fn(env, "int?",        1, fn_int_p);
	env_set_native_fn(env, "list",        0, fn_list);
	env_set_native_fn(env, "list?",       1, fn_list_p);
	env_set_native_fn(env, "pr-str",      0, fn_pr_str);
	env_set_native_fn(env, "println",     0, fn_println);
	env_set_native_fn(env, "prn",         0, fn_prn);
	env_set_native_fn(env, "read-string", 1, fn_read_str);
	env_set_native_fn(env, "reset!",      2, fn_reset_bang);
	env_set_native_fn(env, "slurp",       1, fn_slurp);
	env_set_native_fn(env, "str",         0, fn_str);
	env_set_native_fn(env, "swap!",       0, fn_swap_bang);
	env_set_native_fn(env, "pair",        2, fn_pair);
	env_set_native_fn(env, "concat",      0, fn_concat);
	return env;
}

int main (void)
{
	// Initialize the REPL environment symbols
	repl_env = init(2000, 8 * 1024);
	if (!repl_env)
		return 1;

	// Initialization code
	const char *code1 = "[def! load-file [fn* [f] [eval [read-string [str \"[do \" [slurp f] \"\nnil]\n\"]]]]]";
	const char *code2 = "[load-file \"lizp.lizp\"]";
	EVAL(READ(code1, strlen(code1)), repl_env);
	EVAL(READ(code2, strlen(code2)), repl_env);

	// REPL
	char buffer[1024];
	while (1)
	{
		printf("LZP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
			break;
		rep(buffer, strlen(buffer), repl_env);
		printf("\n");
	}

	return 0;
}
