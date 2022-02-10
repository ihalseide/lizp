#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>

// TODO: make def! work in file loading

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
			Cell *(*func)(Cell *);
		} as_native_fn;
		struct
		{
			const Cell *params;
			const Cell *ast;
			Cell *env;
		} as_fn;
	} val;
};

Cell *EVAL (Cell *, Cell *env); 
void PRINT (const Cell *expr);
int pr_str (const Cell *x, char *out, int length, int readable);

// Cell memory (holds the actual cell values):
Cell *cell_pool = NULL;
int cell_pool_cap = 0;

// String memory (holds the actual characters):
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;

// Strings interned
Cell *string_list = NULL;

// Interned strings for special use
const char *s_nil,
	  *s_false,
	  *s_true,
	  *s_def_bang,
	  *s_let_star,
	  *s_if,
	  *s_fn_star,
	  *s_do;

int char_is_symbol (char c)
{
	return (c > ' ') && (c != '"') && (c != '|') && (c != '[') && (c != ']')
		&& (c != '(') && (c != ')') && (c != '{') && (c != '}');
}

// Get a new cell
Cell *cell_get ()
{
	if (!cell_pool)
		return NULL;

	Cell *x = cell_pool->val.as_pair.rest;

	// Remove the cell from the free list
	if (x)
		cell_pool->val.as_pair.rest = x->val.as_pair.rest;

	return x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
		x->kind = k;

	return x;
}

int is_kind (const Cell *x, enum Cell_kind kind)
{
	return x && (x->kind == kind);
}

Cell *make_int (int n)
{
	Cell *x = cell_init(CK_INT);
	if (x)
		x->val.as_int = n;
	return x;
}

Cell *make_native_fn (int n_params, Cell *(*func)(Cell *))
{
	if (n_params < 0 || !func)
		return NULL;

	Cell *x = cell_init(CK_NATIVE_FUNC);
	if (x)
	{
		x->val.as_native_fn.n_params = n_params;
		x->val.as_native_fn.func = func;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(CK_PAIR);
	if (x)
	{
		x->val.as_pair.first = first;
		x->val.as_pair.rest = rest;
	}
	return x;
}

Cell *make_atom (Cell *ref)
{
	Cell *x = cell_init(CK_ATOM);
	if (x)
	{
		x->val.as_atom = ref;
	}
	return x;
}

Cell *make_symbol (const char *str)
{
	Cell *x = cell_init(CK_SYMBOL);
	if (x)
		x->val.as_str = str;
	return x;
}

Cell *make_string (const char *str)
{
	Cell *x = cell_init(CK_STRING);
	if (x)
		x->val.as_str = str;
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
		x->val.as_fn.params = params;
		x->val.as_fn.ast = body;
		x->val.as_fn.env = outer_env;
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, NULL);
}

int is_empty_list (const Cell *x)
{
	return is_kind(x, CK_PAIR) && (x->val.as_pair.first == NULL);
}

int list_length (const Cell *list)
{
	int i;
	for (i = 0; is_kind(list, CK_PAIR) && !is_empty_list(list); i++)
		list = list->val.as_pair.rest;
	return i;
}

// Add string to the char_pool.
// Helper function for use by string_intern and string_intern_c
const char *string_create (const char *start, int length)
{
	// Validate inputs
	if (start == NULL || length < 0)
		return NULL;

	// Check memory space
	if ((char_free - char_pool) >= char_pool_cap)
	{
		fprintf(stderr, "string_create: out of string character memory\n");
		return NULL;
	}

	// Copy string, with null terminator
	char *s = memcpy(char_free, start, length);
	char_free += length;
	*char_free++ = '\0';

	return s;
}

// Use this when creating new strings from short-lived char pointers
// returns a string cell
const char *string_intern (const char *start, int length)
{
	if ((start == NULL) || (length < 0))
		return NULL;

	// Linear search through the string list
	char *p = char_pool;
	while (p < char_free)
	{
		int p_len = strlen(p);

		// Comapre with an internal string, (so return that)
		if (p_len == length && strncmp(p, start, p_len) == 0)
			return p;

		// Next string
		p += p_len + 1;
	}

	// Did not find an internal string, so make new one
	return string_create(start, length);
}

// For C strings
const char *string_intern_c (const char *start)
{
	return string_intern(start, strlen(start));
}

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
	return s1 && (s1->kind == CK_SYMBOL) && s1->val.as_str
		&& s2 && (s2->kind == CK_SYMBOL) && s2->val.as_str
		&& (strcmp(s1->val.as_str, s2->val.as_str) == 0);
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
		if (alist->val.as_pair.first && symbol_eq(alist->val.as_pair.first->val.as_pair.first, sym))
			return alist->val.as_pair.first;

		// Next
		alist = alist->val.as_pair.rest;
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
		if (alist_assoc(sym, env->val.as_pair.first))
			return env;

		// Move on to the outer environment
		env = env->val.as_pair.rest;
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

	// Find the environment which contains the symbol
	Cell *containing_env = env_find(env, sym);
	// Return the slot if environment was found
	if (containing_env)
		return alist_assoc(sym, containing_env->val.as_pair.first);

	// Symbol not found
	printf("env_get : error : symbol undefined\n");
	return NULL;
}

void env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!is_kind(env, CK_PAIR) || !is_kind(sym, CK_SYMBOL))
		return;

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = alist_assoc(sym, env->val.as_pair.first);
	if (slot)
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->val.as_pair.rest = val;
	}
	else
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		slot = make_pair(sym, val);
		// Handle special case of pushing to empty list
		// TODO: move to function for pushing to a list
		if (is_empty_list(env->val.as_pair.first))
			env->val.as_pair.first->val.as_pair.first = slot;
		else
			env->val.as_pair.first = make_pair(slot, env->val.as_pair.first);
	}
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
		Cell *sym = binds->val.as_pair.first;
		Cell *val = exprs->val.as_pair.first;

		// Make sure it only binds symbols
		if (!is_kind(sym, CK_SYMBOL))
		{
			printf("env_create : error : a member of the bindings list is not a symbol\n");
			return NULL;
		}
		env_set(env, sym, val);

		// Next
		binds = binds->val.as_pair.rest;
		exprs = exprs->val.as_pair.rest;
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

int read_string_literal (const char *start, int length, Cell **out)
{
	// Validate inputs
	if (!out)
		return 0;
	if (!start || length <= 0)
	{
		*out = NULL;
		return 0;
	}

	const char *view = start;

	char *write = char_free;

	// Read opening quote
	string_step(&view, &length, 1);

	// Read string contents, with escaping
	while (*view && (*view != '"') && length && (char_free - char_pool) < char_pool_cap)
	{
		char c = *view;
		if (c == '\\')
		{
			string_step(&view, &length, 1);
			c = *view;
			switch (c)
			{
				case 'n': c = '\n'; break;
				case 't': c = '\t'; break;
			}
		}

		*write++ = c;
		string_step(&view, &length, 1);
	}

	if (*view != '"')
	{
		// Error: unexpected end of string
		printf("read_string_literal : error : unexpected end of string\n");
		*out = NULL;
	}
	else if ((char_free - char_pool) >= char_pool_cap)
	{
		printf("read_string_literal : error : not enough room to read string\n");
		*out = NULL;
	}
	else
	{
		// Read closing quote
		string_step(&view, &length, 1);

		// Write null terminator
		*write++ = '\0';

		// Intern string, remove quotes
		*out = make_string(string_intern(char_free, write - char_free));
	}

	// Keep the quotes in the length,
	// because those characters were actually read.
	return view - start;
}

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
	int rem = length;

	// Symbol or number
	while (rem > 0 && char_is_symbol(*p))
		string_step(&p, &rem, 1);

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
			return 0;
		}
		*out = make_int(x);
	}
	else
	{
		// Symbol
		const char *name = string_intern(start, p - start);
		*out = make_symbol(name);
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
		string_step(&view, &rem, read_str(view, rem, &p->val.as_pair.first));

		// Read the rest of the normal elements (don't handle the "dot")
		while ((rem > 0) && (*view != end) && (*view != '|'))
		{
			// Read an element
			Cell *e;
			string_step(&view, &rem, read_str(view, rem, &e));
			if (!e)
				break;

			p->val.as_pair.rest = make_pair(e, NULL);
			p = p->val.as_pair.rest;
		}

		if (*view == '|')
		{
			// Dotted list
			Cell *e;
			string_step(&view, &rem, 1);
			string_step(&view, &rem, read_str(view, rem, &e));
			p->val.as_pair.rest = e;
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
		string_step((const char**)&view, &rem, pr_str(list->val.as_pair.first, view, rem, readable));
		// Next item
		list = list->val.as_pair.rest;
	}

	// Print normal list elements
	while (is_kind(list, CK_PAIR) && !is_empty_list(list))
	{
		string_step((const char**)&view, &rem, print_char(' ', view, rem));
		string_step((const char**)&view, &rem, pr_str(list->val.as_pair.first, view, rem, readable));
		// Next item
		list = list->val.as_pair.rest;
	}

	// If there is a value (except nil) in the final rest slot, then print it dotted
	if (list && !is_empty_list(list) && !(is_kind(list, CK_SYMBOL) && list->val.as_str == s_nil))
	{
		string_step((const char**)&view, &rem, print_string(" | ", view, rem, 0));
		string_step((const char**)&view, &rem, pr_str(list, view, rem, readable));
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(']', view, rem));

	int len = length - rem;
	return len;
}

// Prints form X to output stream
// Returns: number of chars written
int pr_str (const Cell *x, char *out, int length, int readable)
{
	// Validate inputs
	if (!out || !x || length <= 0)
	{
		return 0;
	}

	switch (x->kind)
	{
		case CK_INT:
			return print_int(x->val.as_int, out, length);
		case CK_STRING:
			return print_string(x->val.as_str, out, length, readable);
		case CK_SYMBOL:
			return print_string(x->val.as_str, out, length, 0);
		case CK_PAIR:
			return print_list(x, out, length, readable);
		case CK_FUNC:
			{
				char *view = out;
				string_step((const char**) &view, &length, print_cstr("#<fn", view, length));
				string_step((const char**) &view, &length, print_list(x->val.as_fn.params, view, length, readable));
				string_step((const char**) &view, &length, print_cstr(">", view, length));
				return view - out;
			}
		case CK_NATIVE_FUNC:
			{
				char *view = out;
				string_step((const char**) &view, &length, print_cstr("#<code ", view, length));
				string_step((const char**) &view, &length, print_int(x->val.as_native_fn.n_params, view, length));
				string_step((const char**) &view, &length, print_cstr(">", view, length));
				return view - out;
			}
		case CK_ATOM:
			return print_cstr("#<atom>", out, length);
	}

	// Error: invalid cell kind
	printf("pr_str : error : invalid cell kind\n");
	return 0;
}

Cell *READ (const char *start, int length)
{
	// Validate inputs
	if (!start || (length < 0))
	{
		return NULL;
	}

	Cell *x;
	read_str(start, length, &x);

	return x;
}

// Evaluate each item of list x.
// Does not modify x.
// Returns: new list.
Cell *eval_each (Cell *x, Cell *env)
{
	// Eval the first element
	Cell *y = make_pair(EVAL(x->val.as_pair.first, env), NULL);
	Cell *p_y = y;

	// eval the rest of the elements
	x = x->val.as_pair.rest;
	while (x != NULL && p_y != NULL)
	{
		if (x->kind != CK_PAIR)
		{
			// dotted list
			p_y->val.as_pair.rest = EVAL(x, env);
			break;
		}

		// Fill in next slot of y
		p_y->val.as_pair.rest = make_pair(EVAL(x->val.as_pair.first, env), NULL);

		// next
		x = x->val.as_pair.rest;
		p_y = p_y->val.as_pair.rest;
	}

	return y;
}

int cell_eq (Cell *a, Cell *b)
{
	if (!a || !b)
		return a == b;
	if (a == b)
		return 1;
	if (a->kind != b->kind)
		return 0;
	switch (a->kind)
	{
		case CK_INT:
			return a->val.as_int == b->val.as_int;
		case CK_STRING:
		case CK_SYMBOL:
		case CK_FUNC:
			// TODO: implement
			return 0;
		case CK_NATIVE_FUNC:
			return (a->val.as_native_fn.func == b->val.as_native_fn.func)
				&& (a->val.as_native_fn.n_params == b->val.as_native_fn.n_params);
		case CK_PAIR:
			return cell_eq(a->val.as_pair.first, b->val.as_pair.first)
				&& cell_eq(a->val.as_pair.rest, b->val.as_pair.rest);
		default:
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

// Print to char_free pointer and return length written
// Does do null termination
int print_scratch (Cell *args, char sep, int readable)
{
	// Validate arguments
	if (!args)
		return 0;

	char *p = char_free;
	int len = char_pool_cap - (char_free - char_pool);

	// Print first item with no separator
	if ((len > 1) && is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		string_step((const char**) &p, &len, pr_str(args->val.as_pair.first, p, len, readable));
		args = args->val.as_pair.rest;
	}

	// Print the remaining items with separators
	while ((len > 1) && is_kind(args, CK_PAIR) && !is_empty_list(args))
	{
		if (sep)
			string_step((const char**) &p, &len, print_char(sep, p, len));
		string_step((const char**) &p, &len, pr_str(args->val.as_pair.first, p, len, readable));
		args = args->val.as_pair.rest;
	}
	print_char('\0', p, len);

	return p - char_free;
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
				if (ast->val.as_str == s_nil || ast->val.as_str == s_true || ast->val.as_str == s_false)
					return ast;

				// Get the value out of the environment's slot
				Cell *slot = env_get(env, ast);
				if (slot)
					return slot->val.as_pair.rest;

				printf("eval_ast : error : undefined symbol '%s'\n", ast->val.as_str);
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
Cell *fn_str (Cell *args)
{
	int len = print_scratch(args, 0, 0);
	Cell *s = make_string(char_free);
	char_free += len;
	return s;
}

// [list a ...] -> [a ...] (variadic)
Cell *fn_list (Cell *args)
{
	return args;
}

// [pr-str a b c ...] -> "a b c ..." (prints readably)
Cell *fn_pr_str (Cell *args)
{
	int len = print_scratch(args, ' ', 1);
	Cell *s = make_string(char_free);
	char_free += len;
	return s;
}

// [prn a b c ...] -> nil (prints readably)
Cell *fn_prn (Cell *args)
{
	print_scratch(args, ' ', 1);
	printf("%s", char_free);
	return make_symbol(s_nil);
}

// [println a b c ...] -> nil (prints non-readably)
Cell *fn_println (Cell *args)
{
	print_scratch(args, ' ', 0);
	printf("%s\n", char_free);
	return make_symbol(s_nil);
}

// [deref atom]
Cell *fn_deref (Cell *args)
{
	if (!is_kind(args->val.as_pair.first, CK_ATOM))
		return NULL;
	return args->val.as_pair.first->val.as_atom;
}

// [atom? x] -> bool
Cell *fn_atom_p (Cell *args)
{
	return make_bool_sym(is_kind(args->val.as_pair.first, CK_ATOM));
}

// [atom x] -> #<atom x>
Cell *fn_atom (Cell *args)
{
	return make_atom(args->val.as_pair.first);
}

Cell *fn_eval (Cell *args)
{
	assert(0 && "Not implemented. This function is only needed for its pointer value.");
	return NULL;
}

// (slurp "file name") -> "file contents"
Cell *fn_slurp (Cell *args)
{
	Cell *a = args->val.as_pair.first;

	// Read all contents of the file...
	// Open file
	FILE *f = fopen(a->val.as_str, "r");
	if (!f) // failed
		return make_symbol(s_nil);

	// Get file length
	fseek(f, 0, SEEK_END);
	long fsize = ftell(f);
	fseek(f, 0, SEEK_SET);

	// See if we have enough room for this string data
	if (((char_free + fsize) - char_pool) >= char_pool_cap)
	{
		fclose(f);
		return make_symbol(s_nil);
	}

	// Read the char data and null-terminate it
	fread(char_free, fsize, 1, f);
	fclose(f);
	char_free[fsize] = 0;

	// Move the character allocation pointer
	const char *s = char_free;
	char_free += fsize + 1;

	return make_string(s);
	return NULL;
}

// [read-string "str"] -> any value
Cell *fn_read_str (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *result = NULL;

	if (is_kind(a, CK_STRING))
		read_str(a->val.as_str, strlen(a->val.as_str), &result);

	return result;
}

// [empty? x]
Cell *fn_empty_p (Cell *args)
{
	return make_symbol(is_empty_list(args->val.as_pair.first)? s_true : s_false);
}

// [count list]
Cell *fn_count (Cell *args)
{
	if (!is_kind(args->val.as_pair.first, CK_PAIR))
		return make_symbol(s_nil);
	return make_int(list_length(args->val.as_pair.first));
}

// [list? x]
Cell *fn_list_p (Cell *args)
{
	return make_bool_sym(is_kind(args->val.as_pair.first, CK_PAIR));
}

// [int? x]
Cell *fn_int_p (Cell *args)
{
	return make_bool_sym(is_kind(args->val.as_pair.first, CK_INT));
}


// [reset! atom value] -> value
Cell *fn_reset_bang (Cell *args)
{
	Cell *atom = args->val.as_pair.first;
	Cell *value = args->val.as_pair.rest->val.as_pair.first;

	if (!is_kind(atom, CK_ATOM))
		return NULL;

	atom->val.as_atom = value;
	return value;
}

// [= x y]
Cell *fn_eq (Cell *args)
{
	return make_bool_sym(cell_eq(args->val.as_pair.first,
				args->val.as_pair.rest->val.as_pair.first));
}

// [< n1 n2]
Cell *fn_lt (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_bool_sym(a->val.as_int < b->val.as_int);
}

// [> n1 n2]
Cell *fn_gt (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_bool_sym(a->val.as_int > b->val.as_int);
}

// [<= n1 n2]
Cell *fn_lte (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_bool_sym(a->val.as_int <= b->val.as_int);
}

// [>= n1 n2]
Cell *fn_gte (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_bool_sym(a->val.as_int >= b->val.as_int);
}

// [+ n1 n2]
Cell *fn_add (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_int(a->val.as_int + b->val.as_int);
}

// [- n1 n2]
Cell *fn_sub (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_int(a->val.as_int - b->val.as_int);
}

// [* n1 n2]
Cell *fn_mul (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_int(a->val.as_int * b->val.as_int);
}

// [/ n1 n2]
Cell *fn_div (Cell *args)
{
	Cell *a = args->val.as_pair.first;
	Cell *b = args->val.as_pair.rest->val.as_pair.first;
	if (!is_kind(a, CK_INT) || !is_kind(b, CK_INT))
		return NULL;
	return make_int(a->val.as_int / b->val.as_int);
}

// [swap! atom fn args...] (variadic)
// The atom's val is modified to the result of applying the
// function with the atom's value as the first argument and the
// optionally given function arguments as the rest of the arguments.
// Return's the atom's new atom's
Cell *fn_swap_bang (Cell *args)
{
	assert(0 && "swap! is not implemented yet");
	return NULL;
}

// For tail-call recursion for lisp fn* functions,
// the next ast and env is returned as a pair.
// Native functions return the result value and the same env.
// --> NULL (when error)
// --> (ast . env)
Cell *apply (Cell *fn, Cell *args, Cell *env)
{
	// Validate arguments
	if (!fn || !args || !is_kind(args, CK_PAIR) || !env)
	{
		printf("apply : error : invalid arguments\n");
		return NULL;
	}

	Cell *result_val;
	Cell *result_env;
	switch (fn->kind)
	{
		case CK_NATIVE_FUNC:
			// Built-in native C function
			// Validate arguments
			if (fn->val.as_native_fn.n_params && (list_length(args) != fn->val.as_native_fn.n_params))
			{
				printf("apply : error : length of actual arguments does not match the native function's formal parameters\n");
				return NULL;
			}

			// Call...

			// Special case for eval, because it needs the environment
			// Don't evan call eval because it's a dummy value.
			if (fn->val.as_native_fn.func == fn_eval)
			{
				result_val = args->val.as_pair.first;
				result_env = env;
			}
			else
			{
				result_val = fn->val.as_native_fn.func(args);
				result_env = NULL;
			}
			break;

		case CK_FUNC:
			// Lisp function created by fn* (see comment before the definition of function make_fn)
			result_val = fn->val.as_fn.ast;
			result_env = env_create(fn->val.as_fn.env, fn->val.as_fn.params, args);
			if (!result_env)
			{
				printf("apply : error : length of actual arguments does not match the function's formal parameters\n");
				return NULL;
			}
			break;

		default: // Error: not a function
			printf("apply : error : first item in list is not a function\n");
			return NULL;
	}

	return make_pair(result_val, result_env);
}

Cell *EVAL (Cell *ast, Cell *env)
{
	while (1)
	{
		if (!ast) // Error? ast is invalid
		{
			printf("EVAL : error : ast is NULL\n");
			return NULL;
		}

		if (!is_kind(ast, CK_PAIR))
			return eval_ast(ast, env);

		// Empty list evals to itself [] -> []
		if (is_empty_list(ast))
			return ast;

		// Check for a special form...
		Cell *head = ast->val.as_pair.first;
		if (is_kind(head, CK_SYMBOL))
		{
			// Special forms...

			Cell *args = ast->val.as_pair.rest;

			// Make sure that args is a list
			if (!args)
				args = make_empty_list();

			if (head->val.as_str == s_def_bang)
			{
				// [def! <symbol> value]
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest
						|| args->val.as_pair.first->kind != CK_SYMBOL)
				{
					// Error: invalid args
					printf("def! : error : invalid args\n");
					return NULL;
				}
				Cell *sym = args->val.as_pair.first;
				Cell *val = EVAL(args->val.as_pair.rest->val.as_pair.first, env);
				env_set(env, sym, val);
				val = env_get(env, sym);
				if (val)
					return val->val.as_pair.rest;
				printf("def! : error : cannot define symbol\n");
				return NULL;
			}
			else if (head->val.as_str == s_let_star)
			{
				// (let* <list of symbols and values> expr)
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest)
				{
					// Error: invalid args
					printf("let* : error : invalid args\n");
					return NULL;
				}
				Cell *let_env = env_create(env, NULL, NULL);
				// Go through the bindings list and add the
				// bindings to the environment
				Cell *p = args->val.as_pair.first; // pointer to bindings list
				while (p)
				{
					if (!p->val.as_pair.rest)
					{
						// Error: odd amount of arguments in first list
						printf("let* : error : odd amount of arguments in first list\n");
						return NULL;
					}
					if (!p->val.as_pair.first || p->val.as_pair.first->kind != CK_SYMBOL)
					{
						// Error: even element in bindings list not a symbol
						printf("let* : error : even element in bindings list not a symbol\n");
						return NULL;
					}

					env_set(let_env, p->val.as_pair.first, EVAL(p->val.as_pair.rest->val.as_pair.first, let_env));

					// Next-next
					p = p->val.as_pair.rest->val.as_pair.rest;
				}

				// TODO: discard env?
				env = let_env;
				ast = args->val.as_pair.rest->val.as_pair.first;
				continue;
			}
			else if (head->val.as_str == s_fn_star)
			{
				// (fn* (symbol1 symbol2 ...) expr)
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest)
				{
					// Error: invalid args
					printf("fn* : error : invalid args");
					return NULL;
				}
				// Check that the parameter list is only symbols
				Cell *p = args->val.as_pair.first;
				while (p && !is_empty_list(p))
				{
					if (!p->val.as_pair.first || p->val.as_pair.first->kind != CK_SYMBOL)
					{
						// Error: parameter list must be all symbols
						printf("fn* : error : parameter list must be all symbols\n");
						return NULL;
					}
					if (p->val.as_pair.rest && p->val.as_pair.rest->kind != CK_PAIR)
					{
						// Something other than NULL or a list is next
						printf("fn* : error : parameter list is not a proper list\n");
						return NULL;
					}
					p = p->val.as_pair.rest;
				}
				return make_fn(args->val.as_pair.first, args->val.as_pair.rest->val.as_pair.first, env);
			}
			else if (head->val.as_str == s_if)
			{
				// [if expr true {optional false?}]
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest || (args->val.as_pair.rest->val.as_pair.rest && args->val.as_pair.rest->val.as_pair.rest->val.as_pair.rest))
				{
					// Error: too few or too many arguments
					printf("if : error : too few or too many arguments\n");
					return NULL;
				}
				Cell *cond = EVAL(args->val.as_pair.first, env);
				if (cond && !(cond->kind == CK_SYMBOL && (cond->val.as_str == s_false || cond->val.as_str == s_nil)))
				{
					ast = args->val.as_pair.rest->val.as_pair.first;
					continue;
				}
				if (args->val.as_pair.rest->val.as_pair.rest)
				{
					ast = args->val.as_pair.rest->val.as_pair.rest->val.as_pair.first;
					continue;
				}
				return make_symbol(s_nil);
			}
			else if (head->val.as_str == s_do) // [do exprs...]
			{
				// Evaluate all but the last item
				while (is_kind(args, CK_PAIR) && !is_empty_list(args) && args->val.as_pair.rest)
				{
					EVAL(args->val.as_pair.first, env);
					args = args->val.as_pair.rest;
				}

				// Has a last item, so do a tail call to evaluate that
				if (is_kind(args, CK_PAIR))
				{
					ast = args->val.as_pair.first;
					continue;
				}

				// If this point is reached, there were no items
				return NULL;
			}
		}

		// Normal function application...

		// Evaluate all list items
		ast = eval_ast(ast, env);
		if (!ast)
			return NULL;

		Cell *fn = ast->val.as_pair.first;
		Cell *args = ast->val.as_pair.rest;

		// Make sure that the arguments are a list
		if (!is_kind(args, CK_PAIR))
			args = make_empty_list();

		// Apply the function
		Cell *pair = apply(fn, args, env);
		if (!pair)
			return NULL;

		// Tail call using the pair's values
		// if the pair's environment is valid
		ast = pair->val.as_pair.first;
		env = pair->val.as_pair.rest;
		if (env)
			continue;

		return ast;
	}
}

void PRINT (const Cell *expr)
{
	char buffer[2 * 1024];

	int p_len = pr_str(expr, buffer, sizeof(buffer), 1);

	printf("%.*s", p_len, buffer);
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

void env_set_native_fn (Cell *env, const char *name, int n_params, Cell *(*func)(Cell *))
{
	if (!env || !name || n_params < 0 || !func)
		return;
	env_set(env, make_symbol(string_intern_c(name)), make_native_fn(n_params, func));
}

// Returns global environment
Cell *init (int ncells, int nchars)
{
	// Init cell pool...

	// Allocate the cell array
	cell_pool = malloc(ncells * sizeof(*cell_pool));
	if (!cell_pool)
	{
		return NULL;
	}

	// Set the capacities
	cell_pool_cap = ncells;

	// Link the free cells together in a list
	for (int i = 0; i < (cell_pool_cap - 1); i++)
	{
		cell_pool[i].val.as_pair.rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].val.as_pair.rest = NULL;

	// Init strings/chars...
	char_pool = malloc(nchars);
	if (!char_pool)
	{
		return NULL;
	}

	char_free = char_pool;
	char_pool_cap = nchars;

	// Set up the internal string list with one item,
	// the empty string.
	string_list = make_pair(make_string(""), NULL);

	// Intern important strings
	// Self-evaluating symbols,
	s_nil      = string_intern_c("nil");
	s_true     = string_intern_c("#t");
	s_false    = string_intern_c("#f");
	// Special symbols
	s_def_bang = string_intern_c("def!");
	s_fn_star  = string_intern_c("fn*");
	s_let_star = string_intern_c("let*");
	s_do       = string_intern_c("do");
	s_if       = string_intern_c("if");

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
	return env;
}

int main (void)
{
	// Initialize the REPL environment symbols
	Cell *repl_env = init(2000, 8 * 1024);
	if (!repl_env)
	{
		return 1;
	}

	// Initialization code
	rep("[do [def! load-file [fn* [f] [eval [read-string [str \"[do \" [slurp f] \"\nnil]\"]]]]] nil]", -1, repl_env);

	// Debug:
	PRINT(repl_env);

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
