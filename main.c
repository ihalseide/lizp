#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

enum Cell_kind
{
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_FUNC,
	T_PAIR,
	_CELL_KIND_COUNT,
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
		int integer;               // Integer value
		Cell *(*native_fn)(Cell*); // C function value
		Cell *sym;                 // Symbol name value (pointer to string cell)
		struct                     // String value
		{
			const char *s_start;
			int s_length;
		};
		struct                     // Pair/list value
		{
			Cell * p_first;
			Cell * p_rest;
			char p_variant;        // Type of parens used to write the list
		};
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

// Forward-declare
void PRINT (Cell *x);

char char_match (char c)
{
	switch (c)
	{
		case '(': return ')';
		case ')': return '(';
		case '[': return ']';
		case ']': return '[';
		case '{': return '}';
		case '}': return '{';
		case '|': return '|';
		case '"': return '"';
		default: return 0;
	}
}

int char_is_paren (char c)
{
	switch (c)
	{
		case '(': case ')':
		case '[': case ']':
		case '{': case '}':
		case '|':
			return 1;
		default:
			return 0;
	}
}

int char_is_symbol (char c)
{
	return !char_is_paren(c) && !isspace(c) && (c != '.');
}

// Checks two character streams for equality.
// If len == 0, then this is equivalent to strcmp(s1,s2) == 0
// Otherwise, this only returns true if s1 and s2 are equal up to
// the `len`th character.
int stream_eq (const char *s1, int len1, const char *s2, int len2)
{
	// Validate arguments
	if ((s1 == NULL) || (s2 == NULL) || (len1 < 0) || (len2 < 0))
	{
		return 0;
	}

	if (len1 != len2)
	{
		return 0;
	}

	if (s1 == s2)
	{
		return 1;
	}

	// Compare length strings
	for (int i = 0; i < len1; i++)
	{
		if (s1[i] != s2[i])
		{
			return 0;
		}
	}

	return 1;
}

// Removes a cell from the cell pool a.k.a. free list
// Can return NULL
Cell *cell_get ()
{
	if (!cell_pool)
	{
		return NULL;
	}

	Cell *x = cell_pool->p_rest;
	if (x)
	{
		cell_pool->p_rest = x->p_rest;
	}
	return x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
	{
		switch (k)
		{
			case T_INT:
			case T_STRING:
			case T_SYMBOL:
			case T_FUNC:
			case T_PAIR:
				x->kind = k;
				break;
			default:
				// Invalid cell kind
				return NULL;
		}
	}
	return x;
}

Cell *make_fn (Cell *args, Cell *body)
{
	Cell *x = cell_init(T_FUNC);
	if (x)
	{
		x->p_first = args;
		x->p_rest = body;
	}
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(T_PAIR);
	if (x)
	{
		x->p_first = first;
		x->p_rest = rest;
		x->p_variant = '(';
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, &c_nil);
}

int is_list (Cell *x)
{
	return x && (x->kind == T_PAIR);
}

int is_empty_list (Cell *x)
{
	return is_list(x) && (x->p_first == NULL);
}

int list_length (Cell *list)
{
	int i;
	for (i = 0; is_list(list) && !is_empty_list(list); i++)
	{
		list = list->p_rest;
	}
	return i;
}

Cell *make_symbol (Cell *name)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
	{
		x->sym = name;
	}
	return x;
}

Cell *make_string (const char *start, int length)
{
	Cell *x = cell_init(T_STRING);
	if (x)
	{
		x->s_start = start;
		x->s_length = length;
	}
	return x;
}

// Insert a new string into the string list and return it
Cell *string_add (const char *start, int length)
{
	Cell *new_string = make_string(start, length);
	Cell *x = make_pair(new_string, string_list->p_rest);
	string_list->p_rest = x;
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
		return string_list->p_first;
	}

	// Linear search through the string list
	Cell *p = string_list;
	while (p != &c_nil)
	{
		Cell *s = p->p_first;
		if (stream_eq(s->s_start, s->s_length, start, length))
		{
			// Found an internal string, so return that
			return s;
		}

		// Next
		p = p->p_rest;
	}

	// Did not find an internal string, so make new one
	return string_create(start, length);
}

// For interning literal constant C-strings in this source code
Cell *string_intern_cstring (const char *str)
{
	return string_intern(str, strlen(str));
}

// Get a canonical symbol with a given name
// Name should be an canonical interned string!
Cell *symbol_intern (Cell *name)
{
	// Search through the circular symbol list
	Cell *p = symbol_list;
	while (p != &c_nil)
	{
		if (p->p_first->sym == name)
		{
			// Found an internal string, so return that
			return p->p_first;
		}

		// Next
		p = p->p_rest;
	}

	// Not found internally, so create new symbol
	// add to list
	Cell *sym_node = make_pair(make_symbol(name), symbol_list->p_rest);
	symbol_list->p_rest = sym_node;
	return sym_node->p_first;
}

// Inserts a cell back into the free list
void cell_free (Cell *x)
{
	if ((x == NULL) || (cell_pool == NULL))
	{
		return;
	}

	x->p_rest = cell_pool->p_rest;
	cell_pool->p_rest = x;
}

void cell_free_all (Cell *x)
{
	if ((x == NULL))
	{
		return;
	}

	if (x->kind == T_PAIR)
	{
		cell_free_all(x->p_first);
		cell_free_all(x->p_rest);
	}

	cell_free(x);
}

Cell *make_int (int n)
{
	Cell *x = cell_init(T_INT);
	if (x)
	{
		x->integer = n;
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

int symbol_eq (const Cell *s1, const Cell *s2)
{
	// Validate inputs
	if (!s1 || !s2
			|| (s1->kind != T_SYMBOL) || (s2->kind != T_SYMBOL)
			|| !s1->sym || !s2->sym)
	{
		return 0;
	}

	return (s1 == s2)
		|| (s1->sym == s2->sym)
		|| stream_eq(s1->sym->s_start, s1->sym->s_length,
				s2->sym->s_start, s2->sym->s_length);
}

// Env = (alist . outer)
Cell *env_create (Cell *env_outer)
{
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
		if (alist->p_first->p_first == sym)
		{
			// Found a a slot with the symbol, return the slot
			return alist->p_first;
		}

		// Next
		alist = alist->p_rest;
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
	return alist_assoc(sym, env->p_first);
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
		env = env->p_rest;
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
		list->p_first = item;
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
		env->p_first = list_push(make_pair(sym, val), env->p_first);
	}
	else
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->p_rest = val;
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

int parse_int (const char *start, int length, int *out)
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
	*out = n * sign;
	return num_len;
}

int read_atom (const char *start, int length, Cell **out)
{
	const char *p = start;
	int rem = length;

	if (*p == '"')
	{
		// String
		while (rem > 0 && *p != '"')
		{
			string_step(&p, &rem, 1);
		}

		if (*p != '"')
		{
			// Error: unexpected end of string
			exit(99);
		}

		// Intern string
		// (remove quotes)
		*out = string_intern(start + 1, p - start - 2); 
		return p - start - 2;
	}
	else
	{
		// Symbol or number
		while (rem > 0 && char_is_symbol(*p))
		{
			string_step(&p, &rem, 1);
		}

		if (isdigit(*start) || ((length > 1) && (*start == '-') && isdigit(*(start + 1))))
		{
			// Number
			int x;
			parse_int(start, p - start, &x);
			*out = make_int(x);
		}
		else
		{
			// Symbol
			*out = symbol_intern(string_intern(start, p - start));
		}

		return p - start;
	}
}

int read_str (const char *start, int length, Cell **out);

int read_list (const char *start, int length, Cell **out)
{
	const char *view = start;
	int rem = length;

	// Consume the opening character
	char opener = *view;
	char closer = char_match(opener);
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
		(*out)->p_variant = opener;

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
		p->p_rest = make_pair(e, &c_nil);
		p = p->p_rest;
		string_skip_white(&view, &rem);
	}

	// Handle either the optionally dotted end of the list
	int has_dot = 0;

	if (*view == '.')
	{
		// Dotted end of the list:
		has_dot = 1;
		// consume the '.' dot
		string_step(&view, &rem, 1);
		// read what should be the final element
		string_step(&view, &rem, read_str(view, rem, &e));
		p->p_rest = e;
		string_skip_white(&view, &rem);
	}

	if (*view == closer)
	{
		// The actual end of list:
		// consume the final character
		string_step(&view, &rem, 1);
		*out = list;
		(*out)->p_variant = opener;
		int len = length - rem;
		return len;
	}
	else
	{
		// Unexpected end of list, or multiple items after the '.' dot
		if (has_dot)
		{
			*out = string_intern_cstring("read_list : error : expected the form directly after the '.' (dot) to be the final form of the enclosing list");
			return (*out)->s_length;
		}
		else
		{
			*out = string_intern_cstring("read_list : error : unexpected end of list");
			return (*out)->s_length;
		}
	}
}

// Read a form from an input stream/string
// Returns: the number of characters read
int read_str (const char *start, int length, Cell **out)
{
	const char *view = start;
	int rem = length;
	while (rem > 0)
	{
		switch (*view)
		{
			case '\0':
				// Null terminator for strings
				*out = string_intern_cstring("read_str : error : unexpected end of input string");
				return 0;
			case ' ':
			case '\n':
			case '\t':
				string_step(&view, &rem, 1);
				continue;
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
				// Error, unmatched closing paren
				*out = string_intern_cstring("read_str : error : unmatched list closing character");
				return 0;
			case '.':
				// Error, dot should only be inside a list
				*out = string_intern_cstring("read_str : error : unexpected '.' (dot) character");
				return 0;
			default:
				// Read symbol, number, string
				string_step(&view, &rem, read_atom(view, rem, out));
				break;
		}
		break;
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
int print_string (const char *start, int ilength, char *out, int length, int quoted)
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
	char opener = (x->p_variant)? x->p_variant : '(';
	char closer = char_match(opener);

	// Print opening char
	string_step((const char**)&view, &rem, print_char(opener, view, rem));

	// Print contents
	while ((rem > 0) && x->p_first)
	{
		string_step((const char**)&view, &rem, pr_str(x->p_first, view, rem));

		// See if the list continues with more pairs...
		if (x->p_rest == &c_nil)
		{
			break;
		}
		else if (x->p_rest->kind == T_PAIR)
		{
			// Next pair in the list
			string_step((const char**)&view, &rem, print_char(' ', view, rem));
			x = x->p_rest;
		}
		else
		{
			// Dotted list because the rest of this pair is not a pair
			string_step((const char**)&view, &rem, print_cstr(" . ", view, rem));
			string_step((const char**)&view, &rem, pr_str(x->p_rest, view, rem));
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

	switch (x->kind)
	{
		case T_INT:
			return print_int(x->integer, out, length);
		case T_STRING:
			return print_string(x->s_start, x->s_length, out, length, 1);
		case T_SYMBOL:
			{
				Cell *name = x->sym;
				return print_string(name->s_start, name->s_length, out, length, 0);
			}
		case T_FUNC:
			return print_cstr("#<fn>", out, length);
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
	// Eval the first element
	Cell *y = make_pair(EVAL(x->p_first, env), &c_nil);

	// eval the rest of the elements
	Cell *p_y = y;
	Cell *p_x = x->p_rest;
	while (p_x != &c_nil)
	{
		if (p_x->kind != T_PAIR)
		{
			// dotted list
			p_y->p_rest = EVAL(p_x, env);
			break;
		}

		// Fill in next slot of y
		p_y->p_rest = make_pair(EVAL(p_x->p_first, env), &c_nil);

		// next
		p_x = p_x->p_rest;
		p_y = p_y->p_rest;
	}

	// Copy the variant kind
	y->p_variant = x->p_variant;

	return y;
}

Cell *apply (Cell *func, Cell *args, Cell *env)
{
	// Handle the types of functions
	if (func->kind != T_FUNC)
	{
		// Error
		return string_intern_cstring("error : apply : 1st item of expression is not a function");
	}
	// Run lisp function (created by fn* )
	// Check length of formal parameters against actual arguments
	int n_params = list_length(func->p_first);
	int n_args = list_length(args);
	if (n_params != n_args)
	{
		return make_pair(string_intern_cstring("error : apply : arguments do not match formal parameters"),
				make_pair(make_int(n_args),
					make_pair(make_int(n_params), &c_nil)));
	}

	// Bind the arguments to a temporary environment
	Cell *fn_env = env_create(env);
	Cell *p_param = func->p_first;
	Cell *p_arg = args;
	while ((p_arg != NULL) && (p_param != NULL))
	{
		// Bind 1
		env_set(fn_env, p_param->p_first, p_arg->p_first);
		// next args
		p_param = p_param->p_rest;
		p_arg = p_arg->p_rest;
	}

	// Evaluate the body
	Cell *result = EVAL(func->p_rest, fn_env);

	// TODO: discard the environment
	return result;
}

Cell *symbol_lookup (Cell *env, Cell *sym)
{
	Cell *slot = env_get(env, sym);

	if (slot == &c_nil)
	{
		// Symbol undefined.
		// TODO: raise better error
		return string_intern_cstring("error: undefined symbol");
	}

	// Return the slot's value
	return slot->p_rest;
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	switch (ast->kind)
	{
		case T_SYMBOL:
			return symbol_lookup(env, ast);
		case T_PAIR:
			return eval_each(ast, env);
		case T_INT:
		case T_STRING:
		case T_FUNC:
		default:
			return ast;
	}
}

Cell *EVAL (Cell *x, Cell *env)
{
	// C NULL -> Lisp nil
	if (!x)
		return &c_nil;

	// Atom is passed to eval_ast
	if (!is_list(x))
		return eval_ast(x, env);

	// Empty list evals to itself
	if (is_empty_list(x))
		return x;

	// Normal function application
	Cell *e_list = eval_ast(x, env);
	Cell *head = e_list->p_first;
	Cell *args = e_list->p_rest;
	if (args == &c_nil)
		args = make_empty_list();
	return apply(head, args, env);
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

// Returns global environment
Cell *init (int ncells, int nchars)
{
	// Init cell pool

	// Allocate the arrays
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
		cell_pool[i].p_rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].p_rest = &c_nil;
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
	string_list = make_pair(make_string("", 0), &c_nil);

	// Symbols...

	// Setup the global environment now
	Cell *env = env_create(&c_nil);
	return env;
}

int main (int argc, char **argv)
{
	// Initialize the REPL environment symbols
	Cell *repl_env = init(1024, 2048);
	if (!repl_env)
	{
		return 1;
	}

	// REPL
	char buffer[1024];
	while (1)
	{
		printf("LSP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
		{
			break;
		}
		rep(buffer, strlen(buffer), repl_env);
	}

	return 0;
}
