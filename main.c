#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

enum Native_func {
	NF_ADD,    NF_SUB,     NF_MUL,
	NF_DIV,    NF_COUNT,   NF_LIST,
	NF_LIST_P, NF_EMPTY_P, NF_PRN,
	NF_EQ,     NF_LT,      NF_GT,
	NF_LTE,    NF_GTE,     NF_INT_P,
};

enum Cell_kind {
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_FUNC,
	T_NATIVE_FUNC,
	T_PAIR,
};

typedef struct cell Cell;
struct cell {
	enum Cell_kind kind;
	union {
		int as_int;
		enum Native_func as_func;
		const char *as_str;
		struct {
			Cell * first;
			Cell * rest;
		} as_pair;
	} val;
};

// Debug forward declare for everything
void PRINT (Cell *expr);

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
	return (c > ' ') && (c != '.') && (c != '[') && (c != ']')
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

Cell *make_int (int n)
{
	Cell *x = cell_init(T_INT);
	if (x)
		x->val.as_int = n;
	return x;
}

Cell *make_native_fn (enum Native_func id)
{
	Cell *x = cell_init(T_NATIVE_FUNC);
	if (x)
		x->val.as_func = id;
	return x;
}

Cell *make_pair (Cell *first, Cell *rest)
{
	Cell *x = cell_init(T_PAIR);
	if (x)
	{
		x->val.as_pair.first = first;
		x->val.as_pair.rest = rest;
	}
	return x;
}

Cell *make_empty_list ()
{
	return make_pair(NULL, NULL);
}

Cell *make_symbol (const char *str)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
		x->val.as_str = str;
	return x;
}

Cell *make_string (const char *str)
{
	Cell *x = cell_init(T_STRING);
	if (x)
		x->val.as_str = str;
	return x;
}

// A custom lisp function is a really just a list.
// The form of that list is: (params env . body)
Cell *make_fn (Cell *params, Cell *body, Cell *outer_env)
{
	Cell *x = cell_init(T_FUNC);
	if (x)
	{
		x->val.as_pair.first = params;
		x->val.as_pair.rest = make_pair(outer_env, body);
	}
	return x;
}

int is_list (Cell *x)
{
	return x && (x->kind == T_PAIR);
}

int is_empty_list (Cell *x)
{
	return is_list(x) && (x->val.as_pair.first == NULL);
}

int list_length (Cell *list)
{
	int i;
	for (i = 0; list && is_list(list) && !is_empty_list(list); i++)
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
	return s1 && (s1->kind == T_SYMBOL) && s1->val.as_str
		&& s2 && (s2->kind == T_SYMBOL) && s2->val.as_str
		&& (strcmp(s1->val.as_str, s2->val.as_str) == 0);
}

// Find a symbol in an alist.
// An alist is a list of the form ((symbol . value) (symbol . value) ....)
Cell *alist_assoc (const Cell *sym, Cell *alist)
{
	// Validate inputs
	if (!sym || !alist || (sym->kind != T_SYMBOL) || (alist->kind != T_PAIR))
	{
		return NULL;
	}

	// Iterate through the list
	while (alist && !is_empty_list(alist))
	{
		if (alist->val.as_pair.first && symbol_eq(alist->val.as_pair.first->val.as_pair.first, sym))
		{
			// Found a a slot with the symbol, return the slot
			return alist->val.as_pair.first;
		}

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
	if (!env || (env->kind != T_PAIR) || !sym || (sym->kind != T_SYMBOL))
	{
		return NULL;
	}

	// Search up the environment hierarchy
	while (env && is_list(env) && !is_empty_list(env))
	{
		if (alist_assoc(sym, env->val.as_pair.first))
		{
			return env;
		}

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
	if (!env || (env->kind != T_PAIR) || !sym || (sym->kind != T_SYMBOL))
	{
		return NULL;
	}

	// Find the environment which contains the symbol
	Cell *containing_env = env_find(env, sym);
	if (!containing_env)
	{
		// Symbol not found
		printf("env_get : error : symbol undefined\n");
		return NULL;
	}

	// Fetch the symbol from the environment it's in
	return alist_assoc(sym, containing_env->val.as_pair.first);
}

void env_set (Cell *env, Cell *sym, Cell *val)
{
	// Validate inputs.
	if (!env || (env->kind != T_PAIR) || !sym || (sym->kind != T_SYMBOL))
	{
		return;
	}

	// If there is already a symbol defined, change the value,
	// otherwise add the new symbol with the value.
	Cell *slot = alist_assoc(sym, env->val.as_pair.first);
	if (slot == NULL)
	{
		// Symbol undefined.
		// Push the new (symbol . value) pair to the env
		slot = make_pair(sym, val);
		if (is_empty_list(env->val.as_pair.first))
		{
			env->val.as_pair.first->val.as_pair.first = slot;
		}
		else
		{
			env->val.as_pair.first = make_pair(slot, env->val.as_pair.first);
		}
	}
	else
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->val.as_pair.rest = val;
	}
}

// An environment is a list of the form (alist . outer-env).
// Create an environment with each item of the
// "binds" list set to the corresponding item in the "exprs" list.
Cell *env_create (Cell *env_outer, Cell *binds, Cell *exprs)
{
	// Validate args
	if (env_outer && env_outer->kind != T_PAIR)
	{
		printf("env_create : error : invalid outer environment\n");
		return NULL;
	}

	Cell *env = make_pair(make_empty_list(), env_outer);
	if (!env) // Could be NULL
		return NULL;

	// Create the bindings by iterating both lists
	while (binds && exprs && !is_empty_list(binds) && !is_empty_list(exprs))
	{
		// Bind 1 pair
		Cell *sym = binds->val.as_pair.first;
		Cell *val = exprs->val.as_pair.first;
		if (!sym || sym->kind != T_SYMBOL)
		{
			printf("env_create : error : member of the bindings list is not a symbol\n");
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
			printf("read_atom : error : unexpected end of string\n");
			return 0;
		}

		// Intern string, remove quotes
		*out = make_string(string_intern(start + 1, p - start - 2));
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
			const char *name = string_intern(start, p - start);
			if (name == s_nil)
				*out = NULL;
			else
				*out = make_symbol(name);
		}

		return p - start;
	}
}

int read_str (const char *start, int length, Cell **out);

int read_list (const char *start, int length, Cell **out)
{
	const char *view = start;
	int rem = length;

	// Consume the opening paren
	char end;
	switch (*view)
	{
		case '[': end = ']'; break;
		case '(': end = ')'; break;
		case '{': end = '}'; break;
	}
	string_step(&view, &rem, 1);
	string_skip_white(&view, &rem);

	Cell *list = make_empty_list();

	if (*view != end)
	{
		Cell *p = list;

		// Read the first element
		string_step(&view, &rem, read_str(view, rem, &p->val.as_pair.first));

		// Read the rest of the normal elements (don't handle the dot)
		while ((rem > 0) && (*view != end) && (*view != '.'))
		{
			Cell *e;
			string_step(&view, &rem, read_str(view, rem, &e));
			if (!e) break;

			p->val.as_pair.rest = make_pair(e, NULL);
			p = p->val.as_pair.rest;
		}

		if (*view == '.')
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
		printf("read_list: error : unexpected end of input\n");
	}

	return view - start;
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
		case '\0':
			// Null terminator for strings
			// ERROR: unexpected end of input
			printf("read_str : error : unexpected end of input\n");
			*out = NULL;
			return 0;
		case ']':
		case ')':
		case '}':
			// Error, unmatched closing paren
			printf("read_str : error : unmatched closing paren\n");
			*out = NULL;
			return 0;
		case '.':
			// Error, dot should only be inside a list
			printf("read_str : error : dot should only be inside a list\n");
			*out = NULL;
			return 0;
		case '[':
		case '(':
		case '{':
			// Opening paren, for lists
			string_step(&view, &rem, read_list(view, rem, out));
			break;
		default:
			// Read symbol, number, string
			string_step(&view, &rem, read_atom(view, rem, out));
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

// Print out a string cell
// returns number of chars written
int print_string (const char *str, char *out, int length, int quoted)
{
	// Validate inputs
	if (!str || !out || (length < 0))
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
	for (int i = 0; *str && (i < length); i++)
	{
		*p_out++ = *str++;
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

	// Print opening char
	string_step((const char**)&view, &rem, print_char('[', view, rem));

	// Print contents
	while ((rem > 0) && x->val.as_pair.first)
	{
		string_step((const char**)&view, &rem, pr_str(x->val.as_pair.first, view, rem));

		// See if the list continues with more pairs...
		if (!x->val.as_pair.rest)
		{
			break;
		}
		else if (x->val.as_pair.rest->kind == T_PAIR)
		{
			// Next pair in the list
			string_step((const char**)&view, &rem, print_char(' ', view, rem));
			x = x->val.as_pair.rest;
		}
		else
		{
			// Dotted list because the rest of this pair is not a pair
			string_step((const char**)&view, &rem, print_cstr(" . ", view, rem));
			string_step((const char**)&view, &rem, pr_str(x->val.as_pair.rest, view, rem));
			break;
		}
	}

	// Print closing char
	string_step((const char**)&view, &rem, print_char(']', view, rem));

	int len = length - rem;
	return len;
}

// Prints form X to output stream
// Returns: number of chars written
int pr_str (Cell *x, char *out, int length)
{
	// Validate inputs
	if (!out || length <= 0)
	{
		return 0;
	}

	// Debug only, should be nil
	if (!x)
	{
		return print_cstr("nil", out, length);
	}

	switch (x->kind)
	{
		case T_INT:
			return print_int(x->val.as_int, out, length);
		case T_STRING:
			return print_string(x->val.as_str, out, length, 1);
		case T_SYMBOL:
			return print_string(x->val.as_str, out, length, 0);
		case T_PAIR:
			return print_list(x, out, length);
		case T_FUNC:
			return print_cstr("#<fn>", out, length);
		case T_NATIVE_FUNC:
			return print_cstr("#<code>", out, length);
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

Cell *EVAL (Cell *, Cell *env); 

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
		if (x->kind != T_PAIR)
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
		case T_INT:
			return a->val.as_int == b->val.as_int;
		case T_STRING:
		case T_SYMBOL:
		case T_FUNC:
			// TODO
			return 0;
		case T_NATIVE_FUNC:
			return a->val.as_func == b->val.as_func;
		case T_PAIR:
			return cell_eq(a->val.as_pair.first, b->val.as_pair.first)
				&& cell_eq(a->val.as_pair.rest, b->val.as_pair.rest);
		default:
			return 0;
	}
}

Cell *eval_ast (Cell *ast, Cell *env)
{
	if (!ast)
		return NULL;

	switch (ast->kind)
	{
		case T_SYMBOL:
			{
				// Self-evaluating symbols
				if (ast->val.as_str == s_true || ast->val.as_str == s_false)
					return ast;
				// Get the value out of the environment's slot
				Cell *slot = env_get(env, ast);
				if (!slot) // Error: undefined symbol
					return NULL;
				return slot->val.as_pair.rest;
			}
		case T_PAIR:
			return eval_each(ast, env);
		case T_INT:
		case T_STRING:
		case T_FUNC:
		default:
			return ast;
	}
}

// Do a native function which should have 1 args
Cell *apply_native1 (enum Native_func fn, Cell *args)
{
	if (!args || is_empty_list(args) || !args->val.as_pair.first)
	{
		printf("apply : error: invalid args\n");
		return NULL;
	}
	switch (fn)
	{
		case NF_PRN:
			PRINT(args->val.as_pair.first);
			return NULL;
		case NF_EMPTY_P:
			return make_symbol(is_empty_list(args->val.as_pair.first)? s_true : s_false);
		case NF_COUNT:
			if (args->val.as_pair.first->kind != T_PAIR)
				return NULL;
			return make_int(list_length(args->val.as_pair.first));
		case NF_LIST_P:
			return make_symbol((args->val.as_pair.first && args->val.as_pair.first->kind == T_PAIR) ? s_true : s_false);
		case NF_INT_P:
			return make_symbol((args->val.as_pair.first && args->val.as_pair.first->kind == T_INT) ? s_true : s_false);
		default:
			printf("apply_native1 : error : invalid native fn\n");
			return NULL;
	}
}

// Do a native function which should have 2 args
Cell *apply_native2 (enum Native_func fn, Cell *args)
{
	if (!args || is_empty_list(args) || !args->val.as_pair.first || !args->val.as_pair.rest || !args->val.as_pair.rest->val.as_pair.first)
	{
		printf("apply : error: invalid args\n");
		return NULL;
	}
	switch (fn)
	{
		case NF_EQ:
			return make_int(cell_eq(args->val.as_pair.first, args->val.as_pair.rest->val.as_pair.first));
		case NF_LT:
			if (!args->val.as_pair.first->kind == T_INT || !args->val.as_pair.rest->val.as_pair.first->kind == T_INT)
				return NULL;
			return make_int(args->val.as_pair.first->val.as_int < args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_GT:
			if (!args->val.as_pair.first->kind == T_INT || !args->val.as_pair.rest->val.as_pair.first->kind == T_INT)
				return NULL;
			return make_int(args->val.as_pair.first->val.as_int > args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_LTE:
			if (!args->val.as_pair.first->kind == T_INT || !args->val.as_pair.rest->val.as_pair.first->kind == T_INT)
				return NULL;
			return make_int(args->val.as_pair.first->val.as_int <= args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_GTE:
			if (!args->val.as_pair.first->kind == T_INT || !args->val.as_pair.rest->val.as_pair.first->kind == T_INT)
				return NULL;
			return make_int(args->val.as_pair.first->val.as_int >= args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_ADD:
			return make_int(args->val.as_pair.first->val.as_int + args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_SUB:
			return make_int(args->val.as_pair.first->val.as_int - args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_MUL:
			return make_int(args->val.as_pair.first->val.as_int * args->val.as_pair.rest->val.as_pair.first->val.as_int);
		case NF_DIV:
			return make_int(args->val.as_pair.first->val.as_int / args->val.as_pair.rest->val.as_pair.first->val.as_int);
		default:
			printf("apply_native2 : error : invalid native fn\n");
			return NULL;
	}
}

Cell *apply (Cell *head, Cell *args, Cell *env)
{
	if (!head)
	{
		// Error: not a function
		printf("apply : error : not a function\n");
		return NULL;
	}

	if (!args)
		args = make_empty_list();

	switch (head->kind)
	{
		case T_NATIVE_FUNC:
			switch (head->val.as_func)
			{
				case NF_PRN:
				case NF_EMPTY_P:
				case NF_COUNT:
				case NF_LIST_P:
				case NF_INT_P:
					return apply_native1(head->val.as_func, args);
				case NF_EQ:
				case NF_LT:
				case NF_GT:
				case NF_LTE:
				case NF_GTE:
				case NF_ADD:
				case NF_SUB:
				case NF_MUL:
				case NF_DIV:
					return apply_native2(head->val.as_func, args);
				case NF_LIST:
					if (!args)
					{
						return make_empty_list();
					}
					return args;
			}
		case T_FUNC:
			{
				// Lisp function (see comment before the definition of function make_fn)
				Cell *params = head->val.as_pair.first;          // function parameter list
				Cell *closure = head->val.as_pair.rest->val.as_pair.first; // function outer closure environment
				Cell *body = head->val.as_pair.rest->val.as_pair.rest;     // function body
				Cell *fn_env = env_create(closure, params, args);
				return EVAL(body, fn_env);
			}
		default:
			// Error: not a function
			printf("apply : error : not a function\n");
			return NULL;
	}
}

Cell *EVAL (Cell *ast, Cell *env)
{
	while (1)
	{
		if (!ast)
			return ast;

		// Atom is passed to eval_ast
		if (!is_list(ast))
			return eval_ast(ast, env);

		// Empty list evals to itself
		if (is_empty_list(ast))
			return ast;

		Cell *head = ast->val.as_pair.first;
		Cell *args = ast->val.as_pair.rest;
		if (!args)
			args = make_empty_list();

		if (head && head->kind == T_SYMBOL)
		{
			// Special forms
			if (head->val.as_str == s_def_bang) // (def! <symbol> value)
			{
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest
						|| args->val.as_pair.first->kind != T_SYMBOL)
				{
					// Error: invalid args
					printf("def! : error : invalid args");
					return NULL;
				}
				Cell *val = EVAL(args->val.as_pair.rest->val.as_pair.first, env);
				env_set(env, args->val.as_pair.first, val);
				return NULL;
			}
			else if (head->val.as_str == s_let_star) // (let* <list of symbols and values> expr)
			{
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest)
				{
					// Error: invalid args
					printf("let* : error : invalid args");
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
					if (!p->val.as_pair.first || p->val.as_pair.first->kind != T_SYMBOL)
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
			else if (head->val.as_str == s_fn_star) // (fn* (symbol1 symbol2 ...) expr)
			{
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
					if (!p->val.as_pair.first || p->val.as_pair.first->kind != T_SYMBOL)
					{
						// Error: parameter list must be all symbols
						printf("fn* : error : parameter list must be all symbols\n");
						return NULL;
					}
					if (p->val.as_pair.rest && p->val.as_pair.rest->kind != T_PAIR)
					{
						// Something other than NULL or a list is next
						printf("fn* : error : parameter list is not a proper list\n");
						return NULL;
					}
					p = p->val.as_pair.rest;
				}
				return make_fn(args->val.as_pair.first, args->val.as_pair.rest->val.as_pair.first, env);
			}
			else if (head->val.as_str == s_if) // (if expr true {optional false?})
			{
				if (!args || !args->val.as_pair.first || !args->val.as_pair.rest || (args->val.as_pair.rest->val.as_pair.rest && args->val.as_pair.rest->val.as_pair.rest->val.as_pair.rest))
				{
					// Error: too few or too many arguments
					printf("if : error : too few or too many arguments\n");
					return NULL;
				}
				Cell *cond = EVAL(args->val.as_pair.first, env);
				if (cond && !(cond->kind == T_SYMBOL && cond->val.as_str == s_false))
				{
					ast = args->val.as_pair.rest->val.as_pair.first;
					continue;
				}
				if (args->val.as_pair.rest->val.as_pair.rest)
				{
					ast = args->val.as_pair.rest->val.as_pair.rest->val.as_pair.first;
					continue;
				}
				return NULL;
			}
			else if (head->val.as_str == s_do) // (do expr1 expr2 ...)
			{
				// Evaluate all but the last item
				while (args && !is_empty_list(args) && args->kind == T_PAIR && args->val.as_pair.rest)
				{
					EVAL(args->val.as_pair.first, env);
					args = args->val.as_pair.rest;
				}

				// Has a last item, so do a tail call to evaluate that
				if (args && args->kind == T_PAIR)
				{
					ast = args->val.as_pair.first;
					continue;
				}

				// If this point is reached, there were no items
				return NULL;
			}
		}

		// Normal function application
		head = EVAL(head, env);
		args = eval_ast(args, env);
		return apply(head, args, env);
	}
}

void PRINT (Cell *expr)
{
	char buffer[1000];

	int p_len = pr_str(expr, buffer, sizeof(buffer));

	printf("%.*s", p_len, buffer);
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
	// 2 types:
	//   self-evaluating symbols,
	//   and the names of special forms.
	s_nil      = string_intern_c("nil");
	s_true     = string_intern_c("#t");
	s_false    = string_intern_c("#f");

	s_def_bang = string_intern_c("def!");
	s_fn_star  = string_intern_c("fn*");
	s_let_star = string_intern_c("let*");
	s_do       = string_intern_c("do");
	s_if       = string_intern_c("if");

	// Setup the global environment now
	Cell *env = env_create(NULL, NULL, NULL);
	env_set(env, make_symbol(string_intern_c("*")), make_native_fn(NF_MUL));
	env_set(env, make_symbol(string_intern_c("+")), make_native_fn(NF_ADD));
	env_set(env, make_symbol(string_intern_c("-")), make_native_fn(NF_SUB));
	env_set(env, make_symbol(string_intern_c("/")), make_native_fn(NF_DIV));
	env_set(env, make_symbol(string_intern_c("<")), make_native_fn(NF_LT));
	env_set(env, make_symbol(string_intern_c("<=")), make_native_fn(NF_LTE));
	env_set(env, make_symbol(string_intern_c("=")), make_native_fn(NF_EQ));
	env_set(env, make_symbol(string_intern_c(">")), make_native_fn(NF_GT));
	env_set(env, make_symbol(string_intern_c(">=")), make_native_fn(NF_GTE));
	env_set(env, make_symbol(string_intern_c("count")), make_native_fn(NF_COUNT));
	env_set(env, make_symbol(string_intern_c("empty?")), make_native_fn(NF_EMPTY_P));
	env_set(env, make_symbol(string_intern_c("int?")), make_native_fn(NF_INT_P));
	env_set(env, make_symbol(string_intern_c("list")), make_native_fn(NF_LIST));
	env_set(env, make_symbol(string_intern_c("list?")), make_native_fn(NF_LIST_P));
	env_set(env, make_symbol(string_intern_c("prn")), make_native_fn(NF_PRN));
	return env;
}

int main (void)
{
	// Initialize the REPL environment symbols
	Cell *repl_env = init(1024, 2048);
	if (!repl_env)
	{
		return 1;
	}

	// Debug
	printf("environment: ");
	PRINT(repl_env);
	printf("\n");

	// REPL
	char buffer[1024];
	while (1)
	{
		printf("LSP> ");
		if(!fgets(buffer, sizeof(buffer), stdin))
			break;
		rep(buffer, strlen(buffer), repl_env);
		printf("\n");
	}

	return 0;
}
