#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

// Pairs & Lists:
// Pair = (first . rest)
// Empty list = () = (NULL . nil)
// List = (cell . List)
//      | (cell . nil)

enum Cell_kind
{
	T_INT,
	T_STRING,
	T_SYMBOL,
	T_FUNC,
	T_NATIVE_FUNC,
	T_PAIR,
};

enum Native_func
{
	NF_ADD,
	NF_SUB,
	NF_MUL,
	NF_DIV,
};

enum Special_op
{
	SO_DEF_BANG,
	SO_LET_STAR,
	SO_IF,
	SO_FN_STAR,
	SO_DO,
};

typedef struct cell Cell;
struct cell
{
	enum Cell_kind kind;          // Type of cell
	union
	{
		int integer;              // Integer value
		enum Native_func func;    // Native function id
		enum Special_op spec;     // Special form id
		const char *s_ptr;        // Symbol/string value
		struct                    // Pair/list value
		{
			Cell * p_first;
			Cell * p_rest;
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

int char_is_symbol (char c)
{
	return c
		&& !isspace(c)
		&& (c != '.')
		&& (c != '[') && (c != ']')
		&& (c != '(') && (c != ')')
		&& (c != '{') && (c != '}');
}

// Get a new cell
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
		// Remove the cell from the free list
		cell_pool->p_rest = x->p_rest;
	}

	return x;
}

Cell *cell_init (enum Cell_kind k)
{
	Cell *x = cell_get();
	if (x)
	{
		x->kind = k;
	}
	return x;
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

Cell *make_spec (enum Special_op id)
{
	Cell *x = cell_init(T_SYMBOL);
	if (x)
	{
		x->spec = id;
	}
	return x;
}

Cell *make_native_fn (enum Native_func id)
{
	Cell *x = cell_init(T_NATIVE_FUNC);
	if (x)
	{
		x->func = id;
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
	{
		x->s_ptr = str;
	}
	return x;
}

Cell *make_string (const char *str)
{
	Cell *x = cell_init(T_STRING);
	if (x)
	{
		x->s_ptr = str;
	}
	return x;
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

// Add string to the char_pool.
// Helper function for use by string_intern and string_intern_cstring.
const char *string_create (const char *start, int length)
{
	// Validate inputs
	if (start == NULL || length < 0)
	{
		return NULL;
	}

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
	{
		return NULL;
	}

	// Linear search through the string list
	char *p = char_pool;
	while (p < char_free)
	{
		if (strncmp(p, start, length) == 0)
		{
			// Found an internal string, so return that
			return p;
		}

		// Next string
		while (p < char_free && *p)
		{
			p++;
		}
		p++;
	}

	// Did not find an internal string, so make new one
	return string_create(start, length);
}

// For C strings
const char *string_intern_c (const char *start)
{
	return string_intern(start, strlen(start));
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
	return s1 && (s1->kind == T_SYMBOL) && s1->s_ptr
		&& s2 && (s2->kind == T_SYMBOL) && s2->s_ptr
		&& (strcmp(s1->s_ptr, s2->s_ptr) == 0);
}

// Env = (alist . outer)
Cell *env_create (Cell *env_outer)
{
	return make_pair(make_empty_list(), env_outer);
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
		if (alist->p_first && symbol_eq(alist->p_first->p_first, sym))
		{
			// Found a a slot with the symbol, return the slot
			return alist->p_first;
		}

		// Next
		alist = alist->p_rest;
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
		if (alist_assoc(sym, env->p_first))
		{
			return env;
		}

		// Move on to the outer environment
		env = env->p_rest;
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
		return NULL;
	}

	// Fetch the symbol from the environment it's in
	return alist_assoc(sym, containing_env->p_first);
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
	Cell *slot = alist_assoc(sym, env->p_first);
	if (slot == NULL)
	{
		// Symbol undefined. (or error)
		// Push the new (symbol . value) pair to the env
		slot = make_pair(sym, val);
		if (is_empty_list(env->p_first))
		{
			env->p_first->p_first = slot;
		}
		else
		{
			env->p_first = make_pair(slot, env->p_first);
		}
	}
	else
	{
		// Symbol already defined.
		// Change the value of the already present slot.
		slot->p_rest = val;
	}
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
			*out = make_symbol(string_intern(start, p - start));
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
		string_step(&view, &rem, read_str(view, rem, &p->p_first));

		// Read the rest of the normal elements (don't handle the dot)
		while ((rem > 0) && (*view != end) && (*view != '.'))
		{
			Cell *e;
			string_step(&view, &rem, read_str(view, rem, &e));
			if (!e) break;

			p->p_rest = make_pair(e, NULL);
			p = p->p_rest;
		}

		if (*view == '.')
		{
			// Dotted list
			Cell *e;
			string_step(&view, &rem, 1);
			string_step(&view, &rem, read_str(view, rem, &e));
			p->p_rest = e;
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
			*out = NULL;
			return 0;
		case ']':
		case ')':
		case '}':
			// Error, unmatched closing paren
			*out = NULL;
			return 0;
		case '.':
			// Error, dot should only be inside a list
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
	while ((rem > 0) && x->p_first)
	{
		string_step((const char**)&view, &rem, pr_str(x->p_first, view, rem));

		// See if the list continues with more pairs...
		if (!x->p_rest)
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
		return print_cstr("NULL", out, length);
	}

	switch (x->kind)
	{
		case T_INT:
			return print_int(x->integer, out, length);
		case T_STRING:
			return print_string(x->s_ptr, out, length, 1);
		case T_SYMBOL:
			return print_string(x->s_ptr, out, length, 0);
		case T_PAIR:
			return print_list(x, out, length);
		case T_FUNC:
			return print_cstr("#<fn>", out, length);
		case T_NATIVE_FUNC:
			return print_cstr("#<code>", out, length);
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
	Cell *y = make_pair(EVAL(x->p_first, env), NULL);
	Cell *p_y = y;

	// eval the rest of the elements
	x = x->p_rest;
	while (x != NULL && p_y != NULL)
	{
		if (x->kind != T_PAIR)
		{
			// dotted list
			p_y->p_rest = EVAL(x, env);
			break;
		}

		// Fill in next slot of y
		p_y->p_rest = make_pair(EVAL(x->p_first, env), NULL);

		// next
		x = x->p_rest;
		p_y = p_y->p_rest;
	}

	return y;
}

Cell *symbol_lookup (Cell *env, Cell *sym)
{
	Cell *slot = env_get(env, sym);

	if (!slot)
	{
		// Error: Symbol undefined.
		return NULL;
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

Cell *apply (Cell *head, Cell *args, Cell *env)
{
	if (!head)
	{
		// Error: not a function
		return NULL;
	}

	switch (head->kind)
	{
		case T_NATIVE_FUNC:
			switch (head->func)
			{
				case NF_ADD:
					if (!args || !args->p_first || !args->p_rest || !args->p_rest->p_first)
					{
						// Error: invalid args
						return NULL;
					}
					return make_int(args->p_first->integer + args->p_rest->p_first->integer);
				case NF_SUB:
					if (!args || !args->p_first || !args->p_rest || !args->p_rest->p_first)
					{
						// Error: invalid args
						return NULL;
					}
					return make_int(args->p_first->integer - args->p_rest->p_first->integer);
				case NF_MUL:
					if (!args || !args->p_first || !args->p_rest || !args->p_rest->p_first)
					{
						// Error: invalid args
						return NULL;
					}
					return make_int(args->p_first->integer * args->p_rest->p_first->integer);
				case NF_DIV:
					if (!args || !args->p_first || !args->p_rest || !args->p_rest->p_first)
					{
						// Error: invalid args
						return NULL;
					}
					return make_int(args->p_first->integer / args->p_rest->p_first->integer);
			}
		case T_FUNC:
			{
				// Lisp function...
				Cell *fn_env = env_create(env);
				Cell *params = head->p_first; // function parameter list
				while (args && params)
				{
					env_set(fn_env, params->p_first, args->p_first);
					args = args->p_rest;
					params = params->p_rest;
				}
				if (args)
				{
					// Error: function given too many arguments
					return NULL;
				}
				if (params)
				{
					// Error: function not given enough arguments
					return NULL;
				}
				Cell *result = EVAL(head->p_rest, fn_env);
				// TODO: discard the fn_env
				return result;
			}
		default:
			// Error: not a function
			return NULL;
	}
}

Cell *EVAL (Cell *x, Cell *env)
{
	// C NULL -> Lisp nil
	if (!x)
		return x;

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
	return apply(head, args, env);
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
		cell_pool[i].p_rest = &cell_pool[i + 1];
	}
	cell_pool[cell_pool_cap - 1].p_rest = NULL;

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

	// Symbols...

	// Setup the global environment now
	Cell *env = env_create(NULL);
	env_set(env, make_symbol(string_intern_c("+")), make_native_fn(NF_ADD));
	env_set(env, make_symbol(string_intern_c("-")), make_native_fn(NF_SUB));
	env_set(env, make_symbol(string_intern_c("*")), make_native_fn(NF_MUL));
	env_set(env, make_symbol(string_intern_c("/")), make_native_fn(NF_DIV));
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
