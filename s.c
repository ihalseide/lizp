#include <stdio.h>
#include <stdlib.h>

typedef struct Cell {
	int flag;
	union {
		const char *as_str;
		struct {
			struct Cell *first;
			struct Cell *rest;
		};
	};
} Cell;

// Stream input
const char *str_in = NULL;
const char *p_in = NULL; 
int len_in = 0;

// Stream output
char *str_out = NULL;
char *p_out = NULL;
int cap_out = 0;

// Cell memory:
Cell *cell_pool = NULL;
Cell *cell_free = NULL;
Cell *cell_pool_prev = NULL;
int cell_pool_cap = 0;

// String memory:
char *string_pool = NULL;
char *string_free = NULL;
int string_pool_cap = 0;

void error (const char *msg) {
	fprintf(stdout, "ERROR: %s\n", msg);
}

Cell *cell_alloc () {
	if (cell_pool_cap > 0 && cell_free - cell_pool < cell_pool_cap) {
		return cell_free++;
	}
	else {
		error("out of cell memory");
		return NULL;
	}
}

const char *string_alloc (const char *str, int len) {
	if (str == NULL || len <= 0) {
		// Invalid args
		return NULL;
	}

	// Check memory space
	if (string_pool_cap <= 0 || (string_free - string_pool) >= string_pool_cap) {
		error("out of string memory");
		return NULL;
	}

	// Copy string
	char *res = string_free;
	for (int i = 0; i < len; i++) {
		*string_free++ = str[i];
	}

	// Add string terminator if the given string didn't already have one included
	if (str[len - 1] != '\0') {
		*string_free++ = '\0';
	}

	return res;
}

int init_cells (int n) {
	// Check args
	if (n <= 0) {
		return 0;
	}

	cell_pool = malloc(n * sizeof(*cell_pool));
	if (cell_pool) {
		cell_pool_cap = n;
	}
	cell_free = cell_pool;
	return cell_pool != NULL;
}

int init_strings (int n) {
	// Check args
	if (n <= 0) {
		return 0;
	}

	string_pool = malloc(n * sizeof(*string_pool));
	if (string_pool) {
		string_pool_cap = n;
	}
	string_free = string_pool;
	return string_pool != NULL;
}

int init_pools (int n_cells, int n_chars) {
	return init_cells(n_cells) && init_strings(n_chars);
}

int str_find (const char *s, char x) {
	const char *p = s;
	while (*p != x) {
		p++;
	}
	return p - s;
}

// Can compare length encoded string and c-style strings
int str_eq (const char *s1, const char *s2, int len) {
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

const char *string_intern (const char *str, int len) {
	// Validate arguments
	if (!str || len < 0) {
		return NULL;
	}

	if (len == 0) {
		// Get length of c-strings
		len = str_find(str, 0);
	}

	// Search for a copy of the string in the string pool
	// (dumb linear search)
	char *p = string_pool;
	while (p < string_free) {
		// "p[len] == 0" is true if the string at p has a length of len
		if (p[len] == 0 && str_eq(p, str, len)) {
			return p;
		}
		// find beginning of next string 
		while (p < string_free && *p != 0) {
			p++;
		}
		p++;
	}
	// Not found in string pool, so create new
	return string_alloc(str, len);
}

Cell *cell_str (const char *cstr) {
	Cell *new = cell_alloc();
	if (new != NULL) {
		new->flag = 0;
		new->as_str = cstr;
	}
	return new;
}

Cell *cell_strn (const char *str, int len) {
	return cell_str(string_intern(str, len));
}

Cell *cons (Cell *first, Cell *rest) {
	Cell *new = cell_alloc();
	if (new != NULL) {
		new->flag = 1;
		new->first = first;
		new->rest = rest;
	}
	return new;
}

int read_morep () {
	return p_in && ((p_in - str_in) < len_in) && (*(p_in) != '\0');
}

int read_start (const char *str, int len) {
	str_in = str;
	p_in = str;
	len_in = len;
	return 1;
}

int print_start (char *str, int len) {
	if (str == NULL || len == 0) {
		// Invalid args
		return 0;
	}
	if (str_out) {
		// Already started
		return 0;
	}
	str_out = str;
	p_out = str;
	cap_out = len;
	return 1;
}

// Have room to print?
int print_roomp () {
	return cap_out > 0 && (p_out - str_out) < cap_out;
}

// Print the c-style string x to str buffer.
// Returns: number of chars wriiten.
int print_str (const char *x) {
	const char *start = x;
	while (print_roomp() && *x != '\0') {
		*p_out++ = *x++;
	}
	return x - start;
}

int char_atomp (char x) {
	return ('!' <= x && x <= '~') && (x != '.') && (x != '(') && (x != ')');
}

int char_spacep (char x) {
	return ' ' == x || '\n' == x || '\t' == x;
}

void skip_spaces (void) {
	while (read_morep() && char_spacep(*p_in)) {
		p_in++;
	}
}

Cell *read_expr () {
	while (read_morep()) {
		switch (*p_in) {
			case '\t':
			case '\n':
			case '\r':
			case ' ':
				// Skip whitespace
				break;
			case '(':
				//// reading a list, with dotted pairs supported!
				// Start reading a list
				{
					// consume '(' that started this call
					p_in++;
					skip_spaces();

					// The empty list
					if (*p_in == ')') {
						// Consume the ')'
						p_in++;
						return cons(NULL, NULL);
					}

					// Read first element
					Cell *e;
					e = read_expr();
					skip_spaces();
					if (!e) {
						if (*p_in == '.') {
							error("expected expression(s) before '.'");
						}
						return NULL;
					}

					// Read the rest of the elements
					Cell *list, *p;
					p = list = cons(e, NULL);
					while (read_morep() && *p_in != ')' && *p_in != '.') {
						e = read_expr();
						skip_spaces();
						if (!e) { // Error reading sub-expression
							error("(while reading sub-expression)");
							return NULL;
						}
						p->rest = cons(e, NULL);
						p = p->rest;
					}

					switch (*p_in) {
						case '.':
							// Consume '.'
							p_in++;
							skip_spaces();

							if (*p_in != ')') {
								Cell *e = read_expr();
								if (e) {
									p->rest = e;
									skip_spaces();
									if (*p_in == ')') {
										// Consume ')'
										p_in++;
										return list;
									}
								}
							}
							error("expected exactly 1 expression after '.'");
							return NULL;
						case ')':
							// Consume ')'
							p_in++;
							return list;
						default:
							error("unexpected end of input while reading list");
							return NULL;
					}
				}
			case '.':
				error("unexpected '.'");
				return NULL;
			case ')':
				error("unmatched `)`");
				return NULL;
			default:
				// Atoms
				if (char_atomp(*p_in)) {
					const char *start = p_in;
					while (read_morep() && char_atomp(*p_in)) {
						p_in++;
					}
					return cell_strn(start, p_in - start);
				}
				else {
					error("unexpected symbol");
					return NULL;
				}
		}
		p_in++;
	}
	// Reached end of input
	error("unexpected end of input");
	return NULL;
}

// Print a cell to the buffer given in print_start.
// Returns: success
// Note: passing cyclical constructs will cause infinite loop
int print_expr (Cell *c) {
	if (!c) {
		// Check arguments
		return 0;
	}

	if (!print_roomp()) {
		return 0;
	}

	if (c->flag) {
		// Print pair/cons cell
		print_str("(");
		if (!print_roomp()) { return 0; }

		int res;
		while (c != NULL && c->first != NULL) {
			res = print_expr(c->first);
			if (!res) { return 0; }

			if (c->rest == NULL) {
				break;
			}

			res = print_str(" ");
			if (!res) { return 0; }

			if (c->rest->flag) {
				c = c->rest;
			}
			else {
				// Dotted list
				res = print_str(". ");
				if (!res) { return 0; }

				print_expr(c->rest);
				break;
			}
		}
		return print_str(")") == 1;
	}
	else {
		// Print atom
		return print_str(c->as_str);
	}
} 

int print_finish () {
	if (p_out && print_roomp()) {
		*p_out++ = '\0';
		p_out = NULL;
		str_out = NULL;
		cap_out = 0;
		return 1;
	}
	else {
		p_out = NULL;
		str_out = NULL;
		cap_out = 0;
		return 0;
	}
}

/* (unused)
int equivalent (Cell *a, Cell *b) {
	if (a == b) {
		// Same pointer or both null
		return 1;
	}
	if (a == NULL || b == NULL) {
		// Only one is null
		return 0;
	}
	if (a->flag != b->flag) {
		// Different types
		return 0;
	}
	if (a->flag) {
		// Recursive comparison of lists
		return equivalent(a->first, b->first)
			&& equivalent(a->rest, b->rest);
	}
	else {
		// Compare string content
		return str_eq(a->as_str, b->as_str, 0);
	}
}
*/

int main (void) {
	// Read the input
	char prog[4000];
	if(!fread(prog, 1, sizeof(prog), stdin)) {
		error("failed to read from stdin");
		return 1;
	}
	init_pools(2000, 2000);
	read_start(prog, str_find(prog, '\0'));
	Cell *c = read_expr();
	if (!c) {
		error("reading input string failed");
		return 1;
	}

	// Print to buffer
	char buf[2000];
	print_start(buf, sizeof(buf));
	int status = print_expr(c);
	if (status) {
		print_finish();
	}
	if (!status) {
		error("failed to print");
	}

	// Write buffer
	char *s = buf;
	while (*s) {
		putchar(*s);
		s++;
	}
	putchar('\n');

	return 0;
}

