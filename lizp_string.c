#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "cell.h"
#include "printer.h"
#include "lizp_string.h"

const char *s_nil = "nil",
	  *s_false    = "#f",
	  *s_true     = "#t",
	  *s_def_bang = "def!",
	  *s_let_star = "let*",
	  *s_if       = "if",
	  *s_fn_star  = "fn*",
	  *s_do       = "do",
	  *s_quote    = "quote";

// String memory (holds the actual characters):
char *char_pool = NULL;
char *char_free = NULL;
int char_pool_cap = 0;

// Strings interned
Cell *string_list = NULL;

// Return 0 upon success
int init_strings (int nchars)
{
	char_pool = malloc(nchars);
	if (!char_pool)
		return 1;

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
	insert_string(s_quote);

	return 0;
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
	if ((len > 1) && is_nonempty_list(args))
	{
		string_step((const char**) &pad, &len, pr_str(args->as_pair.first, pad, len, readable));
		args = args->as_pair.rest;
	}

	// Print the remaining items with separators
	while ((len > 1) && is_nonempty_list(args))
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
int string_step (const char **stream, int *length, int n)
{
	if (n <= *length)
	{
		*stream += n;
		*length -= n;
		return n;
	}
	return 0;
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


