/*
<!DOCTYPE html><html>

<h1>
LIZP
</h1>

<p>
Lizp is a simple list data serialization "format".

<p>
This source code is written in the way of the C99 standard and should also work with C++.

<p>
The license for the lizp source code is at the end of the file.

<h2>
Usage
</h2>

<p>
You only need to include this header file in your project. You can

<pre>
#include "lizp.h"
</pre>

in any file that needs it. Then in one place, you need to type:

<pre>
#define LIZP_IMPLEMENTATION
#include "lizp.h"
</pre>

<p>
If you want to use lizp as a scripting language and need to enable evaluation,
then you should define:

<pre>
#define LIZP_EVAL
</pre>

And if you want to include some useful macros and functions, then
you should define:

<pre>
#define LIZP_CORE_FUNCTIONS
</pre>

Since the lizp core functions implies that you are doing evaluation,
this also automatically defines LIZP_EVAL,
so you don't need to define it too.

<h2>
Documentation
</h2>

<h3 id=data-types>
Data types
</h3>

<p>
The main data type is the value, which is known as <code>Val_t</code> in the source.
Every value is either a symbol or a list.

<p>
Symbols are null-terminated "C" strings.
Symbols can be enclosed in quotation marks or not. 
Quotation marks are optional for strings that do not use characters that also have a meaning in the lizp syntax,
but for strings which use those characters, the quotation marks are required to include those characters in the
symbol string.
The characters which play a role in the lizp syntax are:
<q>[</q>, <q>]</q>, <q>(</q>, <q>)</q>, `"` (the double quotation mark), <q>\</q>
and whitespace (whitespace is used to separate symbols).
Symbols can additionally be interpretted as more data types if you wish,
but you would have to parse the symbol to convert it into the desired data type.
In this header file, the valAsInteger() function is an example of this.
Some examples are provided below (in reader syntax):
<ol>
<li><code>a</code>
<li><code>1.00</code>
<li><code>_dog_</code>
<li><code>:key</code>
<li><code>"quoted with spaces"</code>
<li><code>"[ a b c ]"</code>
<li><code>"I said \"hi\" to them."</code>
<li><code>%appdata%</code>
</ol>

<p>
Lists, known as <code>List_t</code> in the source, are ordered sequences of 0 or more values.
A null pointer for a <code>List_t</code> is considered to be an empty list.
The syntax for a list is an opening bracket followed by zero or more whitespace-separated values followed by a closing bracket.
Some examples are provided below (in reader syntax):
<ol>
<li><code>[]</code> (empty list)
<li><code>[a]</code> (a list containing only the symbol "a")
<li><code>[1 2 3]</code> (a list containing three numbers (which are symbols))
</ol>

</html>
*/

#ifndef _lizp_h_
#define _lizp_h_

#include <stdbool.h>

// Using Lizp core functions requires evaluation
#ifdef LIZP_CORE_FUNCTIONS
#define LIZP_EVAL
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef enum ValKind
{
    VK_SYMBOL,
    VK_LIST,
} ValKind_t;

typedef struct List List_t;
typedef struct Val Val_t;

// Lists
List_t *listAlloc(unsigned length);
Val_t *listFindFirst(List_t *list, Val_t *item);
Val_t *listGetAssociate(List_t *list, Val_t *key);
Val_t *listItem(List_t *list, unsigned index);
bool listContainsItem(List_t *list, Val_t *item);
unsigned listCountItem(List_t *list, Val_t *item);
unsigned listLength(List_t *list);
void listAppend(List_t **p_list, Val_t item);
void listInsert(List_t **p_list, unsigned index, Val_t value);
void listPrepend(List_t **p_list, Val_t item);
void listRemove(List_t **p_list, unsigned index);

// value assignment
void valSetSymbol(Val_t *p, char *symbol);
void valSetList(Val_t *p, List_t *list);

// value allocation/freeing
Val_t *valCopy(Val_t *p);
Val_t *valCreateInteger(long n);
Val_t *valCreateList(List_t *list);
Val_t *valCreateSymbol(char *string);
Val_t *valCreateSymbolCopy(const char *start, unsigned len);
Val_t *valCreateSymbolStr(const char *string);
void valFreeValue(Val_t *p);
void valFreeValueRec(Val_t *p);

// value type checking
ValKind_t valKind(Val_t *v);
bool valIsEmptyList(Val_t *p);
bool valIsInteger(Val_t *v);
bool valIsList(Val_t *v);
bool valIsSymbol(Val_t *v);

// value utility functions
int valCompare(Val_t *lhs, Val_t *rhs);
bool valIsEqual(Val_t *x, Val_t *y);
long valAsInteger(Val_t *v);
unsigned valListLength(Val_t *list);
Val_t *valListItem(Val_t *list, unsigned index);

// value serialization
char *valWriteToNewString(Val_t *p, bool readable);
unsigned valWriteToBuffer(Val_t *p, char *out, unsigned length, bool readable);
void valWriteToFile(Val_t *v, int readable, FILE *f);

// value de-serialization
unsigned valReadAllFromBuffer(const char *start, unsigned length, List_t **out);
unsigned valReadOneFromBuffer(const char *start, unsigned length, Val_t **out);

#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

// Val: is a symbol string or a List
struct Val
{
    ValKind_t kind;
    union
    {
        char *symbol;
        List_t *list;
    };
};

// List is an array
struct List
{
    unsigned length;
    Val_t values[];
};

// Free value data
void valFreeValue(Val_t *p)
{
    switch (valKind(p))
    {
    case VK_SYMBOL:
        if (p->symbol) { free(p->symbol); }
        break;
    case VK_LIST:
        if (p->list) { free(p->list); }
        break;
    }
}

// Free value data recursively
void valFreeValueRec(Val_t *p)
{
    if (valIsList(p) && !valIsEmptyList(p))
    {
        unsigned len = listLength(p->list);
        for (unsigned i = 0; i < len; i++)
        {
            valFreeValueRec(&p->list->values[i]);
        }
    }
    valFreeValue(p);
}

// Compare values
// Return value:
// returns an integer less than, equal to, or greater than zero depending
// on whether the left-hand-side value is less than, equal to, or greater than the right-hand-side
//   COMPARE:  lhs <> rhs
int valCompare(Val_t *lhs, Val_t *rhs)
{
    if (valIsEqual(lhs, rhs)) { return 0; }
    ValKind_t l_kind = valKind(lhs);
    ValKind_t r_kind = valKind(rhs);
    switch (l_kind)
    {
    case VK_SYMBOL:
        switch (r_kind)
        {
        case VK_SYMBOL:
            // symbol <> symbol
            return strcmp(lhs->symbol, rhs->symbol);
        case VK_LIST:
            // symbol <> list
            return -1;
        }
        break;
    case VK_LIST:
        switch (r_kind)
        {
        case VK_SYMBOL:
            // list <> symbol
            return 1;
        case VK_LIST:
            {
                // list <> list
                unsigned l_len = valListLength(lhs);
                unsigned r_len = valListLength(rhs);
                bool l_is_shorter = (l_len < r_len);
                // compare each element
                unsigned length = l_is_shorter? l_len : r_len;
                for (unsigned i = 0; i < length; i++)
                {
                    Val_t *l_e = &lhs->list->values[i];
                    Val_t *r_e = &rhs->list->values[i];
                    int cmp = valCompare(l_e, r_e);
                    if (cmp == 0) { continue; }
                    return cmp;
                }
                // if the elements so far are equal,
                // then the shorter list comes first
                return l_is_shorter? -1 : 1;
            }
        }
        break;
    }
    return 0;
}

bool valIsEqual(Val_t *x, Val_t *y)
{
    if (x == NULL || y == NULL) { return x == y; }
    if (valIsSymbol(x))
    {
        // compare strings
        return strcmp(x->symbol, y->symbol) == 0;
    }
    if (valIsList(x))
    {
        unsigned length1 = valListLength(x);
        unsigned length2 = valListLength(y);
        if (length1 != length2) { return false; }
        for (unsigned i = 0; i < length1; i++)
        {
            if (!valIsEqual(&x->list->values[i], &y->list->values[i]))
            {
                return false;
            }
        }
        return true;
    }
    return 0;
}

// Get a value's kind a.k.a type
ValKind_t valKind(Val_t *v)
{
    if (!v) { return VK_LIST; }
    return v->kind;
}

// Check if a value is a list
// NULL is also considered an empty list
bool valIsList(Val_t *p)
{
    return p && valKind(p) == VK_LIST;
}

bool valIsEmptyList(Val_t *p)
{
    return valIsList(p) && !p->list;
}

// Check if a value is a symbol
bool valIsSymbol(Val_t *p)
{
    return p && valKind(p) == VK_SYMBOL;
}

//  check if a value is a integer symbol
bool valIsInteger(Val_t *v)
{
    if (!valIsSymbol(v)) { return false; }
    char *end;
    unsigned base = 10;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}

// Dereference and free a value pointer.
// Is useful for storing a value into an array.
// "unwrap"
static Val_t valGet(Val_t *p)
{
    Val_t v = *p;
    free(p);
    return v;
}

void valSetSymbol(Val_t *p, char *symbol)
{
    if (!p) { return; }
    p->kind = VK_SYMBOL;
    p->symbol = symbol;
}

void valSetList(Val_t *p, List_t *list)
{
    if (!p) { return; }
    p->kind = VK_LIST;
    p->list = list;
}

// Make symbol
// NOTE: "s" MUST be a free-able string
// NOTE: does not make a copy of the "s" string
Val_t *valCreateSymbol(char *s)
{
    Val_t *p = malloc(sizeof *p);
    if (!p) { return p; }
    p->kind = VK_SYMBOL;
    p->symbol = s;
    return p;
}

// Make symbol
// - copies buf to take as a name
// - empty string -> null []
Val_t *valCreateSymbolCopy(const char *buf, unsigned len)
{
    if (!buf || len <= 0) { return NULL; }
    char *s = malloc(len + 1);
    memcpy(s, buf, len);
    s[len] = 0;
    return valCreateSymbol(s);
}

// Make symbol by copying the null-terminated `str`
Val_t *valCreateSymbolStr(const char *str)
{
    unsigned len = strlen(str);
    char *s = malloc(len + 1);
    memcpy(s, str, len);
    s[len] = 0;
    return valCreateSymbol(s);
}

// Make a symbol for an integer
Val_t *valCreateInteger(long n)
{
    const char *fmt = "%ld";
    const unsigned sz = snprintf(NULL, 0, fmt, n);
    char buf[sz + 1];
    snprintf(buf, sizeof(buf), fmt, n);
    return valCreateSymbolCopy(buf, sz);
}

// Make a proper list.
// - first: sym or list (null included)
// - rest: list (NULL included)
// NOTE: rest must be NULL or a list Val_t.
Val_t *valCreateList(List_t *list)
{
    Val_t *p = malloc(sizeof *p);
    if (!p) { return p; }
    p->kind = VK_LIST;
    p->list = list;
    return p;
}

// New copy, with no structure-sharing
Val_t *valCopy(Val_t *p)
{
    if (!p) { return p; }
    if (valIsSymbol(p)) { return valCreateSymbolStr(p->symbol); }
    if (!valIsList(p)) {return NULL; }
    // Copy list
    unsigned length = valListLength(p);
    List_t *copy = listAlloc(length);
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = valCopy(&p->list->values[i]);
        copy->values[i] = *e;
        free(e);
    }
    return valCreateList(copy);
}

// String needs quotes?
// Check if a string for a symbol name needs to be quoted
// in order to be printed "readably".
bool stringNeedsQuotes(const char *s)
{
    while (*s)
    {
        switch (*s)
        {
        case '[':
        case ']':
        case '(':
        case ')':
        case '\n':
        case '\t':
        case '"':
        case '\\':
            return 1;
        default:
            if (isspace(*s))
            {
                return 1;
            }
        }
        s++;
    }
    return 0;
}

// Escape a string.
//   (or is this called "un-escaping"?)
// Converts escape sequences into the corresponding real ASCII
// values.
// Modifies the string in-place
unsigned stringEscape(char *str, unsigned len)
{
    if (!str || len <= 0) { return 0; }
    unsigned r = 0; // read index
    unsigned w = 0; // write index
    while (r < len && str[r])
    {
        char c = str[r];
        if (c == '\\')
        {
            r++;
            c = str[r];
            switch (c)
            {
            case 'e': // escape
                c = '\x1b';
                break;
            case 'a':
                c = '\a';
                break;
            case 'b':
                c = '\b';
                break;
            case 'n':
                c = '\n';
                break;
            case 'r':
                c = '\r';
                break;
            case 't':
                c = '\t';
                break;
            default:
                // default result character is itself
                break;
            }
        }
        str[w] = c;
        r++;
        w++;
    }
    str[w] = '\0';
    return w;
}

// Skip space and nested comments within `str`
// Returns the next index into `str`
unsigned stringSkipSpace(const char *str, unsigned len)
{
    unsigned i = 0;
    unsigned level = 0; // nesting level
    while (i < len)
    {
        char c = str[i];
        if (!c) { break; }
        if (c == '(')
        {
            i++;
            level++;
            continue;
        }
        if (level && c == ')')
        {
            i++;
            level--;
            continue;
        }
        if (!level && !isspace(c)) { break; }
        i++;
    }
    return i;
}

// Read a value from the input buffer.
// Return value: the number of CHARACTERS read.
// see also: valReadAllFromBuffer()
unsigned valReadOneFromBuffer(const char *str, unsigned len, Val_t **out)
{
    if (!out || !str || len <= 0) { return 0; }
    unsigned i = 0;
    i += stringSkipSpace(str + i, len - i);
    switch (str[i])
    {
    case '\0': // end of string
    case ']':  // unmatched list
    case '(':  // uncaught comment
    case ')':  // uncaught comment
        *out = NULL;
        return i;
    case '[':
        {
            // begin list
            i++; // consume the '['
            // leading space
            i += stringSkipSpace(str + i, len - i);
            // empty list?
            if (str[i] == ']')
            {
                i++;
                *out = valCreateList(NULL);
                return i;
            }
            // read list items
            List_t *list = NULL;
            while (i < len && str[i] != ']')
            {
                Val_t *e;
                unsigned step = valReadOneFromBuffer(str + i, len - i, &e);
                if (!step || !e) { break; }
                i += step;
                listAppend(&list, valGet(e));
                i += stringSkipSpace(str + i, len - i);
            }
            if (str[i] == ']')
            {
                i++; // consume the ']'
            }
            *out = valCreateList(list);
            return i;
        }
    default:
        {
            // Symbol
            switch (str[i])
            {
            case '\0':
                // No symbol
                if (out) { *out = NULL; }
                return i;
            case '"':
                // Quoted symbol
                {
                    i++;
                    const unsigned j = i;
                    bool done = 0, good = 0;
                    while (!done && i < len)
                    {
                        switch (str[i])
                        {
                        case '\n':
                        case '\0':
                            done = 1;
                            break;
                        case '\\':
                            i++;
                            if (str[i] == '"') { i++; }
                            break;
                        case '"':
                            done = 1;
                            good = 1;
                            i++;
                            break;
                        default:
                            i++;
                            break;
                        }
                    }
                    if (!good)
                    {
                        // invalid
                        *out = NULL;
                        return i;
                    }
                    // Make symbol string with escape sequences processed
                    unsigned len = i - j - 1;
                    if (len == 0)
                    {
                        *out = NULL;
                        return i;
                    }
                    char *str1 = malloc(len + 1);
                    memcpy(str1, str + j, len);
                    str1[len] = 0;
                    unsigned len2 = stringEscape(str1, len);
                    str1 = realloc(str1, len2);
                    *out = valCreateSymbolCopy(str1, len2);
                    free(str1);
                    return i;
                }
            default:
                // Symbol
                {
                    const unsigned j = i;
                    bool done = 0;
                    while (!done && i < len)
                    {
                        char c = str[i];
                        switch (c)
                        {
                        case '\0':
                        case '"':
                        case '[':
                        case ']':
                        case '(':
                        case ')':
                            done = 1;
                            break;
                        default:
                            if (isspace(c))
                            {
                                done = 1;
                                break;
                            }
                            i++;
                            break;
                        }
                    }
                    *out = valCreateSymbolCopy(str + j, i - j);
                    return i;
                }
            }
        }
    }
}

// Read all values from buffer.
// Return value: the number of CHARACTERS read
// see also: valReadOneFromBuffer()
unsigned valReadAllFromBuffer(const char *str, unsigned len, List_t **out)
{
    List_t *list = NULL;
    unsigned i = 0;

    do
    {
        Val_t *item;
        unsigned step = valReadOneFromBuffer(str + i, len - i, &item);
        if (!step) { break; }
        i += step;
        i += stringSkipSpace(str + i, len - i);
        if (!item) { break; }
        listAppend(&list, valGet(item));
    }
    while (i < len && str[i]);

    *out = list;
    return i;
}

// Prints p to the given `out` buffer.
// Does not do null termination.
// If out is NULL, it just calculates the print length
// Returns: number of chars written
unsigned valWriteToBuffer(Val_t *v, char *out, unsigned length, bool readable)
{
    // String output count / index
    unsigned i = 0;
    if (valIsSymbol(v))
    {
        // Symbol
        char *s = v->symbol;
        // It needs to scan the string an extra time before-hand
        // in order to know if the string should be quoted and have
        // escape sequences processed.
        bool quoted = readable && stringNeedsQuotes(s);
        if (quoted)
        {
            // Opening quote
            if (out && i < length) { out[i] = '"'; }
            i++;
        }
        // Contents
        while (*s)
        {
            char c = *s;
            if (quoted)
            {
                // escaping
                bool esc = false;
                switch (c)
                {
                case '\x1b': // escape
                    c = 'e';
                    esc = true;
                    break;
                case '\a':
                    c = 'a';
                    esc = true;
                    break;
                case '\b':
                    c = 'b';
                    esc = true;
                    break;
                case '\n':
                    c = 'n';
                    esc = true;
                    break;
                case '\r':
                    c = 'r';
                    esc = true;
                    break;
                case '\t':
                    c = 't';
                    esc = true;
                    break;
                case '"':
                    c = '"';
                    esc = true;
                    break;
                case '\\':
                    c = '\\';
                    esc = true;
                    break;
                }
                if (esc)
                {
                    if (out && i < length) { out[i] = '\\'; }
                    i++;
                }
            }
            if (out && i < length) { out[i] = c; }
            i++;
            s++;
        }
        if (quoted)
        {
            // Closing quote
            if (out && i < length) { out[i] = '"'; }
            i++;
        }
        return i;
    }

    if (!valIsList(v)) { return i; }

    // List...
    // opening '['
    if (out && i < length) { out[i] = '['; }
    i++; // account for the '['
    unsigned listLength = valListLength(v);
    for (unsigned j = 0; j < listLength; j++)
    {
        // space
        if (j)
        {
            if (out && i < length) { out[i] = ' '; }
            i++; // account for the ' '
        }
        // item
        Val_t e = v->list->values[j];
        if (out)
        {
            i += valWriteToBuffer(&e, out + i, length - i, readable);
        }
        else
        {
            i += valWriteToBuffer(&e, NULL, 0, readable);
        }
    }
    // closing ']'
    if (out && i < length) { out[i] = ']'; }
    i++;
    return i;
}

// Print value to a new string
char *valWriteToNewString(Val_t *v, bool readable)
{
    unsigned len1 = valWriteToBuffer(v, NULL, 0, readable);
    if (len1 <= 0) { return NULL; }
    char *s = malloc(len1 + 1);
    if (!s) { return NULL; }
    unsigned len2 = valWriteToBuffer(v, s, len1, readable);
    if (len1 != len2) { return NULL; } // should not happen unless there is a bug in valWriteToBuffer()
    s[len2] = '\0';
    return s;
}

// Print value to a file
void valWriteToFile(Val_t *v, int readable, FILE *f)
{
    char *s = valWriteToNewString(v, readable);
    fprintf(f, "%s", s);
    free(s);
}

unsigned valListLength(Val_t *v)
{
    return listLength(v->list);
}

// Get list item from value
Val_t *valListItem(Val_t *v, unsigned index)
{
    if (!v) { return NULL; }
    if (!valIsList(v)) { return NULL; }
    return listItem(v->list, index);
}

// Get the length of a list.
// Returns 0 for a non-list value.
unsigned listLength(List_t *l)
{
    if (!l) { return 0; }
    return l->length;
}

// NOTE: for now, it just allocates one more slot in the list
unsigned listSize(unsigned length)
{
    List_t l;
    return sizeof(l) + sizeof(*l.values) * length;
}

List_t *listAlloc(unsigned length)
{
    if (length == 0) { return NULL; }
    List_t *p = malloc(listSize(length));
    if (!p) { return p; }
    p->length = length;
    return p;
}

// Get list item
Val_t *listItem(List_t *list, unsigned index)
{
    if (!list) { return NULL; }
    if (index >= list->length) { return NULL; }
    return &list->values[index];
}

// Prepend an item to the beginning of the list
void listPrepend(List_t **p_list, Val_t item)
{
    listInsert(p_list, 0, item);
}

// Append an item to the end of the list
void listAppend(List_t **p_list, Val_t item)
{
    if (!p_list) { return; }
    unsigned end = listLength(*p_list);
    listInsert(p_list, end, item);
}

// Remove the item at the index from the list
void listRemove(List_t **p_list, unsigned index)
{
    if (!p_list) { return; }
    unsigned length = listLength(*p_list);
    if (index >= length) { return; }
    // move items down an index
    for (unsigned i = length - 1; i > index; i--)
    {
        (*p_list)->values[i - 1] = (*p_list)->values[i];
    }
    List_t *new_p = realloc(*p_list, listSize(length - 1));
    if (!new_p) { return; }
    *p_list = new_p;
    (*p_list)->length--;
}

// Insert a new item into the list at the given index
void listInsert(List_t **p_list, unsigned index, Val_t item)
{
    if (!p_list) { return; }
    unsigned length = listLength(*p_list);
    if (index > length) { return; }
    if (length == 0)
    {
        unsigned new_len = 1;
        List_t *new_p = malloc(listSize(new_len)); 
        new_p->length = new_len;
        new_p->values[0] = item;
        *p_list = new_p;
        return;
    }
    List_t *new_p = realloc(*p_list, listSize(length + 1));
    if (!new_p) { return; }
    *p_list = new_p;
    (*p_list)->length++;
    if (index != length)
    {
        for (unsigned i = length; i > index; i--)
        {
            (*p_list)->values[i] = (*p_list)->values[i - 1];
        }
    }
    (*p_list)->values[index] = item;
}

unsigned listCountItem(List_t *list, Val_t *item)
{
    if (!list) { return 0; }
    if (!item) { return 0; }
    unsigned length = listLength(list);
    unsigned count = 0;
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = &list->values[i];
        if (!valIsEqual(e, item)) { continue; }
        count++;
    }
    return count;
}

bool listContainsItem(List_t *list, Val_t *item)
{
    if (!list) { return false; }
    if (!item) { return false; }
    unsigned length = listLength(list);
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = &list->values[i];
        if (valIsEqual(e, item)) { continue; }
        return true;
    }
    return false;
}

// Return the address of the first item found in the list that
// is equal to the given item
Val_t *listFindFirst(List_t *list, Val_t *item)
{
    if (!list) { return NULL; }
    if (!item) { return NULL; }
    unsigned length = listLength(list);
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = &list->values[i];
        if (valIsEqual(e, item)) { continue; }
        return e;
    }
    return NULL;
}


long valAsInteger(Val_t *v)
{
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}

// Get value right after a symbol in a list
Val_t *listGetAssociate(List_t *list, Val_t *key)
{
    unsigned length = listLength(list);
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = &list->values[i];
        if (!valIsEqual(e, key)) { continue; }
        unsigned next = i + 1;
        if (next >= length) { return NULL; }
        return &list->values[next];
    }
    return NULL;
}

#endif /* LIZP_IMPLEMENTATION */

#ifdef __cplusplus
} /* extern "C" */
#endif

/*
MIT License

Copyright (c) 2022 Izak Nathanael Halseide

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
