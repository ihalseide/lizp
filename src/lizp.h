/*
LIZP

    Programming language and linked list data serialization.

    This is written in the way of the C99 standard and should also work
    with C++.

    The license for this is at the end of the file.

Notes:

    FIXME: Currently this program also requires "stb_ds.h".

Usage

    You only need to include this header file in your project. You can

        #include "lizp.h"

    in any file that needs it. Then in one place, you need to type:

        #define LIZP_IMPLEMENTATION
        #include "lizp.h"

    If you want to use lizp as a scripting language and need to enable
    evaluation, then you should define:

        #define LIZP_EVAL

    And if you want to include some useful macros and functions, then
    you should define:

        #define LIZP_CORE_FUNCTIONS

    Since the lizp core functions implies that you are doing evaluation,
    this also automatically defines LIZP_EVAL, so you don't need to
    define it too.

Data types

    The only real data type is the "Value" known as Val_t, which can can
    either by a symbol or a list. Symbols are arbitrary null-terminated
    strings, and lists are linked lists of 0 or more values. The empty
    list is a NULL pointer.

    Symbols can additionally be interpretted as more data types if you wish,
    but you would have to provide the parsing functions to convert a string
    into the desired data type. In this header file, the valAsInteger() function
    is an example of this.

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

struct List;

// Val: is a symbol string or a List
typedef struct Val
{
    ValKind_t kind;
    union
    {
        char *symbol;
        struct List *list;
    };
} Val_t;

// List is an array
typedef struct List
{
    unsigned length;
    Val_t values[];
} List_t;

// Lists
List_t *listAlloc(unsigned length);
unsigned listLength(List_t *list);
void listAppend(List_t **p_list, Val_t item);
void listInsert(List_t **p_list, unsigned index, Val_t value);
void listPrepend(List_t **p_list, Val_t item);
void listRemove(List_t **p_list, unsigned index);
bool listGetItemAfterSymbol(List_t *list, const char *symbol, Val_t **out);

// value allocation/freeing
Val_t *valAlloc(void);
Val_t *valCopy(Val_t *p);
Val_t *valCreateInteger(long n);
Val_t *valCreateList(List_t *list);
Val_t *valCreateSymbol(char *string);
Val_t *valCreateSymbolCopy(const char *start, unsigned len);
Val_t *valCreateSymbolStr(const char *string);
void valFree(Val_t p);
void valFreeRec(Val_t p);

// value type checking
ValKind_t valKind(Val_t *v);
bool valIsEmptyList(Val_t *p);
bool valIsInteger(Val_t *v);
bool valIsList(Val_t *v);
bool valIsSymbol(Val_t *v);

// value utility functions
Val_t *valNthItem(Val_t *v, unsigned n);
//bool argsIsMatchForm(const char *form, Val_t *args, Val_t **err);
bool valIsEqual(Val_t *x, Val_t *y);
long valAsInteger(Val_t *v);
unsigned valListLength(Val_t *list);

// value serialization
unsigned valReadOneFromBuffer(const char *start, unsigned length, Val_t **out);
unsigned valReadAllFromBuffer(const char *start, unsigned length, Val_t **out);
unsigned valWriteToBuffer(Val_t *p, char *out, unsigned length, bool readable);
char *valWriteToNewString(Val_t *p, bool readable);

#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

// Free value
void valFree(Val_t v)
{
    switch (valKind(&v))
    {
    case VK_SYMBOL:
        if (v.symbol) { free(v.symbol); }
        break;
    case VK_LIST:
        if (v.list) { free(v.list); }
        break;
    }
}

// Free value recursively
void valFreeRec(Val_t v)
{
    if (valIsList(&v) && !valIsEmptyList(&v))
    {
        unsigned len = listLength(v.list);
        for (unsigned i = 0; i < len; i++)
        {
            valFreeRec(v.list->values[i]);
        }
    }
    valFree(v);
}

static bool stringEqual(const char *a, const char *b)
{
    if (a == b) { return true; }
    while (*a && *b && *a == *b)
    {
        a++;
        b++;
    }
    return *a == *b;
}

bool valIsEqual(Val_t *x, Val_t *y)
{
    if (x == NULL || y == NULL) { return x == y; }
    if (valIsSymbol(x))
    {
        // compare strings
        return stringEqual(x->symbol, y->symbol);
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
    return valKind(p) == VK_LIST;
}

bool valIsEmptyList(Val_t *p)
{
    return valIsList(p) && !p->list;
}

// Check if a value is a symbol
bool valIsSymbol(Val_t *p)
{
    return valKind(p) == VK_SYMBOL;
}

//  check if a value is a integer symbol
bool valIsInteger(Val_t *v)
{
    if (!valIsSymbol(v)) { return 0; }
    char *end;
    unsigned base = 10;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}

// Dereference and free a value pointer.
// Is useful for storing a value into an array
Val_t valGet(Val_t *p)
{
    Val_t v = *p;
    free(p);
    return v;
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
bool StrNeedsQuotes(const char *s)
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
unsigned EscapeStr(char *str, unsigned len)
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
                case 'a':
                    c = '\a';
                    break;
                case 'b':
                    c = '\b';
                    break;
                case 'e': // escape
                    c = '\x1b';
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
                i += stringSkipSpace(str + i, len - i);
            }
            *out = valCreateList(list);
            return i;
        }
    default:
        {
            // Symbol
            // leading space
            i += stringSkipSpace(str, len);
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
                        // Make escaped symbol string
                        unsigned len = i - j - 1;
                        if (len == 0)
                        {
                            *out = NULL;
                            return i;
                        }
                        char *str1 = malloc(len + 1);
                        memcpy(str1, str + j, len);
                        str1[len] = 0;
                        unsigned len2 = EscapeStr(str1, len);
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
// Return value: the number of VALUES read.
// see also: valReadOneFromBuffer()
unsigned valReadAllFromBuffer(const char *str, unsigned len, Val_t **out)
{
    unsigned i = 0;
    Val_t *val = NULL;

    // Read first item... (may be the only item)
    unsigned read_len = valReadOneFromBuffer(str + i, len - i, &val);
    if (!read_len) { return 0; }
    i += read_len;
    i += stringSkipSpace(str + i, len - i);
    if (i >= len || !str[i])
    {
        *out = val;
        return 1;
    }

    // Read additional items into a list...
    unsigned n; // number of items read
    List_t *list = listAlloc(1);
    list->values[0] = valGet(val);
    for (n = 1; i < len && str[i]; n++)
    {
        Val_t *e;
        read_len = valReadOneFromBuffer(str + i, len - i, &e);
        if (!read_len) { break; }
        i += read_len;
        i += stringSkipSpace(str + i, len - i);
        listAppend(&list, valGet(e));
    }

    *out = valCreateList(list);
    return n;
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
        bool quoted = readable && StrNeedsQuotes(s);
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
                    case '\r':
                        c = 'r';
                        esc = true;
                        break;
                    case '\n':
                        c = 'n';
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

unsigned valListLength(Val_t *v)
{
    return listLength(v->list);
}

// Get the length of a list.
// Returns 0 for a non-list value.
unsigned listLength(List_t *l)
{
    if (!l) { return 0; }
    return l->length;
}

// NOTE: for now, it just allocates one more slot in the list
static unsigned listSize(unsigned length)
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

long valAsInteger(Val_t *v)
{
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}

// Get value right after a symbol in a list
bool listGetItemAfterSymbol(List_t *list, const char *symbolName, Val_t **out)
{
    unsigned length = listLength(list);
    for (unsigned i = 0; i < length; i++)
    {
        Val_t *e = &list->values[i];
        if (valIsSymbol(e) && stringEqual(e->symbol, symbolName))
        {
            *out = e;
            return 1;
        }
    }
    return 0;
}

// Get the Nth item in a list value (0-based index)
Val_t *valNthItem(Val_t *v, unsigned n)
{
    if (!valIsList(v)) { return NULL; }
    List_t *list = v->list;
    unsigned length = listLength(list);
    if (n >= length) { return NULL; }
    return &list->values[n];
}

/*
// Meant to be used by argsIsMatchForm()
static bool isArgMatch(char c, Val_t *arg, Val_t **err)
{
    switch (c)
    {
        case 'v':
            // any value
            return 1;
        case 'l':
            // list
            if (valIsList(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = valCreateSymbolStr("should be a list");
            }
            return 0;
        case 'L':
            // non-empty list
            if (arg && valIsList(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = valCreateSymbolStr("should be a non-empty list");
            }
            return 0;
        case 's':
            // symbol
            if (valIsSymbol(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = valCreateSymbolStr("should be a symbol");
            }
            return 0;
        case 'n':
            // symbol for number/integer
            if (valIsInteger(arg))
            {
                return 1;
            }
            if (err)
            {
                *err = valCreateSymbolStr("should be a symbol for an integer");
            }
            return 0;
        default:
            // error
            return 0;
    }
}

// Match Arguments
// Check if the `args` list matches the given `form`
// If the `args` do not match, then `err` is set to a new value (which can be
//   passed to `valCreateError`)
// Meanings for characters in the `form` string:
// - "v" : any value
// - "l" : a list
// - "s" : a symbol
// - "L" : a non-empty list
// - "n" : an integer symbol (number)
// - "(" : mark the rest of the arguments as optional. must be last
// - "&" : variadic, mark the rest of the arguments as optional and all with the same type of the
//   very next character. must be last
bool argsIsMatchForm(const char *form, Val_t *args, Val_t **err)
{
    unsigned i = 0;
    bool optional = false;
    Val_t *p = args;
    while (form[i] && (optional? (p != NULL) : 1))
    {
        switch (form[i])
        {
        case 'v':
        case 'l':
        case 's':
        case 'L':
        case 'n':
            // types
            if (!p)
            {
                if (err)
                {
                    int n = strcspn(form, "&(");
                    char *arguments = (n == 1)? "argument" : "arguments";
                    List_t *list = listAlloc(3);
                    list->values[0] = valGet(valCreateSymbolStr("not enough arguments: requires at least"));
                    list->values[1] = valGet(valCreateInteger(n));
                    list->values[2] = valGet(valCreateSymbolStr(arguments));
                    *err = valCreateList(list);
                }
                return 0;
            }
            if (!isArgMatch(form[i], p->first, err))
            {
                if (err)
                {
                    // wrap message with more context
                    List_t *list = listAlloc(3);
                    list->values[0] = valGet(valCreateList(valCreateSymbolStr("argument")));
                    list->values[1] = valGet(valCreateInteger(i + 1));
                    list->values[2] = valGet(*err);
                    *err = valCreateList(list);
                }
                return 0;
            }
            p = p->rest;
            i++;
            break;
        case '(':
            // optional marker
            if (optional)
            {
                if (err)
                {
                    *err = valCreateSymbolStr(
                        "`argsIsMatchForm` invalid `form` string: optional marker '(' may only appear once");
                }
                return 0;
            }
            optional = 1;
            i++;
            break;
        case '&':
            // variadic marker
            i++;
            switch (form[i])
            {
            case 'v':
            case 'l':
            case 's':
            case 'L':
            case 'n':
                // the rest of the arguments should match the given type
                while (p)
                {
                    if (!isArgMatch(form[i], p->first, err))
                    {
                        return 0;
                    }
                    p = p->rest;
                }
                i++;
                if (!form[i])
                {
                    return 1;
                }
                if (err)
                {
                    *err = valCreateSymbolStr(
                        "`argsIsMatchForm` invalid `form` string: '&' requires a only directive after it");
                }
                return 1;
            default:
                // invalid char
                if (err)
                {
                    *err = valCreateSymbolStr(
                        "`argsIsMatchForm` invalid `form` string: '&' requires a directive after it");
                }
                return 0;
            }
        default:
            // invalid char
            if (err)
            {
                *err = valCreateSymbolStr(
                    "`argsIsMatchForm` invalid `form` string: requires a directive");
            }
            return 0;
        }
    }
    if (p)
    {
        if (err)
        {
            unsigned n = strlen(form) - (optional? 1 : 0);
            char *arguments = (n == 1) ? "argument" : "arguments";
            *err = valCreateList(valCreateSymbolStr("too many arguments, requires at most"),
                            valCreateList(valCreateInteger(n),
                                     valCreateList(valCreateSymbolStr(arguments),
                                              NULL)));
        }
        return 0;
    }
    return 1;
}
*/

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
