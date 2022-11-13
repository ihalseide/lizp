/*
LIZP

    Programming language and linked list data serialization.

    This is written in the way of the C99 standard and should also work
    with C++.

    The license for this is at the end of the file.

Notes:

    None

Usage

    You only need to include this header file in your project. You can

        #include "lizp.h"

    in any file that needs it. Then in one place, you need to type:

        #define LIZP_IMPLEMENTATION
        #include "lizp.h"

Data types

    The only real data type is the "Value" known as Val, which can can
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
#include <stddef.h>


struct Val;


typedef struct Val *LizpFunc(struct Val *args);
typedef struct Val *LizpMacro(struct Val *args, struct Val *env);


typedef enum ValKind {
    VK_FREE = 0,
    VK_SYMBOL,
    VK_LIST,
    VK_FUNC,
    VK_MACRO,
} ValKind;


typedef struct Val {
    ValKind kind;
    union {
        char *symbol;
        LizpFunc *func;
        LizpMacro *macro;
        struct {
            struct Val *first;
            struct Val *rest;
        };
    };
} Val;


// memory management
Val *valAlloc();
Val *valAllocKind(ValKind k);
Val *valCopy(Val *p);
void valFree(Val *p);
void valFreeRec(Val *p);

// value creation
Val *valCreateInteger(long n);
Val *valCreateList(Val *first, Val *rest);
Val *valCreateSymbol(char *string);
Val *valCreateSymbolCopy(const char *start, unsigned len);
Val *valCreateSymbolStr(const char *string);

// value type checking
ValKind valKind(Val *v);
bool valIsInteger(Val *v);
bool valIsList(Val *v);
bool valIsSymbol(Val *v);
bool valIsFunc(Val *v);
bool valIsMacro(Val *v);

// value utility functions
bool valGetListItemAfterSymbol(Val *list, const char *symbol, Val **out);
Val *valGetListIndex(Val *list, size_t symbol);
bool valIsEqual(const Val *x, const Val *y);
long valAsInteger(const Val *v);
unsigned valListLength(const Val *l);
bool valListLengthIsWithin(const Val *l, unsigned min, unsigned max);
bool valListLengthIsMoreThan(const Val *l, unsigned n);
bool valListLengthIsLessThan(const Val *l, unsigned n);

// value serialization
unsigned valReadOneFromBuffer(const char *start, unsigned length, Val **out);
unsigned valReadAllFromBuffer(const char *start, unsigned length, Val **out);
unsigned valWriteToBuffer(Val *p, char *out, unsigned length, bool readable);
char *valWriteToNewString(Val *p, bool readable);

Val *valCreateTrue(void);
Val *valCreateError(Val *rest);
Val *valCreateErrorMessage(const char *msg);
Val *valCreateFalse(void);

bool valIsError(Val *v);
bool valIsLambda(Val *v);
bool valIsTrue(Val *v);

Val *evaluate(Val *ast, Val *env);
Val *evaluateList(Val *list, Val *env);
bool EnvGet(Val *env, Val *key, Val **out);
bool EnvSet(Val *env, Val *key, Val *val);
bool EnvSetFunc(Val *env, const char *name, LizpFunc *func);
bool EnvSetMacro(Val *env, const char *name, LizpMacro *macro);
bool EnvSetSym(Val *env, const char *symbol, Val *val);
void EnvPop(Val *env);
void EnvPush(Val *env);

void lizpRegisterCore(Val *env);
Val *reverse_func(Val *args);    // [reverse list] reverse a list
Val *concat_func(Val *args);     // [concat list.1 (list.N)...] concatenate lists together
Val *join_func(Val *args);       // [join separator (list)...] join together each list with the separator list in between
Val *without_func(Val *args);    // [without item list] remove all occurrences of item from the list
Val *replace_func(Val *args);    // [replace item1 item2 list] replace all occurrences of item1 in list with item2
Val *replace1_func(Val *args);   // [replaceN item1 item2 list n] replace up to n of item1 with item2 in list
Val *replaceI_func(Val *args);   // [replaceI index item list] replace element in list at index with item
Val *zip_func(Val *args);        // [zip list.1 (list.N)...]
Val *append_func(Val *args);     // [append val list]
Val *prepend_func(Val *args);    // [prepend val list]
Val *print_func(Val *args);      // [print (v)...]
Val *plus_func(Val *args);       // [+ integers...] sum
Val *multiply_func(Val *args);   // [* integers...] product
Val *subtract_func(Val *args);   // [- x (y)] subtraction
Val *divide_func(Val *args);     // [/ x y] division
Val *mod_func(Val *args);        // [% x y] modulo
Val *equal_func(Val *args);      // [= x y (expr)...] check equality
Val *not_func(Val *args);        // [not expr] boolean not
Val *symbol_q_func(Val *args);   // [symbol? val] check if value is a symbol
Val *integer_q_func(Val *args);  // [integer? val] check if value is a integer symbol
Val *list_q_func(Val *args);     // [list? val] check if value is a list
Val *empty_q_func(Val *args);    // [empty? val] check if value is a the empty list
Val *nth_func(Val *args);        // [nth index list] get the nth item in a list
Val *list_func(Val *args);       // [list (val)...] create list from arguments (variadic)
Val *length_func(Val *args);     // [length list]
Val *lambda_q_func(Val *args);   // [lambda? v]
Val *function_q_func(Val *args); // [function? v]
Val *native_q_func(Val *args);   // [native? v]
Val *increasing_func(Val *args); // [<= x y (expr)...] check number order
Val *decreasing_func(Val *args); // [>= x y (expr)...] check number order
Val *strictly_increasing_func(Val *args);   // [< x y (expr)...] check number order
Val *strictly_decreasing_func(Val *args);   // [> x y (expr)...] check number order
Val *chars_func(Val *args);      // [chars sym] -> list
Val *symbol_func(Val *args);     // [symbol list] -> symbol
Val *member_q_func(Val *args);   // [member? item list] -> boolean value
Val *count_func(Val *args);      // [count item list] -> integer symbol
Val *position_func(Val *args);   // [position item list] -> integer symbol
Val *slice_func(Val *args);      // [slice list start (end)] gets a sublist "slice" inclusive of start and end

// macros
Val *quote_func(Val *args, Val *env);   // [quote expr]
Val *if_func(Val *args, Val *env);      // [if condition consequence alternative]
Val *cond_func(Val *args, Val *env);    // [cond condition1 consequence1 condition2 consequence2 ...]
Val *do_func(Val *args, Val *env);      // [do expr ...]
Val *lambda_func(Val *args, Val *env);  // [lambda [args ...] body-expr]
Val *and_func(Val *args, Val *env);     // [and expr1 expr2 ...]
Val *or_func(Val *args, Val *env);      // [or expr1 expr2 ...]
Val *let_func(Val *args, Val *env);     // [let [sym1 expr1 sym2 expr2 ...] body-expr]

#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // for snprintf


static char *stringCopy(const char *buf, unsigned len) {
    if (!buf) { return NULL; }
    char *s = malloc(len + 1);
    if (s) {
        memcpy(s, buf, len);
        s[len] = 0;
    }
    return s;
}


static Val *pool;


// Allocate a new value
// Potential problem: memory use currently cannot shrink
Val *valAlloc()
{
    if (!pool) {
        // Create another free list of things linked together
        const int count = 1000;
        pool = malloc(count * sizeof(*pool));
        for (Val *p = pool; p < pool + count - 1; p++) {
            p->rest = p + 1;
        }
        pool[count - 1].rest = NULL;
    }

    Val *p = pool;
    pool = pool->rest;
    return p;
}


Val *valAllocKind(ValKind k)
{
    Val *p = valAlloc();
    if (p) { p->kind = k; }
    return p;
}


// Free value
void valFree(Val *p)
{
    if (!p) { return; }
    if (valIsSymbol(p) && p->symbol) { free(p->symbol); }
    p->kind = VK_FREE;
    p->rest = pool;
    pool = p;
}


// Free value recursively
void valFreeRec(Val *v)
{
    if (!v) { return; }
    if (valIsSymbol(v))
    {
        // Symbol
        valFree(v);
        return;
    }
    if (!valIsList(v)) { return; }
    // List
    Val *p = v;
    Val *n;
    while (p && valIsList(p))
    {
        valFreeRec(p->first);
        n = p->rest;
        valFree(p);
        p = n;
    }
    return;
}


bool valIsEqual(const Val *x, const Val *y)
{
    if (x == NULL || y == NULL) { return x == y; }
    if (valIsSymbol(x))
    {
        // Symbol equality
        char *a = x->symbol;
        char *b = y->symbol;
        while (*a && *b && *a == *b)
        {
            a++;
            b++;
        }
        return *a == *b;
    }
    if (valIsList(x))
    {
        // List equality
        Val *px = x, *py = y;
        while (px && valIsList(px) && py && valIsList(py))
        {
            if (!valIsEqual(px->first, py->first)) { break; }
            px = px->rest;
            py = py->rest;
        }
        return px == NULL && py == NULL;
    }
    return 0;
}


// Get a value's kind a.k.a type
ValKind valKind(Val *v)
{
    if (!v) { return VK_LIST; }
    return v->kind;
}


// Check if a value is a list
// NULL is also considered an empty list
bool valIsList(Val *p) { return valKind(p) == VK_LIST; }


// Check if a value is a symbol
bool valIsSymbol(Val *p) { return valKind(p) == VK_SYMBOL; }


bool valIsFunc(Val *v) { return v && valKind(v) == VK_FUNC; }


bool valIsMacro(Val *v) { return v && valKind(v) == VK_MACRO; }


//  check if a value is a integer symbol
bool valIsInteger(Val *v)
{
    if (!valIsSymbol(v)) { return 0; }
    unsigned base = 10;
    char *end;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}


// Make symbol
// NOTE: "s" MUST be a free-able string
// NOTE: does not make a copy of the "s" string
Val *valCreateSymbol(char *s)
{
    Val *p = valAllocKind(VK_SYMBOL);
    if (p) { p->symbol = s; }
    return p;
}


// Make symbol
// - copies buf to take as a name
// - empty string -> null []
Val *valCreateSymbolCopy(const char *buf, unsigned len)
{
    return valCreateSymbol(stringCopy(buf, len));
}


// Make symbol by copying the null-terminated `str`
Val *valCreateSymbolStr(const char *str)
{
    return valCreateSymbolCopy(str, strlen(str));
}


// Make a symbol for an integer
Val *valCreateInteger(long n)
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
// NOTE: rest must be NULL or a list Val.
Val *valCreateList(Val *first, Val *rest) {
    if (!valIsList(rest)) { return NULL; } // allow proper lists only
    Val *p = valAllocKind(VK_LIST);
    if (p)
    {
        p->first = first;
        p->rest = rest;
    }
    return p;
}


static Val *valCreateFunc(LizpFunc *f) 
{
    Val *p = valAllocKind(VK_FUNC);
    if (p) { p->func = f; }
    return p;
}


static Val *valCreateMacro(LizpMacro *m) 
{
    Val *p = valAllocKind(VK_MACRO);
    if (p) { p->macro = m; }
    return p;
}


// New copy, with no structure-sharing
Val *valCopy(Val *p)
{
    if (!p) { return p; }
    if (valIsSymbol(p)) { return valCreateSymbolStr(p->symbol); }
    if (valIsFunc(p)) { return valCreateFunc(p->func); }
    if (valIsMacro(p)) { return valCreateMacro(p->macro); }
    if (!valIsList(p)) { return NULL; }
    // Copy list
    Val *copy = valCreateList(valCopy(p->first), NULL);
    Val *pcopy = copy;
    p = p->rest;
    while (valIsList(p) && p)
    {
        pcopy->rest = valCreateList(valCopy(p->first), NULL);
        pcopy = pcopy->rest;
        p = p->rest;
    }
    return copy;
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
unsigned SkipChars(const char *str, unsigned len)
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
unsigned valReadOneFromBuffer(const char *str, unsigned len, Val **out)
{
    if (!out || !str || len <= 0) { return 0; }
    unsigned i = 0;
    i += SkipChars(str + i, len - i);
    switch (str[i])
    {
    case '\0': // end of string
    case ']':  // unmatched list
    case '(':  // uncaught comment
    case ')':  // uncaught comment
        *out = NULL;
        return i;
    case '[':
        // begin list
        {
            i++;
            // empty list?
            i += SkipChars(str + i, len - i);
            if (str[i] == ']')
            {
                i++;
                *out = NULL;
                return i;
            }
            // first item
            Val *e;
            unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
            if (!l)
            {
                *out = NULL;
                return i;
            };
            i += l;
            Val *list = valCreateList(e, NULL);
            Val *p = list;
            // rest of items
            while (i < len && str[i] != ']')
            {
                Val *e;
                unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
                i += l;
                p->rest = valCreateList(e, NULL);
                p = p->rest;
                if (l <= 0)
                {
                    *out = list;
                    return l;
                }
                i += SkipChars(str + i, len - i);
            }
            *out = list;
            if (str[i] == ']')
            {
                i++;
                i += SkipChars(str + i, len - i);
            }
            return i;
        }
    default:
        // Symbol
        {
            // leading space
            i += SkipChars(str + i, len);
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
                        if (good && out)
                        {
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
                        if (out) { *out = NULL; } // invalid
                        return i;
                    }
                default:
                    // Symbol (not in quotes)
                    {
                        const unsigned j = i;
                        bool done = 0;
                        while (!done && i < len)
                        {
                            switch (str[i])
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
                                    if (isspace(str[i]))
                                    {
                                        done = 1;
                                        break;
                                    }
                                    i++;
                                    break;
                            }
                        }
                        if (out)
                        {
                            *out = valCreateSymbolCopy(str + j, i - j);
                        }
                        return i;
                    }
            }
        }
    }
}

// Read all values from buffer.
// Return value: the number of VALUES read.
// see also: valReadOneFromBuffer()
unsigned valReadAllFromBuffer(const char *str, unsigned len, Val **out)
{
    unsigned i = 0;
    Val *val = NULL;

    // Read first item... (may be the only item)
    unsigned read_len = valReadOneFromBuffer(str + i, len - i, &val);
    if (!read_len) { return 0; }
    i += read_len;
    i += SkipChars(str + i, len - i);
    if (i >= len || !str[i])
    {
        *out = val;
        return 1;
    }

    // Read additional items into a list...
    unsigned n; // number of items read
    val = valCreateList(val, NULL);
    Val *p = val;
    for (n = 1; i < len && str[i]; n++, p = p->rest)
    {
        Val *e;
        read_len = valReadOneFromBuffer(str + i, len - i, &e);
        if (!read_len) { break; }
        i += read_len;
        i += SkipChars(str + i, len - i);
        p->rest = valCreateList(e, NULL);
    }

    *out = val;
    return n;
}

// Prints p to the given `out` buffer.
// Does not do null termination.
// If out is NULL, it just calculates the print length
// Returns: number of chars written
unsigned valWriteToBuffer(Val *v, char *out, unsigned length, bool readable)
{
    // String output count / index
    unsigned i = 0;
    if (valIsList(v))
    {
        if (out && i < length) { out[i] = '['; }
        i++;
        if (v)
        {
            // first item
            if (out)
            {
                i += valWriteToBuffer(v->first, out + i, length - i, readable);
            }
            else
            {
                i += valWriteToBuffer(v->first, NULL, 0, readable);
            }
            v = v->rest;
            while (v)
            {
                // space
                if (out && i < length) { out[i] = ' '; }
                i++;
                // item
                if (out)
                {
                    i += valWriteToBuffer(v->first, out + i, length - i, readable);
                }
                else
                {
                    i += valWriteToBuffer(v->first, NULL, 0, readable);
                }
                v = v->rest;
            }
        }
        if (out && i < length) { out[i] = ']'; }
        i++;
        return i;
    }
    else if (valIsSymbol(v))
    {
        // Symbol
        char *s = v->symbol;
        bool quoted = readable && StrNeedsQuotes(s);
        if (quoted)
        {
            // Opening quote
            if (out && i < length)
            {
                out[i] = '"';
            }
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
                    if (out && i < length)
                    {
                        out[i] = '\\';
                    }
                    i++;
                }
            }
            if (out && i < length)
            {
                out[i] = c;
            }
            i++;
            s++;
        }
        if (quoted)
        {
            // Closing quote
            if (out && i < length)
            {
                out[i] = '"';
            }
            i++;
        }
        return i;
    }
    else if (valIsFunc(v)) {
        const char txt[] = "<native func>";
        const size_t len = sizeof(txt) - 1;
        if (out) { memcpy(out, txt, len); }
        return len;
    }
    else if (valIsMacro(v)) {
        const char txt[] = "<native macro>";
        const size_t len = sizeof(txt) - 1;
        if (out) { memcpy(out, txt, len); }
        return len;
    }
    else {
        return 0;
    }
}

// Print value to a new string
char *valWriteToNewString(Val *v, bool readable)
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


bool valListLengthIsMoreThan(const Val *l, unsigned len)
{
    while (l && len)
    {
        len--;
        l = l->rest;
    }
    return !len && l;
}


bool valListLengthIsLessThan(const Val *l, unsigned len)
{
    while (l && len)
    {
        len--;
        l = l->rest;
    }
    return len && !l;
}


// inclusive range
bool valListLengthIsWithin(const Val *l, unsigned min, unsigned max)
{
    return !valListLengthIsLessThan(l, min) && !valListLengthIsMoreThan(l, max);
}


// Get the length of a list.
// Returns 0 for a non-list value.
unsigned valListLength(const Val *l)
{
    unsigned len = 0;
    while (l && valIsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}


long valAsInteger(const Val *v)
{
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}


// Get value right after a symbol in a list
bool valGetListItemAfterSymbol(Val *list, const char *symname, Val **out)
{
    if (!valIsList(list)) { return 0; }
    while (list)
    {
        Val *e = list->first;
        if (valIsSymbol(e) && !strcmp(e->symbol, symname))
        {
            // found a spot
            list = list->rest;
            if (!list) { return 0; }
            *out = list->first;
            return 1;
        }
        list = list->rest;
    }
    return 0;
}


Val *valGetListIndex(Val *list, size_t index)
{
    while (list && index) { list = list->rest; }
    return list;
}


// Get the Nth item in a list (0-based index)
Val *NthItem(Val *list, unsigned n)
{
    if (!valIsList(list)) { return NULL; }
    for (; n > 0; n--)
    {
        list = list->rest;
    }
    return list? list->first : list;
}

// Check if two values do not share structure
bool IsSeparate(Val *a, Val *b)
{
    // empty list (NULL) is separate from anything
    if (!a || !b) { return 1; }
    // the same object is not separate from itself
    if (a == b) { return 0; }
    // symbols are separate if they have different symbol strings
    if (valIsSymbol(a) && valIsSymbol(b)) { return a->symbol != b->symbol; }
    // lists are separate if everything under them are separate
    if (valIsList(a) && valIsList(b))
    {
        return IsSeparate(a->first, b->first)
            && IsSeparate(a->first, b->rest)
            && IsSeparate(a->rest, b->first)
            && IsSeparate(a->rest, b->rest);
    }
    // symbol and list are separate if the list
    // (and sublists) never contains the symbol
    if (valIsSymbol(a))
    {
        // swap to make sure `a` is list and `b` is symbol
        Val *t = b;
        b = a;
        a = t;
    }
    return IsSeparate(a->first, b)
        && IsSeparate(a->rest, b);
}


// Check whether a value is considered as true
bool valIsTrue(Val *v)
{
    return v != NULL;
}

Val *valCreateTrue(void)
{
    return valCreateSymbolCopy("true", 4);
}

Val *valCreateFalse(void)
{
    return NULL;
}

// make a list of the form [error rest...]
Val *valCreateError(Val *rest)
{
    Val *e = valCreateSymbolStr("error");
    if (valIsList(rest)) { return valCreateList(e, rest); }
    return valCreateList(e, valCreateList(rest, NULL));
}

Val *valCreateErrorMessage(const char *msg)
{
    return valCreateError(valCreateSymbolStr(msg));
}

// Check whether a value is a lambda value (special list)
bool valIsLambda(Val *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val *l = v->first;
    if (!l || !valIsSymbol(l)) { return 0; }
    if (strcmp(l->symbol, "lambda")) { return 0; }
    if (!v->rest) { return 0; }
    Val *params = v->rest->first;
    if (!valIsList(params)) { return 0; }
    Val *pp = params;
    while (pp && valIsList(pp))
    {
        if (!valIsSymbol(pp->first)) { return 0; }
        pp = pp->rest;
    }
    if (!v->rest->rest) { return 0; }
    return 1;
}


// check if the value matches the form [error ...]
bool valIsError(Val *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val *first = v->first;
    return valIsSymbol(first) && !strcmp("error", first->symbol);
}

// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet(Val *env, Val *key, Val *val)
{
    if (!env || !valIsList(env)) { return 0; }
    Val *pair = valCreateList(key, valCreateList(val, NULL));
    if (!pair) { return 0; }
    // push key-value pair onto the front of the list
    env->first = valCreateList(pair, env->first);
    return 1;
}

// Environment Get.
// Get value in environment, does not return a copy
// Return value: whether the symbol is present
bool EnvGet(Val *env, Val *key, Val **out)
{
    if (!env) { return 0; }
    Val *scope = env;
    while (scope && valIsList(scope))
    {
        Val *p = scope->first;
        while (p && valIsList(p))
        {
            Val *pair = p->first;
            if (pair && valIsList(pair) && valIsEqual(pair->first, key))
            {
                if (out) { *out = pair->rest->first; }
                return 1;
            }
            p = p->rest;
        }
        // outer scope
        scope = scope->rest;
    }
    return 0;
}

// push a new context onto the environment
void EnvPush(Val *env)
{
    if (!env) { return; }
    env->rest = valCreateList(env->first, env->rest);
    env->first = NULL;
}

// pop off the latest context from the environment
void EnvPop(Val *env)
{
    if (!env) { return; }
    valFreeRec(env->first);
    Val *pair = env->rest;
    env->first = pair->first;
    env->rest = pair->rest;
    // Only free the one pair
    valFree(pair);
}

// Return values must not share structure with first, args, or env
static Val *ApplyLambda(Val *first, Val *args, Val *env)
{
    Val *params = first->rest->first;
    Val *body = first->rest->rest->first;
    // push env
    EnvPush(env);
    // bind values
    Val *p_params = params;
    Val *p_args = args;
    while (p_params && valIsList(p_params) && p_args && valIsList(p_args))
    {
        Val *param = p_params->first;
        // parameter beginning with '&' binds the rest of the arguments
        if ('&' == param->symbol[0])
        {
            if (p_params->rest)
            {
                // error: not the last parameter
                EnvPop(env);
                return NULL;
            }
            EnvSet(env, valCopy(param), valCopy(p_args));
            // p_params and p_args will both be non-null
            break;
        }
        // normal parameter
        EnvSet(env, valCopy(param), valCopy(p_args->first));
        p_params = p_params->rest;
        p_args = p_args->rest;
    }
    // check a parameters-arguments arity mismatch
    if ((p_params == NULL) != (p_args == NULL))
    {
        // error
        EnvPop(env);
        return NULL;
    }
    Val *result = evaluate(body, env);
    EnvPop(env);
    return result;
}


static Val *ApplyNative(Val *f, Val *args) { return f->func(args); }


static Val *ApplyMacro(Val *m, Val *args, Val *env) { return m->macro(args, env); }


// Apply functions
// Return values must not share structure with first, args, or env
static Val *Apply(Val *first, Val *args, Val *env)
{
    if (valIsLambda(first)) { return ApplyLambda(first, args, env); }
    if (valIsFunc(first)) { return ApplyNative(first, args); }
    // invalid function
    return valCreateError(valCreateList(valCopy(first),
                              valCreateList(valCreateSymbolStr("is not a function"),
                                       NULL)));
}


// Evaluate each item in a list
Val *evaluateList(Val *list, Val *env)
{
    if (!list || !valIsList(list)) { return NULL; }

    Val *result = valCreateList(NULL, NULL);
    Val *p_result = result;
    while (list && valIsList(list))
    {
        Val *e = evaluate(list->first, env);
        if (valIsError(e))
        {
            valFreeRec(result);
            return e;
        }

        p_result->first = e;
        if (list->rest)
        {
            p_result->rest = valCreateList(NULL, NULL);
        }
        p_result = p_result->rest;
        list = list->rest;
    }

    return result;
}


// Evaluate a Val value
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs for bindings
// Returns the evaluated value
// NOTE: must only return new values that do not share any
//       structure with the ast or the env
Val *evaluate(Val *ast, Val *env)
{
    if (!ast) { return NULL; } // empty list
    if (valIsInteger(ast)) { return valCopy(ast); } // integers are self-evaluating
    if (valIsLambda(ast)) { return valCopy(ast); } // lambda values are self-evaluating
    if (valIsSymbol(ast))
    {
        // lookup symbol value
        Val *val;
        if (EnvGet(env, ast, &val))
        {
            return valCopy(val);
        }
        // symbol not found
        Val *name = valCopy(ast);
        return valCreateError(
            valCreateList(name,
                     valCreateList(valCreateSymbolStr("is undefined"),
                              NULL)));
    }
    // evaluate list application...
    Val *first = evaluate(ast->first, env);
    if (valIsError(first)) { return first; }
    if (valIsMacro(first)) { return ApplyMacro(first, ast->rest, env); }
    // evaluate rest of elements for normal function application
    Val *args = evaluateList(ast->rest, env);
    if (valIsError(args))
    {
        valFreeRec(first);
        return args;
    }
    Val *result = Apply(first, args, env);
    valFreeRec(first);
    valFreeRec(args);
    return result;
}


// Environment Set Function
// Set a symbol value to be associated with a C function
// Also, use the form string to always check the arguments before the function
// is called (see `argsIsMatchForm`).
bool EnvSetFunc(Val *env, const char *name, LizpFunc *func)
{
    if (!env || !name || !func) { return 0; }

    Val *key = valCreateSymbolStr(name);
    if (!key) { return false; }

    Val *val = valCreateFunc(func);
    if (!val)
    {
        valFreeRec(key);
        return 0;
    }

    bool success = EnvSet(env, key, val);
    if (!success)
    {
        valFreeRec(key);
        valFreeRec(val);
    }

    return success;
}


// Environment set macro extended.
// Add a custom C macro to the environment.
bool EnvSetMacro(Val *env, const char *name, LizpMacro *m)
{
    if (!env || !name || !m) { return 0; }

    Val *key = valCreateSymbolStr(name);
    if (!key) { return false; }

    Val *val = valCreateMacro(m);
    if (!val)
    {
        valFreeRec(key);
        return 0;
    }

    bool success = EnvSet(env, key, val);
    if (!success)
    {
        valFreeRec(key);
        valFreeRec(val);
    }

    return success;
}


// Environment Set Symbol
bool EnvSetSym(Val *env, const char *sym, Val *v)
{
    if (!env || !sym) { return 0; }
    return EnvSet(env, valCreateSymbolStr(sym), v);
}


void lizpRegisterCore(Val *env)
{
    // macros
    EnvSetMacro(env, "quote", quote_func);
    EnvSetMacro(env, "if", if_func);
    EnvSetMacro(env, "cond", cond_func);
    EnvSetMacro(env, "do", do_func);
    EnvSetMacro(env, "^", lambda_func);
    EnvSetMacro(env, "and", and_func);
    EnvSetMacro(env, "or", or_func);
    EnvSetMacro(env, "let", let_func);
    // functions
    EnvSetFunc(env, "+", plus_func);
    EnvSetFunc(env, "*", multiply_func);
    EnvSetFunc(env, "/", divide_func);
    EnvSetFunc(env, "-", subtract_func);
    EnvSetFunc(env, "%", mod_func);
    EnvSetFunc(env, "=", equal_func);
    EnvSetFunc(env, "<=", increasing_func);
    EnvSetFunc(env, ">=", decreasing_func);
    EnvSetFunc(env, "<", strictly_increasing_func);
    EnvSetFunc(env, ">", strictly_decreasing_func);
    EnvSetFunc(env, "empty?", empty_q_func);
    EnvSetFunc(env, "member?", member_q_func);
    EnvSetFunc(env, "symbol?", symbol_q_func);
    EnvSetFunc(env, "integer?", integer_q_func);
    EnvSetFunc(env, "list?", list_q_func);
    EnvSetFunc(env, "lambda?", lambda_q_func);
    EnvSetFunc(env, "function?", function_q_func);
    EnvSetFunc(env, "native?", native_q_func);
    EnvSetFunc(env, "chars", chars_func);
    EnvSetFunc(env, "symbol", symbol_func);
    EnvSetFunc(env, "list", list_func);
    EnvSetFunc(env, "count", count_func);
    EnvSetFunc(env, "position", position_func);
    EnvSetFunc(env, "slice", slice_func);
    EnvSetFunc(env, "length", length_func);
    EnvSetFunc(env, "not", not_func);
    EnvSetFunc(env, "nth", nth_func);
    EnvSetFunc(env, "prepend", prepend_func);
    EnvSetFunc(env, "append", append_func);
    EnvSetFunc(env, "without", without_func);
}


// [reverse list]
Val *reverse_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [concat list.1 (list.N)...]
Val *concat_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [join separator (list)...]
Val *join_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [without item list]
// Create a list without the given item
Val *without_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *item = NthItem(args, 0);
    Val *list = NthItem(args, 1);
    if (!list) { return NULL; }
    Val *result = NULL;
    Val *p = result;
    while (list)
    {
        Val *e = list->first;
        if (valIsEqual(e, item))
        {
            list = list->rest;
            continue;
        }
        if (result)
        {
            p->rest = valCreateList(valCopy(e), NULL);
            p = p->rest;
            list = list->rest;
            continue;
        }
        // This is the first time adding an item
        result = valCreateList(valCopy(e), NULL);
        p = result;
        list = list->rest;
    }
    return result;
}

// [replace item1 item2 list]
Val *replace_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [replaceN item1 item2 list n]
Val *replace1_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [replaceI index item list]
Val *replaceI_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [zip list.1 (list.N)...]
Val *zip_func(Val *args)
{
    // TODO: implement
    return NULL;
}

// [append val list]
Val *append_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *v = args->first;
    Val *list = args->rest->first;
    Val *last = valCreateList(valCopy(v), NULL);
    if (!list)
    {
        // empty list -> single-item list
        return last;
    }
    // Create a new list and put "last" at the end
    Val *copy = valCopy(list);
    Val *p = copy;
    while (p->rest)
    {
        p = p->rest;
    }
    p->rest = last;
    return copy;
}

// [prepend val list]
Val *prepend_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *v = args->first;
    Val *list = args->rest->first;
    return valCreateList(valCopy(v), valCopy(list));
}

// [+ (integer)...] sum
Val *plus_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("&n", args, &err)) { return valCreateError(err); }
    long sum = 0;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        if (!valIsInteger(e)) { return valCreateErrorMessage("not an integer number"); }
        sum += valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(sum);
}

// [* (integer)...] product
Val *multiply_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("&n", args, &err)) { return valCreateError(err); }
    long product = 1;
    Val *p = args;
    while (p)
    {
        Val *e = p->first;
        product *= valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(product);
}

// [- x:int (y:int)] subtraction
Val *subtract_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("n(n", args, &err)) { return valCreateError(err); }
    if (!valListLengthIsWithin(args, 1, 2)) { return valCreateErrorMessage("takes 1 or 2 arguments"); }
    Val *vx = args->first;
    long x = atol(vx->symbol);
    if (!args->rest) { return valCreateInteger(-x); }
    Val *vy = args->rest->first;
    long y = atol(vy->symbol);
    return valCreateInteger(x - y);
}

// [/ x:int y:int] division
Val *divide_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn", args, &err)) { return valCreateError(err); }
    long x = valAsInteger(NthItem(args, 0));
    long y = valAsInteger(NthItem(args, 1));
    if (y == 0)
    {
        // division by zero
        return valCreateErrorMessage("division by zero");
    }
    return valCreateInteger(x / y);
}

// [% x:int y:int] modulo
Val *mod_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn", args, &err)) { return valCreateError(err); }
    long x = valAsInteger(NthItem(args, 0));
    long y = valAsInteger(NthItem(args, 1));
    if (y == 0)
    {
        // division by zero
        return valCreateErrorMessage("division by zero");
    }
    return valCreateInteger(x % y);
}

// [= x y (expr)...] check equality
Val *equal_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vv&v", args, &err)) { return valCreateError(err); }
    Val *f = args->first;
    Val *p = args->rest;
    while (p && valIsList(p))
    {
        if (!valIsEqual(f, p->first)) { return valCreateFalse(); }
        p = p->rest;
    }
    return valCreateTrue();
}

// [not expr] boolean not
Val *not_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsTrue(args->first)? valCreateFalse() : valCreateTrue();
}

// [symbol? val] check if value is a symbol
Val *symbol_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    Val *v = args->first;
    return !valIsSymbol(v)? valCreateTrue() : valCreateFalse();
}

// [integer? val] check if value is a integer symbol
Val *integer_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsInteger(args->first)? valCreateTrue() : valCreateFalse();
}

// [list? val] check if value is a list
Val *list_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsList(args->first)? valCreateTrue() : valCreateFalse();
}

// [empty? val] check if value is a the empty list
Val *empty_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return (!args->first)? valCreateTrue() : valCreateFalse();
}

// [nth index list] get the nth item in a list
Val *nth_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nl", args, &err)) { return valCreateError(err); }
    Val *i = args->first;
    Val *list = args->rest->first;
    long n = atol(i->symbol);
    if (n < 0)
    {
        // index negative
        return valCreateErrorMessage("index cannot be negative");
    }
    Val *p = list;
    while (n > 0 && p && valIsList(p))
    {
        p = p->rest;
        n--;
    }
    if (p) { return valCopy(p->first); }
    return valCreateErrorMessage("index too big");
}

// [list (val)...] create list from arguments (variadic)
Val *list_func(Val *args)
{
    return valCopy(args);
}

// [length list]
Val *length_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("l", args, &err)) { return valCreateError(err); }
    return valCreateInteger(valListLength(args->first));
}

// [lambda? v]
Val *lambda_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsLambda(args->first)? valCreateTrue() : valCreateFalse();
}

// [function? v]
Val *function_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    Val *v = args->first;
    return (valIsFunc(v) || valIsLambda(v))? valCreateTrue() : valCreateFalse();
}

// [native? v]
Val *native_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsFunc(args->first)? valCreateTrue() : valCreateFalse();
}

// [<= x y (expr)...] check number order
Val *increasing_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && valIsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x <= y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [>= x y (expr)...] check number order
Val *decreasing_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && valIsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x >= y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [< x y (expr)...] check number order
Val *strictly_increasing_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && valIsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x < y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [> x y (expr)...] check number order
Val *strictly_decreasing_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val *f = args->first;
    long x = atol(f->symbol);
    Val *p = args->rest;
    while (p && valIsList(p))
    {
        Val *e = p->first;
        long y = atol(e->symbol);
        if (!(x > y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [chars sym] -> list
Val *chars_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("s", args, &err)) { return valCreateError(err); }
    Val *sym = args->first;
    char *s = sym->symbol;
    Val *result = valCreateList(valCreateSymbolCopy(s, 1), NULL);
    s++;
    Val *p = result;
    while (*s)
    {
        p->rest = valCreateList(valCreateSymbolCopy(s, 1), NULL);
        p = p->rest;
        s++;
    }
    return result;
}

// [symbol list] -> symbol
Val *symbol_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("L", args, &err)) { return valCreateError(err); }
    Val *list = args->first;
    int len = valListLength(list);
    char *sym = malloc(1 + len);
    int i = 0;
    Val *p = list;
    while (p && valIsList(list))
    {
        Val *e = p->first;
        if (!valIsSymbol(e))
        {
            free(sym);
            return valCreateErrorMessage("list must only contain symbols");
        }
        sym[i] = e->symbol[0];
        i++;
        p = p->rest;
    }
    sym[len] = 0;
    // ok because sym was created with malloc()
    return valCreateSymbol(sym);
}

// [member? item list]
Val *member_q_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *item = args->first;
    Val *list = args->rest->first;
    while (list && valIsList(list))
    {
        if (valIsEqual(list->first, item)) { return valCreateTrue(); }
        list = list->rest;
    }
    return valCreateFalse();
}

// [count item list] -> int
Val *count_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *item = args->first;
    Val *list = args->rest->first;
    long count = 0;
    while (list && valIsList(list))
    {
        if (valIsEqual(list->first, item)) { count++; }
        list = list->rest;
    }
    return valCreateInteger(count);
}

// [position item list] -> list
Val *position_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val *item = args->first;
    Val *list = args->rest->first;
    long i = 0;
    while (list && valIsList(list))
    {
        if (valIsEqual(item, list->first)) { return valCreateInteger(i); }
        i++;
        list = list->rest;
    }
    return valCreateFalse();
}

// [slice list start (end)]
// gets a sublist "slice" inclusive of start and end
Val *slice_func(Val *args)
{
    Val *err;
    //if (!argsIsMatchForm("ln(n", args, &err)) { return valCreateError(err); }
    Val *list = args->first;
    Val *start = args->rest->first;
    long start_i = atol(start->symbol);
    if (start_i < 0) { return valCreateErrorMessage("start index cannot be negative"); }
    if (!args->rest->rest)
    {
        // [slice list start]
        while (start_i > 0 && list && valIsList(list))
        {
            list = list->rest;
            start_i--;
        }
        if (!list)
        {
            // TODO: what causes this error?
            return NULL;
        }
        Val *result = valCreateList(valCopy(list->first), NULL);
        list = list->rest;
        Val *p_result = result;
        while (list && valIsList(list))
        {
            p_result->rest = valCreateList(valCopy(list->first), NULL);
            p_result = p_result->rest;
            list = list->rest;
        }
        return result;
    }
    // [slice list start end]
    Val *end = args->rest->rest->first;
    long end_i = atol(end->symbol);
    if (end_i <= start_i)
    {
        return valCreateErrorMessage("start index must be less than the end index");
    }
    while (start_i > 0 && list && valIsList(list))
    {
        list = list->rest;
        start_i--;
    }
    if (!list)
    {
        // TODO: what error is this?
        return NULL;
    }
    Val *result = valCreateList(valCopy(list->first), NULL);
    list = list->rest;
    Val *p_result = result;
    long i = end_i - start_i;
    while (i > 0 && list && valIsList(list))
    {
        p_result->rest = valCreateList(valCopy(list->first), NULL);
        p_result = p_result->rest;
        list = list->rest;
        i--;
    }
    return result;
}

// (macro) [let [key val...] expr]
// create bindings
Val *let_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("Lv", args, &err)) { return valCreateError(err); }
    Val *bindings = args->first;
    Val *body = args->rest->first;
    // create and check bindings
    EnvPush(env);
    Val *p_binds = bindings;
    while (p_binds && valIsList(p_binds))
    {
        Val *sym = p_binds->first;
        if (!valIsSymbol(sym) || !p_binds->rest || !valIsList(p_binds->rest))
        {
            // invalid symbol or uneven amount of args
            EnvPop(env);
            return valCreateErrorMessage(
                    "`let` bindings list must consist of alternating symbols and expressions");
        }
        p_binds = p_binds->rest;
        Val *expr = p_binds->first;
        Val *val = evaluate(expr, env);
        if (valIsError(val))
        {
            // eval error
            EnvPop(env);
            return val;
        }
        EnvSet(env, valCopy(sym), val);
        p_binds = p_binds->rest;
    }
    // eval body
    Val *result = evaluate(body, env);
    // destroy bindings
    EnvPop(env);
    return result;
}

// (macro) [if condition consequent (alternative)]
Val *if_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("vv(v", args, &err)) { return valCreateError(err); }
    Val *f = evaluate(args->first, env);
    if (valIsError(f)) { return f; } // eval error
    int t = valIsTrue(f);
    valFreeRec(f);
    if (t)
    {
        Val *consequent = args->rest->first;
        return evaluate(consequent, env);
    }
    Val *alt_list = args->rest->rest;
    if (!alt_list) { return NULL; } // no alternative
    Val *alternative = alt_list->first;
    return evaluate(alternative, env);
}

// (macro) [quote expr]
Val *quote_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valCopy(args->first);
}

// (macro) [do (expr)...]
Val *do_func(Val *args, Val *env)
{
    Val *p = args;
    Val *e = NULL;
    while (p && valIsList(p))
    {
        e = evaluate(p->first, env);
        if (valIsError(e)) { return e; } // eval error
        p = p->rest;
        if (p) { valFreeRec(e); } // free all values except the last one
    }
    return e;
}

// (macro) [and expr1 (expr)...]
Val *and_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("v&v", args, &err)) { return valCreateError(err); }
    Val *p = args;
    while (p && valIsList(p))
    {
        Val *e = evaluate(p->first, env);
        if (valIsError(e)) { return e; }
        if (!valIsTrue(e)) { return e; } // item is false
        p = p->rest;
        if (!p) { return e; } // last item is true
        valFreeRec(e);
    }
    // malformed list
    return NULL;
}

// (macro) [or expr1 (expr)...]
Val *or_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("v&v", args, &err)) { return valCreateError(err); }
    Val *p = args;
    while (p && valIsList(p))
    {
        Val *e = evaluate(p->first, env);
        if (valIsError(e)) { return e; }
        if (valIsTrue(e)) { return e; } // item is true
        p = p->rest;
        if (!p) { return e; } // last item is false
        valFreeRec(e);
    }
    // malformed list
    return NULL;
}

// (macro) [cond (condition result)...] (no nested lists)
Val *cond_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("vv&v", args, &err)) { return valCreateError(err); }
    if (valListLength(args) % 2 != 0)
    {
        return valCreateErrorMessage("`cond` requires an even amount of"
                                " alternating condition expressions and"
                                " consequence expressions");
    }
    Val *p = args;
    while (p && valIsList(p))
    {
        Val *e = evaluate(p->first, env);
        if (valIsError(e)) { return e; }
        if (valIsTrue(e))
        {
            valFreeRec(e);
            return evaluate(p->rest->first, env);
        }
        valFreeRec(e);
        p = p->rest;
        p = p->rest;
    }
    // no condition matched
    return NULL;
}

// (macro) [lambda [(symbol)...] (expr)]
Val *lambda_func(Val *args, Val *env)
{
    Val *err;
    //if (!argsIsMatchForm("l(v", args, &err)) { return valCreateError(err); }
    Val *params = args->first;
    if (!valIsList(params))
    {
        return valCreateErrorMessage("`lambda` first argument must be a list of"
                                " symbols");
    }
    Val *p = params;
    // params must be symbols
    while (p && valIsList(p))
    {
        Val *e = p->first;
        if (!valIsSymbol(e))
        {
            return valCreateError(
                valCreateList(valCopy(e),
                         valCreateList(valCreateSymbolStr("is not a symbol"),
                                  NULL)));
        }
        p = p->rest;
    }
    // make lambda... with an explicit NULL body if a body is not provided
    Val *body = args->rest;
    if (body) { body = body->first; }
    return valCreateList(valCreateSymbolCopy("lambda", 6),
                    valCreateList(valCopy(params),
                             valCreateList(valCopy(body),
                                      NULL)));
}

#endif /* LIZP_IMPLEMENTATION */

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
