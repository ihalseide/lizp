/*
LIZP

    Programming language and linked list data serialization.
    This is written in the way of the C99 standard.
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

    The only real data type is the "Value" known as LizpVal, which can can
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


struct LizpVal;


typedef struct LizpVal *LizpFunc(struct LizpVal *args);
typedef struct LizpVal *LizpMacro(struct LizpVal *args, struct LizpVal *env);


typedef enum LizpValKind {
    VK_FREE = 0,
    VK_SYMBOL,
    VK_LIST,
    VK_FUNC,   // built-in function
    VK_MACRO,  // built-in macro
} LizpValKind;


typedef struct LizpVal {
    LizpValKind kind;
    union {
        char *symbol;
        LizpFunc *func;
        LizpMacro *macro;
        struct {
            struct LizpVal *first;
            struct LizpVal *rest;
        };
    };
} LizpVal;


// memory management
LizpVal *valAlloc(void);
LizpVal *valAllocKind(LizpValKind k);
LizpVal *valCopy(const LizpVal *p);
void valFree(LizpVal *p);
void valFreeRec(LizpVal *p);

// value creation
LizpVal *valCreateInteger(long n);
LizpVal *valCreateList(LizpVal *first, LizpVal *rest);
LizpVal *valCreateSymbol(char *string);
LizpVal *valCreateSymbolCopy(const char *start, unsigned len);
LizpVal *valCreateSymbolStr(const char *string);

// value type checking
LizpValKind valKind(const LizpVal *v);
bool valIsInteger(const LizpVal *v);
bool valIsList(const LizpVal *v);
bool valIsSymbol(const LizpVal *v);
bool valIsFunc(const LizpVal *v);
bool valIsMacro(const LizpVal *v);

// value utility functions
bool valGetListItemAfterSymbol(LizpVal *list, const char *symbol, LizpVal **out);
LizpVal *valGetListIndex(LizpVal *list, size_t symbol);
bool valIsEqual(const LizpVal *x, const LizpVal *y);
long valAsInteger(const LizpVal *v);
unsigned valListLength(const LizpVal *l);
bool valListLengthIsWithin(const LizpVal *l, unsigned min, unsigned max);
bool valListLengthIsMoreThan(const LizpVal *l, unsigned n);
bool valListLengthIsLessThan(const LizpVal *l, unsigned n);

// value serialization
unsigned valReadOneFromBuffer(const char *start, unsigned length, LizpVal **out);
unsigned valReadAllFromBuffer(const char *start, unsigned length, LizpVal **out);
unsigned valWriteToBuffer(const LizpVal *p, char *out, unsigned length, bool readable);
/*new*/ char *valWriteToNewString(const LizpVal *p, bool readable);

LizpVal *valCreateTrue(void);
LizpVal *valCreateError(LizpVal *rest);
LizpVal *valCreateErrorMessage(const char *msg);
LizpVal *valCreateFalse(void);

bool valIsError(const LizpVal *v);
bool valIsLambda(const LizpVal *v);
bool valIsTrue(const LizpVal *v);

LizpVal *evaluate(const LizpVal *ast, LizpVal *env);
LizpVal *evaluateList(LizpVal *list, LizpVal *env);
bool EnvGet(LizpVal *env, const LizpVal *key, LizpVal **out);
bool EnvSet(LizpVal *env, LizpVal *key, LizpVal *val);
bool EnvSet_const(LizpVal *env, const LizpVal *key, const LizpVal *val);
bool EnvSetFunc(LizpVal *env, const char *name, LizpFunc *func);
bool EnvSetMacro(LizpVal *env, const char *name, LizpMacro *macro);
bool EnvSetSym(LizpVal *env, const char *symbol, LizpVal *val);
void EnvPop(LizpVal *env);
void EnvPush(LizpVal *env);

void lizpRegisterCore(LizpVal *env);
// functions
LizpVal *plus_func(LizpVal *args);       // [+ integers...] sum
LizpVal *multiply_func(LizpVal *args);   // [* integers...] product
LizpVal *subtract_func(LizpVal *args);   // [- x (y)] subtraction
LizpVal *divide_func(LizpVal *args);     // [/ x y] division
LizpVal *mod_func(LizpVal *args);        // [% x y] modulo
LizpVal *equal_func(LizpVal *args);      // [= x y (expr)...] check equality
LizpVal *not_func(LizpVal *args);        // [not expr] boolean not
LizpVal *symbol_q_func(LizpVal *args);   // [symbol? val] check if value is a symbol
LizpVal *integer_q_func(LizpVal *args);  // [integer? val] check if value is a integer symbol
LizpVal *list_q_func(LizpVal *args);     // [list? val] check if value is a list
LizpVal *empty_q_func(LizpVal *args);    // [empty? val] check if value is a the empty list
LizpVal *nth_func(LizpVal *args);        // [nth index list] get the nth item in a list
LizpVal *list_func(LizpVal *args);       // [list (val)...] create list from arguments (variadic)
LizpVal *length_func(LizpVal *args);     // [length list]
LizpVal *lambda_q_func(LizpVal *args);   // [lambda? v]
LizpVal *function_q_func(LizpVal *args); // [function? v]
LizpVal *native_q_func(LizpVal *args);   // [native? v]
LizpVal *strictly_increasing_func(LizpVal *args);   // [< x y (expr)...] check number order
LizpVal *chars_func(LizpVal *args);      // [chars sym] -> list
LizpVal *symbol_func(LizpVal *args);     // [symbol list] -> symbol
LizpVal *member_q_func(LizpVal *args);   // [member? item list] -> boolean value
// macros
LizpVal *quote_func(LizpVal *args, LizpVal *env);   // [quote expr]
LizpVal *if_func(LizpVal *args, LizpVal *env);      // [if condition consequence alternative]
LizpVal *cond_func(LizpVal *args, LizpVal *env);    // [cond condition1 consequence1 condition2 consequence2 ...]
LizpVal *do_func(LizpVal *args, LizpVal *env);      // [do expr ...]
LizpVal *lambda_func(LizpVal *args, LizpVal *env);  // [lambda [args ...] body-expr]
LizpVal *and_func(LizpVal *args, LizpVal *env);     // [and expr1 expr2 ...]
LizpVal *or_func(LizpVal *args, LizpVal *env);      // [or expr1 expr2 ...]
LizpVal *let_func(LizpVal *args, LizpVal *env);     // [let [sym1 expr1 sym2 expr2 ...] body-expr]

#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // for snprintf


static LizpVal *pool;
static const char const_lambda[] = "lambda";
static const char const_true[] = "#t";
static const char const_error[] = "error";


// Create a newly allocated string that is a copy from `buf` of length `len`
static /*new*/ char *stringCopy(const char *buf, unsigned len) {
    if (!buf) { return NULL; }
    char *s = malloc(len + 1);
    if (s) {
        memcpy(s, buf, len);
        s[len] = 0;
    }
    return s;
}


// Allocate a new value
// Potential problem: memory use currently cannot shrink
LizpVal *valAlloc() {
    if (!pool) {
        // Create another free list of things linked together
        const int count = 1000;
        pool = malloc(count * sizeof(*pool));
        for (LizpVal *p = pool; p < pool + count - 1; p++) {
            p->rest = p + 1;
        }
        pool[count - 1].rest = NULL;
    }

    LizpVal *p = pool;
    pool = pool->rest;
    return p;
}


LizpVal *valAllocKind(LizpValKind k) {
    LizpVal *p = valAlloc();
    if (p) { p->kind = k; }
    return p;
}


static bool symbolIsStatic(const char *string);


// Free value
void valFree(LizpVal *p) {
    if (!p) { 
	    return;
    }
    else {
	    if (valIsSymbol(p) && p->symbol && !symbolIsStatic(p->symbol)) {
		    free(p->symbol);
	    }
	    p->kind = VK_FREE;
	    p->rest = pool;
	    pool = p;
    }
}


// Free value recursively
void valFreeRec(LizpVal *v) {
    if (!v) { return; }
    if (valIsSymbol(v)) {
        // Symbol
        valFree(v);
        return;
    }
    if (!valIsList(v)) { return; }
    // List
    LizpVal *p = v;
    LizpVal *n;
    while (p && valIsList(p)) {
        valFreeRec(p->first);
        n = p->rest;
        valFree(p);
        p = n;
    }
    return;
}


static bool symbolIsStatic(const char *string) {
    return string == const_lambda
        || string == const_true
        || string == const_error;
}


static bool symbolsEqual(char *a, char *b) {
    // Symbol equality
    while (*a && *b && *a == *b) {
        a++;
        b++;
    }
    return *a == *b;
}

bool valIsEqual(const LizpVal *x, const LizpVal *y) {
    if (x == NULL || y == NULL) {
        return x == y; 
    }
    else if (valIsSymbol(x)) {
        return symbolsEqual(x->symbol, y->symbol);
    }
    else if (valIsList(x)) {
        // List equality
        const LizpVal *px = x, *py = y;
        while (px && valIsList(px) && py && valIsList(py)) {
            if (!valIsEqual(px->first, py->first)) { 
                break; 
            }
            else {
                px = px->rest;
                py = py->rest;
            }
        }
        return (px == NULL) && (py == NULL);
    }
    else if (valIsFunc(x) && valIsFunc(y)) {
        return x->func == y->func; 
    }
    if (valIsMacro(x) && valIsMacro(y)) {
        return x->macro == y->macro; 
    }
    return 0;
}


// Get a value's kind a.k.a type
LizpValKind valKind(const LizpVal *v) {
    if (!v) {
        return VK_LIST; 
    }
    else {
        return v->kind;
    }
}


// Check if a value is a list
// NULL is also considered an empty list
bool valIsList(const LizpVal *p) { return valKind(p) == VK_LIST; }


// Check if a value is a symbol
bool valIsSymbol(const LizpVal *p) { return valKind(p) == VK_SYMBOL; }


bool valIsFunc(const LizpVal *v) { return v && valKind(v) == VK_FUNC; }


bool valIsMacro(const LizpVal *v) { return v && valKind(v) == VK_MACRO; }


//  check if a value is a integer symbol
bool valIsInteger(const LizpVal *v) {
    if (!valIsSymbol(v)) { return 0; }
    const unsigned base = 10;
    char *end;
    strtol(v->symbol, &end, base);
    return end && !(*end);
}


// Make symbol
// NOTE: "s" MUST be a free-able string unless it is considered "static"
// (see function symbolIsStatic).
// NOTE: does NOT make a copy of the given "s" string
LizpVal *valCreateSymbol(char *s) {
    LizpVal *p = valAllocKind(VK_SYMBOL);
    if (p) {
        p->symbol = s; 
    }
    return p;
}


// Make symbol
// - copies buf to take as a name
// - empty string -> null []
LizpVal *valCreateSymbolCopy(const char *buf, unsigned len) {
    return valCreateSymbol(stringCopy(buf, len));
}


// Make symbol by copying the null-terminated `str`
LizpVal *valCreateSymbolStr(const char *str) {
    return valCreateSymbolCopy(str, strlen(str));
}


// Make a symbol for an integer
LizpVal *valCreateInteger(long n) {
    const char *fmt = "%ld";
    const unsigned sz = snprintf(NULL, 0, fmt, n);
    char buf[sz + 1];
    snprintf(buf, sizeof(buf), fmt, n);
    return valCreateSymbolCopy(buf, sz);
}


// Make a proper list.
// - first: sym or list (null included)
// - rest: list (NULL included)
// NOTE: rest must be NULL or a list LizpVal.
LizpVal *valCreateList(LizpVal *first, LizpVal *rest) {
    if (!valIsList(rest)) { return NULL; } // allow proper lists only
    LizpVal *p = valAllocKind(VK_LIST);
    if (p) {
        p->first = first;
        p->rest = rest;
    }
    return p;
}


static LizpVal *valCreateFunc(LizpFunc *f) {
    LizpVal *p = valAllocKind(VK_FUNC);
    if (p) { p->func = f; }
    return p;
}


static LizpVal *valCreateMacro(LizpMacro *m) {
    LizpVal *p = valAllocKind(VK_MACRO);
    if (p) { p->macro = m; }
    return p;
}


// New copy, with no structure-sharing
LizpVal *valCopy(const LizpVal *p) {
    if (!p) { 
        return NULL; 
    }
    else {
        switch(p->kind) {
        case VK_SYMBOL:
            return valCreateSymbolStr(p->symbol); 
        case VK_FUNC:
            return valCreateFunc(p->func); 
        case VK_MACRO:
            return valCreateMacro(p->macro); 
        case VK_LIST:
            {
                // Copy list
                LizpVal *copy = valCreateList(valCopy(p->first), NULL);
                LizpVal *pcopy = copy;
                p = p->rest;
                while (valIsList(p) && p) {
                    pcopy->rest = valCreateList(valCopy(p->first), NULL);
                    pcopy = pcopy->rest;
                    p = p->rest;
                }
                return copy;
            }
        default:
            return NULL;
        }
    }
}


// String needs quotes?
// Check if a string for a symbol name needs to be quoted
// in order to be printed "readably".
bool StrNeedsQuotes(const char *s) {
    while (*s) {
        switch (*s) {
        case '[':
        case ']':
        case '(':
        case ')':
        case '\n':
        case '\t':
        case ' ':
        case '\r':
        case '"':
        case '\\':
            return true;
        default:
            break;
        }
        s++;
    }
    return false;
}

// Escape a string.
//   (or is this called "un-escaping"?)
// Converts escape sequences into the corresponding real ASCII
// values.
// Modifies the string in-place
unsigned EscapeStr(char *str, unsigned len) {
    if (!str || len <= 0) { return 0; }
    unsigned r = 0; // read index
    unsigned w = 0; // write index
    while (r < len && str[r]) {
        char c = str[r];
        if (c == '\\') {
            r++;
            c = str[r];
            switch (c) {
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
unsigned SkipChars(const char *str, unsigned len) {
    unsigned i = 0;
    unsigned level = 0; // nesting level
    while (i < len) {
        char c = str[i];
        if (!c) { break; }
        if (c == '(') {
            i++;
            level++;
            continue;
        }
        if (level && c == ')') {
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
unsigned valReadOneFromBuffer(const char *str, unsigned len, LizpVal **out) {
    if (!out || !str || len <= 0) { return 0; }
    unsigned i = 0;
    i += SkipChars(str + i, len - i);
    switch (str[i]) {
    case '\0': // end of string
        *out = valCreateErrorMessage("reached an unexpected end of input");
        return i;
    case ']':  // unmatched list
        *out = valCreateErrorMessage("read an unexpected '['");
        return i;
    case '(':  // uncaught comment
        *out = valCreateErrorMessage("read an unexpected '('");
        return i;
    case ')':  // uncaught comment
        *out = valCreateErrorMessage("read an unexpected ')'");
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
            LizpVal *e;
            unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
            if (!l)
            {
                *out = NULL;
                return i;
            };
            i += l;
            LizpVal *list = valCreateList(e, NULL);
            LizpVal *p = list;
            // rest of items
            while (i < len && str[i] != ']') {
                LizpVal *e;
                unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
                i += l;
                p->rest = valCreateList(e, NULL);
                p = p->rest;
                if (l <= 0) {
                    *out = list;
                    return l;
                }
                i += SkipChars(str + i, len - i);
            }
            *out = list;
            if (str[i] == ']') {
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
unsigned valReadAllFromBuffer(const char *str, unsigned len, LizpVal **out)
{
    unsigned i = 0;
    LizpVal *val = NULL;

    // Read first item... (may be the only item)
    unsigned read_len = valReadOneFromBuffer(str + i, len - i, &val);
    if (!read_len) { return 0; }
    i += read_len;
    i += SkipChars(str + i, len - i);
    if (valIsError(val) || i >= len || !str[i])
    {
        *out = val;
        return 1;
    }

    // Read additional items into a list...
    unsigned n; // number of items read
    val = valCreateList(val, NULL);
    LizpVal *p = val;
    for (n = 1; i < len && str[i]; n++, p = p->rest)
    {
        LizpVal *e = NULL;
        read_len = valReadOneFromBuffer(str + i, len - i, &e);
        if (valIsError(e))
        {
            valFreeRec(val);
            val = e;
            break;
        }
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
unsigned valWriteToBuffer(const LizpVal *v, char *out, unsigned length, bool readable)
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
char *valWriteToNewString(const LizpVal *v, bool readable)
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


bool valListLengthIsMoreThan(const LizpVal *l, unsigned len)
{
    while (l && len)
    {
        len--;
        l = l->rest;
    }
    return !len && l;
}


bool valListLengthIsLessThan(const LizpVal *l, unsigned len)
{
    while (l && len)
    {
        len--;
        l = l->rest;
    }
    return len && !l;
}


// inclusive range
bool valListLengthIsWithin(const LizpVal *l, unsigned min, unsigned max)
{
    return !valListLengthIsLessThan(l, min) && !valListLengthIsMoreThan(l, max);
}


// Get the length of a list.
// Returns 0 for a non-list value.
unsigned valListLength(const LizpVal *l)
{
    unsigned len = 0;
    while (l && valIsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}


long valAsInteger(const LizpVal *v) {
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}


// Get value right after a symbol in a list
bool valGetListItemAfterSymbol(LizpVal *list, const char *symname, LizpVal **out)
{
    if (!valIsList(list)) { return 0; }
    while (list)
    {
        LizpVal *e = list->first;
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


LizpVal *valGetListIndex(LizpVal *list, size_t index)
{
    while (list && index) { list = list->rest; }
    return list;
}


// Get the Nth item in a list (0-based index)
LizpVal *NthItem(LizpVal *list, unsigned n)
{
    if (!valIsList(list)) { return NULL; }
    for (; n > 0; n--)
    {
        list = list->rest;
    }
    return list? list->first : list;
}

// Check if two values do not share structure
bool IsSeparate(LizpVal *a, LizpVal *b)
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
        LizpVal *t = b;
        b = a;
        a = t;
    }
    return IsSeparate(a->first, b)
        && IsSeparate(a->rest, b);
}


// Check whether a value is considered as true (right now, ONLY #t is true)
bool valIsTrue(const LizpVal *v) { 
    return v
        && valIsSymbol(v) 
        && v->symbol == const_true;
}


LizpVal *valCreateTrue(void) { return valCreateSymbolStr(const_true); }


LizpVal *valCreateFalse(void) { return NULL; }


// make a list of the form [error rest...]
LizpVal *valCreateError(LizpVal *rest) {
    LizpVal *e = valCreateSymbol(const_error); // direct reference to the constant `static` error symbol string
    if (valIsList(rest)) { return valCreateList(e, rest); }
    return valCreateList(e, valCreateList(rest, NULL));
}


LizpVal *valCreateErrorMessage(const char *msg) {
    return valCreateError(valCreateSymbolStr(msg));
}

// Check whether a value is a lambda value (special list)
// - 1st element must be the special "const_lambda" pointer
// - 2nd element must be an argument list
// - 3rd element is the lambda body
// - total length of this list: 3
bool valIsLambda(const LizpVal *v) {
    // Value must be a list
    if (!v || !valIsList(v)) { return false; }
    // Check lambda pointer (1st element)
    LizpVal *l = v->first;
    if (!l || !valIsSymbol(l) || l->symbol != const_lambda) { return false; }
    // Check parameters list (2nd element)
    if (!v->rest) { return false; }
    LizpVal *params = v->rest->first;
    if (!valIsList(params)) { return false; }
    LizpVal *pp = params;
    while (pp && valIsList(pp)) {
        if (!valIsSymbol(pp->first)) { return false; }
        pp = pp->rest;
    }
    // check that the length is not more than 3
    if (!v->rest->rest) { return false; }
    return true;
}


// check if the value matches the form [error ...]
bool valIsError(const LizpVal *v) {
    if (!v || !valIsList(v)) { return 0; }
    LizpVal *first = v->first;
    return valIsSymbol(first) && first->symbol == const_error; // must be the exact const_error pointer
}


// Set value in environment
// Key and LizpVal Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet(LizpVal *env, LizpVal *key, LizpVal *val)
{
    if (!env || !valIsList(env)) { return 0; }
    LizpVal *pair = valCreateList(key, valCreateList(val, NULL));
    if (!pair) { return 0; }
    // push key-value pair onto the front of the list
    env->first = valCreateList(pair, env->first);
    return 1;
}


// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet_const(LizpVal *env, const LizpVal *key, const LizpVal *val)
{
    LizpVal *key_copy = valCopy(key);
    LizpVal *val_copy = valCopy(val);
    return EnvSet(env, key_copy, val_copy);
}


// Environment Get.
// Get value in environment, does not return a copy
// Return value: whether the symbol is present
bool EnvGet(LizpVal *env, const LizpVal *key, LizpVal **out)
{
    if (!env) { return 0; }
    LizpVal *scope = env;
    while (scope && valIsList(scope))
    {
        LizpVal *p = scope->first;
        while (p && valIsList(p))
        {
            LizpVal *pair = p->first;
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
void EnvPush(LizpVal *env)
{
    if (!env) { return; }
    env->rest = valCreateList(env->first, env->rest);
    env->first = NULL;
}


// pop off the latest context from the environment
void EnvPop(LizpVal *env)
{
    if (!env) { return; }
    valFreeRec(env->first);
    LizpVal *pair = env->rest;
    env->first = pair->first;
    env->rest = pair->rest;
    // Only free the one pair
    valFree(pair);
}


// Return values must not share structure with first, args, or env
static LizpVal *ApplyLambda(LizpVal *first, LizpVal *args, LizpVal *env) {
    LizpVal *params = first->rest->first;
    LizpVal *body = first->rest->rest->first;
    // push env
    EnvPush(env);
    // bind values
    LizpVal *p_params = params;
    LizpVal *p_args = args;
    while (p_params && valIsList(p_params) && p_args && valIsList(p_args))
    {
        LizpVal *param = p_params->first;
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
    if ((p_params == NULL) != (p_args == NULL)) {
        // error
        EnvPop(env);
        return NULL;
    }
    LizpVal *result = evaluate(body, env);
    EnvPop(env);
    return result;
}


// Functions should return copied values
static LizpVal *ApplyNative(LizpVal *f, LizpVal *args)
{
    return f->func(args);
}


// Macros should return copied values
static LizpVal *ApplyMacro(LizpVal *m, LizpVal *args, LizpVal *env)
{
    return m->macro(args, env);
}


// Apply functions
// Return values must not share structure with first, args, or env
static LizpVal *Apply(LizpVal *first, LizpVal *args, LizpVal *env) {
    if (valIsLambda(first)) { return ApplyLambda(first, args, env); }
    if (valIsFunc(first)) { return ApplyNative(first, args); }
    // invalid function
    return valCreateError(valCreateList(valCopy(first),
                              valCreateList(valCreateSymbolStr("is not a function"),
                                       NULL)));
}


// Evaluate each item in a list
LizpVal *evaluateList(LizpVal *list, LizpVal *env) {
    if (!list || !valIsList(list)) { return NULL; }
    LizpVal *result = valCreateList(NULL, NULL);
    LizpVal *p_result = result;
    while (list && valIsList(list)) {
        LizpVal *e = evaluate(list->first, env);
        if (valIsError(e)) {
            valFreeRec(result);
            return e;
        }

        p_result->first = e;
        if (list->rest) {
            p_result->rest = valCreateList(NULL, NULL);
        }
        p_result = p_result->rest;
        list = list->rest;
    }
    return result;
}


// Evaluate a LizpVal value
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs for bindings
// Returns the evaluated value
// NOTE: must only return new values that do not share any
//       structure with the ast or the env
LizpVal *evaluate(const LizpVal *ast, LizpVal *env) {
    if (!ast) { 
        // empty list
        return NULL; 
    } 
    if (valIsInteger(ast)) {
        // integers are self-evaluating
        return valCopy(ast); 
    }
    if (valIsLambda(ast)) {
        // lambda values are self-evaluating
        return valCopy(ast); 
    }
    if (valIsSymbol(ast)) {
        // lookup symbol value
        LizpVal *val;
        if (EnvGet(env, ast, &val)) {
            return valCopy(val);
        }
        // symbol not found
        LizpVal *name = valCopy(ast);
        return valCreateError(
                valCreateList(name,
                    valCreateList(valCreateSymbolStr("is undefined"),
                        NULL)));
    }
    // evaluate list application...
    LizpVal *first = evaluate(ast->first, env);
    if (valIsError(first)) { return first; }
    if (valIsMacro(first)) { return ApplyMacro(first, ast->rest, env); }
    // evaluate rest of elements for normal function application
    LizpVal *args = evaluateList(ast->rest, env);
    if (valIsError(args)) {
        valFreeRec(first);
        return args;
    }
    LizpVal *result = Apply(first, args, env);
    valFreeRec(first);
    valFreeRec(args);
    return result;
}


// Environment Set Function
// Set a symbol value to be associated with a C function
bool EnvSetFunc(LizpVal *env, const char *name, LizpFunc *func) {
    if (!env || !name || !func) {
        return 0; 
    }

    LizpVal *key = valCreateSymbolStr(name);
    if (!key) { return false; }

    LizpVal *val = valCreateFunc(func);
    if (!val) {
        valFreeRec(key);
        return 0;
    }

    bool success = EnvSet(env, key, val);
    if (!success) {
        valFreeRec(key);
        valFreeRec(val);
    }

    return success;
}


// Environment set macro extended.
// Add a custom C macro to the environment.
bool EnvSetMacro(LizpVal *env, const char *name, LizpMacro *m) {
    if (!env || !name || !m) { return 0; }

    LizpVal *key = valCreateSymbolStr(name);
    if (!key) { return false; }

    LizpVal *val = valCreateMacro(m);
    if (!val) {
        valFreeRec(key);
        return 0;
    }

    bool success = EnvSet(env, key, val);
    if (!success) {
        valFreeRec(key);
        valFreeRec(val);
    }

    return success;
}


// Environment Set Symbol
bool EnvSetSym(LizpVal *env, const char *sym, LizpVal *v) {
    if (!env || !sym) { return 0; }
    return EnvSet(env, valCreateSymbolStr(sym), v);
}


void lizpRegisterCore(LizpVal *env)
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
    EnvSetFunc(env, "<", strictly_increasing_func);
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
    EnvSetFunc(env, "length", length_func);
    EnvSetFunc(env, "not", not_func);
    EnvSetFunc(env, "nth", nth_func);
}


// [prepend val list]
LizpVal *prepend_func(LizpVal *args) {
    LizpVal *v = args->first;
    LizpVal *list = args->rest->first;
    return valCreateList(valCopy(v), valCopy(list));
}

// [+ (integer)...] sum
LizpVal *plus_func(LizpVal *args) {
    long sum = 0;
    LizpVal *p = args;
    while (p) {
        LizpVal *e = p->first;
        if (!valIsInteger(e)) { return valCreateErrorMessage("not an integer number"); }
        sum += valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(sum);
}

// [* (integer)...] product
LizpVal *multiply_func(LizpVal *args) {
    long product = 1;
    LizpVal *p = args;
    while (p)
    {
        LizpVal *e = p->first;
        product *= valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(product);
}

// [- x:int (y:int)] subtraction
LizpVal *subtract_func(LizpVal *args) {
    if (!valListLengthIsWithin(args, 1, 2)) { return valCreateErrorMessage("takes 1 or 2 arguments"); }
    LizpVal *vx = args->first;
    long x = atol(vx->symbol);
    if (!args->rest) { return valCreateInteger(-x); }
    LizpVal *vy = args->rest->first;
    long y = atol(vy->symbol);
    return valCreateInteger(x - y);
}

// [/ x:int y:int] division
LizpVal *divide_func(LizpVal *args) {
    long x = valAsInteger(NthItem(args, 0));
    long y = valAsInteger(NthItem(args, 1));
    if (y == 0) {
        // division by zero
        return valCreateErrorMessage("division by zero");
    }
    return valCreateInteger(x / y);
}

// [% x:int y:int] modulo
LizpVal *mod_func(LizpVal *args) {
    long x = valAsInteger(NthItem(args, 0));
    long y = valAsInteger(NthItem(args, 1));
    if (y == 0) {
        // division by zero
        return valCreateErrorMessage("division by zero");
    }
    return valCreateInteger(x % y);
}

// [= x y (expr)...] check equality
LizpVal *equal_func(LizpVal *args) {
    LizpVal *f = args->first;
    LizpVal *p = args->rest;
    while (p && valIsList(p)) {
        if (!valIsEqual(f, p->first)) {
            return valCreateFalse(); 
        }
        p = p->rest;
    }
    return valCreateTrue();
}

// [not expr] boolean not
LizpVal *not_func(LizpVal *args) {
    return valIsTrue(args->first)? valCreateFalse() : valCreateTrue();
}

// [symbol? val] check if value is a symbol
LizpVal *symbol_q_func(LizpVal *args) {
    LizpVal *v = args->first;
    return !valIsSymbol(v)? valCreateTrue() : valCreateFalse();
}

// [integer? val] check if value is a integer symbol
LizpVal *integer_q_func(LizpVal *args) {
    return valIsInteger(args->first)? valCreateTrue() : valCreateFalse();
}

// [list? val] check if value is a list
LizpVal *list_q_func(LizpVal *args) {
    return valIsList(args->first)? valCreateTrue() : valCreateFalse();
}

// [empty? val] check if value is a the empty list
LizpVal *empty_q_func(LizpVal *args) {
    return (!args->first)? valCreateTrue() : valCreateFalse();
}

// [nth index list] get the nth item in a list
LizpVal *nth_func(LizpVal *args) {
    LizpVal *i = args->first;
    LizpVal *list = args->rest->first;
    long n = atol(i->symbol);
    if (n < 0) {
        // index negative
        return valCreateErrorMessage("index cannot be negative");
    }
    LizpVal *p = list;
    while (n > 0 && p && valIsList(p)) {
        p = p->rest;
        n--;
    }
    if (p) { return valCopy(p->first); }
    return valCreateErrorMessage("index too big");
}

// [list (val)...] create list from arguments (variadic)
LizpVal *list_func(LizpVal *args) {
    return valCopy(args);
}

// [length list]
LizpVal *length_func(LizpVal *args) {
    return valCreateInteger(valListLength(args->first));
}

// [lambda? v]
LizpVal *lambda_q_func(LizpVal *args) {
    LizpVal *v = args->first;
    return valIsLambda(v)? valCreateTrue() : valCreateFalse();
}


// [function? v]
LizpVal *function_q_func(LizpVal *args) {
    LizpVal *v = args->first;
    return (valIsFunc(v) || valIsLambda(v))? valCreateTrue() : valCreateFalse();
}


// [native? v]
LizpVal *native_q_func(LizpVal *args) {
    LizpVal *v = args->first;
    return (valIsFunc(v))? valCreateTrue() : valCreateFalse();
}


// [< x y (expr)...] check number order
LizpVal *strictly_increasing_func(LizpVal *args) {
    LizpVal *f = args->first;
    long x = atol(f->symbol);
    LizpVal *p = args->rest;
    while (p && valIsList(p))
    {
        LizpVal *e = p->first;
        long y = atol(e->symbol);
        if (!(x < y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}


// [chars sym] -> list
LizpVal *chars_func(LizpVal *args) {
    LizpVal *sym = args->first;
    char *s = sym->symbol;
    LizpVal *result = valCreateList(valCreateSymbolCopy(s, 1), NULL);
    s++;
    LizpVal *p = result;
    while (*s) {
        p->rest = valCreateList(valCreateSymbolCopy(s, 1), NULL);
        p = p->rest;
        s++;
    }
    return result;
}


// [symbol list] -> symbol
LizpVal *symbol_func(LizpVal *args) {
    LizpVal *list = args->first;
    int len = valListLength(list);
    char *sym = malloc(1 + len);
    int i = 0;
    LizpVal *p = list;
    while (p && valIsList(list)) {
        LizpVal *e = p->first;
        if (!valIsSymbol(e)) {
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
LizpVal *member_q_func(LizpVal *args) {
    if (!valListLengthIsWithin(args, 2, 2)) {
        return valCreateErrorMessage("`member?` function requires 2 parameters"); 
    }
    else {
        LizpVal *item = args->first;
        LizpVal *list = args->rest->first;
        while (list && valIsList(list)) {
            if (valIsEqual(list->first, item)) {
                return valCreateTrue(); 
            }
            list = list->rest;
        }
        return valCreateFalse();
    }
}


// (macro) [let [key val...] expr]
// create bindings
LizpVal *let_func(LizpVal *args, LizpVal *env) {
    LizpVal *bindings = args->first;
    if (!valIsList(bindings)) {
        return valCreateErrorMessage("`let` macro's first argument should be a list");
    }
    LizpVal *body = args->rest->first;
    // create and check bindings
    EnvPush(env);
    LizpVal *p_binds = bindings;
    while (p_binds && valIsList(p_binds)) {
        LizpVal *sym = p_binds->first;
        if (!valIsSymbol(sym) || !p_binds->rest || !valIsList(p_binds->rest)) {
            // invalid symbol or uneven amount of args
            EnvPop(env);
            return valCreateErrorMessage(
                    "`let` bindings list must consist of alternating symbols and expressions");
        }
        p_binds = p_binds->rest;
        LizpVal *expr = p_binds->first;
        LizpVal *val = evaluate(expr, env);
        if (valIsError(val)) {
            // eval error
            EnvPop(env);
            return val;
        }
        EnvSet(env, valCopy(sym), val);
        p_binds = p_binds->rest;
    }
    // eval body
    LizpVal *result = evaluate(body, env);
    // destroy bindings
    EnvPop(env);
    return result;
}

// (macro) [if condition consequent (alternative)]
LizpVal *if_func(LizpVal *args, LizpVal *env) {
    if (!valListLengthIsWithin(args, 2, 3)) { return valCreateErrorMessage("`if` macro requires 2 or 3 expressions"); }
    LizpVal *f = evaluate(args->first, env);
    if (valIsError(f)) { return f; } // eval error
    int t = valIsTrue(f);
    valFreeRec(f);
    if (t) {
        LizpVal *consequent = args->rest->first;
        return evaluate(consequent, env);
    }
    LizpVal *alt_list = args->rest->rest;
    if (!alt_list) { return NULL; } // no alternative
    LizpVal *alternative = alt_list->first;
    return evaluate(alternative, env);
}

// (macro) [quote expr]
LizpVal *quote_func(LizpVal *args, LizpVal *env) {
    (void)env;
    if (!valListLengthIsWithin(args, 1, 1)) {
        return valCreateErrorMessage("`quote` macro requires 1 expression");
    }
    else {
        LizpVal *arg1 = args->first;
        return valCopy(arg1);
    }
}

// (macro) [do (expr)...]
LizpVal *do_func(LizpVal *args, LizpVal *env) {
    LizpVal *p = args;
    LizpVal *e = NULL;
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
LizpVal *and_func(LizpVal *args, LizpVal *env) {
    LizpVal *p = args;
    while (p && valIsList(p)) {
        LizpVal *e = evaluate(p->first, env);
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
LizpVal *or_func(LizpVal *args, LizpVal *env) {
    LizpVal *p = args;
    while (p && valIsList(p)) {
        LizpVal *e = evaluate(p->first, env);
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
LizpVal *cond_func(LizpVal *args, LizpVal *env) {
    unsigned n = valListLength(args);
    if ((n < 2) || (n % 2)) {
        return valCreateErrorMessage("`cond` requires an even amount of"
                                " alternating condition expressions and"
                                " consequence expressions");
    }
    LizpVal *p = args;
    while (p && valIsList(p)) {
        LizpVal *e = evaluate(p->first, env);
        if (valIsError(e)) { return e; }
        if (valIsTrue(e)) {
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
LizpVal *lambda_func(LizpVal *args, LizpVal *env) {
    (void)env;
    if (!valListLengthIsWithin(args, 2, 2)) { return valCreateErrorMessage("lambda macro requires 2 arguments"); }
    LizpVal *params = args->first;
    if (!valIsList(params)) {
        return valCreateErrorMessage("`lambda` first argument must be a list of"
                                " symbols");
    }
    LizpVal *p = params;
    // params must be symbols
    while (p && valIsList(p)) {
        LizpVal *e = p->first;
        if (!valIsSymbol(e)) {
            return valCreateError(
                valCreateList(valCopy(e),
                         valCreateList(valCreateSymbolStr("is not a symbol"),
                                  NULL)));
        }
        p = p->rest;
    }
    // make lambda... with an explicit NULL body if a body is not provided
    LizpVal *body = args->rest;
    if (body) { body = body->first; }
    return valCreateList(valCreateSymbolStr(const_lambda),
                    valCreateList(valCopy(params),
                             valCreateList(valCopy(body),
                                      NULL)));
}


#endif /* LIZP_IMPLEMENTATION */

/*
MIT License

Copyright (c) 2023 Izak Nathanael Halseide

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
