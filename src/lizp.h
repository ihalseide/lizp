/*
LIZP

    Programming language and linked list data serialization.
    This is written in the way of the C99 standard.
    The license for this is at the end of the file.

Notes:

    None
    TODO: fix the EnvGet and EnvSet working with arrays. RIght now the symbols cannot be found!

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
    VK_FREE = 0,  // free, for memory management
    VK_SYMBOL,    // string symbol
    VK_LIST,      // list=array
    VK_FUNC,      // built-in function
    VK_MACRO,     // built-in macro
} LizpValKind;


typedef struct LizpArray {
    struct LizpVal **start;
    unsigned length;
    unsigned capacity;
} LizpArray;


typedef struct LizpVal {
    LizpValKind kind;
    union {
        char *symbol;              // string symbol
        LizpArray *list;           // list=array
        LizpFunc *func;            // built-in function
        LizpMacro *macro;          // built-in macro
        struct LizpVal *nextFree;  // only for memory management
    };
} LizpVal;


// memory management
void *LizpAlloc(unsigned size);
void LizpFree(void *pointer);
LizpVal *valAlloc(void);
LizpVal *valAllocKind(LizpValKind k);
void valFree(LizpVal *p);
void valFreeRec(LizpVal *p);

// value creation
LizpVal *valCopy(const LizpVal *p);
LizpVal *valCreateInteger(long n);
LizpVal *valCreateList(LizpArray *list);
LizpVal *valCreateListFirst(LizpVal *first);
LizpVal *valCreateSymbol(char *string);
LizpVal *valCreateSymbolCopy(const char *start, unsigned len);
LizpVal *valCreateSymbolStr(const char *string);
LizpVal *valCreateTrue(void);
LizpVal *valCreateError(LizpVal *rest);
LizpVal *valCreateErrorMessage(const char *msg);
LizpVal *valCreateFalse(void);

// value type checking
bool valIsInteger(const LizpVal *v);
bool valIsList(const LizpVal *v);
bool valIsSymbol(const LizpVal *v);
bool valIsFunc(const LizpVal *v);
bool valIsMacro(const LizpVal *v);
bool valIsError(const LizpVal *v);
bool valIsLambda(const LizpVal *v);
bool valIsTrue(const LizpVal *v);

// value utility functions
bool valGetListItemAfterSymbol(LizpVal *list, const char *symbol, LizpVal **out);
bool valIsEqual(const LizpVal *x, const LizpVal *y);
bool valListLengthIsLessThan(const LizpVal *l, unsigned n);
bool valListLengthIsMoreThan(const LizpVal *l, unsigned n);
bool valListLengthIsWithin(const LizpVal *l, unsigned min, unsigned max);
long valAsInteger(const LizpVal *v);
unsigned valListLength(const LizpVal *l);

// LizpArray methods
LizpArray *LizpArrayMake(unsigned initialCapacity);
LizpArray *LizpArrayMakeFrom(LizpArray *list, unsigned firstIndex);
LizpVal **LizpArrayGet(LizpArray *list, unsigned index);
LizpVal *LizpArrayFirst(LizpArray *list);
unsigned LizpArrayTrimCapacity(LizpArray *list);
void LizpArrayAppend(LizpArray *list, LizpVal *val);
void LizpArrayGrow(LizpArray *list);
void LizpArrayPrepend(LizpArray *list, LizpVal *val);
void LizpArrayRemove(LizpArray *list, unsigned index);

// value serialization & deserialization
unsigned valReadOneFromBuffer(const char *start, unsigned length, LizpVal **out);
unsigned valReadAllFromBuffer(const char *start, unsigned length, LizpVal **out);
unsigned valWriteToBuffer(const LizpVal *p, char *out, unsigned length, bool readable);
char *valWriteToNewString(const LizpVal *p, bool readable);

// evaluation
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
#if 0
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
#endif

#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h> // for snprintf


// static LizpVal *pool; // TODO: use
static const char const_lambda[] = "lambda";
static const char const_true[] = "#t";
static const char const_error[] = "error";


void *LizpAlloc(unsigned size) {
    return malloc(size);
}


void LizpFree(void *pointer) {
    free(pointer);
}

// Create a newly allocated string that is a copy from `buf` of length `len`
static /*new*/ char *stringCopy(const char *buf, unsigned len) {
    if (!buf) { return NULL; }
    char *s = LizpAlloc(len + 1);
    if (s) {
        memcpy(s, buf, len);
        s[len] = 0;
    }
    return s;
}


// Allocate a new value
// Potential problem: memory use currently cannot shrink
LizpVal *valAlloc() {
    // todo: use memory pool
    return LizpAlloc(sizeof(LizpVal));
}


LizpVal *valAllocKind(LizpValKind k) {
    LizpVal *p = valAlloc();
    if (p) {
        p->kind = k;
    }
    return p;
}


static bool symbolIsStatic(const char *string);


// Return a pointer to a LispVal* so that it can be modified
LizpVal **LizpArrayGet(LizpArray *list, unsigned index) {
    if (index >= list->length) {
        return NULL;
    }
    return &list->start[index];
}


// Get the first element in a Lizp List
LizpVal *LizpArrayFirst(LizpArray *list) {
    LizpVal **loc = LizpArrayGet(list, 0);
    if (!loc) {
        return NULL;
    }
    return *loc;
}


// Create a new LizpArray
LizpArray *LizpArrayMake(unsigned initialCapacity) {
    assert(initialCapacity > 0); // temporory, for debugging now
    LizpArray *p = LizpAlloc(sizeof(*p));
    if (p) {
        p->length = 0;
        p->capacity = initialCapacity;
        p->start = LizpAlloc(initialCapacity * sizeof(*p->start));
    }
    return p;
}


// Returns a new LizpArray with the copied contents of the given one, but withot the first #"firstIndex" items
// NOTE: this does not do a deep copy (just the pointers for the list elements are copied)!
LizpArray *LizpArrayMakeFrom(LizpArray *list, unsigned firstIndex) {
    unsigned cap = list->length - firstIndex;
    LizpArray *result = LizpArrayMake(cap);
    for (unsigned i = firstIndex; i < list->length; i++) {
        LizpArrayAppend(result, *LizpArrayGet(list, i));
    }
    return result;
}


// Grow the capacity for a LizpArray
void LizpArrayGrow(LizpArray *list) {
    unsigned newCapacity = list->capacity * 2;
    unsigned nBytes = list->length * sizeof(*list->start);
    LizpVal **newArray = LizpAlloc(newCapacity * sizeof(*newArray));
    assert(newArray);
    if (list->start && nBytes != 0) {
        memcpy(newArray, list->start, nBytes);
    }
    LizpFree(list->start);
    list->start = newArray;
    list->capacity = newCapacity;
    assert(list->capacity >= list->length);
}


// Shrink down the capacity to the actual used length (opposite of grow)
unsigned LizpArrayTrimCapacity(LizpArray *list) {
    assert(list->capacity >= list->length);
    unsigned nBytes = list->length * sizeof(*list->start);
    LizpVal *newArray = LizpAlloc(list->length * sizeof(*newArray));
    memcpy(newArray, list->start, nBytes);
    LizpFree(list->start);
    list->start = newArray;
    list->capacity = list->length;
    assert(list->capacity >= list->length);
}


// Append a LizpVal to a LizpArray
void LizpArrayAppend(LizpArray *list, LizpVal *val) {
    if (list->length == list->capacity) {
        LizpArrayGrow(list);
    }
    assert(list->capacity >= list->length);
    list->length++;
    list->start[list->length - 1] = val;
    assert(list->capacity >= list->length);
}


void LizpArrayPrepend(LizpArray *list, LizpVal *val) {
    assert(list->capacity >= list->length);
    if (list->length == list->capacity) {
        LizpArrayGrow(list);
    }
    unsigned nBytes = list->length * sizeof(*list->start);
    list->length++;
    LizpVal **loc0 = LizpArrayGet(list, 0);
    LizpVal **loc1 = LizpArrayGet(list, 1);
    memmove(loc1, loc0, nBytes);
    *loc0 = val;
    assert(list->capacity >= list->length);
}


void LizpArrayRemove(LizpArray *list, unsigned index) {
    assert(index == 0); // other indices not implemented yet
    assert(list->capacity >= list->length);
    LizpVal **loc0 = LizpArrayGet(list, index);
    LizpVal **loc1 = LizpArrayGet(list, index + 1);
    unsigned nBytes = (list->length - 1) * sizeof(*list->start);
    memmove(loc0, loc1, nBytes); // move [loc1...end] to [loc0...end-1]
    list->length--;
    assert(list->capacity >= list->length);
}


bool LizpArrayEqual(const LizpArray *a, const LizpArray *b) {
    if (a == NULL || b == NULL) { return false; }
    if (a->length != b->length) { return false; }
    for (unsigned i = 0; i < a->length; i++) {
        LizpVal **ae = LizpArrayGet(a, i);
        LizpVal **be = LizpArrayGet(b, i);
        assert(ae != NULL);
        assert(be != NULL);
        if (!valIsEqual(*ae, *be)) {
            return false;
        }
    }
    return true;
}


// Free value
void valFree(LizpVal *p) {
    if (!p) { return; }
    if (valIsSymbol(p) && p->symbol && !symbolIsStatic(p->symbol)) {
        LizpFree(p->symbol);
    }
    else if (valIsList(p) && p->list != NULL) {
        LizpFree(p->list);
    }
    // TODO: use memory pool
    LizpFree(p);
}


// Free value recursively
void valFreeRec(LizpVal *v) {
    if (!v) { return; }
    if (v->kind == VK_LIST) {
        LizpArray *l = v->list;
        unsigned length = l->length;
        for (unsigned i = 0; i < length; i++) {
            valFreeRec(*LizpArrayGet(l, i));
        }
    }
    valFree(v);
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
    if (x == NULL || y == NULL) { return x == y; }
    if (x->kind != y->kind) { return false; }
    switch (x->kind) {
    case VK_SYMBOL:
        return symbolsEqual(x->symbol, y->symbol);
    case VK_LIST:
        return LizpArrayEqual(x->list, y->list);
    case VK_FUNC:
        return x->func == y->func; 
    case VK_MACRO:
        return x->macro == y->macro; 
    default:
        return false;
    }
}


// Check if a value is a list
// NULL is NOT considered an empty list
bool valIsList(const LizpVal *v) {
    return v && v->kind == VK_LIST;
}


// Check if a value is a symbol
bool valIsSymbol(const LizpVal *v) {
    return v && v->kind == VK_SYMBOL;
}


bool valIsFunc(const LizpVal *v) {
    return v && v->kind == VK_FUNC; 
}


bool valIsMacro(const LizpVal *v) {
    return v && v->kind == VK_MACRO; 
}


//  check if a value is a integer symbol
bool valIsInteger(const LizpVal *v) {
    if (!valIsSymbol(v)) { return false; }
    const unsigned base = 10;
    char *end;
    strtol(v->symbol, &end, base);
    return end && 0 == *end;
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
LizpVal *valCreateList(LizpArray *list) {
    LizpVal *p = valAllocKind(VK_LIST);
    if (p) {
        p->list = list;
    }
    return p;
}


// Create a new list value with a given first element
LizpVal *valCreateListFirst(LizpVal *first) {
    LizpArray *list = LizpArrayMake(1);
    LizpArrayAppend(list, first);
    return valCreateList(list);
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
    if (p) { 
        switch(p->kind) {
        case VK_SYMBOL:
            return valCreateSymbolStr(p->symbol); 
        case VK_FUNC:
            return valCreateFunc(p->func); 
        case VK_MACRO:
            return valCreateMacro(p->macro); 
        case VK_LIST:
            {
                // Copy list (deep copy)
                const LizpArray *list = p->list;
                LizpArray *copy = LizpArrayMake(list->length);
                for (unsigned i = 0; i < list->length; i++) {
                    const LizpVal **listSrc = LizpArrayGet(list, i);
                    assert(listSrc);
                    LizpArrayAppend(copy, valCopy(*listSrc));
                }
                return valCreateList(copy);
            }
        }
    }
    return NULL; 
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
            if (str[i] == ']') {
                i++;
                *out = NULL;
                return i;
            }
            // first item
            LizpVal *e;
            unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
            if (!l) {
                *out = NULL;
                return i;
            };
            i += l;
            LizpVal *list = valCreateListFirst(e);
            // rest of items
            while (i < len && str[i] != ']') {
                LizpVal *e;
                unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
                i += l;
                LizpArrayAppend(list->list, e);
                if (l <= 0) {
                    LizpArrayTrimCapacity(list->list);
                    *out = list;
                    return l;
                }
                i += SkipChars(str + i, len - i);
            }
            LizpArrayTrimCapacity(list->list);
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
            switch (str[i]) {
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
                    while (!done && i < len) {
                        switch (str[i]) {
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
                    if (good && out) {
                        // Make escaped symbol string
                        unsigned len = i - j - 1;
                        if (len == 0) {
                            *out = NULL;
                            return i;
                        }
                        char *str1 = LizpAlloc(len + 1);
                        memcpy(str1, str + j, len);
                        str1[len] = 0;
                        unsigned len2 = EscapeStr(str1, len);
                        *out = valCreateSymbolCopy(str1, len2);
                        LizpFree(str1);
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
                    while (!done && i < len) {
                        switch (str[i]) {
                        case '\0':
                        case '"':
                        case '[':
                        case ']':
                        case '(':
                        case ')':
                            done = 1;
                            break;
                        default:
                            if (isspace(str[i])) {
                                done = 1;
                                break;
                            }
                            i++;
                            break;
                        }
                    }
                    if (out) {
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
    if (valIsError(val) || i >= len || !str[i]) {
        *out = val;
        return 1;
    }

    // Read additional items into a list...
    unsigned n = 1; // number of items read
    val = valCreateListFirst(val);
    LizpVal *p = val;
    for (; i < len && str[i]; n++) {
        LizpVal *e = NULL;
        read_len = valReadOneFromBuffer(str + i, len - i, &e);
        if (valIsError(e)) {
            valFreeRec(val);
            val = e;
            break;
        }
        if (!read_len) { break; }
        i += read_len;
        i += SkipChars(str + i, len - i);
        LizpArrayAppend(val->list, e);
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
    if (valIsList(v)) {
        if (out && i < length) { out[i] = '['; }
        i++;
        if (v) {
            // first item
            if (out) {
                i += valWriteToBuffer(LizpArrayFirst(v->list), out + i, length - i, readable);
            }
            else {
                i += valWriteToBuffer(LizpArrayFirst(v->list), NULL, 0, readable);
            }
            for (unsigned j = 1; j < v->list->length; j++) {
                // space
                if (out && i < length) { out[i] = ' '; }
                i++;
                // item
                LizpVal *vv = *LizpArrayGet(v->list, j);
                if (out) {
                    i += valWriteToBuffer(vv, out + i, length - i, readable);
                }
                else {
                    i += valWriteToBuffer(vv, NULL, 0, readable);
                }
            }
        }
        if (out && i < length) { out[i] = ']'; }
        i++;
        return i;
    }
    else if (valIsSymbol(v)) {
        // Symbol
        char *s = v->symbol;
        bool quoted = readable && StrNeedsQuotes(s);
        if (quoted) {
            // Opening quote
            if (out && i < length) {
                out[i] = '"';
            }
            i++;
        }
        // Contents
        while (*s) {
            char c = *s;
            if (quoted) {
                // escaping
                bool esc = false;
                switch (c) {
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
                if (esc) {
                    if (out && i < length) {
                        out[i] = '\\';
                    }
                    i++;
                }
            }
            if (out && i < length) {
                out[i] = c;
            }
            i++;
            s++;
        }
        if (quoted) {
            // Closing quote
            if (out && i < length) {
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
    else if (v == NULL) {
        const char txt[] = "nil";
        const size_t len = sizeof(txt) - 1;
        if (out) { memcpy(out, txt, len); }
        return len;
    }
    else {
        const char txt[] = "<invalid LIZP value>";
        const size_t len = sizeof(txt) - 1;
        if (out) { memcpy(out, txt, len); }
        return len;
        return 0;
    }
}

// Print value to a new string
char *valWriteToNewString(const LizpVal *v, bool readable)
{
    unsigned len1 = valWriteToBuffer(v, NULL, 0, readable);
    if (len1 <= 0) { return NULL; }
    char *s = LizpAlloc(len1 + 1);
    if (!s) { return NULL; }
    unsigned len2 = valWriteToBuffer(v, s, len1, readable);
    if (len1 != len2) { return NULL; } // should not happen unless there is a bug in valWriteToBuffer()
    s[len2] = '\0';
    return s;
}


bool valListLengthIsMoreThan(const LizpVal *l, unsigned len) {
    return l->list->length > len;
}


bool valListLengthIsLessThan(const LizpVal *l, unsigned len) {
    return l->list->length < len;
}


// inclusive range
bool valListLengthIsWithin(const LizpVal *l, unsigned min, unsigned max) {
    return !valListLengthIsLessThan(l, min) && !valListLengthIsMoreThan(l, max);
}


// Get the length of a list.
// Returns 0 for a non-list value.
unsigned valListLength(const LizpVal *l) {
    return l->list->length;
}


long valAsInteger(const LizpVal *v) {
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}


// Get value right after a symbol in a list
bool valGetListItemAfterSymbol(LizpVal *list, const char *symname, LizpVal **out) {
    *out = NULL;
    if (!valIsList(list)) { return false; }
    for (unsigned i = 0; i < list->list->length; i++) {
        LizpVal *e = *LizpArrayGet(list->list, i);
        if (valIsSymbol(e) && !strcmp(e->symbol, symname)) {
            // found a spot
            LizpVal **loc = LizpArrayGet(list->list, i + 1);
            if (loc) {
                *out = *loc;
                return true;
            }
        }
    }
    return false;
}


// Get the Nth item in a list (0-based index)
LizpVal *NthItem(LizpVal *list, unsigned n) {
    if (!valIsList(list)) { return NULL; }
    LizpVal **loc = LizpArrayGet(list->list, n);
    if (loc) {
        return *loc;
    }
    else {
        return NULL;
    }
}


// Check whether a value is considered as true (right now, ONLY #t is true)
bool valIsTrue(const LizpVal *v) { 
    return valIsSymbol(v) && v->symbol == const_true;
}


LizpVal *valCreateTrue(void) { return valCreateSymbolStr(const_true); }


LizpVal *valCreateFalse(void) { return NULL; }


// make a list of the form [error rest...]
LizpVal *valCreateError(LizpVal *data) {
    LizpVal *e = valCreateSymbol(const_error); // direct reference to the constant `static` error symbol string
    LizpVal *result = valCreateListFirst(e);
    if (valIsList(data)) { 
        for (unsigned i = 0; i < data->list->length; i++) {
            LizpVal *val = *LizpArrayGet(data->list, i);
            LizpArrayAppend(result->list, val);
        }
        return result;
    }
    else {
        LizpArrayAppend(result->list, data);
        return result;
    }
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
    // Value must be a 3-element list
    if (v == NULL || !valIsList(v) || v->list->length != 3) { return false; }
    // Check lambda pointer (1st element)
    LizpVal *l = *LizpArrayGet(v->list, 0);
    if (!l || !valIsSymbol(l) || l->symbol != const_lambda) { return false; }
    // Check parameters list (2nd element)
    LizpVal *params = *LizpArrayGet(v->list, 1);
    if (!valIsList(params)) { return false; }
    for (unsigned i = 0; i < params->list->length; i++) {
        // All parameters must be a symbol
        LizpVal **loc = LizpArrayGet(params->list, i);
        if (!valIsSymbol(*loc)) { return false; }
    }
    return true;
}


// check if the value matches the form [error ...]
bool valIsError(const LizpVal *v) {
    if (!v || !valIsList(v) || v->list->length < 1) { return false; }
    LizpVal *first = *LizpArrayGet(v->list, 0);
    return valIsSymbol(first) && first->symbol == const_error; // must be the exact const_error pointer
}


// Set value in environment
// Key and LizpVal Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet(LizpVal *env, LizpVal *key, LizpVal *val) {
    if (!env || !valIsList(env)) { return false; }
    LizpVal *pair = valCreateListFirst(key);
    LizpArrayAppend(pair->list, val);
    // push key-value pair onto the front of the list
    LizpArrayPrepend(env->list, pair);
    return true;
}


// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet_const(LizpVal *env, const LizpVal *key, const LizpVal *val) {
    LizpVal *key_copy = valCopy(key);
    LizpVal *val_copy = valCopy(val);
    return EnvSet(env, key_copy, val_copy);
}


// Environment Get.
// Get value in environment, does not return a copy
// Return value: whether the symbol is present
bool EnvGet(LizpVal *env, const LizpVal *key, LizpVal **out) {
    assert(env);
    assert(out != NULL);
    if (!env || !valIsList(env)) { return 0; }
    // Iterate the "scopes" list which is the Env
    for (unsigned i = 0; i < env->list->length; i++) {
        LizpVal *scope = *LizpArrayGet(env->list, i);
        //assert(scope && valIsList(scope));
        if (!scope) { continue; }
        for (unsigned j = 0; j < scope->list->length; j++) {
            LizpVal *pair = *LizpArrayGet(scope->list, j);
            if (pair && valIsList(pair)) {
                LizpArray *pairList = pair->list;
                if ((pair->list->length > 2) && valIsEqual(*LizpArrayGet(pair->list, 0), key)) {
                    *out = *LizpArrayGet(pair->list, 1);
                    return true;
                }
            }
        }
    }
    *out = NULL;
    return false;
}


// push a new context onto the environment
void EnvPush(LizpVal *env) {
    assert(valIsList(env));
    LizpArrayPrepend(env->list, valCreateList(1));
}


// pop off the latest context from the environment
void EnvPop(LizpVal *env) {
    assert(valIsList(env));
    LizpVal *loc0 = LizpArrayFirst(env->list);
    valFreeRec(loc0);
    LizpArrayRemove(env->list, 0);
}


// Return values must not share structure with first, args, or env
static LizpVal *ApplyLambda(LizpVal *first, LizpVal *args, LizpVal *env) {
    assert(valIsList(first));
    assert(valIsList(args));
    assert(valIsList(env));
    // push env
    EnvPush(env);
    // bind values
    LizpVal *params = NthItem(first, 1);
    LizpVal *body = NthItem(first, 2);
    unsigned remainingParams = params->list->length;
    unsigned remainingArgs = args->list->length;
    for (unsigned i = 0; i < params->list->length; i++) {
        LizpVal *param = *LizpArrayGet(params, i);
        LizpVal *arg = *LizpArrayGet(args, i);
        assert(valIsSymbol(param));
        if ('&' == param->symbol[0] && i + 1 == params->list->length) {
            // the last parameter beginning with '&' binds the rest of the arguments
            LizpVal *restArgs = valCreateListFirst(valCopy(arg));
            for (unsigned j = i; j < args->list->length; j++) {
                LizpVal **argLocJ = LizpArrayGet(args->list, j);
                LizpArrayAppend(restArgs->list, valCopy(*argLocJ));
                remainingArgs--;
            }
            EnvSet(env, valCopy(param), restArgs);
            remainingParams--;
        }
        else {
            // normal parameter
            EnvSet(env, valCopy(param), valCopy(arg));
            remainingArgs--;
            remainingParams--;
        }
    }
    // check a parameters-arguments arity mismatch
    if (remainingParams != 0 || remainingArgs != 0) {
        // error
        EnvPop(env);
        assert(0 && "better lambda fail not implemented yet");
        return NULL;
    }
    LizpVal *result = evaluate(body, env);
    EnvPop(env);
    return result;
}


// Functions should return copied values
static LizpVal *ApplyNative(LizpVal *f, LizpVal *args) {
    return f->func(args);
}


// Macros should return copied values
static LizpVal *ApplyMacro(LizpVal *m, LizpVal *args, LizpVal *env) {
    return m->macro(args, env);
}


// Apply functions
// Return values must not share structure with first, args, or env
static LizpVal *Apply(LizpVal *first, LizpVal *args, LizpVal *env) {
    if (valIsLambda(first)) { return ApplyLambda(first, args, env); }
    if (valIsFunc(first)) { return ApplyNative(first, args); }
    // invalid function
    LizpArray *l = LizpArrayMake(2);
    LizpArrayAppend(l, valCopy(first));
    LizpArrayAppend(l, valCreateSymbolStr("is not a function"));
    return valCreateError(valCreateList(l));
}


// Evaluate each item in a list
LizpVal *evaluateList(LizpVal *list, LizpVal *env) {
    if (!list || !valIsList(list)) { return NULL; }
    LizpArray *result = LizpArrayMake(list->list->length);
    for (unsigned i = 0; i < list->list->length; i++) {
        LizpVal *e = evaluate(*LizpArrayGet(list->list, i), env);
        if (valIsError(e)) {
            LizpFree(result);
            return e;
        }
        LizpArrayAppend(result, e);
    }
    return valCreateList(result);
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
        LizpArray *errList = LizpArrayMake(2);
        LizpArrayAppend(errList, name);
        LizpArrayAppend(errList, valCreateSymbolStr("is undefined"));
        return valCreateError(valCreateList(errList));
    }
    // evaluate list application...
    LizpVal *first = evaluate(*LizpArrayGet(ast->list, 0), env);
    if (valIsError(first)) { return first; }
    if (valIsMacro(first)) {
        LizpVal *theRest = valCreateList(LizpArrayMakeFrom(ast->list, 1));
        return ApplyMacro(first, theRest, env); 
    }
    // evaluate rest of elements for normal function application
    LizpVal *theRest = valCreateList(LizpArrayMakeFrom(ast->list, 1));
    LizpVal *args = evaluateList(theRest, env);
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
    /*
    EnvSetMacro(env, "quote", quote_func);
    EnvSetMacro(env, "if", if_func);
    EnvSetMacro(env, "cond", cond_func);
    EnvSetMacro(env, "do", do_func);
    EnvSetMacro(env, "^", lambda_func);
    EnvSetMacro(env, "and", and_func);
    EnvSetMacro(env, "or", or_func);
    EnvSetMacro(env, "let", let_func);
    */
    // functions
    EnvSetFunc(env, "+", plus_func);
    EnvSetFunc(env, "*", multiply_func);
    EnvSetFunc(env, "/", divide_func);
    EnvSetFunc(env, "-", subtract_func);
    EnvSetFunc(env, "%", mod_func);
    EnvSetFunc(env, "=", equal_func);
    /*
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
    */
}


// [+ (integer)...] sum
LizpVal *plus_func(LizpVal *args) {
    long sum = 0;
    for (unsigned i = 0; i < args->list->length; i++) {
        LizpVal *e = *LizpArrayGet(args->list, i);
        if (!valIsInteger(e)) { return valCreateErrorMessage("not an integer number"); }
        sum += valAsInteger(e);
    }
    return valCreateInteger(sum);
}


// [* (integer)...] product
LizpVal *multiply_func(LizpVal *args) {
    long product = 1;
    for (unsigned i = 0; i < args->list->length; i++) {
        LizpVal *e = NthItem(args, i);
        if (!valIsInteger(e)) { return valCreateErrorMessage("not an integer number"); }
        product *= valAsInteger(e);
    }
    return valCreateInteger(product);
}

// [- x:int (y:int)] subtraction
LizpVal *subtract_func(LizpVal *args) {
    if (!valListLengthIsWithin(args, 1, 2)) { return valCreateErrorMessage("takes 1 or 2 arguments"); }
    LizpVal *vx = NthItem(args, 0);
    long x = atol(vx->symbol);
    if (args->list->length == 1) {
        return valCreateInteger(-x); 
    }
    else {
        LizpVal *vy = NthItem(args, 1);
        long y = atol(vy->symbol);
        return valCreateInteger(x - y);
    }
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
    LizpVal *first = NthItem(args, 0);
    for (unsigned i = 0; i < args->list->length; i++) {
        if (!valIsEqual(first, *LizpArrayGet(args->list, i))) {
            return valCreateFalse(); 
        }
    }
    return valCreateTrue();
}

// [not expr] boolean not
LizpVal *not_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return valIsTrue(arg1)? valCreateFalse() : valCreateTrue();
}

// [symbol? val] check if value is a symbol
LizpVal *symbol_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return (!valIsSymbol(arg1))? valCreateTrue() : valCreateFalse();
}

// [integer? val] check if value is a integer symbol
LizpVal *integer_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return valIsInteger(arg1)? valCreateTrue() : valCreateFalse();
}

// [list? val] check if value is a list
LizpVal *list_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return valIsList(arg1)? valCreateTrue() : valCreateFalse();
}

// [empty? val] check if value is a the empty list
LizpVal *empty_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return (valIsList(arg1) && arg1->list->length == 0)? valCreateTrue() : valCreateFalse();
}

// [nth index list] get the nth item in a list
LizpVal *nth_func(LizpVal *args) {
    LizpVal *i = NthItem(args, 0);
    LizpVal *list = NthItem(args, 1);
    long n = atol(i->symbol);
    if (n < 0) {
        // index negative
        return valCreateErrorMessage("index cannot be negative");
    }
    LizpVal **loc = LizpArrayGet(list->list, n);
    if (loc == NULL) {
        return valCreateErrorMessage("index too big");
    }
    else {
        return valCopy(*loc);
    }
}

// [list (val)...] create list from arguments (variadic)
LizpVal *list_func(LizpVal *args) {
    return valCopy(args);
}

// [length list]
LizpVal *length_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return valCreateInteger(valListLength(arg1));
}

// [lambda? v]
LizpVal *lambda_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return valIsLambda(arg1)? valCreateTrue() : valCreateFalse();
}


// [function? v]
LizpVal *function_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return (valIsFunc(arg1) || valIsLambda(arg1))? valCreateTrue() : valCreateFalse();
}


// [native? v]
LizpVal *native_q_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    return (valIsFunc(arg1))? valCreateTrue() : valCreateFalse();
}


// [chars sym] -> list
LizpVal *chars_func(LizpVal *args) {
    LizpVal *arg1 = NthItem(args, 0);
    const char *s = arg1->symbol;
    LizpArray *result = LizpArrayMake(strlen(s));
    for (unsigned i = 0; i < result->length; i++) {
        LizpArrayAppend(result, valCreateSymbolCopy(&s[i], 1));
    }
    return valCreateList(result);
}


// [member? item list]
LizpVal *member_q_func(LizpVal *args) {
    if (!valListLengthIsWithin(args, 2, 2)) {
        return valCreateErrorMessage("`member?` function requires 2 parameters"); 
    }
    else {
        LizpVal *item = NthItem(args, 0);
        LizpVal *list = NthItem(args, 1);
        for (unsigned i = 0; i < list->list->length; i++) {
            LizpVal *e = *LizpArrayGet(list->list, i);
            if (valIsEqual(e, item)) {
                return valCreateTrue(); 
            }
        }
        return valCreateFalse();
    }
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
