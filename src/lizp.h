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

// Val: (tagged union)
typedef struct Val
{
    ValKind_t kind;
    union
    {
        char *symbol;
        struct
        {
            struct Val *first;
            struct Val *rest;
        };
    };
} Val_t;

// memory management
Val_t *valAlloc(void);
Val_t *valCopy(Val_t *p);
void valFree(Val_t *p);
void valFreeRec(Val_t *p);

// value creation
Val_t *valCreateInteger(long n);
Val_t *valCreateList(Val_t *first, Val_t *rest);
Val_t *valCreateSymbol(char *string);
Val_t *valCreateSymbolCopy(const char *start, unsigned len);
Val_t *valCreateSymbolStr(const char *string);

// value type checking
ValKind_t valKind(Val_t *v);
bool valIsInteger(Val_t *v);
bool valIsList(Val_t *v);
bool valIsSymbol(Val_t *v);

// value utility functions
bool argsIsMatchForm(const char *form, Val_t *args, Val_t **err);
bool valGetListItemAfterSymbol(Val_t *list, const char *symbol, Val_t **out);
bool valIsEqual(Val_t *x, Val_t *y);
long valAsInteger(Val_t *v);
unsigned valListLength(Val_t *l);

// value serialization
unsigned valReadOneFromBuffer(const char *start, unsigned length, Val_t **out);
unsigned valReadAllFromBuffer(const char *start, unsigned length, Val_t **out);
unsigned valWriteToBuffer(Val_t *p, char *out, unsigned length, bool readable);
char *valWriteToNewString(Val_t *p, bool readable);

#ifdef LIZP_EVAL

typedef Val_t *LizpFunc_t(Val_t *args);
typedef Val_t *LizpMacro_t(Val_t *args, Val_t *env);

typedef struct FuncRecord
{
    const char *name;
    const char *form;
    LizpFunc_t *func;
} FuncRecord_t;

typedef struct MacroRecord
{
    const char *name;
    const char *form;
    LizpMacro_t *macro;
} MacroRecord_t;

Val_t *valCreateTrue(void);
Val_t *valCreateError(Val_t *rest);
Val_t *valCreateErrorMessage(const char *msg);
Val_t *valCreateFalse(void);

bool valIsError(Val_t *v);
bool valIsFunction(Val_t *v);
bool valIsLambda(Val_t *v);
bool valIsTrue(Val_t *v);

Val_t *evaluate(Val_t *ast, Val_t *env);
Val_t *evaluateList(Val_t *list, Val_t *env);
bool EnvGet(Val_t *env, Val_t *key, Val_t **out);
bool EnvSet(Val_t *env, Val_t *key, Val_t *val);
bool EnvSetFunc(Val_t *env, const char *name, LizpFunc_t *func);
bool EnvSetMacro(Val_t *env, const char *name, LizpMacro_t *macro);
bool EnvSetFuncEx(Val_t *env, const char *name, const char *form, LizpFunc_t *func);
bool EnvSetMacroEx(Val_t *env, const char *name, const char *form, LizpMacro_t *macro);
bool EnvSetSym(Val_t *env, const char *symbol, Val_t *val);
void EnvPop(Val_t *env);
void EnvPush(Val_t *env);

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS
// For optional core Lizp functions that would be useful for most lizp scripts

void lizpRegisterCore(Val_t *env);
Val_t *reverse_func(Val_t *args);    // [reverse list] reverse a list
Val_t *concat_func(Val_t *args);     // [concat list.1 (list.N)...] concatenate lists together
Val_t *join_func(Val_t *args);       // [join separator (list)...] join together each list with the separator list in between
Val_t *without_func(Val_t *args);    // [without item list] remove all occurrences of item from the list
Val_t *replace_func(Val_t *args);    // [replace item1 item2 list] replace all occurrences of item1 in list with item2
Val_t *replace1_func(Val_t *args);   // [replaceN item1 item2 list n] replace up to n of item1 with item2 in list
Val_t *replaceI_func(Val_t *args);   // [replaceI index item list] replace element in list at index with item
Val_t *zip_func(Val_t *args);        // [zip list.1 (list.N)...]
Val_t *append_func(Val_t *args);     // [append val list]
Val_t *prepend_func(Val_t *args);    // [prepend val list]
Val_t *print_func(Val_t *args);      // [print (v)...]
Val_t *plus_func(Val_t *args);       // [+ integers...] sum
Val_t *multiply_func(Val_t *args);   // [* integers...] product
Val_t *subtract_func(Val_t *args);   // [- x (y)] subtraction
Val_t *divide_func(Val_t *args);     // [/ x y] division
Val_t *mod_func(Val_t *args);        // [% x y] modulo
Val_t *equal_func(Val_t *args);      // [= x y (expr)...] check equality
Val_t *not_func(Val_t *args);        // [not expr] boolean not
Val_t *symbol_q_func(Val_t *args);   // [symbol? val] check if value is a symbol
Val_t *integer_q_func(Val_t *args);  // [integer? val] check if value is a integer symbol
Val_t *list_q_func(Val_t *args);     // [list? val] check if value is a list
Val_t *empty_q_func(Val_t *args);    // [empty? val] check if value is a the empty list
Val_t *nth_func(Val_t *args);        // [nth index list] get the nth item in a list
Val_t *list_func(Val_t *args);       // [list (val)...] create list from arguments (variadic)
Val_t *length_func(Val_t *args);     // [length list]
Val_t *lambda_q_func(Val_t *args);   // [lambda? v]
Val_t *function_q_func(Val_t *args); // [function? v]
Val_t *native_q_func(Val_t *args);   // [native? v]
Val_t *increasing_func(Val_t *args); // [<= x y (expr)...] check number order
Val_t *decreasing_func(Val_t *args); // [>= x y (expr)...] check number order
Val_t *strictly_increasing_func(Val_t *args);   // [< x y (expr)...] check number order
Val_t *strictly_decreasing_func(Val_t *args);   // [> x y (expr)...] check number order
Val_t *chars_func(Val_t *args);      // [chars sym] -> list
Val_t *symbol_func(Val_t *args);     // [symbol list] -> symbol
Val_t *member_q_func(Val_t *args);   // [member? item list] -> boolean value
Val_t *count_func(Val_t *args);      // [count item list] -> integer symbol
Val_t *position_func(Val_t *args);   // [position item list] -> integer symbol
Val_t *slice_func(Val_t *args);      // [slice list start (end)] gets a sublist "slice" inclusive of start and end

// macros
Val_t *quote_func(Val_t *args, Val_t *env);   // [quote expr]
Val_t *if_func(Val_t *args, Val_t *env);      // [if condition consequence alternative]
Val_t *cond_func(Val_t *args, Val_t *env);    // [cond condition1 consequence1 condition2 consequence2 ...]
Val_t *do_func(Val_t *args, Val_t *env);      // [do expr ...]
Val_t *lambda_func(Val_t *args, Val_t *env);  // [lambda [args ...] body-expr]
Val_t *and_func(Val_t *args, Val_t *env);     // [and expr1 expr2 ...]
Val_t *or_func(Val_t *args, Val_t *env);      // [or expr1 expr2 ...]
Val_t *let_func(Val_t *args, Val_t *env);     // [let [sym1 expr1 sym2 expr2 ...] body-expr]

#endif /* LIZP_CORE_FUNCTIONS */
#endif /* _lizp_h_ */

#ifdef LIZP_IMPLEMENTATION

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "stb_ds.h"

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
                    *err = valCreateList(valCreateSymbolStr("not enough arguments: requires at least"),
                                    valCreateList(valCreateInteger(n),
                                             valCreateList(valCreateSymbolStr(arguments),
                                                      NULL)));
                }
                return 0;
            }
            if (!isArgMatch(form[i], p->first, err))
            {
                if (err)
                {
                    // wrap message with more context
                    *err = valCreateList(valCreateSymbolStr("argument"),
                                    valCreateList(valCreateInteger(i + 1),
                                             valCreateList(*err,
                                                      NULL)));
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

// Allocate a new value
Val_t *valAlloc(void)
{
    Val_t *p = malloc(sizeof(Val_t));
    return p;
}

// Free value
void valFree(Val_t *p)
{
    if (valIsSymbol(p) && p->symbol)
    {
        free(p->symbol);
    }
    free(p);
}

// Free value recursively
void valFreeRec(Val_t *v)
{
    if (!v) { return; }
    if (valIsSymbol(v))
    {
        // Symbol
        free(v->symbol);
        valFree(v);
        return;
    }
    if (!valIsList(v)) { return; }
    // List
    Val_t *p = v;
    Val_t *n;
    while (p && valIsList(p))
    {
        valFreeRec(p->first);
        n = p->rest;
        valFree(p);
        p = n;
    }
    return;
}

bool valIsEqual(Val_t *x, Val_t *y)
{
    if (x == NULL || y == NULL)
    {
        return x == y;
    }
    if (valIsSymbol(x))
    {
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
        Val_t *px = x, *py = y;
        while (px && valIsList(px) && py && valIsList(py))
        {
            if (!valIsEqual(px->first, py->first))
            {
                break;
            }
            px = px->rest;
            py = py->rest;
        }
        return px == NULL && py == NULL;
    }
    return 0;
}

// Get a value's kind a.k.a type
ValKind_t valKind(Val_t *v)
{
    if (!v)
    {
        return VK_LIST;
    }
    return v->kind;
}

// Check if a value is a list
// NULL is also considered an empty list
bool valIsList(Val_t *p)
{
    return valKind(p) == VK_LIST;
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

// Make symbol
// NOTE: "s" MUST be a free-able string
// NOTE: does not make a copy of the "s" string
Val_t *valCreateSymbol(char *s)
{
    Val_t *p = valAlloc();
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
Val_t *valCreateList(Val_t *first, Val_t *rest)
{
    if (!valIsList(rest)) { return NULL; }
    Val_t *p = valAlloc();
    if (!p) { return p; }
    p->kind = VK_LIST;
    p->first = first;
    p->rest = rest;
    return p;
}

// New copy, with no structure-sharing
Val_t *valCopy(Val_t *p)
{
    if (!p) { return p; }
    if (valIsSymbol(p)) { return valCreateSymbolStr(p->symbol); }
    if (!valIsList(p)) {return NULL; }
    // Copy list
    Val_t *copy = valCreateList(valCopy(p->first), NULL);
    Val_t *pcopy = copy;
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
                    // - '
                    // - \
                    // - "
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
unsigned valReadOneFromBuffer(const char *str, unsigned len, Val_t **out)
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
            Val_t *e;
            unsigned l = valReadOneFromBuffer(str + i, len - i, &e);
            if (!l)
            {
                *out = NULL;
                return i;
            };
            i += l;
            Val_t *list = valCreateList(e, NULL);
            Val_t *p = list;
            // rest of items
            while (i < len && str[i] != ']')
            {
                Val_t *e;
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
            i += SkipChars(str, len);
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
                    // Symbol
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
unsigned valReadAllFromBuffer(const char *str, unsigned len, Val_t **out)
{
    unsigned i = 0;
    Val_t *val = NULL;

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
    Val_t *p = val;
    for (n = 1; i < len && str[i]; n++, p = p->rest)
    {
        Val_t *e;
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
unsigned valWriteToBuffer(Val_t *v, char *out, unsigned length, bool readable)
{
    // String output count / index
    unsigned i = 0;
    if (valIsList(v))
    {
        if (out && i < length)
        {
            out[i] = '[';
        }
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
                if (out && i < length)
                {
                    out[i] = ' ';
                }
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
        if (out && i < length)
        {
            out[i] = ']';
        }
        i++;
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
    }
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

// Get the length of a list.
// Returns 0 for a non-list value.
unsigned valListLength(Val_t *l)
{
    unsigned len = 0;
    while (l && valIsList(l))
    {
        len++;
        l = l->rest;
    }
    return len;
}

long valAsInteger(Val_t *v)
{
    if (!valIsInteger(v)) { return 0; }
    return atol(v->symbol);
}

// Get value right after a symbol in a list
bool valGetListItemAfterSymbol(Val_t *list, const char *symname, Val_t **out)
{
    if (!valIsList(list)) { return 0; }
    while (list)
    {
        Val_t *e = list->first;
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

// Get the Nth item in a list (0-based index)
Val_t *NthItem(Val_t *list, unsigned n)
{
    if (!valIsList(list)) { return NULL; }
    for (; n > 0; n--)
    {
        list = list->rest;
    }
    return list? list->first : list;
}

// Check if two values do not share structure
bool IsSeparate(Val_t *a, Val_t *b)
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
        Val_t *t = b;
        b = a;
        a = t;
    }
    return IsSeparate(a->first, b)
        && IsSeparate(a->rest, b);
}

#ifdef LIZP_EVAL

// Dynamic array of function pointers for native Lizp Functions
FuncRecord_t *da_funcs;

// Dynamic array of function pointers for native Lizp Macros
MacroRecord_t *da_macros;

// Put function in the dynamic array
// Meant to be used by `EnvSetFunc` to bind native functions
static size_t PutFunc(const char *name, const char *form, LizpFunc_t *func)
{
    FuncRecord_t r = (FuncRecord_t)
    {
        .name = name,
        .form = form,
        .func = func,
    };
    size_t id = arrlen(da_funcs);
    arrput(da_funcs, r);
    return id;
}

// Get function from the dynamic array
// Meant to be used by Apply() to look up native functions
static FuncRecord_t *GetFunc(size_t id)
{
    size_t len = arrlen(da_funcs);
    if (id < 0 || id >= len) { return NULL; }
    return &(da_funcs[id]);
}

// Put a macro function into the dynamic array
// Meant to be used by `EnvSetMacro` to bind native macros
static size_t PutMacro(const char *name, const char *form, LizpMacro_t *macro)
{
    MacroRecord_t r = (MacroRecord_t)
    {
        .name = name,
        .form = form,
        .macro = macro,
    };
    size_t id = arrlen(da_macros);
    arrput(da_macros, r);
    return id;
}

// Get macro from the dynamic array
// Meant to be used by evaluate() to look up native macros
static MacroRecord_t *GetMacro(size_t id)
{
    size_t len = arrlen(da_macros);
    if (id < 0 || id >= len) { return NULL; }
    return &(da_macros[id]);
}

// Check whether a value is considered as true
bool valIsTrue(Val_t *v)
{
    return v != NULL;
}

Val_t *valCreateTrue(void)
{
    return valCreateSymbolCopy("true", 4);
}

Val_t *valCreateFalse(void)
{
    return NULL;
}

// make a list of the form [error rest...]
Val_t *valCreateError(Val_t *rest)
{
    Val_t *e = valCreateSymbolStr("error");
    if (valIsList(rest)) { return valCreateList(e, rest); }
    return valCreateList(e, valCreateList(rest, NULL));
}

Val_t *valCreateErrorMessage(const char *msg)
{
    return valCreateError(valCreateSymbolStr(msg));
}

// Check whether a value is a lambda value (special list)
bool valIsLambda(Val_t *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val_t *l = v->first;
    if (!l || !valIsSymbol(l)) { return 0; }
    if (strcmp(l->symbol, "lambda")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *params = v->rest->first;
    if (!valIsList(params)) { return 0; }
    Val_t *pp = params;
    while (pp && valIsList(pp))
    {
        if (!valIsSymbol(pp->first)) { return 0; }
        pp = pp->rest;
    }
    if (!v->rest->rest) { return 0; }
    return 1;
}

// Check if a list is a wrapper for a C function
// (a "native func")
// [func number]
bool valIsFunction(Val_t *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val_t *sym = v->first;
    if (!sym) { return 0; }
    if (!valIsSymbol(sym)) { return 0; }
    if (strcmp(sym->symbol, "native func")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *id = v->rest->first;
    if (!valIsInteger(id)) { return 0; }
    if (v->rest->rest) { return 0; } // too many items
    return 1;
}

// Check if a list is a wrapper for a native C macro
// (a "native macro")
// [macro number]
bool IsMacro(Val_t *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val_t *sym = v->first;
    if (!sym ) { return 0; }
    if (!valIsSymbol(sym)) { return 0; }
    if (strcmp(sym->symbol, "native macro")) { return 0; }
    if (!v->rest) { return 0; }
    Val_t *id = v->rest->first;
    if (!valIsInteger(id)) { return 0; }
    if (v->rest->rest) { return 0; } // too many items
    return 1;
}

// check if the value matches the form [error ...]
bool valIsError(Val_t *v)
{
    if (!v || !valIsList(v)) { return 0; }
    Val_t *first = v->first;
    return valIsSymbol(first) && !strcmp("error", first->symbol);
}

// Set value in environment
// Arguments should by copies of Values
// Returns non-zero upon success
bool EnvSet(Val_t *env, Val_t *key, Val_t *val)
{
    if (!env || !valIsList(env)) { return 0; }
    Val_t *pair = valCreateList(key, valCreateList(val, NULL));
    if (!pair) { return 0; }
    // push key-value pair onto the front of the list
    env->first = valCreateList(pair, env->first);
    return 1;
}

// Environment Get.
// Get value in environment, does not return a copy
// Return value: whether the symbol is present
bool EnvGet(Val_t *env, Val_t *key, Val_t **out)
{
    if (!env) { return 0; }
    Val_t *scope = env;
    while (scope && valIsList(scope))
    {
        Val_t *p = scope->first;
        while (p && valIsList(p))
        {
            Val_t *pair = p->first;
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
void EnvPush(Val_t *env)
{
    if (!env) { return; }
    env->rest = valCreateList(env->first, env->rest);
    env->first = NULL;
}

// pop off the latest context from the environment
void EnvPop(Val_t *env)
{
    if (!env) { return; }
    valFreeRec(env->first);
    Val_t *pair = env->rest;
    env->first = pair->first;
    env->rest = pair->rest;
    // Only free the one pair
    valFree(pair);
}

// Return values must not share structure with first, args, or env
static Val_t *ApplyLambda(Val_t *first, Val_t *args, Val_t *env)
{
    Val_t *params = first->rest->first;
    Val_t *body = first->rest->rest->first;
    // push env
    EnvPush(env);
    // bind values
    Val_t *p_params = params;
    Val_t *p_args = args;
    while (p_params && valIsList(p_params) && p_args && valIsList(p_args))
    {
        Val_t *param = p_params->first;
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
    Val_t *result = evaluate(body, env);
    EnvPop(env);
    return result;
}

// Apply a native function
static Val_t *ApplyNative(Val_t *first, Val_t *args)
{
    long id = atol(first->rest->first->symbol);
    FuncRecord_t *record = GetFunc(id);
    if (!record || !record->func)
    {
        // error: invalid function id or pointer
        return valCreateError(valCreateList(valCopy(first),
                                  valCreateList(valCreateSymbolStr("is not a native function id"),
                                           NULL)));
    }
    if (record->form)
    {
        Val_t *err;
        bool match = argsIsMatchForm(record->form, args, &err);
        if (!match && record->name)
        {
            return valCreateError(valCreateList(valCreateSymbolStr("native function"),
                                      valCreateList(valCreateSymbolStr(record->name),
                                               err)));
        }
        if (!match)
        {
            return valCreateError(valCreateList(valCreateSymbolStr("native function #"),
                                      valCreateList(valCreateInteger(id),
                                               err)));
        }
    }
    Val_t *result = record->func(args);
    if (valIsError(result))
    {
        // patch-in more function info
        Val_t *info;
        if (record->func)
        {
            info = valCreateList(valCreateSymbolStr("native function "),
                            valCreateList(valCreateSymbolStr(record->name),
                                     result->rest));
            result->rest = info;
            return result;
        }
        // no name
        info = valCreateList(valCreateSymbolStr("native function #"),
                        valCreateList(valCreateInteger(id),
                                 result->rest));
        result->rest = info;
        return result;
    }
    return result;
}

// Apply functions
// Return values must not share structure with first, args, or env
static Val_t *Apply(Val_t *first, Val_t *args, Val_t *env)
{
    if (valIsLambda(first)) { return ApplyLambda(first, args, env); }
    if (valIsFunction(first)) { return ApplyNative(first, args); }
    // invalid function
    return valCreateError(valCreateList(valCopy(first),
                              valCreateList(valCreateSymbolStr("is not a function"),
                                       NULL)));
}

static Val_t *ApplyMacro(Val_t *macro, Val_t *args, Val_t *env)
{
    long id = atol(macro->rest->first->symbol);
    MacroRecord_t *record = GetMacro(id);
    if (!record || !record->macro)
    {
        // error: invalid function id or pointer
        return valCreateError(valCreateList(valCopy(macro),
                                  valCreateList(valCreateSymbolStr("is not a native macro id"),
                                           NULL)));
    }
    if (record->form)
    {
        Val_t *err;
        bool match = argsIsMatchForm(record->form, args, &err);
        if (!match && record->name)
        {
            return valCreateError(valCreateList(valCreateSymbolStr("native macro"),
                                      valCreateList(valCreateSymbolStr(record->name),
                                               err)));
        }
        if (!match)
        {
            return valCreateError(valCreateList(valCreateSymbolStr("native macro #"),
                                      valCreateList(valCreateInteger(id),
                                               err)));
        }
    }
    Val_t *result = record->macro(args, env);
    if (valIsError(result))
    {
        // patch-in more function info
        Val_t *info;
        if (record->macro)
        {
            info = valCreateList(valCreateSymbolStr("native macro "),
                            valCreateList(valCreateSymbolStr(record->name),
                                     result->rest));
            result->rest = info;
            return result;
        }
        // no name
        info = valCreateList(valCreateSymbolStr("native macro #"),
                        valCreateList(valCreateInteger(id),
                                 result->rest));
        result->rest = info;
        return result;
    }
    return result;
}

// Evaluate each item in a list
Val_t *evaluateList(Val_t *list, Val_t *env)
{
    if (!list || !valIsList(list)) { return NULL; }
    Val_t *result = valCreateList(NULL, NULL);
    Val_t *p_result = result;
    while (list && valIsList(list))
    {
        Val_t *e = evaluate(list->first, env);
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

// Evaluate a Val_t value
// - ast = Abstract Syntax Tree to evaluate
// - env = environment of symbol-value pairs for bindings
// Returns the evaluated value
// NOTE: must only return new values that do not share any
//       structure with the ast or the env
Val_t *evaluate(Val_t *ast, Val_t *env)
{
    if (!ast) { return ast; } // empty list
    if (valIsInteger(ast)) { return valCopy(ast); } // integers are self-evaluating
    if (valIsLambda(ast)) { return valCopy(ast); } // lambda values are self-evaluating
    if (valIsSymbol(ast))
    {
        // lookup symbol value
        Val_t *val;
        if (EnvGet(env, ast, &val))
        {
            return valCopy(val);
        }
        // symbol not found
        Val_t *name = valCopy(ast);
        return valCreateError(
            valCreateList(name,
                     valCreateList(valCreateSymbolStr("is undefined"),
                              NULL)));
    }
    // evaluate list application...
    Val_t *first = evaluate(ast->first, env);
    if (valIsError(first)) { return first; }
    if (IsMacro(first)) { return ApplyMacro(first, ast->rest, env); }
    // evaluate rest of elements for normal function application
    Val_t *args = evaluateList(ast->rest, env);
    if (valIsError(args))
    {
        valFreeRec(first);
        return args;
    }
    Val_t *result = Apply(first, args, env);
    valFreeRec(first);
    valFreeRec(args);
    return result;
}

// Environment Set Function Extended.
// Set a symbol value to be associated with a C function
// Also, use the form string to always check the arguments before the function
// is called (see `argsIsMatchForm`).
bool EnvSetFuncEx(Val_t *env, const char *name, const char *form, LizpFunc_t *func)
{
    if (!env || !name || !func) { return 0; }
    Val_t *key = valCreateSymbolStr(name);
    if (!key) { return 0; }
    long handle = PutFunc(name, form, func);
    Val_t *val = valCreateList(valCreateSymbolCopy("native func", 11), valCreateList(valCreateInteger(handle), NULL));
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
        return 0;
    }
    return success;
}

// Environment Set Function.
// Set a symbol value to be associated with a C function
bool EnvSetFunc(Val_t *env, const char *name, LizpFunc_t *func)
{
    return EnvSetFuncEx(env, name, NULL, func);
}

// Environment set macro extended.
// Add a custom C macro to the environment.
bool EnvSetMacroEx(Val_t *env, const char *name, const char *form, LizpMacro_t *m)
{
    if (!env || !name || !m) { return 0; }
    Val_t *key = valCreateSymbolStr(name);
    if (!key) { return 0; }
    long handle = PutMacro(name, form, m);
    Val_t *val = valCreateList(valCreateSymbolCopy("native macro", 12), valCreateList(valCreateInteger(handle), NULL));
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
        return 0;
    }
    return success;
}

// Environment set macro.
// Add a custom C macro to the environment.
bool EnvSetMacro(Val_t *env, const char *name, LizpMacro_t *m)
{
    return EnvSetMacroEx(env, name, NULL, m);
}

// Environment Set Symbol
bool EnvSetSym(Val_t *env, const char *sym, Val_t *v)
{
    if (!env || !sym) { return 0; }
    return EnvSet(env, valCreateSymbolStr(sym), v);
}

#endif /* LIZP_EVAL */

#ifdef LIZP_CORE_FUNCTIONS

void lizpRegisterCore(Val_t *env)
{
    // macros
    EnvSetMacroEx(env, "quote", "v", quote_func);
    EnvSetMacroEx(env, "if", "vv&v", if_func);
    EnvSetMacroEx(env, "cond", "vv&v", cond_func);
    EnvSetMacroEx(env, "do", "&v", do_func);
    EnvSetMacroEx(env, "^", "l(v", lambda_func);
    EnvSetMacroEx(env, "and", "v&v", and_func);
    EnvSetMacroEx(env, "or", "v&v", or_func);
    EnvSetMacroEx(env, "let", "&v_func", let_func);
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
Val_t *reverse_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [concat list.1 (list.N)...]
Val_t *concat_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [join separator (list)...]
Val_t *join_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [without item list]
// Create a list without the given item
Val_t *without_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *item = NthItem(args, 0);
    Val_t *list = NthItem(args, 1);
    if (!list) { return NULL; }
    Val_t *result = NULL;
    Val_t *p = result;
    while (list)
    {
        Val_t *e = list->first;
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
Val_t *replace_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [replaceN item1 item2 list n]
Val_t *replace1_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [replaceI index item list]
Val_t *replaceI_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [zip list.1 (list.N)...]
Val_t *zip_func(Val_t *args)
{
    // TODO: implement
    return NULL;
}

// [append val list]
Val_t *append_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *v = args->first;
    Val_t *list = args->rest->first;
    Val_t *last = valCreateList(valCopy(v), NULL);
    if (!list)
    {
        // empty list -> single-item list
        return last;
    }
    // Create a new list and put "last" at the end
    Val_t *copy = valCopy(list);
    Val_t *p = copy;
    while (p->rest)
    {
        p = p->rest;
    }
    p->rest = last;
    return copy;
}

// [prepend val list]
Val_t *prepend_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *v = args->first;
    Val_t *list = args->rest->first;
    return valCreateList(valCopy(v), valCopy(list));
}

// [+ (integer)...] sum
Val_t *plus_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("&n", args, &err)) { return valCreateError(err); }
    long sum = 0;
    Val_t *p = args;
    while (p)
    {
        Val_t *e = p->first;
        sum += valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(sum);
}

// [* (integer)...] product
Val_t *multiply_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("&n", args, &err)) { return valCreateError(err); }
    long product = 1;
    Val_t *p = args;
    while (p)
    {
        Val_t *e = p->first;
        product *= valAsInteger(e);
        p = p->rest;
    }
    return valCreateInteger(product);
}

// [- x:int (y:int)] subtraction
Val_t *subtract_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("n(n", args, &err)) { return valCreateError(err); }
    Val_t *vx = args->first;
    long x = atol(vx->symbol);
    if (!args->rest) { return valCreateInteger(-x); }
    Val_t *vy = args->rest->first;
    long y = atol(vy->symbol);
    return valCreateInteger(x - y);
}

// [/ x:int y:int] division
Val_t *divide_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn", args, &err)) { return valCreateError(err); }
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
Val_t *mod_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn", args, &err)) { return valCreateError(err); }
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
Val_t *equal_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vv&v", args, &err)) { return valCreateError(err); }
    Val_t *f = args->first;
    Val_t *p = args->rest;
    while (p && valIsList(p))
    {
        if (!valIsEqual(f, p->first)) { return valCreateFalse(); }
        p = p->rest;
    }
    return valCreateTrue();
}

// [not expr] boolean not
Val_t *not_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsTrue(args->first)? valCreateFalse() : valCreateTrue();
}

// [symbol? val] check if value is a symbol
Val_t *symbol_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    Val_t *v = args->first;
    return !valIsSymbol(v)? valCreateTrue() : valCreateFalse();
}

// [integer? val] check if value is a integer symbol
Val_t *integer_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsInteger(args->first)? valCreateTrue() : valCreateFalse();
}

// [list? val] check if value is a list
Val_t *list_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsList(args->first)? valCreateTrue() : valCreateFalse();
}

// [empty? val] check if value is a the empty list
Val_t *empty_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return (!args->first)? valCreateTrue() : valCreateFalse();
}

// [nth index list] get the nth item in a list
Val_t *nth_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nl", args, &err)) { return valCreateError(err); }
    Val_t *i = args->first;
    Val_t *list = args->rest->first;
    long n = atol(i->symbol);
    if (n < 0)
    {
        // index negative
        return valCreateErrorMessage("index cannot be negative");
    }
    Val_t *p = list;
    while (n > 0 && p && valIsList(p))
    {
        p = p->rest;
        n--;
    }
    if (p) { return valCopy(p->first); }
    return valCreateErrorMessage("index too big");
}

// [list (val)...] create list from arguments (variadic)
Val_t *list_func(Val_t *args)
{
    return valCopy(args);
}

// [length list]
Val_t *length_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("l", args, &err)) { return valCreateError(err); }
    return valCreateInteger(valListLength(args->first));
}

// [lambda? v]
Val_t *lambda_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsLambda(args->first)? valCreateTrue() : valCreateFalse();
}

// [function? v]
Val_t *function_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valIsFunction(args->first)? valCreateTrue() : valCreateFalse();
}

// [native? v]
Val_t *native_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    Val_t *v = args->first;
    return (valIsFunction(v) || valIsLambda(v))? valCreateTrue() : valCreateFalse();
}

// [<= x y (expr)...] check number order
Val_t *increasing_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && valIsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x <= y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [>= x y (expr)...] check number order
Val_t *decreasing_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && valIsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x >= y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [< x y (expr)...] check number order
Val_t *strictly_increasing_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && valIsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x < y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [> x y (expr)...] check number order
Val_t *strictly_decreasing_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("nn&n", args, &err)) { return valCreateError(err); }
    Val_t *f = args->first;
    long x = atol(f->symbol);
    Val_t *p = args->rest;
    while (p && valIsList(p))
    {
        Val_t *e = p->first;
        long y = atol(e->symbol);
        if (!(x > y)) { return valCreateFalse(); }
        x = y;
        p = p->rest;
    }
    return valCreateTrue();
}

// [chars sym] -> list
Val_t *chars_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("s", args, &err)) { return valCreateError(err); }
    Val_t *sym = args->first;
    char *s = sym->symbol;
    Val_t *result = valCreateList(valCreateSymbolCopy(s, 1), NULL);
    s++;
    Val_t *p = result;
    while (*s)
    {
        p->rest = valCreateList(valCreateSymbolCopy(s, 1), NULL);
        p = p->rest;
        s++;
    }
    return result;
}

// [symbol list] -> symbol
Val_t *symbol_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("L", args, &err)) { return valCreateError(err); }
    Val_t *list = args->first;
    int len = valListLength(list);
    char *sym = malloc(1 + len);
    int i = 0;
    Val_t *p = list;
    while (p && valIsList(list))
    {
        Val_t *e = p->first;
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
Val_t *member_q_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
    while (list && valIsList(list))
    {
        if (valIsEqual(list->first, item)) { return valCreateTrue(); }
        list = list->rest;
    }
    return valCreateFalse();
}

// [count item list] -> int
Val_t *count_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
    long count = 0;
    while (list && valIsList(list))
    {
        if (valIsEqual(list->first, item)) { count++; }
        list = list->rest;
    }
    return valCreateInteger(count);
}

// [position item list] -> list
Val_t *position_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("vl", args, &err)) { return valCreateError(err); }
    Val_t *item = args->first;
    Val_t *list = args->rest->first;
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
Val_t *slice_func(Val_t *args)
{
    Val_t *err;
    if (!argsIsMatchForm("ln(n", args, &err)) { return valCreateError(err); }
    Val_t *list = args->first;
    Val_t *start = args->rest->first;
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
        Val_t *result = valCreateList(valCopy(list->first), NULL);
        list = list->rest;
        Val_t *p_result = result;
        while (list && valIsList(list))
        {
            p_result->rest = valCreateList(valCopy(list->first), NULL);
            p_result = p_result->rest;
            list = list->rest;
        }
        return result;
    }
    // [slice list start end]
    Val_t *end = args->rest->rest->first;
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
    Val_t *result = valCreateList(valCopy(list->first), NULL);
    list = list->rest;
    Val_t *p_result = result;
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
Val_t *let_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("Lv", args, &err)) { return valCreateError(err); }
    Val_t *bindings = args->first;
    Val_t *body = args->rest->first;
    // create and check bindings
    EnvPush(env);
    Val_t *p_binds = bindings;
    while (p_binds && valIsList(p_binds))
    {
        Val_t *sym = p_binds->first;
        if (!valIsSymbol(sym) || !p_binds->rest || !valIsList(p_binds->rest))
        {
            // invalid symbol or uneven amount of args
            EnvPop(env);
            return valCreateErrorMessage(
                    "`let` bindings list must consist of alternating symbols and expressions");
        }
        p_binds = p_binds->rest;
        Val_t *expr = p_binds->first;
        Val_t *val = evaluate(expr, env);
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
    Val_t *result = evaluate(body, env);
    // destroy bindings
    EnvPop(env);
    return result;
}

// (macro) [if condition consequent (alternative)]
Val_t *if_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("vv(v", args, &err)) { return valCreateError(err); }
    Val_t *f = evaluate(args->first, env);
    if (valIsError(f)) { return f; } // eval error
    int t = valIsTrue(f);
    valFreeRec(f);
    if (t)
    {
        Val_t *consequent = args->rest->first;
        return evaluate(consequent, env);
    }
    Val_t *alt_list = args->rest->rest;
    if (!alt_list) { return NULL; } // no alternative
    Val_t *alternative = alt_list->first;
    return evaluate(alternative, env);
}

// (macro) [quote expr]
Val_t *quote_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("v", args, &err)) { return valCreateError(err); }
    return valCopy(args->first);
}

// (macro) [do (expr)...]
Val_t *do_func(Val_t *args, Val_t *env)
{
    Val_t *p = args;
    Val_t *e = NULL;
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
Val_t *and_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("v&v", args, &err)) { return valCreateError(err); }
    Val_t *p = args;
    while (p && valIsList(p))
    {
        Val_t *e = evaluate(p->first, env);
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
Val_t *or_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("v&v", args, &err)) { return valCreateError(err); }
    Val_t *p = args;
    while (p && valIsList(p))
    {
        Val_t *e = evaluate(p->first, env);
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
Val_t *cond_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("vv&v", args, &err)) { return valCreateError(err); }
    if (valListLength(args) % 2 != 0)
    {
        return valCreateErrorMessage("`cond` requires an even amount of"
                                " alternating condition expressions and"
                                " consequence expressions");
    }
    Val_t *p = args;
    while (p && valIsList(p))
    {
        Val_t *e = evaluate(p->first, env);
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
Val_t *lambda_func(Val_t *args, Val_t *env)
{
    Val_t *err;
    if (!argsIsMatchForm("l(v", args, &err)) { return valCreateError(err); }
    Val_t *params = args->first;
    if (!valIsList(params))
    {
        return valCreateErrorMessage("`lambda` first argument must be a list of"
                                " symbols");
    }
    Val_t *p = params;
    // params must be symbols
    while (p && valIsList(p))
    {
        Val_t *e = p->first;
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
    Val_t *body = args->rest;
    if (body) { body = body->first; }
    return valCreateList(valCreateSymbolCopy("lambda", 6),
                    valCreateList(valCopy(params),
                             valCreateList(valCopy(body),
                                      NULL)));
}

#endif /* LIZP_CORE_FUNCTIONS */
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
