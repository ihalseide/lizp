#include <ctype.h>
#include <stdbool.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "reader.h"
#include "value.h"
#include "lizp.h"

bool CharIsSpace(char c)
{
    if (!c)
    {
        // Don't consider NULL char a space!
        return false;
    }
    else if (c == '"')
    {
        // Don't consider string literal quote as space
        return false;
    }
    else if (c == '[' || c == ']')
    {
        // Don't consider list characters as space
        return false;
    }
    else if (c == '#' || c == '$' || c == '%' || c == '-' || c == '_')
    {
        // Don't consider integer sigils as space either
        return false;
    }
    else
    {
        // All other non-alphanumeric characters are space
        return !isalnum(c);
    }
}

int ReadSpace(const char *s, int len)
{
    const char *view = s;
    while ((view - s) < len && CharIsSpace(*view))
    {
        view++;
    }
    return view - s;
}

// Digit to integer value
// Returns -1 upon error
int DigitValue(char d)
{
    if ('0' <= d && d <= '9')
    {
        return d - '0';
    }
    else if ('a' <= d && d <= 'z')
    {
        return d - 'a' + 10;
    }
    else if ('A' <= d && d <= 'Z')
    {
        return d - 'A' + 10;
    }
    else
    {
        return -1;
    }
}

// TODO: check for overflow
// Returns the number of characters read
// number read -> out
int ReadInt(const char *start, int length, int *valOut)
{
    // Validate inputs
    if (!start || length <= 0)
    {
        if (valOut)
        {
            *valOut = 0;
        }
        return 0;
    }

    const char *view = start;

    // Read prefix sigil(s)
    bool neg = false;
    int base = 0;
    if (*view == '-')
    {
        neg = true;
        view++;
    }
    switch (*view)
    {
        case '#':
            // Decimal
            base = 10;
            view++;
            break;
        case '$':
            // Hexadecimal
            base = 16;
            view++;
            break;
        case '%':
            // Binary
            base = 2;
            view++;
            break;
        default:
            // Default is base 36.
            // There is no sigil, so view does not need to be incremented
            if (isalnum(*view))
            {
                base = 36;
            }
            else
            {
                // Invalid beginning of integer
                LizpError(LE_INVALID_INT);
            }
            break;
    }

    // Keep a pointer to where the digits start
    const char *viewDigits = view;

    int n = 0;
    int d;
    while (*view && (view - start < length))
    {
        if (isalnum(*view))
        {
            d = DigitValue(*view);
            if (0 <= d && d < base)
            {
                n = (n * base) + d;
                if (n < 0)
                {
                    // There was an overflow
                    LizpError(LE_INVALID_INT_OVERFLOW);
                }
            }
            else
            {
                // Invalid digit for base
                LizpError(LE_INVALID_INT_DIGIT);
                if (valOut)
                {
                    *valOut = 0;
                }
                return 0;
            }
        }
        else if (*view != '_')
        {
            // Allow underscore to separate digits.
            // All other characters are invalid, so
            // this must be the end of the number.
            break;
        }
        view++;
    }

    // Check if there were any valid digits
    if (view == viewDigits)
    {
        // No valid digits were read after the sigils
        LizpError(LE_INVALID_INT);
        if (valOut)
        {
            *valOut = 0;
        }
        return 0;
    }

    // Apply sign
    if (neg)
    {
        n = -n;
    }

    // Return results
    if (valOut)
    {
        *valOut = n;
    }
    return view - start;
}

// Returns number of chars read
int ReadString(const char *start, int length, Val **toList)
{
    if (start && length > 0)
    {
        const char *view = start;

        // Consume the opening quote
        assert(*view == '"');
        view++;

        // make form: [[str] ...]
        Val *s = ValMakeSeq(ValMakeSeq(ValMakeInt(STR), NULL), NULL);
        // Read the rest of the characters
        Val *ps = s;
        while (*view && *view != '"' && view < start + length)
        {
            ps->rest = ValMakeSeq(ValMakeInt(*view), NULL);
            ps = ps->rest;
            view++;
        }

        // Consume closing quote
        if (*view != '"')
        {
            LizpError(LE_LIST_UNFINISHED);
        }
        view++;

        *toList = s;
        return view - start;
    }
    return 0;
}

// Returns number of chars read
int ReadSeq(const char *start, int length, Val **toList)
{
    // Validate arguments
    if (!start || length <= 0)
    {
        return 0;
    }

    const char *view = start;

    // Consume the opening paren
    assert(*view == '[');
    view++;

    Val *s = NULL;

    // Skip whitespace
    view += ReadSpace(view, (start+length)-view);
    if (*view != ']')
    {
        // Non-empty list
        if (*view && *view != ']' && view < start+length)
        {
            Val *e;
            int len = ReadVal(view, (start+length)-view, &e);
            if (!len)
            {
                // Error reading element
                return 0;
            }
            // Create first item
            s = ValMakeSeq(e, NULL);
            view += len;
        }
        // Pointer for appending to s
        Val *ps = s;
        while (*view && *view != ']' && view < start+length)
        {
            Val *e;
            int len = ReadVal(view, (start+length)-view, &e);
            if (!len)
            {
                // Error reading element
                return 0;
            }
            // Append
            ps->rest = ValMakeSeq(e, NULL);
            ps = ps->rest;
            view += len;
        }
    }
    *toList = s;

    if (*view == ']')
    {
        // Consume the closing paren
        view++;
        return view - start;
    }
    else
    {
        // Reading error
        LizpError(LE_LIST_UNFINISHED);
        return 0;
    }
}

int ReadVal(const char *start, int length, Val **out)
{
    // Validate arguments
    if (!out || !start || length <= 0)
        return 0;

    const char *view = start;

    // Loop is for allowing comments to restart the read
    while (1)
    {
        view += ReadSpace(view, start+length-view);
        switch (*view)
        {
            case '\0':
                // End of input
                *out = NULL;
                break;
            case '"':
                // String literal
                {
                    Val *s = NULL;
                    int len = ReadString(view, start+length-view, &s);
                    view += len;
                    if (len)
                    {
                        *out = s;
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
            case ']':
                // Unmatched list
                LizpError(LE_BRACKET_MISMATCH);
                *out = NULL;
                break;
            case '[':
                // Read sequence / list
                {
                    Val *s = NULL;
                    int len = ReadSeq(view, start+length-view, &s);
                    view += len;
                    if (len)
                    {
                        *out = s;
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
            default:
                // Read integer
                {
                    int n;
                    int len = ReadInt(view, start+length-view, &n);
                    if (len)
                    {
                        view += len;
                        *out = ValMakeInt(n);
                    }
                    else
                    {
                        *out = NULL;
                    }
                }
                break;
        }
        break;
    }
    view += ReadSpace(view, start+length-view);

    return view - start;
}


