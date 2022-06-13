#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Note: does not free memory

// utility function to get value's symbol
char *valSymbol(Val_t *v)
{
    if (!v) { return NULL; }
    if (!valIsSymbol(v)) { return NULL; }
    return v->symbol;
}

// utility function to get value's list
List_t *valList(Val_t *v)
{
    if (!v) { return NULL; }
    if (!valIsList(v)) { return NULL; }
    return v->list;
}

void valIsEqual_test(void)
{
    assert(0 &&  "not implemented yet");
}

void valReadOneFromBuffer_test_symbol(void)
{
    unsigned len;
    Val_t *v;

    // one-character symbol
    char s1[] = "x";
    len = valReadOneFromBuffer(s1, sizeof(s1), &v);
    assert(len == 1);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), s1));

    // multi-character symbol
    char s2[] = "xyz";
    len = valReadOneFromBuffer(s2, sizeof(s2), &v);
    assert(len == sizeof(s2) - 1);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), s2));

    // leading space, one-character symbol
    char s3[] = "  y";
    len = valReadOneFromBuffer(s3, sizeof(s3), &v);
    assert(len == sizeof(s3) - 1);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "y"));

    // leading space, multi-character symbol
    char s4[] = "  bap";
    len = valReadOneFromBuffer(s4, sizeof(s4), &v);
    assert(len == sizeof(s4) - 1);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "bap"));

    // quoted symbol
    char s5[] = "\"s p a c e\"";
    len = valReadOneFromBuffer(s5, sizeof(s5), &v);
    assert(len == sizeof(s5) - 1);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "s p a c e"));

    // symbol before quoted symbol
    char s6[] = "correct\"s p a c e\"";
    len = valReadOneFromBuffer(s6, sizeof(s6), &v);
    assert(len == 7);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "correct"));

    // leading space, symbol, trailing space
    char s7[] = "  seven   ";
    len = valReadOneFromBuffer(s7, sizeof(s7), &v);
    assert(len == 7);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "seven"));

    // leading space, symbol, list end
    char s8[] = "  eight]  ";
    len = valReadOneFromBuffer(s8, sizeof(s8), &v);
    assert(len == 7);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "eight"));

    // leading space, symbol, list begin
    char s9[] = "  nine]  ";
    len = valReadOneFromBuffer(s9, sizeof(s9), &v);
    assert(len == 6);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "nine"));
    
    // comment, symbol, comment
    char s10[] = "(-->)ten!(<--)";
    len = valReadOneFromBuffer(s10, sizeof(s10), &v);
    assert(len == 9);
    assert(v);
    assert(valIsSymbol(v));
    assert(!strcmp(valSymbol(v), "ten!"));
}

void valReadOneFromBuffer_test_list(void)
{
    Val_t *v;
    unsigned len;

    // empty list
    char s1[] = "[]";
    len = valReadOneFromBuffer(s1, sizeof(s1), &v);
    assert(len == 2);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // space, empty list
    char s2[] = "   []";
    len = valReadOneFromBuffer(s2, sizeof(s2), &v);
    assert(len == 5);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // space, empty list with space
    char s3[] = "   [   ]";
    len = valReadOneFromBuffer(s3, sizeof(s3), &v);
    assert(len == 8);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // space, empty list with space, space
    char s4[] = "   [   ]    ";
    len = valReadOneFromBuffer(s4, sizeof(s4), &v);
    assert(len == 8);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // empty list with space
    char s5[] = "[ ]";
    len = valReadOneFromBuffer(s5, sizeof(s5), &v);
    assert(len == 3);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // empty list with comment
    char s6[] = "[(comment)]";
    len = valReadOneFromBuffer(s6, sizeof(s6), &v);
    assert(len == 11);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // comment, empty list with comment
    char s7[] = "(comment)[(empty)]";
    len = valReadOneFromBuffer(s7, sizeof(s7), &v);
    assert(len == 18);
    assert(v);
    assert(valIsList(v));
    assert(valList(v) == NULL);

    // 1 item list
    char s8[] = "[a]";
    len = valReadOneFromBuffer(s8, sizeof(s8), &v);
    assert(len == 3);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 1);
    assert(valListItem(v, 0));
    assert(valIsSymbol(valListItem(v, 0)));
    assert(!strcmp(valListItem(v, 0)->symbol, "a"));

    // space, 1 item list with space, space
    char s9[] = " [ I ] ";
    len = valReadOneFromBuffer(s9, sizeof(s9), &v);
    assert(len == 6);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 1);
    assert(valListItem(v, 0));
    assert(valIsSymbol(valListItem(v, 0)));
    assert(!strcmp(valListItem(v, 0)->symbol, "I"));

    // 1 item list with space
    char s10[] = "[ 1 ]";
    len = valReadOneFromBuffer(s10, sizeof(s10), &v);
    assert(len == 5);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 1);
    assert(valListItem(v, 0));
    assert(valIsSymbol(valListItem(v, 0)));
    assert(!strcmp(valListItem(v, 0)->symbol, "1"));

    // comment, 3 item list with comments
    char s11[] = "(comment)[(x:) 2 (y:) 3 (z:) 4]";
    len = valReadOneFromBuffer(s11, sizeof(s11), &v);
    assert(len == 31);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 3);
    assert(valListItem(v, 0));
    assert(valListItem(v, 1));
    assert(valListItem(v, 2));
    assert(valIsSymbol(valListItem(v, 0)));
    assert(valIsSymbol(valListItem(v, 1)));
    assert(valIsSymbol(valListItem(v, 2)));
    assert(!strcmp(valListItem(v, 0)->symbol, "2"));
    assert(!strcmp(valListItem(v, 1)->symbol, "3"));
    assert(!strcmp(valListItem(v, 2)->symbol, "4"));

    // nested empty list
    char s12[] = "[[]]";
    len = valReadOneFromBuffer(s12, sizeof(s12), &v);
    assert(len == 4);
    assert(v);
    assert(valListLength(v) == 1);
    assert(valListItem(v, 0));
    assert(valIsList(valListItem(v, 0)));
    assert(valIsEmptyList(valListItem(v, 0)));

    // list of 2 lists each with 1 item
    char s13[] = "[[a][b]]";
    len = valReadOneFromBuffer(s13, sizeof(s13), &v);
    assert(len == 8);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 2);
    assert(valListItem(v, 0));
    assert(valIsList(valListItem(v, 0)));
    assert(valIsSymbol(valListItem(valListItem(v, 0), 0)));
    assert(!strcmp(valSymbol(valListItem(valListItem(v, 0), 0)), "a"));
    assert(valListItem(v, 1));
    assert(valIsList(valListItem(v, 1)));
    assert(valIsSymbol(valListItem(valListItem(v, 1), 0)));
    assert(!strcmp(valSymbol(valListItem(valListItem(v, 1), 0)), "b"));

    // list of 5 numbers
    char s14[] = "[ 1  2  3  4  5 ]";
    len = valReadOneFromBuffer(s14, sizeof(s14), &v);
    assert(len == 17);
    assert(v);
    assert(valIsList(v));
    assert(valListLength(v) == 5);
    for (unsigned i = 0; i < 5; i++)
    {
        Val_t *item = valListItem(v, i);
        assert(item);
        assert(valIsInteger(item));
        assert(valAsInteger(item) == (i + 1));
    }
}

void valReadOneFromBuffer_test(void)
{
    valReadOneFromBuffer_test_symbol();
    valReadOneFromBuffer_test_list();
}

void valReadAllFromBuffer_test(void)
{
    assert(0 &&  "not implemented yet");
}

void valWriteToBuffer_test_symbol(void)
{
    unsigned len;
    char buf[256];

    // single-character symbol, readable
    len = valWriteToBuffer(valCreateSymbol("x"), buf, sizeof(buf), true);
    assert(len == 1);
    buf[len] = '\0';
    assert(!strcmp(buf, "x"));
}

void valWriteToBuffer_test_list(void)
{
    assert(0 &&  "not implemented yet");
}

void valWriteToBuffer_test(void)
{
    valWriteToBuffer_test_symbol();
    valWriteToBuffer_test_list();
}

void valCopy_test(void)
{
    assert(0 &&  "not implemented yet");
}

void valCompare_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listAlloc_test(void)
{
    List_t *l;

    // null test
    l = listAlloc(0);
    assert(!l);

    // [1]
    l = listAlloc(1);
    assert(l);
    assert(l->length == 1);
    l->values[0] = (Val_t){ 0 };
    assert(l->values[0].kind == 0);

    // [2]
    l = listAlloc(2);
    assert(l);
    assert(l->length == 2);
    l->values[0] = (Val_t){ 0 };
    l->values[1] = (Val_t){ 0 };
    assert(l->values[0].kind == 0);
    assert(l->values[1].kind == 0);
}

void listFindFirst_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listGetAssociate_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listItem_test(void)
{
    assert(listItem(NULL, 0) == NULL);
    assert(listItem(NULL, 1) == NULL);

    List_t *l;
    l = listAlloc(1);
    assert(listItem(l, 1) == NULL);
    assert(listItem(l, 2) == NULL);
    assert(listItem(l, 0) != NULL);
}

void listContainsItem_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listCountItem_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listLength_test(void)
{
    List_t *l;

    // note: listAlloc(0) is tested in listAlloc_test()

    l = NULL;
    assert(listLength(l) == 0);

    l = listAlloc(0);
    assert(!l);
    assert(listLength(l) == 0);

    l = listAlloc(1);
    assert(l);
    assert(listLength(l) == 1);

    l = listAlloc(5);
    assert(l);
    assert(listLength(l) == 5);
}

void listAppend_test(void)
{
    List_t *l;

    l = NULL;
    listAppend(&l, (Val_t){ 0 });
    assert(l);
    assert(listLength(l) == 1);

    listAppend(&l, (Val_t){ 0 });
    assert(l);
    assert(listLength(l) == 2);

    listAppend(&l, (Val_t){ 0 });
    assert(l);
    assert(listLength(l) == 3);
}

void listInsert_test(void)
{
    List_t *l;
    Val_t v1 = { .kind=VK_SYMBOL, .symbol="value 1", };

    // insert a symbol into an empty list
    l = NULL;
    assert(listLength(l) == 0);
    listInsert(&l, 0, v1);
    assert(listLength(l) == 1);
    assert(valIsSymbol(listItem(l, 0)));
    assert(!strcmp(valSymbol(listItem(l, 0)), "value 1"));
}

void listPrepend_test(void)
{
    assert(0 &&  "not implemented yet");
}

void listRemove_test(void)
{
    assert(0 &&  "not implemented yet");
}

int main(void)
{
    listAlloc_test();
    listItem_test();
    listLength_test();
    listInsert_test();
    listAppend_test();
    listPrepend_test();
    listRemove_test();
    valReadOneFromBuffer_test();
    valReadAllFromBuffer_test();
    valWriteToBuffer_test();
    valIsEqual_test();
    listContainsItem_test();
    listCountItem_test();
    listFindFirst_test();
    listGetAssociate_test();
    valCopy_test();
    valCompare_test();
    fprintf(stderr, "testing complete\n");
    return 0;
}

