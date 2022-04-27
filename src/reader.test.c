#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"
#include "reader.test.h"

static void DigitValueTest(void)
{
    assert(DigitValue('\0') < 0);
    assert(DigitValue('+') < 0);
    assert(DigitValue('[') < 0);
    assert(DigitValue(']') < 0);
    assert(DigitValue('_') < 0);

    assert(DigitValue('0') == 0);
    assert(DigitValue('1') == 1);
    assert(DigitValue('9') == 9);
    assert(DigitValue('a') == 10);
    assert(DigitValue('A') == 10);
    assert(DigitValue('f') == 15);
    assert(DigitValue('F') == 15);
    assert(DigitValue('z') == 35);
    assert(DigitValue('Z') == 35);
}

static void ReadIntTest(void)
{
    const char *s;
    int n;

    s = "";
    assert(ReadInt(s, strlen(s), &n) == 0);
    assert(n == 0);

    // error
    //s = "!";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    // error
    //s = "-";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    // error
    //s = "+";
    //assert(ReadInt(s, strlen(s), &n) == 0);
    //assert(n == 0);

    s = "0]";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 0);

    s = "0";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 0);

    s = "1";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 1);

    s = "#1";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == 1);

    s = "#12";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 12);

    s = "10";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == 36);

    s = "-4";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == -4);

    s = "$FF";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 0xFF);

    s = "$B3";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == 0xB3);

    // 1011 0011 == $B3
    s = "%1011_0011 ";
    assert(ReadInt(s, strlen(s), &n) == 10);
    assert(n == 0xB3);

    s = "#2_022 ";
    assert(ReadInt(s, strlen(s), &n) == 6);
    assert(n == 2022);

    s = "z ";
    assert(ReadInt(s, strlen(s), &n) == 1);
    assert(n == 35);

    s = "-z";
    assert(ReadInt(s, strlen(s), &n) == 2);
    assert(n == -35);

    s = "-$a";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == -0xa);

    s = "-#9";
    assert(ReadInt(s, strlen(s), &n) == 3);
    assert(n == -9);

    s = "%1001";
    assert(ReadInt(s, strlen(s), &n) == 5);
    assert(n == 9);
}

static void ReadSeqTest(void)
{
    const char *s;
    int len;
    Val *seq;

    s = "";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 0);
    assert(ValSeqLength(seq) == 0);

    s = "[]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 2);
    assert(ValSeqLength(seq) == 0);

    s = "[ ]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 3);
    assert(ValSeqLength(seq) == 0);

    s = "[0]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 3);
    assert(ValSeqLength(seq) == 1);
    assert(seq->first->integer == 0);

    s = "[1 2 3]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 7);
    assert(ValSeqLength(seq) == 3);
    assert(seq->first->integer == 1);
    assert(seq->rest->first->integer == 2);
    assert(seq->rest->rest->first->integer == 3);

    s = "[ 1 2 3 ]";
    seq = NULL;
    len = ReadSeq(s, strlen(s), &seq);
    assert(len == 9);
    assert(ValSeqLength(seq) == 3);
    assert(seq->first->integer == 1);
    assert(seq->rest->first->integer == 2);
    assert(seq->rest->rest->first->integer == 3);
}

static void ReadValTest(void)
{
    const char *s;
    int len;
    Val *v;

    v = NULL;
    s = " 5";
    len = ReadVal(s, strlen(s), &v);
    assert(len == 2);
    assert(ValIsInt(v));
    assert(v->integer == 5);

    v = NULL;
    s = "[[1]2 3]";
    len = ReadVal(s, strlen(s), &v);
    assert(len == 8);
    assert(ValIsSeq(v));
    assert(ValSeqLength(v) == 3);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(ValIsInt(v->rest->rest->first));
    assert(ValSeqLength(v->first) == 1);
    assert(ValIsInt(v->first->first));
    assert(v->first->first->integer == 1);
}

void ReadStringTest(void)
{
    const char *s;
    int len;
    Val *v;

    v = NULL;
    s = "";
    len = ReadString(s, strlen(s), &v);
    assert(len == 0);

    v = NULL;
    s = "\"\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 2);
    assert(v);
    assert(ValIsSeq(v));
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->first->first));
    assert(v->first->first->integer == STR);
    assert(v->rest == NULL);
    assert(ValSeqLength(v) == 1);

    v = NULL;
    s = "\"X\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 3);
    assert(v);
    assert(ValSeqLength(v) == 2);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == 'X');

    v = NULL;
    s = "\" lizp\"";
    len = ReadString(s, strlen(s), &v);
    assert(len == 7);
    assert(v);
    assert(ValSeqLength(v) == 6);
    assert(ValIsSeq(v->first));
    assert(ValIsInt(v->rest->first));
    assert(v->rest->first->integer == ' ');
}

void ReaderTest(void)
{
    DigitValueTest();
    ReadIntTest();
    ReadSeqTest();
    ReadStringTest();
    ReadValTest();
}

