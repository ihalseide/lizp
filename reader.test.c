#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "sequence.h"
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
	s = "+1011_0011 ";
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

	s = "+1001";
	assert(ReadInt(s, strlen(s), &n) == 5);
	assert(n == 9);
}

static void ReadSeqTest(void)
{
	const char *s;
	int len;
	Seq *seq;

	s = "[]";
	seq = NULL;
	len = ReadSeq(s, strlen(s), &seq);
	assert(len == 2);
	assert(SeqLength(seq) == 0);
	SeqFree(seq);

	s = "[ ]";
	seq = NULL;
	len = ReadSeq(s, strlen(s), &seq);
	assert(len == 3);
	assert(SeqLength(seq) == 0);
	SeqFree(seq);

	s = "[0]";
	seq = NULL;
	len = ReadSeq(s, strlen(s), &seq);
	assert(len == 3);
	assert(SeqLength(seq) == 1);
	assert(((Val*)SeqGet(seq, 0))->integer == 0);
	SeqFree(seq);

	s = "[1 2 3]";
	seq = NULL;
	len = ReadSeq(s, strlen(s), &seq);
	assert(len == 7);
	assert(SeqLength(seq) == 3);
	assert(((Val*)SeqGet(seq, 0))->integer == 1);
	assert(((Val*)SeqGet(seq, 1))->integer == 2);
	assert(((Val*)SeqGet(seq, 2))->integer == 3);
	SeqFree(seq);

	s = "[ 1 2 3 ]";
	seq = NULL;
	len = ReadSeq(s, strlen(s), &seq);
	assert(len == 9);
	assert(SeqLength(seq) == 3);
	assert(((Val*)SeqGet(seq, 0))->integer == 1);
	assert(((Val*)SeqGet(seq, 1))->integer == 2);
	assert(((Val*)SeqGet(seq, 2))->integer == 3);
	SeqFree(seq);
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
	ValFree(v);

	v = NULL;
	s = "[[1]2 3]";
	len = ReadVal(s, strlen(s), &v);
	assert(len == 8);
	assert(ValIsSeq(v));
	assert(SeqLength(v->sequence) == 3);
	assert(ValIsSeq(SeqGet(v->sequence, 0)));
	assert(ValIsInt(SeqGet(v->sequence, 1)));
	assert(ValIsInt(SeqGet(v->sequence, 2)));
	assert(SeqLength(((Val*)SeqGet(v->sequence, 0))->sequence) == 1);
	SeqFree(((Val*)SeqGet(v->sequence, 0))->sequence);
	ValFree(SeqGet(v->sequence, 0));
	SeqFree(v->sequence);
	ValFree(v);
}

void ReaderTest(void)
{
	DigitValueTest();
	ReadIntTest();
	ReadSeqTest();
	ReadValTest();
}

