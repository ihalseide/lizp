#include <assert.h>
#include "printer.h"
#include "printer.test.h"

static void ValueToDigitTest(void)
{
	assert(ValueToDigit(1, false) == '1');
	assert(ValueToDigit(1, true) == '1');

	assert(ValueToDigit(9, true) == '9');
	assert(ValueToDigit(9, false) == '9');

	assert(ValueToDigit(10, true) == 'A');
	assert(ValueToDigit(10, false) == 'a');

	assert(ValueToDigit(15, true) == 'F');
	assert(ValueToDigit(15, false) == 'f');

	assert(ValueToDigit(16, true) == 'G');
	assert(ValueToDigit(16, false) == 'g');

	assert(ValueToDigit(35, true) == 'Z');
	assert(ValueToDigit(35, false) == 'z');

	assert(ValueToDigit(36, true) == '?');
	assert(ValueToDigit(36, false) == '?');

	assert(ValueToDigit(-1, true) == '?');
	assert(ValueToDigit(-1, false) == '?');
}

void PrinterTest(void)
{
	ValueToDigitTest();
}

