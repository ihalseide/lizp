#include <stdio.h>
#include "sequence.test.h"
#include "reader.test.h"
#include "printer.test.h"
#include "lizp.test.h"

void Test(void)
{
    printf("SequenceTest\n");
    SequenceTest();
    printf("ReaderTest\n");
    ReaderTest();
    printf("PrinterTest\n");
    PrinterTest();
    printf("LizpTest\n");
    LizpTest();
}

int main(void)
{
    printf("Testing...\n");
    Test();
    printf("Good.\n");
    return 0;
}

