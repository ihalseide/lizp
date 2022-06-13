#include <stdio.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Load all of the contents of a given file
// Return value:
// - 0 upon success
// - non-zero upon failure
unsigned loadFile(const char *filename, char **text_out, unsigned *len_out)
{
    FILE *fp = fopen(filename, "r");
    if (!fp) { return 1; }

    fseek(fp, 0, SEEK_END);
    unsigned len = ftell(fp);
    rewind(fp);

    char *text = malloc(len + 1);
    if (!text)
    {
        fclose(fp);
        return 1;
    }

    fread(text, 1, len, fp);
    text[len] = 0;
    fclose(fp);

    *text_out = text;
    if (len_out) { *len_out = len; }
    return 0;
}

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s file\n", argv[0]);
        return 1;
    }

    // load file contents
    char *fname = argv[1];
    char *text;
    unsigned textLength;
    if (loadFile(fname, &text, &textLength))
    {
        perror("fopen");
        return 1;
    }

    // convert to data structure
    Val_t *val;
    unsigned n = valReadAllFromBuffer(text, textLength, &val);
    free(text);

    if (n > 1)
    {
        fprintf(stderr, "info: read multiple values: %u values\n", n);
    }

    // print back out
    text = valWriteToNewString(val, 1);
    printf("%s\n", text);
    free(text);

    return 0;
}
