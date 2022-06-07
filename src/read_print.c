#include <stdio.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Load all of the contents of a given file
// Return value:
// - 0 upon success
// - non-zero upon failure
int loadFile(const char *filename, char **text_out, int *len_out)
{
    FILE *fp = fopen(filename, "r");
    if (!fp) { return 1; }

    fseek(fp, 0, SEEK_END);
    int len = ftell(fp);
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
    *len_out = len;
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
    int length;
    if (loadFile(fname, &text, &length))
    {
        perror("fopen");
        return 1;
    }

    // convert to data structure
    Val_t *val;
    int n = valReadAllFromBuffer(text, length, &val);
    free(text);

    // print back out
    text = valWriteToNewString(val, 1);
    printf("%s\n", text);
    free(text);

    return 0;
}
