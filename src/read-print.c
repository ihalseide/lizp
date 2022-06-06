#include <stdio.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Load all of the contents of a given file
// Return value:
// - 0 upon success
// - non-zero upon failure
int LoadFile(const char *filename, char **text_out, int *len_out)
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

// Read as many expressions from the str as possible
Val *ReadAll(const char *str, int len)
{
    int i = 0;
    Val *val = NULL;

    // Read first item... (may be the only item)
    int read_len = ReadVal(str + i, len - i, &val);
    if (!read_len) { return val; }
    i += read_len;
    i += SkipChars(str + i, len - i);
    if (i >= len || !str[i]) { return val; }

    // Read additional items into a list...
    val = MakeList(val, NULL);
    Val *p = val;
    while (i < len && str[i])
    {
        Val *e;
        read_len = ReadVal(str + i, len - i, &e);
        if (!read_len) { return val; }
        i += read_len;
        i += SkipChars(str + i, len - i);
        p->rest = MakeList(e, NULL);
        p = p->rest;
    }

    return val;
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
    if (LoadFile(fname, &text, &length))
    {
        perror("fopen");
        return 1;
    }

    // convert to data structure
    Val *val = ReadAll(text, length);
    free(text);

    // print back out
    text = PrintValStr(val, 1);
    printf("%s\n", text);
    free(text);

    return 0;
}
