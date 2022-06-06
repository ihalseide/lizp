#include <stdio.h>

#define LIZP_IMPLEMENTATION
#include "lizp.h"

// Load all of the contents of a given file
int LoadFile(const char *filename, char **text_out, int *len_out)
{
    FILE *fp = fopen(filename, "r");
    if (!fp) { return 0; }

    fseek(fp, 0, SEEK_END);
    int len = ftell(fp);
    rewind(fp);

    char *text = malloc(len + 1);
    if (!text)
    {
        fclose(fp);
        return 0;
    }

    fread(text, 1, len, fp);
    text[len] = 0;
    fclose(fp);

    *text_out = text;
    *len_out = len;
    return 1;
}

// Read as many expressions from the str as possible
Val *ReadAll(const char *str, int len)
{
    int i = 0;
    Val *val = NULL;

    // Read first item...
    int read_len = ReadVal(str + i, len - i, &val);
    if (!read_len) { return val; }
    i += read_len;
    i += SkipChars(str + i, len - i);
    if (i >= len || !str[i]) { return val; }

    // Multiple items...
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

    char *fname = argv[1];
    char *text;
    int length;
    if (!LoadFile(fname, &text, &length))
    {
        perror("fopen");
        return 1;
    }

    Val *val = ReadAll(text, length);
    free(text);

    text = PrintValStr(val, 1);
    printf("%s\n", text);
    free(text);

    return 0;
}
