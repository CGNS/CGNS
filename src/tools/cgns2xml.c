#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "cgns_io.h"
#include "getargs.h"

#define INDENT     2
#define THRESHOLD  0
#define ADF_DTD    "ADFXfile.dtd"
#define ADF_XSD    "ADFXfile.xsd"
#define ADF_XMLNS  "http://www.cgns.org/ADFXfile"

static int use_schema = 0;
static int show_data = 1;
static char prefix[33] = "";
static int indent = INDENT;
static int follow_links = 0;
static int threshold = THRESHOLD;
static int idcounter = 0;
static char ByteOrder[3];

static char data_map[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static char options[] = "xn:i:ft:d";
static char *usgmsg[] = {
    "usage  : cgns2xml [options] ADFfile",
    "options:",
    "   -x       = use XML Schema",
    "   -n<ns>   = use namespace ns",
    "   -i<cnt>  = set indent level (default 2)",
    "   -f       = follow links",
    "   -t<size> = formatted output up to this size in bytes",
    "   -d       = don't print data",
    NULL
};

#define print_indent(I) if((I)>0)printf("%*.*s",I,I," ")

/*---------------------------------------------------------*/

static void print_string (char *str, int cnt)
{
    while (cnt-- > 0 && *str) {
        if (*str == '"')
            printf ("&quot;");
        else if (*str == '\'')
            printf ("&apos;");
        else if (*str == '&')
            printf ("&amp;");
        else if (*str == '<')
            printf ("&lt;");
        else if (*str == '>')
            printf ("&gt;");
        else
            putchar (*str);
        str++;
    }
}

/*---------------------------------------------------------*/

static size_t data_size (char *type, int ndim, int *dims)
{
    int n;
    size_t bytes;

    if (ndim < 1) return 0;
    if (0 == strcmp (type, "C1") ||
        0 == strcmp (type, "B1"))
        bytes = sizeof(char);
    else if (0 == strcmp (type, "I4") ||
             0 == strcmp (type, "U4"))
        bytes = sizeof(int);
    else if (0 == strcmp (type, "I8") ||
             0 == strcmp (type, "U8"))
        bytes = sizeof(long);
    else if (0 == strcmp (type, "R4"))
        bytes = sizeof(float);
    else if (0 == strcmp (type, "R8"))
        bytes = sizeof(double);
    else
        return 0;

    for (n = 0; n < ndim; n++)
        bytes *= (size_t)dims[n];
    return bytes;
}

/*---------------------------------------------------------*/

static void print_C1 (char *data, int cnt, int inlen)
{
    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:C1 Size=\"0\"/>\n", prefix);
        else
            printf ("<C1 Size=\"0\"/>\n");
        return;
    }
    if (use_schema && *prefix) {
        printf ("<%s:C1 Size=\"%d\">", prefix, cnt);
        print_string (data, cnt);
        printf ("</%s:C1>\n", prefix);
    }
    else {
        printf ("<C1 Size=\"%d\">", cnt);
        print_string (data, cnt);
        printf ("</C1>\n");
    }
#else
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:C1/>\n", prefix);
        else
            printf ("<C1/>\n");
        return;
    }
    if (use_schema && *prefix) {
        printf ("<%s:C1>", prefix);
        print_string (data, cnt);
        printf ("</%s:C1>\n", prefix);
    }
    else {
        printf ("<C1>");
        print_string (data, cnt);
        printf ("</C1>\n");
    }
#endif
}

/*---------------------------------------------------------*/

static void print_I4 (int *data, int cnt, int inlen)
{
    int n;

    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:I4 Size=\"0\"/>\n", prefix);
        else
            printf ("<I4 Size=\"0\"/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:I4 Size=\"%d\">\n", prefix, cnt);
    else
        printf ("<I4 Size=\"%d\">\n", cnt);
#else
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:I4/>\n", prefix);
        else
            printf ("<I4/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:I4>\n", prefix);
    else
        printf ("<I4>\n");
#endif
    for (n = 0; n < cnt; n++) {
        if ((n % 20) == 0) {
            if (n) putchar ('\n');
            print_indent (inlen + indent - 1);
        }
        printf (" %d", data[n]);
    }
    putchar ('\n');
    print_indent (inlen);
    if (use_schema && *prefix)
        printf ("</%s:I4>\n", prefix);
    else
        printf ("</I4>\n");
}

/*---------------------------------------------------------*/

static void print_R4 (float *data, int cnt, int inlen)
{
    int n;

    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:R4 Size=\"0\"/>\n", prefix);
        else
            printf ("<R4 Size=\"0\"/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:R4 Size=\"%d\">\n", prefix, cnt);
    else
        printf ("<R4 Size=\"%d\">\n", cnt);
#else
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:R4/>\n", prefix);
        else
            printf ("<R4/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:R4>\n", prefix);
    else
        printf ("<R4>\n");
#endif
    for (n = 0; n < cnt; n++) {
        if ((n % 10) == 0) {
            if (n) putchar ('\n');
            print_indent (inlen + indent - 1);
        }
        printf (" %g", data[n]);
    }
    putchar ('\n');
    print_indent (inlen);
    if (use_schema && *prefix)
        printf ("</%s:R4>\n", prefix);
    else
        printf ("</R4>\n");
}

/*---------------------------------------------------------*/

static void print_R8 (double *data, int cnt, int inlen)
{
    int n;

    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:R8 Size=\"0\"/>\n", prefix);
        else
            printf ("<R8 Size=\"0\"/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:R8 Size=\"%d\">\n", prefix, cnt);
    else
        printf ("<R8 Size=\"%d\">\n", cnt);
#else
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:R8/>\n", prefix);
        else
            printf ("<R8/>\n");
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:R8>\n", prefix);
    else
        printf ("<R8>\n");
#endif
    for (n = 0; n < cnt; n++) {
        if ((n % 10) == 0) {
            if (n) putchar ('\n');
            print_indent (inlen + indent - 1);
        }
        printf (" %g", data[n]);
    }
    putchar ('\n');
    print_indent (inlen);
    if (use_schema && *prefix)
        printf ("</%s:R8>\n", prefix);
    else
        printf ("</R8>\n");
}

/*---------------------------------------------------------*/

static int can_encode (char *type)
{
    int bytes;

    if (*ByteOrder == 0) return 0;
    if (0 == strcmp (type, "C1")) {
        bytes = sizeof(char);
        return (bytes == 1);
    }
    if (0 == strcmp (type, "I4")) {
        bytes = sizeof(int);
        return (bytes == 4);
    }
    if (0 == strcmp (type, "R4")) {
        bytes = sizeof(float);
        return (bytes == 4);
    }
    if (0 == strcmp (type, "R8")) {
        bytes = sizeof(int);
        return (bytes == 8);
    }
    return 0;
}

/*---------------------------------------------------------*/

static void print_base64 (char *type, unsigned char *data, int cnt, int inlen)
{
    int c, i, j = 0, n = 0;
    int buff[3];

    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:B64 Size=\"0\" ByteOrder=\"%s\" DataType=\"%s\"/>\n",
                prefix, ByteOrder, type);
        else
            printf ("<B64 Size=\"0\" ByteOrder=\"%s\" DataType=\"%s\"/>\n",
                ByteOrder, type);
        return;
    }
    i = ((cnt + 2)/3) << 2;
    if (use_schema && *prefix)
        printf ("<%s:B64 Size=\"%d\" ByteOrder=\"%s\" DataType=\"%s\">\n",
            prefix, i, ByteOrder, type);
    else
        printf ("<B64 Size=\"%d\" ByteOrder=\"%s\" DataType=\"%s\">\n",
            i, ByteOrder, type);
#else
    if (cnt == 0) {
        if (use_schema && *prefix)
            printf ("<%s:B64 ByteOrder=\"%s\"/>\n", prefix, ByteOrder);
        else
            printf ("<B64 ByteOrder=\"%s\"/>\n", ByteOrder);
        return;
    }
    if (use_schema && *prefix)
        printf ("<%s:B64 ByteOrder=\"%s\">\n", prefix, ByteOrder);
    else
        printf ("<B64 ByteOrder=\"%s\">\n", ByteOrder);
#endif
    print_indent (inlen + indent);
    while (n < cnt) {
        for (i = 0; i < 3; i++)
            buff[i] = 0;
        for (i = 0; i < 3; i++) {
            if (n >= cnt) break;
            buff[i] = data[n++];
        }
        if (j == 100) {
            putchar('\n');
            print_indent (inlen + indent);
            j = 0;
        }
        c = (buff[0] & 0xfc) >> 2;
        putchar (data_map[c]);
        c = ((buff[0] & 0x03) << 4) | ((buff[1] & 0xf0) >> 4);
        putchar (data_map[c]);
        if (1 == i) {
            putchar ('=');
            putchar ('=');
            break;
        }
        c = ((buff[1] & 0x0f) << 2) | ((buff[2] & 0xc0) >> 6);
        putchar (data_map[c]);
        if (2 == i) {
            putchar ('=');
            break;
        }
        c = buff[2] & 0x3f;
        putchar (data_map[c]);
        j += 4;
    }
    putchar ('\n');
    print_indent (inlen);
    if (use_schema && *prefix)
        printf ("</%s:B64>\n", prefix);
    else
        printf ("</B64>\n");
}

/*---------------------------------------------------------*/

static void print_data (int cgio, double id, char *type,
    int ndim, int *dims, int inlen)
{
    size_t bytes = 0;
    void *data = NULL;

    if (0 == strcmp (type, "MT")) return;
    if (show_data) {
        bytes = data_size (type, ndim, dims);
        if (bytes > 0) {
            data = malloc (bytes);
            if (data == NULL) {
                fprintf (stderr, "malloc failed for %ld bytes\n", bytes);
                exit (1);
            }
            if (cgio_read_all_data (cgio, id, data))
                cgio_error_exit ("cgio_read_all_data");
        }
    }

    if (0 == strcmp (type, "C1"))
        print_C1 (data, bytes, inlen);
    else if (threshold && bytes > threshold && can_encode (type))
        print_base64 (type, data, bytes, inlen);
    else if (0 == strcmp (type, "I4"))
        print_I4 (data, bytes / sizeof(int), inlen);
    else if (0 == strcmp (type, "R4"))
        print_R4 (data, bytes / sizeof(float), inlen);
    else if (0 == strcmp (type, "R8"))
        print_R8 (data, bytes / sizeof(double), inlen);
    else {
        fprintf (stderr, "unknown data type %s\n", type);
        exit (1);
    }

    if (data != NULL) free (data);
}

/*---------------------------------------------------------*/

static void print_link (int cgio, double id, int len_file, int len_name,
    int inlen)
{
    char name_in_file[CGIO_MAX_LINK_LENGTH+1];
    char file_name[CGIO_MAX_FILE_LENGTH+1];

    if (cgio_get_link (cgio, id, file_name, name_in_file))
        cgio_error_exit ("cgio_get_link");

    print_indent (inlen);
#ifdef USE_OPTIONAL
    if (use_schema && *prefix)
        printf ("<%s:LK Size=\"%d\">%s|%s</%s:LK>\n",
            prefix, len_name + len_file + 1,
            file_name, name_in_file, prefix);
    else
        printf ("<LK Size=\"%d\">%s|%s</LK>\n",
            len_name + len_file + 1, file_name, name_in_file);
#else
    if (use_schema && *prefix)
        printf ("<%s:LK>%s|%s</%s:LK>\n", prefix,
            file_name, name_in_file, prefix);
    else
        printf ("<LK>%s|%s</LK>\n",
            file_name, name_in_file);
#endif
}

/*---------------------------------------------------------*/

static void print_children (int cgio, double parent_id, int inlen)
{
    int n, nc, nchildren, len_file, len_name;
    int pidnum, ndim, dims[CGIO_MAX_DIMENSIONS];
    char node[25];
    char name[CGIO_MAX_NAME_LENGTH+1];
    char label[CGIO_MAX_NAME_LENGTH+1];
    char type[CGIO_MAX_NAME_LENGTH+1];
    double id;

    if (cgio_number_children (cgio, parent_id, &nchildren))
        cgio_error_exit ("cgio_number_children");
    if (!nchildren) return;

    pidnum = idcounter;

    for (nc = 1; nc <= nchildren; nc++) {
        if (cgio_children_ids (cgio, parent_id, nc, 1, &n, &id))
            cgio_error_exit ("cgio_children_ids");
        if (cgio_get_name (cgio, id, name))
            cgio_error_exit ("cgio_get_name");
        if (cgio_get_label (cgio, id, label))
            cgio_error_exit ("cgio_get_label");
        if (cgio_get_data_type (cgio, id, type))
            cgio_error_exit ("cgio_get_data_type");
        if (cgio_is_link (cgio, id, &len_name))
            cgio_error_exit ("cgio_is_link");
        if (len_name > 0) {
            if (cgio_link_size (cgio, id, &len_file, &len_name))
                cgio_error_exit ("cgio_link_size");
        }
        if (len_name > 0 && (len_file == 0 || follow_links == 0)) {
            ndim = 1;
            dims[0] = len_name + len_file + 1;
            if (use_schema && *prefix)
                sprintf (node, "%s:LinkNode", prefix);
            else
                strcpy (node, "LinkNode");
            strcpy (type, "LK");
        }
        else if (0 == strcmp (type, "MT")) {
            ndim = 0;
            if (use_schema && *prefix)
                sprintf (node, "%s:ContNode", prefix);
            else
                strcpy (node, "ContNode");
        }
        else {
            if (cgio_get_dimensions (cgio, id, &ndim, dims))
                cgio_error_exit ("cgio_get_dimensions");
            if (use_schema && *prefix)
                sprintf (node, "%s:DataNode", prefix);
            else
                strcpy (node, "DataNode");
        }

        print_indent (inlen);
        printf ("<%s ID=\"n%d\" PID=\"n%d\" Name=\"",
            node, ++idcounter, pidnum);
        print_string (name, strlen(name));
        if (strcmp (type, "LK")) {
            printf ("\" Label=\"");
            print_string (label, strlen(label));
        }
        putchar ('"');
#ifndef USE_OPTIONAL
        if (strcmp (type, "MT") && strcmp (type, "LK"))
#endif
        printf (" DataType=\"%s\"", type);

        if (ndim > 0) {
            printf (" Dimensions=\"%d", dims[0]);
            for (n = 1; n < ndim; n++)
                printf (" %d", dims[n]);
            putchar ('"');
        }
        printf (">\n");

        if (0 == strcmp (type, "LK")) {
            print_link (cgio, id, len_file, len_name, inlen + indent);
        }
        else {
            print_data (cgio, id, type, ndim, dims, inlen + indent);
            print_children (cgio, id, inlen + indent);
        }
        print_indent (inlen);
        printf ("</%s>\n", node);
    }
}

/*---------------------------------------------------------*/

static void get_encoding ()
{
    int one = 1;
    unsigned char *cc = (unsigned char *)&one;

    if (cc[0] == 1)
        strcpy (ByteOrder, "LE");
    else if (cc[sizeof(int)-1] == 1)
        strcpy (ByteOrder, "BE");
    else {
        *ByteOrder = 0;
        threshold = 0;
    }
}

/*---------------------------------------------------------*/

int main (int argc, char *argv[])
{
    double root_id;
    int n, cgio;
    char rootname[CGIO_MAX_NAME_LENGTH+1];

    if (argc < 2)
        print_usage (usgmsg, NULL);
    while ((n = getargs (argc, argv, options)) > 0) {
        switch (n) {
            case 'x':
                use_schema = 1;
                break;
            case 'n':
                strcpy (prefix, argarg);
                break;
            case 'i':
                indent = atoi (argarg);
                break;
            case 'f':
                follow_links = 1;
                break;
            case 't':
                threshold = atoi (argarg);
                break;
            case 'd':
                show_data = 0;
                break;
        }
    }

    if (argind == argc)
        print_usage (usgmsg, "ADFfile not given");
    get_encoding ();

    if (cgio_open_file (argv[argind], 'r', CGIO_FILE_NONE, &cgio))
        cgio_error_exit ("cgio_open_file");
    if (cgio_get_root_id (cgio, &root_id))
        cgio_error_exit ("cgio_get_root_id");
    if (cgio_get_name (cgio, root_id, rootname))
        cgio_error_exit ("cgio_get_name");

    printf ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    if (use_schema) {
        if (*prefix)
            printf ("<%s:ADFXfile\n", prefix);
        else
            printf ("<ADFXfile\n");
        printf ("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        printf ("\txsi:schemaLocation=\"%s %s.xsd\"\n", ADF_XMLNS, ADF_XMLNS);
        if (*prefix) {
#ifdef USE_OPTIONAL
            printf ("\txmlns:%s=\"%s\"\n", prefix, ADF_XMLNS);
            printf ("\tVersion=\"1.1\"\n>\n");
            printf ("<%s:RootNode ID=\"n%d\" Name=\"/\" Label=\"%s\">\n",
                prefix, idcounter, rootname);
#else
            printf ("\txmlns:%s=\"%s\"\n>\n", prefix, ADF_XMLNS);
            printf ("<%s:RootNode ID=\"n%d\">\n", prefix, idcounter);
#endif
            print_children (cgio, root_id, indent);
            printf ("</%s:RootNode>\n</%s:ADFXfile>\n", prefix, prefix);
        }
        else {
#ifdef USE_OPTIONAL
            printf ("\txmlns=\"%s\"\n", ADF_XMLNS);
            printf ("\tVersion=\"1.1\"\n>\n");
            printf ("<RootNode ID=\"n%d\" Name=\"/\" Label=\"%s\">\n",
                idcounter, rootname);
#else
            printf ("\txmlns=\"%s\"\n>\n", ADF_XMLNS);
            printf ("<RootNode ID=\"n%d\">\n", idcounter);
#endif
            print_children (cgio, root_id, indent);
            printf ("</RootNode>\n</ADFXfile>\n");
        }
    }
    else {
        *prefix = 0;
        printf ("<!DOCTYPE ADFXfile SYSTEM \"%s\">\n", ADF_DTD);
#ifdef USE_OPTIONAL
        printf ("<ADFXfile Version=\"1.1\">\n");
        printf ("<RootNode ID=\"n%d\" Name=\"/\" Label=\"%s\">\n",
            idcounter, rootname);
#else
        printf ("<ADFXfile>\n");
        printf ("<RootNode ID=\"n%d\">\n", idcounter);
#endif
        print_children (cgio, root_id, indent);
        printf ("</RootNode>\n</ADFXfile>\n");
    }

    if (cgio_close_file (cgio))
        cgio_error_exit ("cgio_close_file");
    return 0;
}

