/*-------------------------------------------------------------------------
This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from
the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not
   be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "adf/ADF.h"
#include "ADFH.h"
#include "hdf5.h"

/* global variables */

static char   TempFile[ADF_FILENAME_LENGTH+1];
static double InputRootID = -1.0;
static double OutputRootID = -1.0;
static int    ErrStat;
static char   ErrMsg[ADF_MAX_ERROR_STR_LENGTH+1];
static int    IncludeLink = 0;
static int    PrintFlag = 0;
static int    NumDims;
static int    PathLength;
static int    DimVals[ADF_MAX_DIMENSIONS*2];
static char   label[ADF_LABEL_LENGTH+1];
static char   status[ADF_STATUS_LENGTH+1];
static char   format[ADF_FORMAT_LENGTH+1];
static char   name[ADF_NAME_LENGTH+1];
static char   DataType[ADF_DATA_TYPE_LENGTH+1];
static char   LinkFileName[ADF_FILENAME_LENGTH+1];
static char   LinkPathName[ADF_MAX_LINK_DATA_SIZE+1];

/*-------------------------------------------------------------------*/

static void ErrorExit()
{
    if (InputRootID >= 0.0)
        ADFH_Database_Close (InputRootID, &ErrStat);
    if (OutputRootID >= 0.0)
        ADF_Database_Close (OutputRootID, &ErrStat);
    if (TempFile[0])
        unlink(TempFile);
    exit(1) ;
}

/*-------------------------------------------------------------------*/

static void InputError(char *funcname)
{
    fflush(stdout);
    fprintf(stderr,"ADFH_%s: errno=%d",funcname,ErrStat);
    if (ErrStat > 0) {
        ADFH_Error_Message(ErrStat,ErrMsg);
        fprintf(stderr,"%s\n",ErrMsg);
    }
    ErrorExit();
}

/*-------------------------------------------------------------------*/

static void OutputError(char *funcname)
{
    fflush(stdout);
    fprintf(stderr,"ADF_%s: errno=%d",funcname,ErrStat);
    if (ErrStat > 0) {
        ADF_Error_Message(ErrStat,ErrMsg);
        fprintf(stderr,"%s\n",ErrMsg);
    }
    ErrorExit();
}

/*-------------------------------------------------------------------*/

static int CalculateDataSize(char *data_type, int ndims, int *dims)
{
    char type[3];
    int i, size = 1;

    if (ndims == 0) return 0;
    for (i = 0; i < ndims; i++)
        size *= dims[i];
    if (size <= 0) return 0;

    strncpy(type, data_type, 2);
    type[2] = 0;
    for (i = 0; i < 2; i++)
        if (islower(type[i]))
            type[i] = toupper(type[i]);

    if (0 == strcmp(type, "MT") ||
        0 == strcmp(type, "LK")) return 0;
    if (0 == strcmp(type, "B1") ||
        0 == strcmp(type, "C1")) return (size * sizeof(char));
    if (0 == strcmp(type, "I4") ||
        0 == strcmp(type, "U4")) return (size * sizeof(int));
    if (0 == strcmp(type, "I8") ||
        0 == strcmp(type, "U8")) return (size * sizeof(long));
    if (0 == strcmp(type, "R4")) return (size * sizeof(float));
    if (0 == strcmp(type, "R8")) return (size * sizeof(double));
    if (0 == strcmp(type, "X4")) return (size * 2 * sizeof(float));
    if (0 == strcmp(type, "X8")) return (size * 2 * sizeof(double));

    fflush(stdout);
    fprintf(stderr,"unknown datatype: %s\n", data_type);
    ErrorExit();
    return 0;
}

/*-------------------------------------------------------------------*/

static void CopyTheNode (double InputID, double OutputID)
{
    int DataSize;
    void *DataBuffer;

    /* Copy the node label type and size information */

    ADFH_Get_Label(InputID, label, &ErrStat);
    if (ErrStat != NO_ERROR) InputError("Get_Label");
    ADF_Set_Label(OutputID, label, &ErrStat);
    if (ErrStat != NO_ERROR) OutputError("Set_Label");

    ADFH_Get_Data_Type(InputID, DataType, &ErrStat);
    if (ErrStat != NO_ERROR) InputError("Get_Data_Type");
    ADFH_Get_Number_of_Dimensions(InputID, &NumDims, &ErrStat);
    if (ErrStat != NO_ERROR) InputError("Get_Number_of_Dimensions");
    if (NumDims == 0) return;
    ADFH_Get_Dimension_Values(InputID, DimVals, &ErrStat);
    if (ErrStat != NO_ERROR) InputError("Get_Dimension_Values");
    ADF_Put_Dimension_Information (OutputID, DataType, NumDims, DimVals,
				   &ErrStat);
    if (ErrStat != NO_ERROR) OutputError("Put_Dimension_Information");

    /* Copy any NodeData */

    DataSize = CalculateDataSize(DataType, NumDims, DimVals);
    if (DataSize == 0) return;
    DataBuffer = (void *) malloc (DataSize);
    if (DataBuffer == NULL) {
        fflush(stdout);
        fprintf(stderr, "malloc failed for the node data (%d bytes)\n",
            DataSize);
        ErrorExit();
    }
    ADFH_Read_All_Data(InputID, DataBuffer, &ErrStat);
    if (ErrStat == NO_DATA) {
        free (DataBuffer);
        return;
    }
    if (ErrStat != NO_ERROR) InputError("Read_All_Data");
    ADF_Write_All_Data(OutputID, DataBuffer, &ErrStat);
    if (ErrStat != NO_ERROR) OutputError("Write_All_Data");
    free (DataBuffer);
}

/*-------------------------------------------------------------------*/

static void WalkTheNodes (double InputID, double OutputID, int indent)
{
    int    ic, cnt, in;
    int    nchildren ;
    double InputChildID ;
    double OutputChildID ;

    /* Copy the data from the current input node to the output node */

    CopyTheNode (InputID, OutputID);

    /* Loop through the children of the current node */

    ADFH_Number_of_Children(InputID, &nchildren, &ErrStat);
    if (ErrStat != NO_ERROR) InputError("Number_of_Children");
    if (nchildren == 0) return;

    for (ic = 1; ic <= nchildren; ic++) {

        /* Get child name and node ID */

        ADFH_Children_IDs(InputID, ic, 1, &cnt, &InputChildID, &ErrStat);
        if (ErrStat != NO_ERROR) InputError("Children_IDs");
        ADFH_Get_Name(InputChildID, name, &ErrStat);
        if (ErrStat != NO_ERROR) InputError("Get_Name");

        /* Write out a little status message for the user */

        if (PrintFlag) {
            for (in = 0; in < indent; in++)
                putchar(' ');
            printf ("%s\n", name);
        }

        /* Check for link, if it is a local link then we always link it!! */

        ADFH_Is_Link(InputChildID, &PathLength, &ErrStat);
        if (ErrStat != NO_ERROR) InputError("Is_Link");
        if (PathLength > 0) {
            ADFH_Get_Link_Path(InputChildID, LinkFileName, LinkPathName,
			       &ErrStat);
            if (ErrStat != NO_ERROR) InputError("Get_Link_Path");
        }

        /* Create the node or the link */

        if (PathLength > 0 && (!IncludeLink || !LinkFileName[0])) {
            ADF_Link(OutputID, name, LinkFileName, LinkPathName,
	        &OutputChildID, &ErrStat);
            if (ErrStat != NO_ERROR) OutputError("Link");
        }
        else {
            ADF_Create(OutputID, name, &OutputChildID, &ErrStat);
            if (ErrStat != NO_ERROR) OutputError("Create");
            WalkTheNodes (InputChildID, OutputChildID, indent+1);
        }
    }
}

/*-------------------------------------------------------------------*/

int main (int argc, char **argv)
{
    int n;
    char *inpfile, *outfile = NULL;
    struct stat inpst, outst;
    time_t ts, te;

    for (n = 1; n < argc; n++) {
        if (argv[n][0] != '-') break;
        if (argv[n][1] == '-') {
            n++;
            break;
        }
        if (argv[n][1] == 'p')
            PrintFlag = 1;
        else if (argv[n][1] == 'l')
            IncludeLink = 1;
        else {
            fprintf(stderr, "unknown option %s\n", argv[n]);
            exit (1);
        }
    }

    if (n >= argc) {
        fprintf (stderr, "usage: hdf2adf [-links] [-print] InputFile [OutputFile]\n");
        exit (1);
    }
    inpfile = argv[n++];
    if (stat (inpfile, &inpst)) {
        fprintf (stderr, "can't stat %s\n", inpfile);
        exit (1);
    }
    if (H5Fis_hdf5(inpfile) <= 0) {
        fprintf (stderr, "%s is not a HDF5 file\n", inpfile);
        exit (1);
    }

    /* output to temporary file */

    outfile = n < argc ? argv[n] : inpfile;
    sprintf(TempFile, "%s.temp", outfile);

    printf("converting HDF5 file %s to ADF file %s\n", inpfile, outfile);
    if (IncludeLink)
        printf ("links will be included in output file\n");
    fflush(stdout);

    ts = time (NULL);
    ADFH_Database_Open(inpfile, "READ_ONLY", "", &InputRootID, &ErrStat);
    if (ErrStat != NO_ERROR) {
        InputRootID = -1.0;
        InputError("Database_Open");
    }
    ADF_Database_Open(TempFile, "NEW", "NATIVE", &OutputRootID, &ErrStat);
    if (ErrStat != NO_ERROR) {
        OutputRootID = -1.0;
        OutputError("Database_Open");
    }

    WalkTheNodes (InputRootID, OutputRootID, 0);

    ADFH_Database_Close(InputRootID, &ErrStat);
    ADF_Database_Close(OutputRootID, &ErrStat);
    te = time (NULL);

    unlink (outfile);
    if (rename (TempFile, outfile)) {
        fprintf (stderr, "rename %s -> %s failed", TempFile, outfile);
        exit (1);
    }

    if (stat (outfile, &outst)) {
        fprintf (stderr, "can't stat %s\n", outfile);
        exit (1);
    }

    printf ("HDF5 file size  = %ld bytes\n", (long)inpst.st_size);
    printf ("ADF  file size  = %ld bytes\n", (long)outst.st_size);
    printf ("conversion time = %d secs\n", (int)(te - ts));
    return 0;
}

