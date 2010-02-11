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
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include "ADF.h"
#ifdef MEM_DEBUG
#include "cg_malloc.h"
#endif

#if defined(_WIN32) && defined(BUILD_DLL)
# define CGNSDLL _declspec(dllexport)
#else
# define CGNSDLL
#endif

/* global constants */

#ifndef FALSE
#define FALSE    0
#endif
#ifndef TRUE
#define TRUE     1
#endif
#define StatusOK -1
#define NO_DATA                        33

/* Define some usefull macros */

#define TO_UPPER( c ) ((islower(c))?(toupper(c)):(c))
#define EVAL_2_BYTES( C0, C1 )  (((C0)<<8)+((C1)))

/* external prototypes */

#if defined (__cplusplus)
extern "C" {
#endif

static void ErrorExit(char *name, int error_return) ;
static void WalkTheNodes( double InputID, double OutputID ) ;
static void CopyTheNode( double InputID, double OutputID ) ;
static long CalculateDataSize( char *type, int ndims, int *dimlist ) ;

/* global variables */

static int    ErrStat ;
static int    IncludeLink = FALSE ;
static int    PrintFlag = FALSE ;
static int    NumDims ;
static int    PathLength ;
static int    DimVals[ADF_MAX_DIMENSIONS*2] ;
static char   input[ADF_FILENAME_LENGTH+1] ;
static char   label[ADF_LABEL_LENGTH+1] ;
static char   format[ADF_FORMAT_LENGTH+1] ;
static char   name[ADF_NAME_LENGTH+1] ;
static char   DataType[ADF_DATA_TYPE_LENGTH+1] ;
static char   LinkFileName[ADF_FILENAME_LENGTH+1] ;
static char   LinkPathName[ADF_MAX_LINK_DATA_SIZE+1] ;

/************************************************************************
  Main Routine, gets user input and then starts the walk through the nodes
*************************************************************************/

CGNSDLL void adf_cond(double InputRootID, double OutputRootID) {

  sprintf(format,"NATIVE") ;
  IncludeLink = FALSE ;

  /* Begin the walk through the nodes */

  if ( PrintFlag == TRUE ) {
    printf ( "\n**** Begin ADF file conditioner ****" ) ;
    printf ( "\n---Output file type is %s.", format ) ;
    if ( IncludeLink == TRUE )
      printf ( "\n---Links will be included in output file." ) ;
    else
      printf ( "\n---Links will not be included in output file." ) ;
  }

  WalkTheNodes( InputRootID, OutputRootID ) ;

  ADF_Database_Close ( OutputRootID, &ErrStat ) ;

  if ( PrintFlag == TRUE )
    printf ( "\n**** ADF file conditioner completed sucessfully ****\n" ) ;

}
/************************************************************************
  Main function which walks through all the adf nodes making copies of the
  node in the output file
*************************************************************************/
static void WalkTheNodes( double InputID, double OutputID )
{
  int    ic, cnt ;
  int    nchildren ;
  double InputChildID ;
  double OutputChildID ;

  /* Copy the data from the current input node to the output node */

  CopyTheNode ( InputID, OutputID ) ;

  /* Loop through the children of the current node */

  ADF_Number_of_Children(InputID,&nchildren,&ErrStat) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "WalkTheNodes:ADF_Number_of_Children", ErrStat ) ;
  if (nchildren == 0)
    /* We hit the end of the Tree so return up one level */
    return ;
  else
  {
    for (ic=1; ic<=nchildren; ic++)
    {
      /* Get child name and node ID */

#ifdef NULL_NODEID_POINTER
      ADF_Children_IDs( InputID, ic, 1, &cnt, &InputChildID, &ErrStat );
      if ( ErrStat != StatusOK )
	ErrorExit( "WalkTheNodes:ADF_Children_IDs", ErrStat );
      ADF_Get_Name (InputChildID, name, &ErrStat );
      if ( ErrStat != StatusOK )
	ErrorExit( "WalkTheNodes:ADF_Get_Name", ErrStat ) ;
#else
      ADF_Children_Names( InputID, ic, 1, ADF_NAME_LENGTH+1, &cnt, name,
			  &ErrStat );
      if ( ErrStat != StatusOK )
	ErrorExit( "WalkTheNodes:ADF_Children_Names", ErrStat );
      ADF_Get_Node_ID( InputID, name, &InputChildID, &ErrStat ) ;
      if ( ErrStat != StatusOK )
	ErrorExit( "WalkTheNodes:ADF_Node_ID", ErrStat ) ;
#endif

      /* Write out a little status message for the user */

      if ( PrintFlag == TRUE )
        printf ( "\nWorking on ADF node %s", name ) ;

      /* Check for link, if it is a local link then we always link it!! */

      ADF_Is_Link ( InputChildID, &PathLength, &ErrStat ) ;
      if ( ErrStat != StatusOK )
        ErrorExit( "WalkTheNodes:ADF_Is_Link", ErrStat ) ;
      if ( PathLength > 0 ) {
        ADF_Get_Link_Path( InputChildID, LinkFileName, LinkPathName,
			   &ErrStat ) ;
        if ( ErrStat != StatusOK )
          ErrorExit( "WalkTheNode:ADF_Link_Path", ErrStat ) ;
      }

      /* Create the node or the link */

      if ( PathLength > 0 &&
           ( LinkFileName[0] == '\0' || IncludeLink == FALSE ) ) {
        /* Just recreate the link and skip the walk from the link point */
        ADF_Link( OutputID, name, LinkFileName, LinkPathName,
		  &OutputChildID, &ErrStat ) ;
        if ( ErrStat != StatusOK )
	  ErrorExit( "WalkTheNodes:ADF_Link", ErrStat ) ;
	continue ; /* Don't walk down the link!! */
      }
      else {
	/* Create the node in the output file and continue walking */
        ADF_Create ( OutputID, name, &OutputChildID, &ErrStat ) ;
        if ( ErrStat != StatusOK )
	  ErrorExit( "WalkTheNodes:ADF_Create", ErrStat ) ;
        WalkTheNodes ( InputChildID, OutputChildID ) ; /* Walk the path */
      }
    }
  }
}
/************************************************************************
 Creates a copy of input node data into the the output node.
*************************************************************************/
static void CopyTheNode( double InputID, double OutputID )
{
  long   DataSize ;
  char  *DataBuffer ;

  /* Copy the node label type and size information */

  ADF_Get_Label( InputID, label, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Get_Label", ErrStat ) ;
  ADF_Set_Label( OutputID, label, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Set_Label", ErrStat ) ;

  ADF_Get_Data_Type( InputID, DataType, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Get_Data_Type", ErrStat ) ;
  ADF_Get_Number_of_Dimensions( InputID, &NumDims, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Get_Number_of_Dimensions", ErrStat ) ;
  if ( NumDims == 0 ) return ;
  ADF_Get_Dimension_Values( InputID, DimVals, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Get_Dimension_Values", ErrStat ) ;
  ADF_Put_Dimension_Information( OutputID, DataType, NumDims, DimVals,
				 &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Put_Dimension_Information", ErrStat ) ;

  /* Copy any NodeData */

  DataSize = CalculateDataSize ( DataType, NumDims, DimVals ) ;
  if ( DataSize == 0 ) return ;
  DataBuffer = (char *) malloc ( DataSize*sizeof(char) ) ;
  if ( DataBuffer == NULL )
    ErrorExit( "CopyTheNode:Failed Memory Allocation", -2 ) ;
  ADF_Read_All_Data( InputID, DataBuffer, &ErrStat ) ;
  if ( ErrStat == NO_DATA ) {
    free ( DataBuffer ) ;
    return ;
  }
  else if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Read_All_Data", ErrStat ) ;
  ADF_Write_All_Data( OutputID, DataBuffer, &ErrStat ) ;
  if ( ErrStat != StatusOK )
    ErrorExit( "CopyTheNode:ADF_Write_All_Data", ErrStat ) ;
  free ( DataBuffer ) ;
}
/************************************************************************
 Function to calculate the size in bytes of node data.  It is based on
 the function ADFI_evaluate_datatype and this function should be modified
 to conform to any changes made to it.
*************************************************************************/
static long CalculateDataSize( char *data_type_string, int ndims, int *dimlist )
{
  int   i ;
  int   str_position ;
  int   str_len ;
  int   size_machine ;
  long  machine_bytes ;

  if ( ndims == 0 ) return 0 ;

  /** Upper_CASE the data-type string **/
  str_len = strlen( data_type_string ) ;
  if ( str_len == 0 ) return 0 ;
  for( i=0; i<str_len; i++ )
    data_type_string[i] = (char)TO_UPPER( data_type_string[i] ) ;

  /* The tough part, first calculate the size of a single data item! */

  str_position = 0 ;
  machine_bytes = 0 ;
  while( data_type_string[ str_position ] != '\0' ) {

    size_machine = 0 ;

        /** Look at the 2-byte datatype code **/
    switch( EVAL_2_BYTES( data_type_string[str_position],
                          data_type_string[str_position+1])) {
      case EVAL_2_BYTES( 'M', 'T' ) :
         if( (str_position == 0) && (data_type_string[ 2 ] == '\0') )
           return 0 ;
         else { /* ERROR, cannot have 'MT' with any other definition */
           ErrorExit( "CalculateDataSize:Invalid Data Type.", -2 );
          }
         break ;

      case EVAL_2_BYTES( 'I', '4' ) :
         size_machine = sizeof( int ) ;
         break ;

      case EVAL_2_BYTES( 'I', '8' ) :
         size_machine = sizeof( long ) ;
         break ;

      case EVAL_2_BYTES( 'U', '4' ) :
         size_machine = sizeof( int ) ;
         break ;

      case EVAL_2_BYTES( 'U', '8' ) :
         size_machine = sizeof( long ) ;
         break ;

      case EVAL_2_BYTES( 'R', '4' ) :
         size_machine = sizeof( float ) ;
         break ;

      case EVAL_2_BYTES( 'R', '8' ) :
         size_machine = sizeof( double ) ;
         break ;

      case EVAL_2_BYTES( 'X', '4' ) :
         size_machine = 2 * sizeof( float ) ;
         break ;

      case EVAL_2_BYTES( 'X', '8' ) :
         size_machine = 2 * sizeof( double ) ;
         break ;

      case EVAL_2_BYTES( 'B', '1' ) :
         size_machine = 1 ;
         break ;

      case EVAL_2_BYTES( 'C', '1' ) :
         size_machine = sizeof( char ) ;
         break ;

      default : /** Error condition **/
         ErrorExit( "CalculateDataSize:Invalid Data Type.", -2 );
         break ;
    } /* end switch */

    str_position += 2 ;

    /** Look for arrays '[', commas ',', of end-of-string '\0' **/

    switch( data_type_string[ str_position ] ) {
      case '\0' :
         machine_bytes += size_machine ;
         break ;

      case '[' :
       {
         int    array_size = 0 ;
         str_position += 1 ;
         while( (data_type_string[ str_position ] >= '0') &&
                (data_type_string[ str_position ] <= '9') ) {
            array_size = array_size * 10 +
                        (data_type_string[ str_position ] - '0') ;
            str_position += 1 ;
         } /* end while */
         if( data_type_string[ str_position ] != ']' ) {
            ErrorExit( "CalculateDataSize:Invalid Data Type.", -2 );
         }
         str_position += 1 ;
         /** Check for comma between types **/
         if( data_type_string[ str_position ] == ',' ) {
            str_position += 1 ;
         }
         machine_bytes = machine_bytes + size_machine * array_size ;
         }
         break ;

      case ',' :
         str_position += 1 ;
         machine_bytes += size_machine ;
         break ;

      default : /** Error condition **/
         ErrorExit( "CalculateDataSize:Invalid Data Type.", -2 );
         break ;
    } /* end switch */
  } /* end while */

  /* Now calculate the total size based on the dimensions */

  for ( i=0; i<ndims; i++ ) machine_bytes *= dimlist[i] ;

  return machine_bytes ;
}
/************************************************************************
 Error output function
*************************************************************************/
static void ErrorExit( char *ermsg, int ErrStat )
{
  sprintf(input,"\nError return from %s; errno = %d",ermsg,ErrStat) ;
  fprintf(stderr,"%s\n",input) ;
  if ( ErrStat > 0 ) {
    ADF_Error_Message(ErrStat,input) ;
    fprintf(stderr,"%s\n",input) ;
  }
  exit(1) ;
}

#if defined (__cplusplus)
}
#endif

