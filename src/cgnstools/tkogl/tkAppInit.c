#include <tk.h>
#include "tkogl.h"

int Tcl_AppInit(Tcl_Interp *interp) 	/* Interpreter for application. */
{
   if (Tcl_Init(interp) == TCL_ERROR)  return TCL_ERROR;

   if (Tk_Init(interp) == TCL_ERROR)   return TCL_ERROR;

   Tcl_StaticPackage(interp, "Tk", Tk_Init, (Tcl_PackageInitProc *) NULL);

   if (Tkogl_Init(interp) == TCL_ERROR)  return TCL_ERROR;

   Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
   return TCL_OK;
}


int main (int argc,                   /* Number of command-line arguments. */
	  char **argv)                /* Values of command-line arguments. */
{
    Tk_Main(argc, argv, Tcl_AppInit);
    return 0;                   /* Needed only to prevent compiler warning. */
}


