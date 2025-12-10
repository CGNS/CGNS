void InitHashTables ();
int ParseGLFunc (Tcl_Interp *interp,
			     int argc,
			     char *argv [],
			     int *nArg);
int SearchEnumVal (Tcl_Interp* interp,
			       char * name,
			       GLenum* val);
int SearchEnumName (Tcl_Interp* interp,
				GLenum val,
				char ** name);



