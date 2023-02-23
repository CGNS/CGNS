/* CGNS file generator for test_back_comp.c, which checks
   backward compatibility of the CGNS library.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils.h"

#if CGNS_VERSION < 3100
# define cgsize_t int
#define CGNS_ENUMV(X) X
#define CGNS_ENUMT(X) X
#endif

#define maxelemi 20*16*8
#define maxelemj 1216
#define maxlen 80
#define ntt 20
#define maxcount 960

int main()
{
  double x[21*17*9],y[21*17*9],z[21*17*9];
  cgsize_t isize[3][1],ielem[maxelemi][8],jelem[maxelemj][4];
  cgsize_t nelem_start,nelem_end;
  int ni,nj,nk,iset,i,j,k,fd,icelldim,iphysdim;
  int index_base,index_zone,index_coord,ielem_no;
  int ifirstnode,nbdyelem,index_section;
  char basename[maxlen],zonename[maxlen],filename[maxlen];

/* create gridpoints */
  ni=21;
  nj=17;
  nk=9;
  iset=0;
  for (k=1; k <= nk; k++) {
    for (j=1; j <=nj; j++){
      for (i=1; i <= ni; i++){
        x[iset]=(float)i-1.;
        y[iset]=(float)j-1.;
        z[iset]=(float)k-1.;
        iset=iset+1;
      }
    }
  }
  snprintf(filename, maxlen, "%s%d%s", "cgnslib_vers-", CGNS_VERSION, ".cgns");

  if(cg_set_file_type(CG_FILE_HDF5))
    cg_error_exit();

  /* WRITE X, Y, Z GRID POINTS TO CGNS FILE */
  /* open CGNS file for write */
  if (cg_open(filename,CG_MODE_WRITE,&fd)) cg_error_exit();

  /* create base (user can give any name) */
  strcpy(basename,"Base");
  icelldim=3;
  iphysdim=3;
  cg_base_write(fd,basename,icelldim,iphysdim,&index_base);

  /* define zone name (user can give any name) */
  strcpy(zonename,"Zone 1");

  /* vertex size */
  isize[0][0]=ni*nj*nk;
  /* cell size */
  isize[1][0]=(ni-1)*(nj-1)*(nk-1);
  /* boundary vertex size (zero if elements not sorted) */
  isize[2][0]=0;
  /* create zone */
  cg_zone_write(fd,index_base,zonename,isize[0],CGNS_ENUMV(Unstructured),&index_zone);
  /* write grid coordinates (user must use SIDS-standard names here) */
  cg_coord_write(fd,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateX",x,&index_coord);
  cg_coord_write(fd,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateY",y,&index_coord);
  cg_coord_write(fd,index_base,index_zone,CGNS_ENUMV(RealDouble),"CoordinateZ",z,&index_coord);

  /* set element connectivity: */
  /* ---------------------------------------------------------- */
  /* do all the HEXA_8 elements (this part is mandatory): */
  /* maintain SIDS-standard ordering */

  ielem_no=0;
  /* index no of first element */
  nelem_start=1;
  for (k=1; k < nk; k++){
    for (j=1; j < nj; j++){
      for (i=1; i < ni; i++) {
        /*
          in this example, due to the order in the node numbering, the
          hexahedral elements can be reconstructed using the following
          relationships:
        */
        ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
        ielem[ielem_no][0]=ifirstnode;
        ielem[ielem_no][1]=ifirstnode+1;
        ielem[ielem_no][2]=ifirstnode+1+ni;
        ielem[ielem_no][3]=ifirstnode+ni;
        ielem[ielem_no][4]=ifirstnode+ni*nj;
        ielem[ielem_no][5]=ifirstnode+ni*nj+1;
        ielem[ielem_no][6]=ifirstnode+ni*nj+1+ni;
        ielem[ielem_no][7]=ifirstnode+ni*nj+ni;
        ielem_no=ielem_no+1;
      }
    }
  }
  /* index no of last element (=2560) */
  nelem_end=ielem_no;
  if (nelem_end > maxelemi) {
    printf("\nError, must increase maxelemi to at least %lu\n",(unsigned long)nelem_end);
    return 1;
  }
  /* unsorted boundary elements */
  nbdyelem=0;
  /* write CGNS_ENUMV(HEXA_8) element connectivity (user can give any name) */
  cg_section_write(fd,index_base,index_zone,"Elem",CGNS_ENUMV(HEXA_8),nelem_start,
                   nelem_end,nbdyelem,ielem[0],&index_section);

  /* ---------------------------------------------------------- */
  /*
    do boundary (QUAD) elements (this part is optional,
    but you must do it if you eventually want to define BCs
    at element faces rather than at nodes):
    maintain SIDS-standard ordering
  */
  /* INFLOW: */
  ielem_no=0;
  /* index no of first element */
  nelem_start=nelem_end+1;
  i=1;
  for (k=1; k < nk; k++) {
    for (j=1; j < nj; j++) {
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode;
      jelem[ielem_no][1]=ifirstnode+ni*nj;
      jelem[ielem_no][2]=ifirstnode+ni*nj+ni;
      jelem[ielem_no][3]=ifirstnode+ni;
      ielem_no=ielem_no+1;
    }
  }
  /* index no of last element */
  nelem_end=nelem_start+ielem_no-1;
  if (ielem_no > maxelemj) {
    printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
    return 1;
  }
  /* write QUAD element connectivity for inflow face (user can give any name) */
  cg_section_write(fd,index_base,index_zone,"InflowElem",CGNS_ENUMV(QUAD_4),nelem_start,
                   nelem_end,nbdyelem,jelem[0],&index_section);
  /* OUTFLOW: */
  ielem_no=0;
  /* index no of first element */
  nelem_start=nelem_end+1;
  i=ni-1;
  for (k=1; k < nk; k++) {
    for (j=1; j < nj; j++) {
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode+1;
      jelem[ielem_no][1]=ifirstnode+1+ni;
      jelem[ielem_no][2]=ifirstnode+ni*nj+1+ni;
      jelem[ielem_no][3]=ifirstnode+ni*nj+1;
      ielem_no=ielem_no+1;
    }
  }
  /* index no of last element */
  nelem_end=nelem_start+ielem_no-1;
  if (ielem_no > maxelemj) {
    printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
    return 1;
  }
  /* write QUAD element connectivity for outflow face (user can give any name) */
  cg_section_write(fd,index_base,index_zone,"OutflowElem",CGNS_ENUMV(QUAD_4),nelem_start,
                   nelem_end,nbdyelem,jelem[0],&index_section);
  /* SIDEWALLS: */
  ielem_no=0;
  /* index no of first element */
  nelem_start=nelem_end+1;
  j=1;
  for (k=1; k < nk; k++) {
    for (i=1; i < ni; i++) {
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode;
      jelem[ielem_no][1]=ifirstnode+ni*nj;
      jelem[ielem_no][2]=ifirstnode+ni*nj+1;
      jelem[ielem_no][3]=ifirstnode+1;
      ielem_no=ielem_no+1;
    }
  }
  j=nj-1;
  for (k=1; k < nk; k++) {
    for (i=1; i < ni; i++){
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode+1+ni;
      jelem[ielem_no][1]=ifirstnode+ni;
      jelem[ielem_no][2]=ifirstnode+ni*nj+ni;
      jelem[ielem_no][3]=ifirstnode+ni*nj+1+ni;
      ielem_no=ielem_no+1;
    }
  }
  k=1;
  for (j=1; j < nj; j++) {
    for (i=1; i < ni; i++) {
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode;
      jelem[ielem_no][1]=ifirstnode+1;
      jelem[ielem_no][2]=ifirstnode+1+ni;
      jelem[ielem_no][3]=ifirstnode+ni;
      ielem_no=ielem_no+1;
    }
  }
  k=nk-1;
  for (j=1; j < nj; j++) {
    for (i=1; i < ni; i++) {
      ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
      jelem[ielem_no][0]=ifirstnode+ni*nj;
      jelem[ielem_no][1]=ifirstnode+ni*nj+ni;
      jelem[ielem_no][2]=ifirstnode+ni*nj+1+ni;
      jelem[ielem_no][3]=ifirstnode+ni*nj+1;
      ielem_no=ielem_no+1;
    }
  }
  /* index no of last element */
  nelem_end=nelem_start+ielem_no-1;
  if (ielem_no > maxelemj) {
    printf("\nError, must increase maxelemj to at least %d\n",ielem_no);
    return 1;
  }
  /* write QUAD element connectivity for sidewall face (user can give any name) */
  cg_section_write(fd,index_base,index_zone,"SidewallElem",CGNS_ENUMV(QUAD_4),nelem_start,
                   nelem_end,nbdyelem,jelem[0],&index_section);

  /* ---------------------------------------------------------- */

  double r[9*17*21],p[9*17*21];
  int index_flow, index_field;
  char solname[maxlen];

  /* create fake flow solution AT CELL CENTERS for simple example: */
  ni=21;
  nj=17;
  nk=9;
  iset=0;
  for (k=0; k < nk; k++) {
    for (j=0; j < nj; j++) {
      for (i=0; i < ni; i++) {
        r[iset]=(float)i;
        p[iset]=(float)j;
        iset=iset+1;
      }
    }
  }

/* WRITE FLOW SOLUTION TO EXISTING CGNS FILE */
  index_base=1;
  index_zone=1;

  /* define flow solution node name (user can give any name) */
  strcpy(solname,"FlowSolution");
  /* create flow solution node */
  cg_sol_write(fd,index_base,index_zone,solname,CGNS_ENUMV(Vertex),&index_flow);
  /* write flow solution (user must use SIDS-standard names here) */
  cg_field_write(fd,index_base,index_zone,index_flow,
                 CGNS_ENUMV(RealDouble),"Density",r,&index_field);
  cg_field_write(fd,index_base,index_zone,index_flow,
                 CGNS_ENUMV(RealDouble),"Pressure",p,&index_field);

  printf("\nSuccessfully added Vertex flow solution data to file %s (unstructured)\n", filename);

  /* ---------------------------------------------------------- */

  double xmach,reue,xmv,xmc,rev,rel,renu,rho0;
  double p0,c0,vm0,xlength0,vx,vy,vz;
  double gamma;
  CGNS_ENUMT(DataClass_t) idata;
  cgsize_t nuse;

  /* define nondimensional parameters */
  xmach=4.6;
  reue=6000000.;
  xmv=xmach;
  xmc=1.;
  rev=xmach;
  rel=1.;
  renu=xmach/reue;
  rho0=1.;
  gamma=1.4;
  p0=1./gamma;
  c0=1.;
  vm0=xmach/reue;
  xlength0=1.;
  vx=xmach;
  vy=0.;
  vz=0.;
  nuse=1;

  /* WRITE NONDIMENSIONAL INFO */
  /* open CGNS file for modify */
  //  if (cg_open("grid_c.cgns",CG_MODE_MODIFY,&fd)) cg_error_exit();
  /* we know there is only one base (real working code would check!) */
  index_base=1;
  /* put DataClass under Base */
  cg_goto(fd,index_base,"end");
  /* check first if a dataclass has already been written */
  if (CG_OK == cg_dataclass_read(&idata)) {
    printf("\nError! DataClass already exists!\n");
    printf("  Re-make CGNS file without it and related info, then try again\n");
    return 1;
  }
  cg_dataclass_write(CGNS_ENUMV(NormalizedByUnknownDimensional));
  /* put ReferenceState under Base */
  cg_state_write("ReferenceQuantities");
  /* Go to ReferenceState node, write Mach array and its dataclass */
  cg_goto(fd,index_base,"ReferenceState_t",1,"end");
  cg_array_write("Mach",CGNS_ENUMV(RealDouble),1,&nuse,&xmach);
  cg_goto(fd,index_base,"ReferenceState_t",1,"DataArray_t",1,"end");
  cg_dataclass_write(CGNS_ENUMV(NondimensionalParameter));
  /* Go to ReferenceState node, write Reynolds array and its dataclass */
  cg_goto(fd,index_base,"ReferenceState_t",1,"end");
  cg_array_write("Reynolds",CGNS_ENUMV(RealDouble),1,&nuse,&reue);
  cg_goto(fd,index_base,"ReferenceState_t",1,"DataArray_t",2,"end");
  cg_dataclass_write(CGNS_ENUMV(NondimensionalParameter));
  /* Go to ReferenceState node to write reference quantities: */
  cg_goto(fd,index_base,"ReferenceState_t",1,"end");
  /* First, write reference quantities that make up Mach and Reynolds: */
  /* Mach_Velocity */
  cg_array_write("Mach_Velocity",CGNS_ENUMV(RealDouble),1,&nuse,&xmv);
  /* Mach_VelocitySound */
  cg_array_write("Mach_VelocitySound",CGNS_ENUMV(RealDouble),1,&nuse,&xmc);
  /* Reynolds_Velocity */
  cg_array_write("Reynolds_Velocity",CGNS_ENUMV(RealDouble),1,&nuse,&rev);
  /* Reynolds_Length */
  cg_array_write("Reynolds_Length",CGNS_ENUMV(RealDouble),1,&nuse,&rel);
  /* Reynolds_ViscosityKinematic */
  cg_array_write("Reynolds_ViscosityKinematic",CGNS_ENUMV(RealDouble),1,&nuse,&renu);

  /* Next, write flow field reference quantities: */
  /* Density */
  cg_array_write("Density",CGNS_ENUMV(RealDouble),1,&nuse,&rho0);
  /* Pressure */
  cg_array_write("Pressure",CGNS_ENUMV(RealDouble),1,&nuse,&p0);
  /* VelocitySound */
  cg_array_write("VelocitySound",CGNS_ENUMV(RealDouble),1,&nuse,&c0);
  /* ViscosityMolecular */
  cg_array_write("ViscosityMolecular",CGNS_ENUMV(RealDouble),1,&nuse,&vm0);
  /* LengthReference */
  cg_array_write("LengthReference",CGNS_ENUMV(RealDouble),1,&nuse,&xlength0);
  /* VelocityX */
  cg_array_write("VelocityX",CGNS_ENUMV(RealDouble),1,&nuse,&vx);
  /* VelocityY */
  cg_array_write("VelocityY",CGNS_ENUMV(RealDouble),1,&nuse,&vy);
  /* VelocityZ */
  cg_array_write("VelocityZ",CGNS_ENUMV(RealDouble),1,&nuse,&vz);

  printf("\nSuccessfully wrote nondimensional info to file %s\n", filename);

  /* ---------------------------------------------------------- */

  char textstring[74];

  /* WRITE DESCRIPTOR NODE AT BASE LEVEL */

  index_base=1;

  /* go to base node */
  cg_goto(fd,index_base,"end");
  /* write descriptor node (user can give any name) */
  strcpy(textstring,"Supersonic vehicle with landing gear\n");
  strcat(textstring,"M=4.6, Re=6 million");
  cg_descriptor_write("Information",textstring);
  /* close CGNS file */
  printf("\nSuccessfully wrote descriptor node to file %s\n", filename);

  /* ---------------------------------------------------------- */

  double cl[ntt];
  int n;

  /* create history array simple example: */
  for (n=0; n < ntt; n++) {
    cl[n]=(float)n+1.;
  }
  printf("\ncreated simple cl history\n");

  index_base=1;

  /* go to base node */
  cg_goto(fd,index_base,"end");
  /* create history node (SIDS names it GlobalConvergenceHistory at base level) */
  cg_convergence_write(ntt,"");
  /* go to new history node */
  cg_goto(fd,index_base,"ConvergenceHistory_t",1,"end");
  /* write lift coefficient array (user must use SIDS-standard name here) */
  nuse=ntt;
  cg_array_write("CoefLift",CGNS_ENUMV(RealDouble),1,&nuse,&cl);

  printf("\nSuccessfully wrote cl history to file %s\n", filename);

  /* ---------------------------------------------------------- */

  int icount,index_bc,ibc;
  cgsize_t ipnts[maxcount],icounts;

/* WRITE BOUNDARY CONDITIONS TO EXISTING CGNS FILE */
  index_base=1;
  index_zone=1;
/* we know that for the unstructured zone, the following face elements */
/* have been defined as inflow*/
  nelem_start=2561;
  nelem_end=2688;
  icount=0;
  for (n=nelem_start; n <= nelem_end; n++) {
    ipnts[icount]=n;
    icount=icount+1;
  }
  if (icount > maxcount) {
    printf("\nError. Need to increase maxcount to at least %i\n",icount);
    return 1;
  }
  /* write boundary conditions for ilo face */
  icounts=icount;
  cg_boco_write(fd,index_base,index_zone,"Ilo",CGNS_ENUMV(BCTunnelInflow),
                CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);
  /* we know that for the unstructured zone, the following face elements */
  /* have been defined as outflow */
  nelem_start=2689;
  nelem_end=2816;
  icount=0;
  for (n=nelem_start; n <= nelem_end; n++) {
    ipnts[icount]=n;
    icount=icount+1;
  }
  if (icount > maxcount) {
    printf("\nError. Need to increase maxcount to at least %i\n",icount);
    return 1;
  }
  /* write boundary conditions for ihi face */
  icounts=icount;
  cg_boco_write(fd,index_base,index_zone,"Ihi",CGNS_ENUMV(BCExtrapolate),
                CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);

  /* we know that for the unstructured zone, the following face elements */
  /* have been defined as walls */
  nelem_start=2817;
  nelem_end=3776;
  icount=0;
  for (n=nelem_start; n <= nelem_end; n++) {
    ipnts[icount]=n;
    icount=icount+1;
  }
  if (icount > maxcount) {
    printf("\nError. Need to increase maxcount to at least %i\n",icount);
    return 1;
  }
  /* write boundary conditions for wall faces */
  icounts=icount;
  cg_boco_write(fd,index_base,index_zone,"Walls",CGNS_ENUMV(BCWallInviscid),
                CGNS_ENUMV(PointList),icounts,ipnts,&index_bc);

  /* the above are all face-center locations for the BCs - must indicate this, */
  /* otherwise Vertices will be assumed! */
  for (ibc=1; ibc <= index_bc; ibc++) {
    /*    (the following call positions you in BC_t - it assumes there */
    /*    is only one Zone_t and one ZoneBC_t - real working code would check!) */
    cg_goto(fd,index_base,"Zone_t",1,"ZoneBC_t",1,"BC_t",ibc,"end");
    cg_gridlocation_write(CGNS_ENUMV(FaceCenter));
  }

  /* close CGNS file */
  cg_close(fd);
  printf("\nSuccessfully wrote and closed unstructured grid to file %s \n", filename);
  return 0;
}
