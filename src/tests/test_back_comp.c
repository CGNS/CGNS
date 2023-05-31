/*
  Testing  backward compatibility of CGNS files created
  using older versions of the CGNS library.  The older
  file versions can be found in the directory "data".
  Only checks HDF5 derived CGNS files, so HDF5 is required.
*/

/*
  Reads simple 3-D unstructured  parameters from a  CGNS file
  (created using data/gen_file.c).
*/

#include <stdio.h>
#include <stdlib.h>

#include "utils.h"

#define maxelemi 20*16*8
#define maxelemj 1216
#define maxlen 80
#define numlibvers 8
#define ntt 20
#define maxpnts 960

int main()
{

#if CG_BUILD_HDF5

  float x[21*17*9],y[21*17*9],z[21*17*9];
  cgsize_t isize[3][1],ielem[maxelemi][8],jelem[maxelemj][4];
  int index_base,index_zone,ielem_no;
  int ifirstnode;
  int fd;
  cgsize_t irmin,irmax,istart,iend;
  int nsections,index_sect,nbndry,iparent_flag;
  cgsize_t iparentdata;
  char zonename[maxlen],sectionname[maxlen], filename[maxlen];
  CGNS_ENUMT(ElementType_t) itype;
  int index_dim;

  int lv;
  int libvers[numlibvers] = {3000, 3110, 3210, 3310, 3420, 4000, 4100, 4400};

  int ni,nj,nk,i,j,k,iset;
  ni=21;
  nj=17;
  nk=9;

  for (lv = 0; lv < numlibvers; lv++) {
    printf("\nCHECKING CGNS, VERSION %d FILE\n", libvers[lv]);

    snprintf(filename, maxlen, "%s%d%s", "data/cgnslib_vers-", libvers[lv], ".cgns");

    /* READ X, Y, Z GRID POINTS FROM CGNS FILE */
    /* open CGNS file for read-only */
    if (cg_open(filename,CG_MODE_READ,&fd))
      cg_error_exit();

    if (cg_nbases(fd, &index_base))
      cg_error_exit();
    if (index_base !=1)
      cg_error_exit();

    if (cg_nzones(fd, index_base, &index_zone))
      cg_error_exit();
    if (index_zone !=1)
      cg_error_exit();

    /* get zone size and name */
    if(cg_zone_read(fd,index_base,index_zone,zonename,isize[0]))
      cg_error_exit();
    if(compareValuesChr(zonename,"Zone 1") == 0)
      cg_error_exit();

    if( (isize[0][0] !=  ni*nj*nk) | ( isize[1][0] !=  (ni-1)*(nj-1)*(nk-1) ) | (isize[2][0] != 0) )
      cg_error_exit();

    if(cg_index_dim(fd,index_base,index_zone,&index_dim))
      cg_error_exit();

    if(index_dim != 1)
      cg_error_exit();

    /* lower range index */
    irmin=1;
    /* upper range index of vertices */
    irmax=isize[0][0];

    /* read grid coordinates */
    if(cg_coord_read(fd,index_base,index_zone,"CoordinateX",
                     CGNS_ENUMV(RealSingle),&irmin,&irmax,x))
      cg_error_exit();

    if(cg_coord_read(fd,index_base,index_zone,"CoordinateY",
                     CGNS_ENUMV(RealSingle),&irmin,&irmax,y))
      cg_error_exit();

    if(cg_coord_read(fd,index_base,index_zone,"CoordinateZ",
                     CGNS_ENUMV(RealSingle),&irmin,&irmax,z))
      cg_error_exit();

    /* check the grid coordinates read */
    iset=0;
    for (k=1; k <= nk; k++)
      {
        for (j=1; j <=nj; j++)
          {
            for (i=1; i <= ni; i++)
              {
                if( compareValuesFloat(x[iset],i-1.) == 0)
                  cg_error_exit();
                if( compareValuesFloat(y[iset],j-1.) == 0)
                  cg_error_exit();
                if( compareValuesFloat(z[iset],k-1.) == 0)
                  cg_error_exit();
                iset=iset+1;
              }
          }
      }


    /* check the number of sections */
    if(cg_nsections(fd,index_base,index_zone,&nsections))
      cg_error_exit();

    if(compareValuesInt(nsections, 4) == 0)
      cg_error_exit();

    /* read element connectivity */
    for (index_sect=1; index_sect <= nsections; index_sect++)
      {
        cg_section_read(fd,index_base,index_zone,index_sect,sectionname,
                        &itype,&istart,&iend,&nbndry,&iparent_flag);

        printf("\nReading section data...\n");
        printf("   section name=%s\n",sectionname);
        printf("   section type=%s\n",ElementTypeName[itype]);
        printf("   istart,iend=%i, %i\n",(int)istart,(int)iend);

        if (itype == CGNS_ENUMV(HEXA_8)) {
          if(compareValuesChr(sectionname, "Elem") == 0)
            cg_error_exit();

          if( cg_elements_read(fd,index_base,index_zone,index_sect,ielem[0], &iparentdata))
            cg_error_exit();

          ielem_no=0;
          /* index no of first element */
          for (k=1; k < nk; k++) {
            for (j=1; j < nj; j++) {
              for (i=1; i < ni; i++) {
                ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
                if( !( (compareValuescgSize_t(ielem[ielem_no][0],ifirstnode) &&
                        compareValuescgSize_t(ielem[ielem_no][1],ifirstnode+1) &&
                        compareValuescgSize_t(ielem[ielem_no][2],ifirstnode+1+ni) &&
                        compareValuescgSize_t(ielem[ielem_no][3],ifirstnode+ni) &&
                        compareValuescgSize_t(ielem[ielem_no][4],ifirstnode+ni*nj) &&
                        compareValuescgSize_t(ielem[ielem_no][5],ifirstnode+ni*nj+1) &&
                        compareValuescgSize_t(ielem[ielem_no][6],ifirstnode+ni*nj+1+ni) &&
                        compareValuescgSize_t(ielem[ielem_no][7],ifirstnode+ni*nj+ni) )
                       )
                    )
                  cg_error_exit();
                ielem_no=ielem_no+1;
              }
            }
          }
        } else if (itype == CGNS_ENUMV(QUAD_4)) {

          if( compareValuesChr(sectionname,"InflowElem") == 1) {

            if( cg_elements_read(fd,index_base,index_zone,index_sect,jelem[0], &iparentdata) )
              cg_error_exit();

            ielem_no=0;
            i=1;
            for (k=1; k < nk; k++) {
              for (j=1; j < nj; j++) {
                ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
                if( !( (compareValuescgSize_t(jelem[ielem_no][0],ifirstnode) &&
                        compareValuescgSize_t(jelem[ielem_no][1],ifirstnode+ni*nj)  &&
                        compareValuescgSize_t(jelem[ielem_no][2],ifirstnode+ni*nj+ni) &&
                        compareValuescgSize_t(jelem[ielem_no][3],ifirstnode+ni) )
                       )
                    )
                  cg_error_exit();
                ielem_no=ielem_no+1;
              }
            }
          } else if( compareValuesChr(sectionname,"OutflowElem") == 1) {

            if( cg_elements_read(fd,index_base,index_zone,index_sect,jelem[0], &iparentdata))
              cg_error_exit();

            ielem_no=0;
            i=ni-1;
            for (k=1; k < nk; k++) {
              for (j=1; j < nj; j++) {
                ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
                if( !( (compareValuescgSize_t(jelem[ielem_no][0],ifirstnode+1) &&
                        compareValuescgSize_t(jelem[ielem_no][1],ifirstnode+1+ni) &&
                        compareValuescgSize_t(jelem[ielem_no][2],ifirstnode+ni*nj+1+ni) &&
                        compareValuescgSize_t(jelem[ielem_no][3],ifirstnode+ni*nj+1) )
                       )
                    )
                  cg_error_exit();
                ielem_no=ielem_no+1;
              }
            }

          } else if( compareValuesChr(sectionname,"SidewallElem") == 1) {

            if( cg_elements_read(fd,index_base,index_zone,index_sect,jelem[0], &iparentdata))
              cg_error_exit();

            ielem_no=0;
            j=1;
            for (k=1; k < nk; k++) {
              for (i=1; i < ni; i++) {
                ifirstnode=i+(j-1)*ni+(k-1)*ni*nj;
                if( !( (compareValuescgSize_t(jelem[ielem_no][0],ifirstnode) &&
                        compareValuescgSize_t(jelem[ielem_no][1],ifirstnode+ni*nj) &&
                        compareValuescgSize_t(jelem[ielem_no][2],ifirstnode+ni*nj+1) &&
                        compareValuescgSize_t(jelem[ielem_no][3],ifirstnode+1) )
                       )
                    )
                  cg_error_exit();
                ielem_no=ielem_no+1;
              }
            }

          } else {
            cg_error_exit();
          }
        } else {
          printf(" failed itype check in cg_section_read\n");
          cg_error_exit();
        }
      }

    /* ---------------------------------- read_flowvert_unst */

    float r[9*17*21],p[9*17*21];
    int index_flow;
    char solname[maxlen];
    CGNS_ENUMT(GridLocation_t) loc;

    /* READ FLOW SOLUTION FROM CGNS FILE */
    index_base=1;
    index_zone=1;

    if( cg_nsols(fd, index_base, index_zone, &index_flow) )
      cg_error_exit();

    if( compareValuesInt(index_flow,1) == 0 )
      cg_error_exit();

    /* get zone size and name */
    if( cg_zone_read(fd,index_base,index_zone,zonename,isize[0]))
      cg_error_exit();

    if( compareValuesChr(zonename,"Zone 1") == 0 )
      cg_error_exit();

    if( !( (compareValuescgSize_t(isize[0][0],9*17*21) &&
            compareValuescgSize_t(isize[1][0],2560) &&
            compareValuescgSize_t(isize[2][0],0) )
           )
        )
      cg_error_exit();

    /* lower range index */
    irmin=1;
    /* upper range index - use vertex dimensions */
    /* checking GridLocation first (real working code would check */
    /* to make sure there are no Rind cells also!): */

    if( cg_sol_info(fd,index_base, index_zone, index_flow, solname, &loc))
      cg_error_exit();

    if( compareValuesChr(solname,"FlowSolution") == 0 )
      cg_error_exit();

    if (loc != CGNS_ENUMV(Vertex)) {
      printf("\nError, GridLocation must be Vertex! Currently: %s\n", GridLocationName[loc]);
      cg_error_exit();
    }

    irmax=isize[0][0];

    /* read flow solution */
    if( cg_field_read(fd,index_base,index_zone,index_flow,"Density", \
                      CGNS_ENUMV(RealSingle),&irmin,&irmax,r))
      cg_error_exit();

    ni=21;
    nj=17;
    nk=9;
    iset=0;
    for (k=0; k < nk; k++) {
        for (j=0; j < nj; j++) {
            for (i=0; i < ni; i++) {
              if( compareValuesFloat(r[iset],i) == 0 )
                cg_error_exit();
              iset=iset+1;
            }
        }
    }

    if( cg_field_read(fd,index_base,index_zone,index_flow,"Pressure", \
                      CGNS_ENUMV(RealSingle),&irmin,&irmax,p))
      cg_error_exit();

    iset=0;
    for (k=0; k < nk; k++) {
        for (j=0; j < nj; j++) {
            for (i=0; i < ni; i++) {
              if( compareValuesFloat(p[iset],j) == 0 )
                cg_error_exit();
              iset=iset+1;
            }
        }
    }

    /* -------------------------------- read_dimensional*/

    double data;
    double gamma = 1.4;
    int narrays,n,idim;
    char *state,arrayname[maxlen];
    CGNS_ENUMT(DataClass_t) id;
    CGNS_ENUMT(DataType_t) idata;
    cgsize_t idimvec;

    /* CGNS with HDF5 Creation Ordering was introduced in CGNS 3.1.3, so the
       ordering is different in the later CGNS versions. */
    const char *chckvar[15];
    double chckval[15];

    if( libvers[lv] < 3130) {

      chckval[0] = 1.;                     /* rho0 */
      chckval[1] = 1.;                     /* xlength0 */
      chckval[2] = 4.6;                    /* xmach */
      chckval[3] = chckval[2];             /* xmv */
      chckval[4] = 1.;                     /* xmc */
      chckval[5] = 1./gamma;               /* p0 */
      chckval[6] = 6000000.;               /* reue */
      chckval[7] = 1.;                     /* rel */
      chckval[8] = chckval[2];             /* rev */
      chckval[9] = chckval[2]/chckval[6];  /* renu */
      chckval[10] = 1.;                    /* c0 */
      chckval[11] = chckval[2];            /* vx */
      chckval[12] = 0.;                    /* vy */
      chckval[13] = 0.;                    /* vz */
      chckval[14] = chckval[2]/chckval[6]; /* vm0 */

      chckvar[0] = "Density";
      chckvar[1] = "LengthReference";
      chckvar[2] = "Mach";
      chckvar[3] = "Mach_Velocity";
      chckvar[4] = "Mach_VelocitySound";
      chckvar[5] = "Pressure";
      chckvar[6] = "Reynolds";
      chckvar[7] = "Reynolds_Length";
      chckvar[8] = "Reynolds_Velocity";
      chckvar[9] = "Reynolds_ViscosityKinematic";
      chckvar[10] = "VelocitySound";
      chckvar[11] = "VelocityX";
      chckvar[12] = "VelocityY";
      chckvar[13] = "VelocityZ";
      chckvar[14] = "ViscosityMolecular";

    } else {

      chckval[0] = 4.6;                    /* xmach */
      chckval[1] = 6000000.;               /* reue */
      chckval[2] = chckval[0];             /* xmv */
      chckval[3] = 1.;                     /* xmc */
      chckval[4] = chckval[0];             /* rev */
      chckval[5] = 1.;                     /* rel */
      chckval[6] = chckval[0]/chckval[1];  /* renu */
      chckval[7] = 1.;                     /* rho0 */
      chckval[8] = 1./gamma;               /* p0 */
      chckval[9] = 1.;                     /* c0 */
      chckval[10] = chckval[0]/chckval[1]; /* vm0 */
      chckval[11] = 1.;                    /* xlength0 */
      chckval[12] = chckval[0];            /* vx */
      chckval[13] = 0.;                    /* vy */
      chckval[14] = 0.;                    /* vz */

      chckvar[0] = "Mach";
      chckvar[1] = "Reynolds";
      chckvar[2] = "Mach_Velocity";
      chckvar[3] = "Mach_VelocitySound";
      chckvar[4] = "Reynolds_Velocity";
      chckvar[5] = "Reynolds_Length";
      chckvar[6] = "Reynolds_ViscosityKinematic";
      chckvar[7] = "Density";
      chckvar[8] = "Pressure";
      chckvar[9] = "VelocitySound";
      chckvar[10] = "ViscosityMolecular";
      chckvar[11] = "LengthReference";
      chckvar[12] = "VelocityX";
      chckvar[13] = "VelocityY";
      chckvar[14] = "VelocityZ";
    }

    /* READ NONDIMENSIONAL INFO */

    if (cg_nbases(fd, &index_base))
      cg_error_exit();
    if (index_base !=1)
      cg_error_exit();

    /* read DataClass under Base */
    if(cg_goto(fd,index_base,"end"))
       cg_error_exit();

    if(cg_dataclass_read(&id))
      cg_error_exit();

    printf("\nDataClass = %s\n",DataClassName[id]);

    if (id != CGNS_ENUMV(NormalizedByUnknownDimensional)) {
      printf("\nError!  Expecting NormalizedByUnknownDimensional\n");
      cg_error_exit();
    }

    /* read ReferenceState under Base */
    if (cg_state_read(&state))
      cg_error_exit();
    if ( ! compareValuesChr(state,"ReferenceQuantities"))
      cg_error_exit();

    printf("\nReferenceState = %s\n",state);

    /* Go to ReferenceState node, read Mach array and its dataclass */
    if(cg_goto(fd,index_base,"ReferenceState_t",1,"end"))
      cg_error_exit();

    /* find out how many data arrays */
    if(cg_narrays(&narrays))
      cg_error_exit();
    if( ! compareValuesInt(narrays,15))
      cg_error_exit();


    for (n=1; n <= narrays; n++) {
      if(cg_array_info(n,arrayname,&idata,&idim,&idimvec))
        cg_error_exit();

      if (idim != 1 || idimvec != 1) {
        printf("\nError! expecting idim,idimvec=1,1\n");
        printf("   they are idim,idimvec= %i, %i\n",idim,(int)idimvec);
        cg_error_exit();
      }
      if(cg_array_read_as(n,CGNS_ENUMV(RealDouble),&data))
        cg_error_exit();

      printf(" CC n-1 %d %s %s \n", n-1, arrayname,chckvar[n-1]);
      if( compareValuesDouble(data,chckval[n-1]) == 0)
        cg_error_exit();

      if( compareValuesChr(arrayname,chckvar[n-1]) == 0)
        cg_error_exit();

      printf("Variable=%s\n",arrayname);
      printf("    data=%18.8f\n",data);
    }


    /* ----------------------------------  write_descriptor */

    int ndescriptors;
    char *text,name[maxlen];
    char text_chk[74];

    strcpy(text_chk,"Supersonic vehicle with landing gear\n");
    strcat(text_chk,"M=4.6, Re=6 million");

    /* READ DESCRIPTOR FROM EXISTING CGNS FILE */
    index_base=1;

    /* go to base node */
    if(cg_goto(fd,index_base,"end"))
      cg_error_exit();

    /* find out how many descriptors are here: */
    if(cg_ndescriptors(&ndescriptors))
      cg_error_exit();

    if( compareValuesInt(ndescriptors,1) == 0)
      cg_error_exit();

    for (n=1; n <= ndescriptors; n++) {
      /* read descriptor */
      if(cg_descriptor_read(n,name,&text))
        cg_error_exit();

      if( compareValuesChr(text,text_chk) == 0)
        cg_error_exit();

      printf("\nThe descriptor is:\n\n%s\n",text);
    }

    /* ---------------------------------- read_convergence*/

    float cl[ntt];
    int index_array,ndim;
    CGNS_ENUMT(DataType_t) itypeD;
    cgsize_t jdim;

    index_base=1;
    /* go to base node */
    if(cg_goto(fd,index_base,"end"))
      cg_error_exit();

    /* go to history node*/
    if(cg_goto(fd,index_base,"ConvergenceHistory_t",1,"end"))
      cg_error_exit();

    /* find out how many arrays are here (there should be only one!): */
    if(cg_narrays(&narrays))
      cg_error_exit();

    index_array=narrays;
    /* some checks: */
    if (narrays != 1) {
      printf("\nError!  Expecting only one array, read %i\n",narrays);
      cg_error_exit();
    }
    if(cg_array_info(index_array,arrayname,&itypeD,&ndim,&jdim))
      cg_error_exit();
    if (jdim > ntt) {
      printf("\nError! must increase ntt to at least %i\n",(int)jdim);
      cg_error_exit();
    }
    if (strcmp(arrayname,"CoefLift") != 0) {
      printf("\nError!  expecting CoefLift, read %s\n",arrayname);
      cg_error_exit();
    }

    /* read lift coefficient array */
    if(cg_array_read_as(index_array,CGNS_ENUMV(RealSingle),cl))
      cg_error_exit();

    for (n=0; n < ntt; n++){
      if( compareValuesFloat(cl[n],(float)n+1.) == 0)
        cg_error_exit();
    }

    /* ---------------------------------- read_bcpnts_unst*/

    int nbocos,ib;
    int normalindex[3],ndataset;
    int normallist;
    char boconame[33];
    CGNS_ENUMT(BCType_t) ibocotype;
    CGNS_ENUMT(PointSetType_t) iptset;
    CGNS_ENUMT(DataType_t) normaldatatype;
    CGNS_ENUMT(GridLocation_t) igr;
    cgsize_t npts,normallistflag;
    cgsize_t ipnts[maxpnts];

    index_base=1;
    index_zone=1;

    /* find out number of BCs that exist under this zone */
    if(cg_nbocos(fd,index_base,index_zone,&nbocos))
       cg_error_exit();

    if( compareValuesInt(nbocos,3) == 0)
      cg_error_exit();

    /* do loop over the total number of BCs */
    for (ib=1; ib <= nbocos; ib++) {
      /* find out what BC grid location is (expecting FaceCenter) */
      if(cg_goto(fd,index_base,"Zone_t",1,"ZoneBC_t",1,"BC_t",ib,"end"))
        cg_error_exit();

      if(cg_gridlocation_read(&igr))
        cg_error_exit();
      if (igr == CGNS_ENUMV(FaceCenter)) {
        printf("\nGridLocation=FaceCenter means BC data refers to elements, not nodes\n");
      }
      /* get BC info */
      if(cg_boco_info(fd,index_base,index_zone,ib,boconame,&ibocotype,
                      &iptset,&npts,normalindex,&normallistflag,&normaldatatype,&ndataset))
        cg_error_exit();
      if (iptset != CGNS_ENUMV(PointList)) {
        printf("\nError.  For this program, BCs must be set up as PointList type %s\n",
               PointSetTypeName[iptset]);
        cg_error_exit();
      }
      printf("\nBC number: %i\n",ib);
      printf("   name= %s\n",boconame);
      printf("   type= %s\n",BCTypeName[ibocotype]);
      printf("   no of elements= %i\n",(int)npts);
      if (npts > maxpnts) {
        printf("\nError.  Must increase maxpnts to at least %i\n",(int)npts);
        cg_error_exit();
      }
      /* read point list in here */

      if(cg_boco_read(fd,index_base,index_zone,ib,ipnts,&normallist))
        cg_error_exit();

      if( strcmp(boconame,"Ilo") == 0) {
        n=2561;
      } else if(strcmp(boconame,"Ihi") == 0) {
        n=2689;
      } else if(strcmp(boconame,"Walls") == 0) {
        n=2817;
      } else {
        cg_error_exit();
      }
      for (i=0; i < (int)npts; i++) {
        if( compareValuescgSize_t(ipnts[i],n) == 0)
          cg_error_exit();
        n++;
      }
    }

    /* close CGNS file */
    if(cg_close(fd))
      cg_error_exit();

    printf("\nSuccessfully read unstructured grid from file %s\n", filename);

  }

  /* Check opening a CGNS file generated with an earlier CGNS version which set the HDF5 library bounds to
     latest, and the HDF5 library version used was > HDF5 1.8 */

  printf ("\nopening cgns file using HDF5 1.10 file format ...");
  if (cg_open("data/cgnslib_vers-3110_hdf5-110.cgns", CG_MODE_MODIFY, &fd)) {
    printf("Failed\n");
    cg_error_exit();
  } else {
    printf("Passed\n");
  }
  printf ("closing cgns file using HDF5 1.10 file format ...");
  if (cg_close(fd)) {
    printf("Failed\n");
    cg_error_exit();
  } else {
    printf("Passed\n");
  }

  return 0;

#else

  printf("\nTEST SKIPPED: Requires HDF5-enabled CGNS build\n");
  return SKIP_RETURN_CODE; /* indicates skipped test */

#endif

}
