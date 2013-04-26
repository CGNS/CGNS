/*
! @file thesis_benchmark.c
! @author Kyle Horne <horne.kyle@gmail.com>
! @version 0.2
!
! @section LICENSE
! BSD style license
!
! @section DESCRIPTION
! Test program for pcgns library
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#include "pcgnslib.h"
#include "mpi.h"

int comm_size;
int comm_rank;
MPI_Info info;

double data_size = 1.0;
int N;
int Nl;
int pc;
int zpp = 2;
int ppz = 2;
int zc;

int* zones;
int* subzones;

double* x;
double* y;
double* z;

cgsize_t* e;

double* u;
double* v;
double* w;

double* h;

int read_inputs(int* argc, char*** argv) {
	int k;
	if(comm_rank==0) {
		for(k=1;k<*argc;k++) {
			if(strcmp((*argv)[k],"-ds")==0) {
				k++;
				sscanf((*argv)[k],"%lf",&data_size);
				printf("data_size=%lf\n",data_size);
				}
			if(strcmp((*argv)[k],"-zpp")==0) {
				k++;
				sscanf((*argv)[k],"%d",&zpp);
				printf("zpp=%d\n",zpp);
				}
			if(strcmp((*argv)[k],"-ppz")==0) {
				k++;
				sscanf((*argv)[k],"%d",&ppz);
				printf("ppz=%d\n",ppz);
				}
			}
		}
	MPI_Bcast(&data_size,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
	MPI_Bcast(&zpp,1,MPI_INT,0,MPI_COMM_WORLD);
	MPI_Bcast(&ppz,1,MPI_INT,0,MPI_COMM_WORLD);
	return 0;
	}

int initialize(int* argc, char*** argv) {
	int j,k;
	double theta;
	double r;

	MPI_Init(argc,argv);
	MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
	MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);
	MPI_Info_create(&info);

	read_inputs(argc,argv);

	N = (int)((data_size*1024*1024)/((double) sizeof(double)));
	pc = comm_size;
	zc = (pc*zpp)/ppz;
	Nl = N/comm_size/zpp;

	zones = malloc(zc*sizeof(int));
	subzones = malloc(zpp*sizeof(int));
	for(k=0;k<zpp;k++) {
		zones[k] = comm_rank/ppz*zpp+k;
		subzones[k] = comm_rank%ppz;
		}

	/* Initialize Arrays */
	x = malloc(Nl*sizeof(double));
	y = malloc(Nl*sizeof(double));
	z = malloc(Nl*sizeof(double));

	e = malloc(Nl*sizeof(cgsize_t));

	u = malloc(Nl*sizeof(double));
	v = malloc(Nl*sizeof(double));
	w = malloc(Nl*sizeof(double));

	h = malloc(Nl*sizeof(double));

	for(k=0;k<Nl;k++) {
		j = Nl*subzones[0]+k;
		theta = ((double) j)/((double) Nl*zpp);
		r = theta;
		x[k] = r*cos(theta);
		y[k] = r*sin(theta);
		z[k] = r;
		e[k] = j+1;
		u[k] = x[k];
		v[k] = y[k];
		w[k] = z[k];
		h[k] = r;
		}

#if 0
	printf("%d: Nl %d\n",comm_rank,Nl);
	for(k=0;k<zpp;k++) printf("%d: Z%d.%d\n",comm_rank,zones[k],subzones[k]);
#endif

	return 0;
	}

int finalize(void) {
	free(zones);
	free(subzones);

	free(x);
	free(y);
	free(z);
	free(e);
	free(u);
	free(v);
	free(w);

	MPI_Finalize();
	return 0;
	}

int main(int argc, char* argv[]) {
	int k;
	int F;
	int B;

	int *Z, *E, *S, *A;
	int *Cx, *Cy, *Cz;
	int *Fu, *Fv, *Fw;

	cgsize_t nijk[3];
	cgsize_t min, max;

	double T0,T1;
	double Tw0,Tw1;
	double Tr0,Tr1;
	double t0,t1;

	initialize(&argc,&argv);

	Z = (int *)malloc(10*zc*sizeof(int));
	E = Z + zc;
	S = E + zc;
	A = S + zc;
	Cx = A + zc;
	Cy = Cx + zc;
	Cz = Cy + zc;
	Fu = Cz + zc;
	Fv = Fu + zc;
	Fw = Fv + zc;

	if (cgp_open("thesis_benchmark.cgns", CG_MODE_WRITE, &F) ||
	    cg_base_write(F,"Base",3,3,&B))
	    cgp_error_exit();

	nijk[0] = Nl*ppz;
	nijk[1] = Nl*ppz;
	nijk[2] = 0;

	for(k=0;k<zc;k++) {
		char zonename[100+1];
		sprintf(zonename,"%s %d","Zone",k);
		if (cg_zone_write(F,B,zonename,nijk,CGNS_ENUMV(Unstructured),&(Z[k])) ||
		    cgp_coord_write(F,B,Z[k],CGNS_ENUMV(RealDouble),"CoordinateX",&(Cx[k])) ||
		    cgp_coord_write(F,B,Z[k],CGNS_ENUMV(RealDouble),"CoordinateY",&(Cy[k])) ||
		    cgp_coord_write(F,B,Z[k],CGNS_ENUMV(RealDouble),"CoordinateZ",&(Cz[k])) ||
		    cgp_section_write(F,B,Z[k],"Elements",CGNS_ENUMV(NODE),1,Nl*ppz,0,&(E[k])) ||
		    cg_sol_write(F,B,Z[k],"Solution",CGNS_ENUMV(Vertex),&S[k]) ||
		    cgp_field_write(F,B,Z[k],S[k],CGNS_ENUMV(RealDouble),"MomentumX",&(Fu[k])) ||
		    cgp_field_write(F,B,Z[k],S[k],CGNS_ENUMV(RealDouble),"MomentumY",&(Fv[k])) ||
		    cgp_field_write(F,B,Z[k],S[k],CGNS_ENUMV(RealDouble),"MomentumZ",&(Fw[k])))
		    cgp_error_exit();
		if (cg_goto(F,B,zonename,0,NULL) ||
		    cg_user_data_write("User Data") ||
		    cg_gorel(F, "User Data", 0, NULL) ||
		    cgp_array_write("phi",CGNS_ENUMV(RealDouble),1,nijk,&A[k]))
		    cgp_error_exit();
		}

	MPI_Barrier(MPI_COMM_WORLD);
	T0 = MPI_Wtime();
	Tw0 = MPI_Wtime();

	/* Writes */
	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_coord_write_data(F,B,Z[zones[k]],Cx[zones[k]],&min,&max,x) ||
		    cgp_coord_write_data(F,B,Z[zones[k]],Cy[zones[k]],&min,&max,y) ||
		    cgp_coord_write_data(F,B,Z[zones[k]],Cz[zones[k]],&min,&max,z))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Coords Write\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",3.0*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_field_write_data(F,B,Z[zones[k]],S[zones[k]],Fu[zones[k]],&min,&max,u) ||
		    cgp_field_write_data(F,B,Z[zones[k]],S[zones[k]],Fv[zones[k]],&min,&max,v) ||
		    cgp_field_write_data(F,B,Z[zones[k]],S[zones[k]],Fw[zones[k]],&min,&max,w))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Solutions Write\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",3.0*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cg_goto(F,B,"Zone_t",Z[zones[k]],
		        "UserDefinedData_t",1,NULL) ||
		    cgp_array_write_data(A[zones[k]],&min,&max,h))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Arrays Write\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_elements_write_data(F,B,Z[zones[k]],E[zones[k]],min,max,e))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Elements Write\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",((double) sizeof(int))/((double) sizeof(double))*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	Tw1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Total Write Time=%lf\n",Tw1-Tw0);
		printf("Total Write Bandwidth=%lf\n",(6.0+((double) sizeof(int))/((double) sizeof(double)))*data_size/(Tw1-Tw0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	cgp_close(F);
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) printf("Close_Time=%lf\n",t1-t0);

	/*=======
	 *=Reads=
	 *=======*/

	if (cgp_open("thesis_benchmark.cgns", CG_MODE_READ, &F))
	    cgp_error_exit();

	MPI_Barrier(MPI_COMM_WORLD);
	Tr0 = MPI_Wtime();

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_coord_read_data(F,B,Z[zones[k]],Cx[zones[k]],&min,&max,x) ||
		    cgp_coord_read_data(F,B,Z[zones[k]],Cy[zones[k]],&min,&max,y) ||
		    cgp_coord_read_data(F,B,Z[zones[k]],Cz[zones[k]],&min,&max,z))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Coords Read\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",3.0*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_field_read_data(F,B,Z[zones[k]],S[zones[k]],Fu[zones[k]],&min,&max,u) ||
		    cgp_field_read_data(F,B,Z[zones[k]],S[zones[k]],Fv[zones[k]],&min,&max,v) ||
		    cgp_field_read_data(F,B,Z[zones[k]],S[zones[k]],Fw[zones[k]],&min,&max,w))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Solutions Read\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",3.0*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cg_goto(F,B,"Zone_t",Z[zones[k]],
		        "UserDefinedData_t",1,NULL) ||
		    cgp_array_read_data(A[zones[k]],&min,&max,h))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Arrays Read\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	for(k=0;k<zpp;k++) {
		min = subzones[k]*Nl+1;
		max = (subzones[k]+1)*Nl;

		if (cgp_elements_read_data(F,B,Z[zones[k]],E[zones[k]],min,max,e))
		    cgp_error_exit();
		}
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Elements Read\n");
		printf("\tTime=%lf\n",t1-t0);
		printf("\tBandwidth=%lf\n",((double) sizeof(int))/((double) sizeof(double))*data_size/(t1-t0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	Tr1 = MPI_Wtime();
	if(comm_rank==0) {
		printf("Total Read Time=%lf\n",Tr1-Tr0);
		printf("Total Read Bandwidth=%lf\n",(6.0+((double) sizeof(int))/((double) sizeof(double)))*data_size/(Tr1-Tr0));
		}

	MPI_Barrier(MPI_COMM_WORLD);
	t0 = MPI_Wtime();
	cgp_close(F);
	MPI_Barrier(MPI_COMM_WORLD);
	t1 = MPI_Wtime();
	if(comm_rank==0) printf("Close_Time=%lf\n",t1-t0);

	MPI_Barrier(MPI_COMM_WORLD);
	T1 = MPI_Wtime();

	if(comm_rank==0) {
		printf("Total Time=%lf\n",T1-T0);
		}

	finalize();

	return 0;
	}
