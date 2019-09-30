// 
// 3/27/2019
// david.gutzwiller@numeca.be
//
// launch with:
// mpirun -np <#> CGNSMeshTest 
//  -nbCellSide <N> 
//  -useMixedElements 
//  -nbZones <N>
//
// This test mimics the writing and reading of a large unstructured mesh 
// using either MIXED or UNIFORM elements.  Multiple zones may be written
// as cubes offset in X. 
//
//----------------------------------------------------------------------

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#include <mpi.h>
#include <pcgnslib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void compareValuesDouble(double val1, double val2)
{
    if (abs(val1 - val2) > 1e-10)
    {
        printf("ERROR - value comparison failed %f, %f\n",val1,val2);
        MPI_Abort(MPI_COMM_WORLD,0);
    }
}

void compareValuescgSize_t(cgsize_t val1, cgsize_t val2)
{
    if (val1 != val2)
    {
        printf("ERROR - value comparison failed %d, %d\n",val1,val2);
        MPI_Abort(MPI_COMM_WORLD,0);
    }
}

void callCGNS(int error)
{
    if (error != CG_OK) cgp_error_exit();
}

int main(int argc, char** argv) 
{
    // set up MPI
    MPI_Init(NULL, NULL);
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    // create parallel/serial read communicators
    MPI_Comm parallelIOComm = MPI_COMM_WORLD;
    MPI_Comm serialIOComm   = MPI_COMM_SELF;

    // various CGNS handles
    int fileHandle    = 0;
    int baseHandle    = 0;
    int zoneHandle    = 0;
    int coordHandle   = 0;
    int elementHandle = 0;

    // parse arguments
    bool useSerialIOComm = false;
    bool useBufferedSectionWrite = false;
    bool useMixed = false;
    cgsize_t nbCellSide = 20;
    int nbZones = 5;
    int iarg = 0;
    char fileName[] = "testFile.cgns";
    if (rank == 0) printf("Unstructured CGNS mesh write test with %d ranks\n",size);
    while (iarg < argc)
    {
        if (!strcmp(argv[iarg], "-nbCellSide"))
        {
            nbCellSide  = atoi(argv[iarg+1]);
            if (rank == 0) printf("nbCellSide %d\n",nbCellSide);
        }
        if (!strcmp(argv[iarg], "-nbZones"))
        {
            nbZones = atoi(argv[iarg+1]);
            if (rank == 0) printf("nbZones %d\n",nbZones);
        }
        if (!strcmp(argv[iarg], "-useMixedElements"))
        {
            useMixed = true;
        }
        iarg++;
    }
    
    // basic mesh information
    cgsize_t nbNodeSide  = nbCellSide+1;
    cgsize_t nbNodeSlice = nbNodeSide*nbNodeSide;
    cgsize_t nbNodeTotal = nbNodeSide*nbNodeSlice;
    cgsize_t nbCellSlice = nbCellSide*nbCellSide;
    cgsize_t nbCellTotal = nbCellSide*nbCellSlice;
    
    ElementType_t eType  = MIXED;
    if (!useMixed) eType = HEXA_8;
    
    // distribute the elements and nodes hosted by each rank
    // Internal numbering... CGNS offsets added later.
    cgsize_t nbNodeIdeal = (nbNodeTotal / size) + 1;
    cgsize_t nbCellIdeal = (nbCellTotal / size) + 1;
    cgsize_t cellOnProcStart = (rank  )*(nbCellIdeal); 
    cgsize_t cellOnProcEnd   = (rank+1)*(nbCellIdeal);
    cgsize_t nodeOnProcStart = (rank  )*(nbNodeIdeal); 
    cgsize_t nodeOnProcEnd   = (rank+1)*(nbNodeIdeal);
    cellOnProcEnd = MIN(cellOnProcEnd,nbCellTotal);
    nodeOnProcEnd = MIN(nodeOnProcEnd,nbNodeTotal);
    cgsize_t nbCellWrite = cellOnProcEnd - cellOnProcStart;
    cgsize_t nbNodeWrite = nodeOnProcEnd - nodeOnProcStart;
    
    printf("rank %d hosts %d cells in range [%d,%d]\n",rank, nbCellWrite, cellOnProcStart, cellOnProcEnd);
    printf("rank %d hosts %d nodes in range [%d,%d]\n",rank, nbNodeWrite, nodeOnProcStart, nodeOnProcEnd);

    // create a simple cube mesh
    double* nodeX = (double*)malloc(sizeof(double)*nbNodeWrite);
    double* nodeY = (double*)malloc(sizeof(double)*nbNodeWrite);
    double* nodeZ = (double*)malloc(sizeof(double)*nbNodeWrite);
    double spacing = 1.0/(nbNodeSide-1);
    int count = 0;
    for (cgsize_t iNode = nodeOnProcStart; iNode < nodeOnProcEnd; iNode++)
    {
        cgsize_t i = floor(iNode / nbNodeSlice);
        cgsize_t j = floor((iNode - i*nbNodeSlice) / nbNodeSide);
        cgsize_t k = floor(iNode - i*nbNodeSlice - j*nbNodeSide);
        nodeX[count] = (i*spacing);
        nodeY[count] = (j*spacing);
        nodeZ[count] = (k*spacing); 
        count++;
    }
    
    cgsize_t* cells = (cgsize_t*)malloc(sizeof(cgsize_t)*nbCellWrite*8);
    count = 0;
    for (cgsize_t iCell = cellOnProcStart; iCell < cellOnProcEnd; iCell++)
    {
        cgsize_t i = floor(iCell / nbCellSlice);
        cgsize_t j = floor((iCell - i*nbCellSlice) / nbCellSide);
        cgsize_t k = floor(iCell - i*nbCellSlice - j*nbCellSide);
        cells[count+0] = (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1;
        cells[count+1] = (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1;
        cells[count+2] = (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1;
        cells[count+3] = (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1;
        cells[count+4] = (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1;
        cells[count+5] = (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1;
        cells[count+6] = (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1;
        cells[count+7] = (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1;
        count = count+8;
    }
    
    double writeTimerStart = MPI_Wtime();

    // turn off compression, prepare for independent parallel access
    callCGNS(cgp_pio_mode(CGP_INDEPENDENT));
    callCGNS(cg_configure(CG_CONFIG_COMPRESS,0));

    // rank 0 creates the file, creates the base node, writes a descriptor note.
    if (rank == 0)
    {
        printf("creating the file\n");        
        int cellDim = 3;
        int physDim = 3;
        char baseName[] = "Unstructured3D";

        callCGNS(cgp_mpi_comm(serialIOComm));
        callCGNS(cgp_open(fileName, CG_MODE_WRITE, &fileHandle));
        callCGNS(cg_base_write(fileHandle,baseName, cellDim, physDim, &baseHandle));
        callCGNS(cg_goto(fileHandle,baseHandle,"end"));
        char name[] ="Descriptor";
        char note[] ="This is a test mesh, note written in serial.";
        callCGNS(cg_descriptor_write(name, note));
        callCGNS(cgp_close(fileHandle));
    }
    MPI_Bcast(&fileHandle,1,MPI_INT,0,parallelIOComm);
    MPI_Bcast(&baseHandle,1,MPI_INT,0,parallelIOComm);
    
    if (rank == 0) printf("writing node data in parallel\n");
    for (int iZone=0; iZone<nbZones; iZone++)
    {
        // offset the nodes for each zone
        for (cgsize_t iNode = 0; iNode < nbNodeWrite; iNode++)
        {
            nodeX[iNode] = nodeX[iNode] + iZone*1.0;
        }
        
        // re-open the file, this time in parallel
        callCGNS(cgp_mpi_comm(parallelIOComm));
        callCGNS(cgp_open(fileName, CG_MODE_MODIFY, &fileHandle));
        
        // create the zone
        char zoneName[10];
        sprintf(zoneName, "domain%d", iZone);
        cgsize_t zoneSize[9];
        zoneSize[0] = nbNodeTotal;
        zoneSize[1] = nbCellTotal;
        zoneSize[2] = 0;
        callCGNS(cg_zone_write(fileHandle,baseHandle, zoneName, zoneSize, Unstructured, &zoneHandle));

        // write the nodes in parallel
        DataType_t precision = CGNS_ENUMV(RealDouble);
        cgsize_t start = nodeOnProcStart+1;
        cgsize_t end   = nodeOnProcEnd;
        callCGNS(cgp_coord_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateX",&coordHandle));
        callCGNS(cg_coord_partial_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateX",&start,&end,&nodeX[0],&coordHandle));
        callCGNS(cgp_coord_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateY",&coordHandle));
        callCGNS(cg_coord_partial_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateY",&start,&end,&nodeY[0],&coordHandle));
        callCGNS(cgp_coord_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateZ",&coordHandle));
        callCGNS(cg_coord_partial_write(fileHandle,baseHandle,zoneHandle,precision,"CoordinateZ",&start,&end,&nodeZ[0],&coordHandle));
        
        // done writing the coordinates, close the file. 
        callCGNS(cgp_close(fileHandle));
    }    
    MPI_Barrier(MPI_COMM_WORLD);

    // write elements, mixed
    if (useMixed)
    {
        // re-open the file in serial to write the MIXED element section from rank 0
        if (rank == 0) printf("writing mixed element data in serial\n");
        if (rank == 0)
        {
            for (int iZone=0; iZone<nbZones; iZone++)
            {
                // re-open the file, this time in serial
                callCGNS(cgp_mpi_comm(serialIOComm));
                callCGNS(cgp_open(fileName, CG_MODE_MODIFY, &fileHandle));
                zoneHandle = iZone+1;

                // create element node
                cgsize_t start = 1;
                cgsize_t end   = nbCellTotal;
                callCGNS(cg_section_partial_write(fileHandle,baseHandle,zoneHandle,"Elements 3D",eType,start,end,0,&elementHandle));

                // loop over all procs, perform partial writes for each chunk of the section. 
                cgsize_t globalOffset = 0;
                for (int iProc=0; iProc<size; iProc++)
                {
                    cgsize_t nbWrite;
                    cgsize_t start;
                    cgsize_t end;
                    cgsize_t* cellsTmp;
                    if (iProc != rank)
                    {
                        MPI_Recv(&nbWrite, 1, MPI_LONG, iProc, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                        MPI_Recv(&start, 1, MPI_LONG, iProc, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                        MPI_Recv(&end, 1, MPI_LONG, iProc, 3, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                        cellsTmp =  (cgsize_t*)malloc(sizeof(cgsize_t)*nbWrite*8);
                        MPI_Recv(cellsTmp,nbWrite*8, MPI_LONG, iProc, 4, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    }
                    else
                    {
                        nbWrite = nbCellWrite;
                        cellsTmp = (cgsize_t*)malloc(sizeof(cgsize_t)*nbWrite*8);
                        for (cgsize_t i=0; i<nbWrite*8; i++) cellsTmp[i] = cells[i];
                        start   = cellOnProcStart+1;
                        end     = cellOnProcEnd;
                    }
                                    
                    // insert the element type flags
                    cgsize_t* cellsWrite = (cgsize_t*)malloc(sizeof(cgsize_t)*nbWrite*9);
                    cgsize_t ipos = 0;
                    for (int i=0; i<nbWrite; i++)
                    {
                        cellsWrite[ipos] = cgsize_t(HEXA_8);
                        ipos++;
                        for (int j=0; j<8; j++)
                        {
                            cellsWrite[ipos] = cellsTmp[i*8+j];
                            ipos++;
                        }
                    }
                    
                    // build the offset buffer
                    cgsize_t localOffset = 0;
                    cgsize_t* offsetsWrite = (cgsize_t*)malloc(sizeof(cgsize_t)*(nbWrite+1));
                    ipos = 0;
                    while (ipos < nbWrite)
                    {
                        offsetsWrite[ipos] = localOffset+globalOffset;
                        localOffset+=9;
                        ipos++;
                    }
                    globalOffset += localOffset;
                    offsetsWrite[ipos] = globalOffset;
                                   
                    // partial write of the current chunk
                    callCGNS(cg_poly_elements_partial_write(fileHandle,baseHandle,zoneHandle,elementHandle,start,end,cellsWrite,offsetsWrite));
                    
                    free(offsetsWrite);
                    free(cellsWrite);
                    free(cellsTmp);
                }

                // done writing the element for the current zone, close the file. 
                callCGNS(cgp_close(fileHandle));
            }
        }
        else
        {
            for (int iZone=0; iZone<nbZones; iZone++)
            {
                cgsize_t nbWrite = nbCellWrite;
                MPI_Send(&nbWrite, 1, MPI_LONG, 0, 1, MPI_COMM_WORLD);
                cgsize_t start = cellOnProcStart+1;
                cgsize_t end   = cellOnProcEnd;
                MPI_Send(&start, 1, MPI_LONG, 0, 2, MPI_COMM_WORLD);
                MPI_Send(&end, 1, MPI_LONG, 0, 3, MPI_COMM_WORLD);
                MPI_Send(&cells[0], nbWrite*8, MPI_LONG, 0, 4, MPI_COMM_WORLD);
            }
        }
        MPI_Barrier(MPI_COMM_WORLD);
    }
    // write elements, uniform
    else
    {
        if (rank == 0) printf("writing uniform element data in parallel\n");
        for (int iZone=0; iZone<nbZones; iZone++)
        {
            // open file in parallel
            callCGNS(cgp_mpi_comm(parallelIOComm));
            callCGNS(cgp_open(fileName, CG_MODE_MODIFY, &fileHandle));
            zoneHandle = iZone+1;

            // create element node
            cgsize_t start = 1;
            cgsize_t end   = nbCellTotal;
            callCGNS(cg_section_partial_write(fileHandle,baseHandle,zoneHandle,"Elements 3D",eType,start,end,0,&elementHandle));
            
            // write element data
            start = cellOnProcStart+1;
            end   = cellOnProcEnd;
            callCGNS(cgp_elements_write_data(fileHandle,baseHandle,zoneHandle,elementHandle,start,end,cells));

            // done writing for the current zone, close the file
            callCGNS(cgp_close(fileHandle));
        }
    }
    
    MPI_Barrier(MPI_COMM_WORLD);
    double elapsedTime = MPI_Wtime() - writeTimerStart;
    if (rank == 0) printf("\ndone, write time = %f\n\n",elapsedTime);

    // now read the data and compare it with the expected values
    if (rank == 0) printf("reading node data in parallel\n");
    for (int iZone=0; iZone<nbZones; iZone++)
    {
        // zero out the node coordinates
        for (cgsize_t iNode = 0; iNode < nbNodeWrite; iNode++)
        {
            nodeX[iNode] = 0.0;
            nodeY[iNode] = 0.0;
            nodeZ[iNode] = 0.0;
        }
        
        // open the file in parallel 
        callCGNS(cgp_mpi_comm(parallelIOComm));
        callCGNS(cgp_open(fileName, CG_MODE_READ, &fileHandle));
        
        // read the nodes and compare
        DataType_t precision = CGNS_ENUMV(RealDouble);
        cgsize_t start = nodeOnProcStart+1;
        cgsize_t end   = nodeOnProcEnd;
        coordHandle = 1;
        callCGNS(cgp_coord_read_data(fileHandle,baseHandle,zoneHandle,coordHandle,&start,&end,&nodeX[0]));
        coordHandle = 2;
        callCGNS(cgp_coord_read_data(fileHandle,baseHandle,zoneHandle,coordHandle,&start,&end,&nodeY[0]));
        coordHandle = 3;
        callCGNS(cgp_coord_read_data(fileHandle,baseHandle,zoneHandle,coordHandle,&start,&end,&nodeZ[0]));
        
        int ipos = 0;
        for (cgsize_t iNode = nodeOnProcStart; iNode < nodeOnProcEnd; iNode++)
        {
            cgsize_t i = floor(iNode / nbNodeSlice);
            cgsize_t j = floor((iNode - i*nbNodeSlice) / nbNodeSide);
            cgsize_t k = floor(iNode - i*nbNodeSlice - j*nbNodeSide);
            compareValuesDouble(nodeX[ipos],(i*spacing + iZone*1.0));
            compareValuesDouble(nodeY[ipos],(j*spacing));
            compareValuesDouble(nodeZ[ipos],(k*spacing));
            ipos++;
        }
    }
    free(nodeX);
    free(nodeY);
    free(nodeZ);
    
    for (int iZone=0; iZone<nbZones; iZone++)
    {
        // read the elements
        if (useMixed)
        {
            if (rank == 0) printf("reading mixed element data in serial\n");
            
            // do the comparison on rank 0 only, read for all procs...
            if (rank == 0)
            {
                callCGNS(cgp_mpi_comm(serialIOComm));
                callCGNS(cgp_open(fileName, CG_MODE_READ, &fileHandle));
                zoneHandle = iZone+1;
                elementHandle = 1;
                cgsize_t start  = cellOnProcStart+1;
                cgsize_t end    = cellOnProcEnd;
                cgsize_t nbRead = cellOnProcEnd - cellOnProcStart;
                for (int iProc=0; iProc<size; iProc++)
                {
                    if (iProc != rank)
                    {
                        MPI_Recv(&nbRead, 1, MPI_LONG, iProc, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                        MPI_Recv(&start, 1, MPI_LONG, iProc, 2, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                        MPI_Recv(&end, 1, MPI_LONG, iProc, 3, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
                    }
                    cgsize_t sizeRead;
                    callCGNS(cg_ElementPartialSize(fileHandle,baseHandle,zoneHandle,elementHandle,start,end,&sizeRead));
                    cgsize_t* cellsRead = (cgsize_t*)malloc(sizeof(cgsize_t)*sizeRead);
                    cgsize_t* offsetsRead = (cgsize_t*)malloc(sizeof(cgsize_t)*(nbRead+1));
                    callCGNS(cg_poly_elements_partial_read(fileHandle,baseHandle,zoneHandle,elementHandle,start,end,cellsRead,offsetsRead,NULL));
                    
                    cgsize_t count = 0;
                    for (cgsize_t iCell = (start-1); iCell < end; iCell++)
                    {
                        cgsize_t i = floor(iCell / nbCellSlice);
                        cgsize_t j = floor((iCell - i*nbCellSlice) / nbCellSide);
                        cgsize_t k = floor(iCell - i*nbCellSlice - j*nbCellSide);
                        compareValuescgSize_t(cellsRead[count+0], (cgsize_t)(HEXA_8));
                        compareValuescgSize_t(cellsRead[count+1], (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1);
                        compareValuescgSize_t(cellsRead[count+2], (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1);
                        compareValuescgSize_t(cellsRead[count+3], (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1);
                        compareValuescgSize_t(cellsRead[count+4], (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1);
                        compareValuescgSize_t(cellsRead[count+5], (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1);
                        compareValuescgSize_t(cellsRead[count+6], (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1);
                        compareValuescgSize_t(cellsRead[count+7], (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1);
                        compareValuescgSize_t(cellsRead[count+8], (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1);
                        count = count+9;
                    }
                    free(cellsRead);
                    free(offsetsRead);
                }
            }
            else
            {
                cgsize_t start  = cellOnProcStart+1;
                cgsize_t end    = cellOnProcEnd;
                cgsize_t nbRead = cellOnProcEnd - cellOnProcStart;
                MPI_Send(&nbRead, 1, MPI_LONG, 0, 1, MPI_COMM_WORLD);
                MPI_Send(&start, 1, MPI_LONG, 0, 2, MPI_COMM_WORLD);
                MPI_Send(&end, 1, MPI_LONG, 0, 3, MPI_COMM_WORLD);
            }
        }
        else
        {
            if (rank == 0) printf("reading uniform element data in parallel\n");
            
            callCGNS(cgp_mpi_comm(parallelIOComm));
            callCGNS(cgp_open(fileName, CG_MODE_READ, &fileHandle));
            zoneHandle = iZone+1;
            
            // read element data
            cgsize_t start = cellOnProcStart+1;
            cgsize_t end   = cellOnProcEnd;
            callCGNS(cgp_elements_read_data(fileHandle,baseHandle,zoneHandle,elementHandle,start,end,cells));

            // done reading for the current zone, close the file
            callCGNS(cgp_close(fileHandle));
            
            // compare values with expected
            count = 0;
            for (cgsize_t iCell = cellOnProcStart; iCell < cellOnProcEnd; iCell++)
            {
                cgsize_t i = floor(iCell / nbCellSlice);
                cgsize_t j = floor((iCell - i*nbCellSlice) / nbCellSide);
                cgsize_t k = floor(iCell - i*nbCellSlice - j*nbCellSide);
                compareValuescgSize_t(cells[count+0], (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1);
                compareValuescgSize_t(cells[count+1], (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+0)+1);
                compareValuescgSize_t(cells[count+2], (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1);
                compareValuescgSize_t(cells[count+3], (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+0)+1);
                compareValuescgSize_t(cells[count+4], (i+0)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1);
                compareValuescgSize_t(cells[count+5], (i+1)*nbNodeSlice+(j+0)*nbNodeSide+(k+1)+1);
                compareValuescgSize_t(cells[count+6], (i+1)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1);
                compareValuescgSize_t(cells[count+7], (i+0)*nbNodeSlice+(j+1)*nbNodeSide+(k+1)+1);
                count = count+8;
            }
        }
    }
    free(cells);
    
    MPI_Finalize();
}
