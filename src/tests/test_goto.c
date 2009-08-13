#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
# include <unistd.h>
#endif
#include "cgnslib.h"

#define NUM_SIDE 5

float coord[NUM_SIDE*NUM_SIDE*NUM_SIDE];

#define CHECK(L,B) if(!(B)){fprintf(stderr,"mismatch in %s\n",L);exit(1);}

void error_exit (int iserr, char *msg)
{
    if (iserr) {
        printf ("ERROR:%s\n", msg);
        exit (1);
    }
    else
        printf ("WARNING:%s\n", msg);
}

int main (int argc, char **argv)
{
    int n, i, j, k, nuser, dim = 1;
    int cgfile, cgbase, cgzone, cgcoord, cgdset;
    int size[9];
    int ptlist[3] = {1, 2, 3};
    int ptrange[6] = {1, 1, 1, 2, 2, 2};
    int bcpoints[6], bcfaces[6];
    static char *fname = "gotest.cgns";
    char name[33];
    float data1 = 1;
    float data2 = 2;
    float exponents[8], rate[3], center[3];
    GridLocation_t gridloc;
    int ordinal, ndata, cgfam, cgbc, nunits, nexps;
    int elecflag, magnflag, condflag, dirichlet, neumann;
    PointSetType_t pttype;
    DataClass_t dclass;
    DataType_t dtype;
    BCType_t bctype;
    MassUnits_t mass;
    LengthUnits_t length;
    TimeUnits_t time;
    TemperatureUnits_t temp;
    AngleUnits_t angle;
    ElectricCurrentUnits_t current;
    SubstanceAmountUnits_t amount;
    LuminousIntensityUnits_t intensity;
    ModelType_t elecmodel;
    ModelType_t magnmodel;
    ModelType_t emconduct;

    /* errors and warnings go to error_exit */

    cg_configure(CG_CONFIG_ERROR, (void *)error_exit);

    if (argc > 1) {
        n = 0;
        if (argv[1][n] == '-') n++;
        if (argv[1][n] == 'a' || argv[1][n] == 'A')
            i = CG_FILE_ADF;
        else if (argv[1][n] == 'h' || argv[1][n] == 'H')
            i = CG_FILE_HDF5;
        else if (argv[1][n] == 'x' || argv[1][n] == 'X')
            i = CG_FILE_XML;
        else {
            fprintf(stderr, "unknown option\n");
            exit (1);
        }
        cg_set_file_type(i);
    }

    for (n = 0; n < 8; n++)
        exponents[n] = (float)n;
    for (n = 0; n < 3; n++) {
        rate[n] = (float)n;
        center[n] = (float)n;
    }
    for (n = 0; n < NUM_SIDE*NUM_SIDE*NUM_SIDE; n++)
        coord[n] = (float)n;

    unlink (fname);
    printf ("creating CGNS file %s\n", fname);
    cg_open (fname, CG_MODE_WRITE, &cgfile);
    cg_base_write (cgfile, "Base", 3, 3, &cgbase);

    /* write electomagnetics model under base */

    puts ("writing electromagnetics");
    cg_goto(cgfile, cgbase, NULL);
    cg_equationset_write (3);
    cg_goto(cgfile, cgbase, "FlowEquationSet_t", 1, NULL);
    cg_model_write("EMElectricFieldModel_t", Voltage);
    cg_model_write("EMMagneticFieldModel_t", Interpolated);
    cg_model_write("EMConductivityModel_t", Equilibrium_LinRessler);

    /* write rotating coordinates under family_t */

    puts ("writing family/rotating");
    cg_family_write(cgfile, cgbase, "Family", &cgfam);
    /* go to a named node */
    cg_goto(cgfile, cgbase, "Family", 0, NULL);
    cg_rotating_write (rate, center);

    /* write BCDataSet under FamilyBC_t */

    puts("writing FamilyBCDataSet");
    cg_fambc_write(cgfile, cgbase, cgfam, "FamilyBC", BCWall, &cgbc);
    /* relative go to */
    cg_gorel(cgfile, "FamilyBC_t", cgbc, NULL);
    cg_bcdataset_write ("FamilyBCDataSet", BCWallInviscid, Dirichlet);

    /* write user data under base */

    puts("writing user defined data under base");
    /* relative path */
    cg_gopath (cgfile, "../..");
    cg_user_data_write ("User");
    /* absolute path */
    cg_gopath (cgfile, "/Base/User");
    cg_gridlocation_write (CellCenter);
    cg_famname_write ("Family");
    cg_ordinal_write (0);
    cg_array_write ("Data1", RealSingle, 1, &dim, &data1);
    cg_array_write ("Data2", RealSingle, 1, &dim, &data2);

    for (n = 1; n <= 2; n++) {
        cg_goto (cgfile, cgbase, "User", 0, "DataArray_t", n, "end");
        cg_dataclass_write (Dimensional);
        cg_units_write (Kilogram, Meter, Second, Kelvin, Radian);
        cg_exponents_write (RealSingle, exponents);
    }

    /* this should fail since ptset not allowed as child of
       user data, except below a zone_t node */

    cg_configure(CG_CONFIG_ERROR, NULL);
    if (cg_ptset_write (PointList, 1, ptlist) == CG_OK)
        printf ("WHAT!! - ptset should not work under base/userdata\n");
    cg_configure(CG_CONFIG_ERROR, (void *)error_exit);

    /* write zone */

    puts("writing zone");
    for (n = 0; n < 3; n++) {
        size[n]   = NUM_SIDE;
        size[n+3] = NUM_SIDE - 1;
        size[n+6] = 0;
    }
    cg_zone_write (cgfile, cgbase, "Zone", size, Structured, &cgzone);
    cg_coord_write(cgfile, cgbase, cgzone, RealSingle,
        "CoordinateX", coord, &cgcoord);
    cg_coord_write(cgfile, cgbase, cgzone, RealSingle,
        "CoordinateY", coord, &cgcoord);
    cg_coord_write(cgfile, cgbase, cgzone, RealSingle,
        "CoordinateZ", coord, &cgcoord);

    /* create a BC node with point range and Dirichlet node*/

    puts("writing Dirichlet BC with vertex range");
    for (n = 0; n < 3; n++) {
        bcpoints[n]   = 1;
        bcpoints[n+3] = NUM_SIDE;
        bcfaces[n]    = 1;
        bcfaces[n+3]  = NUM_SIDE - 1;
    }
    bcpoints[5] = bcfaces[5] = 1;
    cg_boco_write (cgfile, cgbase, cgzone, "BC", BCWall,
        PointList, 1, bcpoints, &cgbc);
    cg_dataset_write (cgfile, cgbase, cgzone, cgbc,
        "DataSet", BCWallViscous, &cgdset);
    cg_bcdata_write (cgbase, cgfile, cgzone, cgbc, cgdset, Dirichlet);

    /* create Dirichlet data at faces */

    puts("writing Dirichlet data at faces");
    cg_gopath (cgfile, "/Base/Zone/ZoneBC/BC/DataSet");
    cg_gridlocation_write (KFaceCenter);
    cg_ptset_write (PointRange, 2, bcfaces);

    size[0] = (NUM_SIDE - 1) * (NUM_SIDE - 1);
    cg_gorel (cgfile, "BCData_t", Dirichlet, NULL);
    cg_array_write ("Data", RealSingle, 1, size, coord);

    /* write recursive user data */

    puts("writing recursive user defined data under zone");
    cg_goto(cgfile, cgbase, "Zone", 0, NULL);
    for (i = 1; i <= 4; i++) {
        sprintf (name, "User%d", i);
        cg_user_data_write (name);
        cg_gorel(cgfile, name, 0, NULL);
        cg_gridlocation_write (CellCenter);
        cg_famname_write ("Family");
        cg_ordinal_write (i);
        cg_ptset_write (PointList, 1, ptlist);
        cg_array_write ("Data1", RealSingle, 1, &dim, &data1);
        cg_array_write ("Data2", RealSingle, 1, &dim, &data2);
        for (n = 1; n <= 2; n++) {
            cg_gorel (cgfile, "DataArray_t", n, "end");
            cg_dataclass_write (Dimensional);
            cg_unitsfull_write (Kilogram, Meter, Second, Kelvin, Radian,
                Ampere, Mole, Candela);
            cg_expfull_write (RealSingle, exponents);
            cg_gopath (cgfile, "..");
        }

        for (j = 1; j <= 3; j++) {
            sprintf (name, "User%d.%d", i, j);
            cg_user_data_write (name);
            cg_gopath (cgfile, name);
            cg_gridlocation_write (Vertex);
            cg_famname_write ("Family");
            cg_ordinal_write (i + j);
            cg_ptset_write (PointRange, 2, ptrange);
            cg_array_write ("Data1", RealSingle, 1, &dim, &data1);
            cg_array_write ("Data2", RealSingle, 1, &dim, &data2);
            for (n = 1; n <= 2; n++) {
                cg_gorel (cgfile, "DataArray_t", n, "end");
                cg_dataclass_write (Dimensional);
                cg_unitsfull_write (Kilogram, Meter, Second, Kelvin,
                    Radian, Ampere, Mole, Candela);
                cg_expfull_write (RealSingle, exponents);
                cg_gorel (cgfile, "..", 0, NULL);
            }

            for (k = 1; k <= 2; k++) {
                sprintf (name, "User%d.%d.%d", i, j, k);
                cg_user_data_write (name);
                cg_gorel (cgfile, name, 0, NULL);
                cg_array_write ("Data1", RealSingle, 1, &dim, &data1);
                cg_array_write ("Data2", RealSingle, 1, &dim, &data2);
                for (n = 1; n <= 2; n++) {
                    cg_gorel (cgfile, "DataArray_t", n, "end");
                    cg_dataclass_write (Dimensional);
                    cg_unitsfull_write (Kilogram, Meter, Second, Kelvin,
                        Radian, Ampere, Mole, Candela);
                    cg_expfull_write (RealSingle, exponents);
                    cg_gopath (cgfile, "..");
                }

                for (n = 1; n <= 2; n++) {
                    sprintf (name, "User%d.%d.%d.%d", i, j, k, n);
                    cg_user_data_write (name);
                    cg_gopath (cgfile, name);
                    cg_array_write ("Data1", RealSingle, 1, &dim, &data1);
                    cg_array_write ("Data2", RealSingle, 1, &dim, &data2);
                    cg_gopath (cgfile, "..");
                }
                cg_gopath (cgfile, "..");
            }
            cg_gopath (cgfile, "..");
        }
        cg_gopath (cgfile, "..");
    }

    puts ("closing and reopening in read mode");
    cg_close (cgfile);

    /* read file and check values */

    cg_configure(CG_CONFIG_ERROR, NULL);

    if (cg_open (fname, CG_MODE_READ, &cgfile))
        cg_error_exit ();
    cgbase = cgzone = 1;

    /* check electomagnetics model under base */

    puts ("checking electromagnetics");
    if (cg_goto(cgfile, cgbase, NULL) ||
        cg_equationset_elecmagn_read(&elecflag, &magnflag, &condflag) ||
        cg_goto(cgfile, cgbase, "FlowEquationSet_t", 1, NULL) ||
        cg_model_read ("EMElectricFieldModel_t", &elecmodel) ||
        cg_model_read ("EMMagneticFieldModel_t", &magnmodel) ||
        cg_model_read ("EMConductivityModel_t", &emconduct))
        cg_error_exit();
    CHECK ("ElectricFieldFlag", elecflag == 1);
    CHECK ("ElectricFieldModel", elecmodel == Voltage);
    CHECK ("MagneticFieldFlag", magnflag == 1);
    CHECK ("MagneticFieldModel", magnmodel == Interpolated);
    CHECK ("EMConductivityFlag", condflag == 1);
    CHECK ("EMConductivityModel", emconduct == Equilibrium_LinRessler);

    /* check rotating coordinates under family_t */

    puts ("checking family/rotating");
    if (cg_goto(cgfile, cgbase, "Family_t", 1, NULL) ||
        cg_rotating_read (rate, center))
        cg_error_exit();
    for (n = 0; n < 3; n++) {
        CHECK ("rotation rate", rate[n] == (float)n);
        CHECK ("rotation center", center[n] == (float)n);
    }

    /* check BCDataSet under FamilyBC_t */

    puts("checking FamilyBCDataSet");
    *name = 0;
    if (cg_goto(cgfile, cgbase, "Family_t", 1, "FamilyBC_t", 1, NULL) ||
        cg_bcdataset_info(&ndata) ||
        cg_bcdataset_read (1, name, &bctype, &dirichlet, &neumann))
        cg_error_exit();
    CHECK("bcdataset_info", ndata == 1);
    CHECK("bcdatset name", strcmp(name, "FamilyBCDataSet") == 0);
    CHECK("bcdatset type", bctype == BCWallInviscid);
    CHECK("bcdatset dirichlet", dirichlet == 1);
    CHECK("bcdatset neumann", neumann == 0);

    /* check BC data */

    puts("checking BC data");
    if (cg_boco_info (cgfile, cgbase, cgzone, 1, name, &bctype, &pttype,
            &n, size, &i, &dtype, &ndata))
        cg_error_exit();
    CHECK("BC_t name", strcmp(name, "BC") == 0);
    CHECK("BC_t type", bctype == BCWall);
    CHECK("BC_t pntset type", pttype == PointList);
    CHECK("BC_t npnts", n == 1);

    if (cg_dataset_read (cgfile, cgbase, cgzone, 1, 1, name,
            &bctype, &dirichlet, &neumann) ||
        cg_goto (cgfile, cgbase, "Zone_t", 1, "ZoneBC_t", 1, "BC_t", 1,
            "BCDataSet_t", 1, NULL) ||
        cg_gridlocation_read (&gridloc) ||
        cg_ptset_info (&pttype, &n))
        cg_error_exit();
    CHECK("BCDataSet_t name", strcmp(name, "DataSet") == 0);
    CHECK("BCDataSet_t type", bctype == BCWallViscous);
    CHECK("BCDataSet_t location", gridloc == KFaceCenter);
    CHECK("BCDataSet_t pntset type", pttype == PointRange);
    CHECK("BC_t npnts", n == 2);
    CHECK("BCDataSet_t dirichlet", dirichlet == 1);
    CHECK("BCDataSet_t neumann", neumann == 0);

    /* check user defined data */

    puts("checking user defined data");
    *name = 0;
    if (cg_goto (cgfile, cgbase, "UserDefinedData_t", 1, "end") ||
        cg_gridlocation_read (&gridloc) ||
        cg_famname_read (name) ||
        cg_ordinal_read (&ordinal) ||
        cg_narrays (&ndata))
        cg_error_exit ();
    CHECK ("gridlocation", gridloc == CellCenter);
    CHECK ("famname", strcmp (name, "Family") == 0);
    CHECK ("ordinal", ordinal == 0);
    CHECK ("narrays", ndata == 2);

    if (cg_goto (cgfile, cgbase, "Zone_t", cgzone, "end") ||
        cg_nuser_data (&nuser))
        cg_error_exit ();
    CHECK ("nuserdata", nuser == 4);

    for (i = 1; i <= 4; i++) {
        *name = 0;
        if (cg_goto (cgfile, cgbase, "Zone_t", cgzone,
                "UserDefinedData_t", i, "end") ||
            cg_gridlocation_read (&gridloc) ||
            cg_famname_read (name) ||
            cg_ordinal_read (&ordinal) ||
            cg_ptset_info (&pttype, &n) ||
            cg_ptset_read (ptlist) ||
            cg_narrays (&ndata) ||
            cg_nuser_data (&nuser))
            cg_error_exit ();
        CHECK ("gridlocation", gridloc == CellCenter);
        CHECK ("famname", strcmp (name, "Family") == 0);
        CHECK ("ordinal", ordinal == i);
        CHECK ("pointtype", pttype == PointList);
        CHECK ("npoints", n == 1);
        CHECK ("narrays", ndata == 2);
        CHECK ("nuserdata", nuser == 3);

        for (j = 1; j <= 3; j++) {
            *name = 0;
            if (cg_goto (cgfile, cgbase, "Zone_t", cgzone,
                    "UserDefinedData_t", i,
                    "UserDefinedData_t", j, "end") ||
                cg_gridlocation_read (&gridloc) ||
                cg_famname_read (name) ||
                cg_ordinal_read (&ordinal) ||
                cg_ptset_info (&pttype, &n) ||
                cg_ptset_read (ptlist) ||
                cg_narrays (&ndata) ||
                cg_nuser_data (&nuser))
                cg_error_exit ();
            CHECK ("gridlocation", gridloc == Vertex);
            CHECK ("famname", strcmp (name, "Family") == 0);
            CHECK ("ordinal", ordinal == (i + j));
            CHECK ("pointtype", pttype == PointRange);
            CHECK ("npoints", n == 2);
            CHECK ("narrays", ndata == 2);
            CHECK ("nuserdata", nuser == 2);

            for (n = 1; n <= 2; n++) {
                if (cg_goto (cgfile, cgbase, "Zone_t", cgzone,
                        "UserDefinedData_t", i,
                        "UserDefinedData_t", j,
                        "DataArray_t", n, "end") ||
                    cg_dataclass_read (&dclass) ||
                    cg_nunits (&nunits) ||
                    cg_unitsfull_read (&mass, &length, &time, &temp, &angle,
                        &current, &amount, &intensity) ||
                    cg_nexponents (&nexps) ||
                    cg_expfull_read (exponents))
                    cg_error_exit ();
                CHECK ("dataclass", dclass == Dimensional);
                CHECK ("nunits", nunits == 8);
                CHECK ("massunits", mass == Kilogram);
                CHECK ("lengthunits", length == Meter);
                CHECK ("timeunits", time == Second);
                CHECK ("tempunits", temp == Kelvin);
                CHECK ("angleunits", angle == Radian);
                CHECK ("currentunits", current == Ampere);
                CHECK ("amountunits", amount == Mole);
                CHECK ("intensityunits", intensity == Candela);
                CHECK ("nexponents", nexps == 8);
                for (n = 0; n < 8; n++)
                    CHECK ("exponents", exponents[n] == (float)n);
            }
        }
    }

    if (cg_goto (cgfile, cgbase, "Zone_t", cgzone,
            "UserDefinedData_t", 2, "UserDefinedData_t", 2,
            "UserDefinedData_t", 2, "UserDefinedData_t", 1, "end") ||
        cg_narrays (&ndata) ||
        cg_nuser_data (&nuser) ||
        cg_array_info (2, name, &dtype, &n, &dim) ||
        cg_array_read (1, &data1) ||
        cg_array_read (2, &data2))
        cg_error_exit ();
    CHECK ("narrays", ndata == 2);
    CHECK ("nuserdata", nuser == 0);
    CHECK ("arrayname", strcmp (name, "Data2") == 0);
    CHECK ("datatype", dtype == RealSingle);
    CHECK ("ndims", n == 1);
    CHECK ("dims", dim == 1);
    CHECK ("data1", data1 == 1.0);
    CHECK ("data2", data2 == 2.0);

    /* read partial units/exponents as full */

    puts("checking units and exponents");
    if (cg_goto(cgfile, cgbase, "UserDefinedData_t", 1,
            "DataArray_t", 1, NULL) ||
        cg_nunits (&nunits) ||
        cg_unitsfull_read (&mass, &length, &time, &temp, &angle,
            &current, &amount, &intensity) ||
        cg_nexponents (&nexps) ||
        cg_expfull_read (exponents))
        cg_error_exit ();
    CHECK ("nunits", nunits == 5);
    CHECK ("massunits", mass == Kilogram);
    CHECK ("lengthunits", length == Meter);
    CHECK ("timeunits", time == Second);
    CHECK ("tempunits", temp == Kelvin);
    CHECK ("angleunits", angle == Radian);
    CHECK ("currentunits", current == 0);
    CHECK ("amountunits", amount == 0);
    CHECK ("intensityunits", intensity == 0);
    CHECK ("nexponents", nexps == 5);
    for (n = 0; n < 5; n++)
        CHECK ("exponents", exponents[n] == (float)n);
    for (n = 6; n < 8; n++)
        CHECK ("exponents", exponents[n] == (float)0.0);

    /* read full units/exponents as partial */

    if (cg_goto(cgfile, cgbase, "Zone_t", 1, "UserDefinedData_t", 1,
            "DataArray_t", 1, NULL) ||
        cg_nunits (&nunits) ||
        cg_units_read (&mass, &length, &time, &temp, &angle) ||
        cg_nexponents (&nexps) ||
        cg_exponents_read (exponents))
        cg_error_exit ();
    CHECK ("nunits", nunits == 8);
    CHECK ("massunits", mass == Kilogram);
    CHECK ("lengthunits", length == Meter);
    CHECK ("timeunits", time == Second);
    CHECK ("tempunits", temp == Kelvin);
    CHECK ("angleunits", angle == Radian);
    CHECK ("nexponents", nexps == 8);
    for (n = 0; n < 5; n++)
        CHECK ("exponents", exponents[n] == (float)n);

    puts ("closing file and reopening in modify mode");
    cg_close (cgfile);

    /* delete userdata node */

    if (cg_open (fname, CG_MODE_MODIFY, &cgfile))
        cg_error_exit ();

    puts ("deleting user defined data and checking");
    if (cg_goto (cgfile, 1, "Zone_t", 1,
            "UserDefinedData_t", 3,
            "UserDefinedData_t", 2,
            "UserDefinedData_t", 1, "end") ||
        cg_nuser_data (&nuser))
        cg_error_exit ();
    CHECK ("nuserdata", nuser == 2);
    if (cg_delete_node ("User3.2.1.1") ||
        cg_nuser_data (&nuser))
        cg_error_exit ();
    CHECK ("nuserdata", nuser == 1);

    /* don't compress file on close */

    cg_configure(CG_CONFIG_COMPRESS, (void *)0);

    puts ("closing file");
    cg_close (cgfile);

    return 0;
}

