#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "tcl.h"
#include "cgnslib.h"
#include "cgns_header.h"
#include "cgnames.h"
#include "ADF.h"

#ifndef CGNS_VERSION
# ifdef NofValidSimulationTypes
#  if NofValidModelTypes > 20
#   define CGNS_VERSION 2100
#  else
#   define CGNS_VERSION 2000
#  endif
# else
#  error must use CGNS Version 2.0 or greater
# endif
#endif

#ifndef CG_OK
# define CG_OK             ALL_OK
# define CG_ERROR          ERROR
# define CG_NODE_NOT_FOUND NODE_NOT_FOUND
# define CG_INCORRECT_PATH INCORRECT_PATH
#endif

static char buff[1024];

#define MAX_GOTO_DEPTH 20

static char goLabel[MAX_GOTO_DEPTH][33];
static int goIndex[MAX_GOTO_DEPTH];
static int goDepth = 0;

static int goFile = 0;
static int goBase = 0;

/*-----------------------------------------------------------------------*/

static int go_depth (
#ifdef PROTOTYPE
    int depth)
#else
    depth)
int depth;
#endif
{
    int n, ier;
    char *labels[MAX_GOTO_DEPTH];

    posit_base = goBase;
    posit_zone = 0;
    for (n = 0; n < depth; n++) {
        labels[n] = goLabel[n];
        if (strcmp (goLabel[n], "Zone_t") == 0)
            posit_zone = goIndex[n];
    }

    if (depth == 0)
        strcpy (posit_label, "CGNSBase_t");
    else
        strcpy (posit_label, goLabel[depth-1]);

    posit = cgi_get_posit (goFile, goBase, depth, goIndex, labels, &ier);

    return ier;
}

/*-----------------------------------------------------------------------*/

static int node_index (
#ifdef PROTOTYPE
    char *name, char *label)
#else
    name, label)
char *name, *label;
#endif
{
    int n;

    /* CGNSBase_t */

    if (0 == strcmp (posit_label, "CGNSBase_t")) {
        cgns_base *b = (cgns_base *)posit;
        if (0 == strcmp (label, "Zone_t")) {
            for (n = 0; n < b->nzones; n++) {
                if (0 == strcmp (b->zone[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ReferenceState_t")) {
            if (b->state && 0 == strcmp (b->state->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "Family_t")) {
            for (n = 0; n < b->nfamilies; n++) {
                if (0 == strcmp (b->family[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "BaseIterativeData_t")) {
            if (b->biter && 0 == strcmp (b->biter->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ConvergenceHistory_t")) {
            if (b->converg && 0 == strcmp (b->converg->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "FlowEquationSet_t")) {
            if (b->equations && 0 == strcmp (b->equations->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "IntegralData_t")) {
            for (n = 0; n < b->nintegrals; n++) {
                if (0 == strcmp (b->integral[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < b->nuser_data; n++) {
                if (0 == strcmp (b->user_data[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "Gravity_t")) {
            if (b->gravity && 0 == strcmp (b->gravity->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "Axisymmetry_t")) {
            if (b->axisym && 0 == strcmp (b->axisym->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "RotatingCoordinates_t")) {
            if (b->rotating && 0 == strcmp (b->rotating->name, name))
                return 1;
        }
        else
            return -1;
    }

    /* Zone_t */

    else if (0 == strcmp (posit_label, "Zone_t")) {
        cgns_zone *z = (cgns_zone *)posit;
        if (0 == strcmp (label, "GridCoordinates_t")) {
            for (n = 0; n < z->nzcoor; n++) {
                if (0 == strcmp (z->zcoor[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ZoneIterativeData_t")) {
            if (z->ziter && 0 == strcmp (z->ziter->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "Elements_t")) {
            for (n = 0; n < z->nsections; n++) {
                if (0 == strcmp (z->section[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "FlowSolution_t")) {
            for (n = 0; n < z->nsols; n++) {
                if (0 == strcmp (z->sol[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "RigidGridMotion_t")) {
            for (n = 0; n < z->nrmotions; n++) {
                if (0 == strcmp (z->rmotion[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ArbitraryGridMotion_t")) {
            for (n = 0; n < z->namotions; n++) {
                if (0 == strcmp (z->amotion[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ZoneGridConnectivity_t")) {
            if (z->zconn && 0 == strcmp (z->zconn->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ZoneBC_t")) {
            if (z->zboco && 0 == strcmp (z->zboco->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "DiscreteData_t")) {
            for (n = 0; n < z->ndiscrete; n++) {
                if (0 == strcmp (z->discrete[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "FlowEquationSet_t")) {
            if (z->equations && 0 == strcmp (z->equations->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ConvergenceHistory_t")) {
            if (z->converg && 0 == strcmp (z->converg->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "IntegralData_t")) {
            for (n = 0; n < z->nintegrals; n++) {
                if (0 == strcmp (z->integral[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ReferenceState_t")) {
            if (z->state && 0 == strcmp (z->state->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < z->nuser_data; n++) {
                if (0 == strcmp (z->user_data[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "RotatingCoordinates_t")) {
            if (z->rotating && 0 == strcmp (z->rotating->name, name))
                return 1;
        }
        else
            return -1;
    }

    /* GridCoordinates_t */

    else if (0 == strcmp (posit_label, "GridCoordinates_t")) {
        cgns_zcoor *z = (cgns_zcoor *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < z->ncoords; n++) {
                if (0 == strcmp (z->coord[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < z->nuser_data; n++) {
                if (0 == strcmp (z->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* FlowSolution_t */

    else if (0 == strcmp (posit_label, "FlowSolution_t")) {
        cgns_sol *s = (cgns_sol *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < s->nfields; n++) {
                if (0 == strcmp (s->field[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < s->nuser_data; n++) {
                if (0 == strcmp (s->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ZoneGridConnectivity_t */

    else if (0 == strcmp (posit_label, "ZoneGridConnectivity_t")) {
        cgns_zconn *z = (cgns_zconn *)posit;
        if (0 == strcmp (label, "OversetHoles_t")) {
            for (n = 0; n < z->nholes; n++) {
                if (0 == strcmp (z->hole[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "GridConnectivity_t")) {
            for (n = 0; n < z->nconns; n++) {
                if (0 == strcmp (z->conn[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "GridConnectivity1to1_t")) {
            for (n = 0; n < z->n1to1; n++) {
                if (0 == strcmp (z->one21[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < z->nuser_data; n++) {
                if (0 == strcmp (z->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* OversetHoles_t */

    else if (0 == strcmp (posit_label, "OversetHoles_t")) {
        cgns_hole *h = (cgns_hole *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < h->nuser_data; n++) {
                if (0 == strcmp (h->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GridConnectivity_t */

    else if (0 == strcmp (posit_label, "GridConnectivity_t")) {
        cgns_conn *c = (cgns_conn *)posit;
        if (0 == strcmp (label, "GridConnectivityProperty_t")) {
            if (c->cprop && 0 == strcmp (c->cprop->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < c->nuser_data; n++) {
                if (0 == strcmp (c->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GridConnectivity1to1_t */

    else if (0 == strcmp (posit_label, "GridConnectivity1to1_t")) {
        cgns_1to1 *c = (cgns_1to1 *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < c->nuser_data; n++) {
                if (0 == strcmp (c->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ZoneBC_t */

    else if (0 == strcmp (posit_label, "ZoneBC_t")) {
        cgns_zboco *z = (cgns_zboco *)posit;
        if (0 == strcmp (label, "BC_t")) {
            for (n = 0; n < z->nbocos; n++) {
                if (0 == strcmp (z->boco[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "ReferenceState_t")) {
            if (z->state && 0 == strcmp (z->state->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < z->nuser_data; n++) {
                if (0 == strcmp (z->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* BC_t */

    else if (0 == strcmp (posit_label, "BC_t")) {
        cgns_boco *b = (cgns_boco *)posit;
        if (0 == strcmp (label, "BCDataSet_t")) {
            for (n = 0; n < b->ndataset; n++) {
                if (0 == strcmp (b->dataset[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "BCProperty_t")) {
            if (b->bprop && 0 == strcmp (b->bprop->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ReferenceState_t")) {
            if (b->state && 0 == strcmp (b->state->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < b->nuser_data; n++) {
                if (0 == strcmp (b->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* BCDataSet_t */

    else if (0 == strcmp (posit_label, "BCDataSet_t")) {
        cgns_dataset *d = (cgns_dataset *)posit;
        if (0 == strcmp (label, "BCData_t")) {
            if ((d->dirichlet && 0 == strcmp (d->dirichlet->name, name)) ||
                (d->neumann && 0 == strcmp (d->neumann->name, name)))
                return 1;
        }
        else if (0 == strcmp (label, "ReferenceState_t")) {
            if (d->state && 0 == strcmp (d->state->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < d->nuser_data; n++) {
                if (0 == strcmp (d->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* BCData_t */

    else if (0 == strcmp (posit_label, "BCData_t")) {
        cgns_bcdata *b = (cgns_bcdata *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < b->narrays; n++) {
                if (0 == strcmp (b->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < b->nuser_data; n++) {
                if (0 == strcmp (b->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* DiscreteData_t */

    else if (0 == strcmp (posit_label, "DiscreteData_t")) {
        cgns_discrete *d = (cgns_discrete *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < d->narrays; n++) {
                if (0 == strcmp (d->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < d->nuser_data; n++) {
                if (0 == strcmp (d->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* FlowEquationSet_t */

    else if (0 == strcmp (posit_label, "FlowEquationSet_t")) {
        cgns_equations *e = (cgns_equations *)posit;
        if (0 == strcmp (label, "GoverningEquations_t")) {
            if (e->governing && 0 == strcmp (e->governing->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "GasModel_t")) {
            if (e->gas && 0 == strcmp (e->gas->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ViscosityModel_t")) {
            if (e->visc && 0 == strcmp (e->visc->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ThermalConductivityModel_t")) {
            if (e->conduct && 0 == strcmp (e->conduct->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "TurbulenceModel_t")) {
            if (e->turbulence && 0 == strcmp (e->turbulence->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "TurbulenceClosure_t")) {
            if (e->closure && 0 == strcmp (e->closure->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ThermalRelaxationModel_t")) {
            if (e->relaxation && 0 == strcmp (e->relaxation->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "ChemicalKineticsModel_t")) {
            if (e->chemkin && 0 == strcmp (e->chemkin->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < e->nuser_data; n++) {
                if (0 == strcmp (e->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GoverningEquations_t */

    else if (0 == strcmp (posit_label, "GoverningEquations_t")) {
        cgns_governing *g = (cgns_governing *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < g->nuser_data; n++) {
                if (0 == strcmp (g->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GasModel_t */
    /* ViscosityModel_t */
    /* ThermalConductivityModel_t */
    /* TurbulenceModel_t */
    /* TurbulenceClosure_t */
    /* ThermalRelaxationModel_t */
    /* ChemicalKineticsModel_t */

    else if (0 == strcmp (posit_label, "GasModel_t") ||
             0 == strcmp (posit_label, "ViscosityModel_t") ||
             0 == strcmp (posit_label, "ThermalConductivityModel_t") ||
             0 == strcmp (posit_label, "TurbulenceModel_t") ||
             0 == strcmp (posit_label, "TurbulenceClosure_t") ||
             0 == strcmp (posit_label, "ThermalRelaxationModel_t") ||
             0 == strcmp (posit_label, "ChemicalKineticsModel_t")) {
        cgns_model *m = (cgns_model *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < m->narrays; n++) {
                if (0 == strcmp (m->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < m->nuser_data; n++) {
                if (0 == strcmp (m->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ConvergenceHistory_t */

    else if (0 == strcmp (posit_label, "ConvergenceHistory_t")) {
        cgns_converg *c = (cgns_converg *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < c->narrays; n++) {
                if (0 == strcmp (c->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < c->nuser_data; n++) {
                if (0 == strcmp (c->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* IntegralData_t */

    else if (0 == strcmp (posit_label, "IntegralData_t")) {
        cgns_integral *i = (cgns_integral *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < i->narrays; n++) {
                if (0 == strcmp (i->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < i->nuser_data; n++) {
                if (0 == strcmp (i->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ReferenceState_t */

    else if (0 == strcmp (posit_label, "ReferenceState_t")) {
        cgns_state *s = (cgns_state *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < s->narrays; n++) {
                if (0 == strcmp (s->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < s->nuser_data; n++) {
                if (0 == strcmp (s->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* Elements_t */

    else if (0 == strcmp (posit_label, "Elements_t")) {
        cgns_section *s = (cgns_section *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < s->nuser_data; n++) {
                if (0 == strcmp (s->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* Family_t */

    else if (0 == strcmp (posit_label, "Family_t")) {
        cgns_family *f = (cgns_family *)posit;
        if (0 == strcmp (label, "GeometryReference_t")) {
            for (n = 0; n < f->ngeos; n++) {
                if (0 == strcmp (f->geo[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < f->nuser_data; n++) {
                if (0 == strcmp (f->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GeometryReference_t */

    else if (0 == strcmp (posit_label, "GeometryReference_t")) {
        cgns_geo *g = (cgns_geo *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < g->nuser_data; n++) {
                if (0 == strcmp (g->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* RigidGridMotion_t */

    else if (0 == strcmp (posit_label, "RigidGridMotion_t")) {
        cgns_rmotion *m = (cgns_rmotion *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < m->narrays; n++) {
                if (0 == strcmp (m->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < m->nuser_data; n++) {
                if (0 == strcmp (m->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ArbitraryGridMotion_t */

    else if (0 == strcmp (posit_label, "ArbitraryGridMotion_t")) {
        cgns_amotion *m = (cgns_amotion *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < m->narrays; n++) {
                if (0 == strcmp (m->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < m->nuser_data; n++) {
                if (0 == strcmp (m->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* BaseIterativeData_t */

    else if (0 == strcmp (posit_label, "BaseIterativeData_t")) {
        cgns_biter *b = (cgns_biter *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < b->narrays; n++) {
                if (0 == strcmp (b->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < b->nuser_data; n++) {
                if (0 == strcmp (b->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* ZoneIterativeData_t */

    else if (0 == strcmp (posit_label, "ZoneIterativeData_t")) {
        cgns_ziter *z = (cgns_ziter *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < z->narrays; n++) {
                if (0 == strcmp (z->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < z->nuser_data; n++) {
                if (0 == strcmp (z->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* UserDefinedData_t */

    /* add recursive UserDefinedData_t */

    /* Gravity_t */

    else if (0 == strcmp (posit_label, "Gravity_t")) {
        cgns_gravity *g = (cgns_gravity *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < g->narrays; n++) {
                if (0 == strcmp (g->vector[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < g->nuser_data; n++) {
                if (0 == strcmp (g->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* Axisymmetry_t */

    else if (0 == strcmp (posit_label, "Axisymmetry_t")) {
        cgns_axisym *a = (cgns_axisym *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < a->narrays; n++) {
                if (0 == strcmp (a->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < a->nuser_data; n++) {
                if (0 == strcmp (a->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* RotatingCoordinates_t */

    else if (0 == strcmp (posit_label, "RotatingCoordinates_t")) {
        cgns_rotating *r = (cgns_rotating *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < r->narrays; n++) {
                if (0 == strcmp (r->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < r->nuser_data; n++) {
                if (0 == strcmp (r->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* BCProperty_t */

    else if (0 == strcmp (posit_label, "BCProperty_t")) {
        cgns_bprop *b = (cgns_bprop *)posit;
        if (0 == strcmp (label, "WallFunction_t")) {
            if (b->bcwall && 0 == strcmp (b->bcwall->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "Area_t")) {
            if (b->bcarea && 0 == strcmp (b->bcarea->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < b->nuser_data; n++) {
                if (0 == strcmp (b->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* WallFunction_t */

    else if (0 == strcmp (posit_label, "WallFunction_t")) {
        cgns_bcwall *w = (cgns_bcwall *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < w->nuser_data; n++) {
                if (0 == strcmp (w->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* Area_t */

    else if (0 == strcmp (posit_label, "Area_t")) {
        cgns_bcarea *a = (cgns_bcarea *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < a->narrays; n++) {
                if (0 == strcmp (a->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < a->nuser_data; n++) {
                if (0 == strcmp (a->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* GridConnectivityProperty_t */

    else if (0 == strcmp (posit_label, "GridConnectivityProperty_t")) {
        cgns_cprop *c = (cgns_cprop *)posit;
        if (0 == strcmp (label, "Periodic_t")) {
            if (c->cperio && 0 == strcmp (c->cperio->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "AverageInterface_t")) {
            if (c->caverage && 0 == strcmp (c->caverage->name, name))
                return 1;
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < c->nuser_data; n++) {
                if (0 == strcmp (c->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* Periodic_t */

    else if (0 == strcmp (posit_label, "Periodic_t")) {
        cgns_cperio *p = (cgns_cperio *)posit;
        if (0 == strcmp (label, "DataArray_t")) {
            for (n = 0; n < p->narrays; n++) {
                if (0 == strcmp (p->array[n].name, name))
                    return n + 1;
            }
        }
        else if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < p->nuser_data; n++) {
                if (0 == strcmp (p->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* AverageInterface_t */

    else if (0 == strcmp (posit_label, "AverageInterface_t")) {
        cgns_caverage *a = (cgns_caverage *)posit;
        if (0 == strcmp (label, "UserDefinedData_t")) {
            for (n = 0; n < a->nuser_data; n++) {
                if (0 == strcmp (a->user_data[n].name, name))
                    return n + 1;
            }
        }
        else
            return -1;
    }

    /* invalid */

    else
        return -1;

    return 0;
}

/*-----------------------------------------------------------------------*/

static int goto_node ()
{
    char *p, name[33], label[33];
    int n, ierr, index;
    double pid, id;

    for (n = 0; n < goDepth; n++) {
        strcpy (name, goLabel[n]);
        p = name + strlen(name) - 2;
        if (goIndex[n] < 1 || strcmp (p, "_t")) {
            if (go_depth (n) || cgi_posit_id (&pid))
                return CG_ERROR;
            ADF_Get_Node_ID (pid, name, &id, &ierr);
            if (ierr > 0) {
                if (ierr == CHILD_NOT_OF_GIVEN_PARENT) {
                    cgi_error ("goto path not found");
                    return CG_NODE_NOT_FOUND;
                }
                adf_error("ADF_Get_Node_ID", ierr);
                return CG_ERROR;
            }
            ADF_Get_Label (id, label, &ierr);
            if (ierr > 0) {
                adf_error("ADF_Get_Label", ierr);
                return CG_ERROR;
            }
            index = node_index (name, label);
            if (index < 0) {
                cgi_error ("invalid goto path");
                return CG_INCORRECT_PATH;
            }
            if (index == 0) {
                cgi_error ("goto path not found");
                return CG_NODE_NOT_FOUND;
            }
            strcpy (goLabel[n], label);
            goIndex[n] = index;
        }
    }
    return go_depth (goDepth);
}

/*-----------------------------------------------------------------------*/

static int get_cg_error (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *funcname)
#else
    interp, funcname)
Tcl_Interp *interp;
char *funcname;
#endif
{
    Tcl_AppendResult (interp, cg_get_error(), NULL);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int sizeof_datatype (
#ifdef PROTOTYPE
    DataType_t dtype)
#else
    dtype)
DataType_t dtype;
#endif
{
    if (dtype == Character)  return sizeof(char);
    if (dtype == Integer)    return sizeof(int);
    if (dtype == RealSingle) return sizeof(float);
    if (dtype == RealDouble) return sizeof(double);
    return 0;
}

/*-----------------------------------------------------------------------*/

static int get_data_size (
#ifdef PROTOTYPE
    Tcl_Interp *interp, DataType_t type, int ndim, int *dims,
    int *count, int *bytes)
#else
    interp, type, ndim, dims, count, bytes)
Tcl_Interp *interp;
DataType_t type;
int ndim, *dims, *count, *bytes;
#endif
{
    int n;

    *count = *bytes = 0;
    if (ndim) {
        *count = 1;
        for (n = 0; n < ndim; n++)
            *count *= dims[n];
    }
    if (*count < 1) {
        Tcl_AppendResult (interp, "dimension is not 1 or greater", NULL);
        return 1;
    }
    *bytes = sizeof_datatype (type);
    if (*bytes == 0) {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int extract_data (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *list, DataType_t type, int *count, void **data)
#else
    interp, list, type, count, data)
Tcl_Interp *interp;
char *list;
DataType_t type;
int *count;
void **data;
#endif
{
    int n, cnt, bytes;
    char **args;

    bytes = sizeof_datatype (type);
    if (bytes == 0) {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    if (TCL_OK != Tcl_SplitList (interp, list, &cnt, &args))
        return 1;
    if (cnt < 1) {
        Tcl_AppendResult (interp, "data length less than 1", NULL);
        return 1;
    }
    if ((*data = malloc (cnt * bytes)) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for data array", NULL);
        return 1;
    }
    if (type == Character) {
        char *c = (char *)*data;
        for (n = 0; n < cnt; n++)
            c[n] = (char) atoi (args[n]);
    }
    else if (type == Integer) {
        int *i = (int *)*data;
        for (n = 0; n < cnt; n++)
            i[n] = (int) atoi (args[n]);
    }
    else if (type == RealSingle) {
        float *f = (float *)*data;
        for (n = 0; n < cnt; n++)
            f[n] = (float) atof (args[n]);
    }
    else {
        double *d = (double *)*data;
        for (n = 0; n < cnt; n++)
            d[n] = (double) atof (args[n]);
    }
    Tcl_Free ((char *)args);
    *count = cnt;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int construct_data (
#ifdef PROTOTYPE
    Tcl_Interp *interp, DataType_t type, int count, void *data)
#else
    interp, type, count, data)
Tcl_Interp *interp;
DataType_t type;
int count;
void *data;
#endif
{
    int n;

    if (type == Character) {
        char *c = (char *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%c", c[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == Integer) {
        int *i = (int *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%d", i[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == RealSingle) {
        float *f = (float *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%g", f[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else if (type == RealDouble) {
        double *d = (double *)data;
        for (n = 0; n < count; n++) {
            sprintf (buff, "%g", d[n]);
            Tcl_AppendElement (interp, buff);
        }
    }
    else {
        Tcl_AppendResult (interp, "invalid data type", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int check_name (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *desc, char *name)
#else
    interp, desc, name)
Tcl_Interp *interp;
char *desc, *name;
#endif
{
    int len = strlen (name);

    if (len < 1 || len > 32) {
        Tcl_AppendResult (interp, "invalid length for ", desc, "name", NULL);
        return 1;
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int get_type (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *desc, char *name, int cnt,
    const char **list, int *type)
#else
    interp, name, cnt, list, desc, type)
Tcl_Interp *interp;
char *desc, *name, **list;
int cnt, *type;
#endif
{
    int n;

    for (n = 0; n < cnt; n++) {
        if (0 == strcmp (name, list[n])) {
            *type = n;
            return 0;
        }
    }
    Tcl_AppendResult (interp, desc, " type \"", name, "\" is invalid", NULL);
    return 1;
}

/*-----------------------------------------------------------------------*/

static int get_data_type (
#ifdef PROTOTYPE
    Tcl_Interp *interp, char *name, DataType_t *datatype)
#else
    interp, name, datatype)
Tcl_Interp *interp;
char *name;
DataType_t *datatype;
#endif
{
    int type;

    if (get_type (interp, "data", name,
            NofValidDataTypes, DataTypeName, &type)) return 1;
    *datatype = (DataType_t)type;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int count_elements (
#ifdef PROTOTYPE
    Tcl_Interp *interp, ElementType_t type, int cnt, int *elements,
    int nelems)
#else
    interp, type, cnt, elements, nelems)
Tcl_Interp *interp;
ElementType_t type;
int cnt, *elements, nelems;
#endif
{
    int npe, ne, n;

    if (type <= ElementTypeUserDefined || type > MIXED) {
        Tcl_AppendResult (interp, "can't handle element type ",
            cg_element_type_name(type), NULL);
        return 1;
    }
    if (type < MIXED) {
        cg_npe (type, &npe);
        if (nelems * npe > cnt) {
            sprintf (buff, "needs %d values", nelems * npe);
            Tcl_AppendResult (interp,
                "insufficient element data - ", buff, NULL);
            return 1;
        }
        return 0;
    }
    for (n = 0, ne = 0; ne < nelems; ne++) {
        type = (ElementType_t)elements[n++];
        if (type <= ElementTypeUserDefined || type >= MIXED) {
            Tcl_AppendResult (interp, "can't handle element type ",
                cg_element_type_name(type), " as MIXED element", NULL);
            return 1;
        }
        cg_npe (type, &npe);
        n += npe;
        if (n > cnt) {
            Tcl_AppendResult (interp,
                "insufficient mixed element data", NULL);
            return 1;
        }
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_physical_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int *phys_dim)
#else
    cgfile, cgbase, phys_dim)
int cgfile, cgbase, *phys_dim;
#endif
{
    cgns_file *file;
    cgns_base *base;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    base = cgi_get_base (file, cgbase);
    if (base == NULL) return 1;
    *phys_dim = base->phys_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_cell_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int *cell_dim)
#else
    cgfile, cgbase, cell_dim)
int cgfile, cgbase, *cell_dim;
#endif
{
    cgns_file *file;
    cgns_base *base;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    base = cgi_get_base (file, cgbase);
    if (base == NULL) return 1;
    *cell_dim = base->cell_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_index_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int *index_dim)
#else
    cgfile, cgbase, cgzone, index_dim)
int cgfile, cgbase, cgzone, *index_dim;
#endif
{
    cgns_file *file;
    cgns_zone *zone;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    *index_dim = zone->index_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_coord_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int *index_dim, int *dims)
#else
    cgfile, cgbase, cgzone, index_dim, dims)
int cgfile, cgbase, cgzone, *index_dim, *dims;
#endif
{
    int n;
    cgns_file *file;
    cgns_zone *zone;
    cgns_zcoor *zcoor;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    zcoor = cgi_get_zcoorGC (file, cgbase, cgzone);
    if (zcoor == NULL) return 1;

    *index_dim = zone->index_dim;
    return cgi_datasize(zone->index_dim, zone->nijk, Vertex,
               zcoor->rind_planes, dims);
}

/*-----------------------------------------------------------------------*/

static int cg_solution_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int cgsoln, int *index_dim, int *dims)
#else
    cgfile, cgbase, cgzone, cgsoln, index_dim, dims)
int cgfile, cgbase, cgzone, cgsoln, *index_dim, *dims;
#endif
{
    int n;
    cgns_file *file;
    cgns_zone *zone;
    cgns_sol *sol;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    zone = cgi_get_zone (file, cgbase, cgzone);
    if (zone == NULL) return 1;
    sol = cgi_get_sol (file, cgbase, cgzone, cgsoln);
    if (sol == NULL) return 1;

    *index_dim = zone->index_dim;
    return cgi_datasize(zone->index_dim, zone->nijk, sol->location,
               sol->rind_planes, dims);
}

/*-----------------------------------------------------------------------*/

static int cg_hole_size (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int cghole, int *size)
#else
    cgfile, cgbase, cgzone, cghole, size)
int cgfile, cgbase, cgzone, cghole, *size;
DataType_t *dtype;
#endif
{
    int n;
    cgns_file *file;
    cgns_hole *hole;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    hole = cgi_get_hole (file, cgbase, cgzone, cghole);
    if (hole == NULL) return 1;

    *size = 0;
    for (n = 0; n < hole->nptsets; n++)
        *size += hole->ptset[n].npts;
    *size *= file->base[cgbase-1].zone[cgzone-1].index_dim;
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_conn_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int cgconn,
    int *cnt, int *size, int *dsize, DataType_t *dtype)
#else
    cgfile, cgbase, cgzone, cgconn, cnt, size, dsize, dtype)
int cgfile, cgbase, cgzone, cgconn, *pcnt, *size, *dsize;
DataType_t *dtype;
#endif
{
    cgns_file *file;
    cgns_conn *conn;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    conn = cgi_get_conn (file, cgbase, cgzone, cgconn);
    if (conn == NULL) return 1;

    *cnt = conn->ptset.npts * file->base[cgbase-1].zone[cgzone-1].index_dim;
    *size = conn->ptset.size_of_patch;
    if (conn->dptset.npts == 0) {
        *dsize = 0;
        *dtype = DataTypeNull;
    }
    else {
        int n, idim = 0;
        for (n = 0; n < file->base[cgbase-1].nzones; n++) {
            if (strcmp (file->base[cgbase-1].zone[n].name, conn->donor) == 0) {
                idim = file->base[cgbase-1].zone[n].type == Structured ?
                       file->base[cgbase-1].cell_dim : 1;
                break;
            }
        }
        if (idim == 0) {
            cgi_error ("cg_conn_read:donor zone %s does not exist",
                conn->donor);
            return 1;
        }
        *dtype = cgi_datatype(conn->dptset.data_type);
    }
    return 0;
}

/*-----------------------------------------------------------------------*/

static int cg_boco_dim (
#ifdef PROTOTYPE
    int cgfile, int cgbase, int cgzone, int cgboco,
    int *pcnt, int *psize, int *ncnt, DataType_t *ntype)
#else
    cgfile, cgbase, cgzone, cgboco, pcnt, psize, ncnt, ntype)
int cgfile, cgbase, cgzone, cgboco, *pcnt, *psize, *ncnt;
DataType_t *ntype;
#endif
{
    cgns_file *file;
    cgns_boco *boco;

    file = cgi_get_file (cgfile);
    if (file == NULL) return 1;
    boco = cgi_get_boco (file, cgbase, cgzone, cgboco);
    if (boco == NULL) return 1;

    *pcnt = boco->ptset->npts * file->base[cgbase-1].zone[cgzone-1].index_dim;
    *psize = boco->ptset->size_of_patch;
    if (boco->normal == NULL) {
        *ncnt = 0;
        *ntype = DataTypeNull;
    }
    else {
        *ncnt = boco->ptset->size_of_patch * file->base[cgbase-1].phys_dim;
        *ntype = cgi_datatype(boco->normal->data_type);
    }
    return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      LIBRARY FUNCTIONS						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_library_version (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    sprintf (buff, "%d", CGNSLibVersion);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_get_names (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, cnt;
    const char **names;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " type_t", NULL);
        return TCL_ERROR;
    }
    if (0 == strcmp (argv[1], "MassUnits_t")) {
        cnt = NofValidMassUnits;
        names = MassUnitsName;
    }
    else if (0 == strcmp (argv[1], "LengthUnits_t")) {
        cnt = NofValidLengthUnits;
        names = LengthUnitsName;
    }
    else if (0 == strcmp (argv[1], "TimeUnits_t")) {
        cnt = NofValidTimeUnits;
        names = TimeUnitsName;
    }
    else if (0 == strcmp (argv[1], "TemperatureUnits_t")) {
        cnt = NofValidTemperatureUnits;
        names = TemperatureUnitsName;
    }
    else if (0 == strcmp (argv[1], "AngleUnits_t")) {
        cnt = NofValidAngleUnits;
        names = AngleUnitsName;
    }
#if CGNS_VERSION >= 2400
    else if (0 == strcmp (argv[1], "ElectricCurrentUnits_t")) {
        cnt = NofValidElectricCurrentUnits;
        names = ElectricCurrentUnitsName;
    }
    else if (0 == strcmp (argv[1], "SubstanceAmountUnits_t")) {
        cnt = NofValidSubstanceAmountUnits;
        names = SubstanceAmountUnitsName;
    }
    else if (0 == strcmp (argv[1], "LuminousIntensityUnits_t")) {
        cnt = NofValidLuminousIntensityUnits;
        names = LuminousIntensityUnitsName;
    }
#endif
    else if (0 == strcmp (argv[1], "DataClass_t")) {
        cnt = NofValidDataClass;
        names = DataClassName;
    }
    else if (0 == strcmp (argv[1], "GridLocation_t")) {
        cnt = NofValidGridLocation;
        names = GridLocationName;
    }
    else if (0 == strcmp (argv[1], "BCDataType_t")) {
        cnt = NofValidBCDataTypes;
        names = BCDataTypeName;
    }
    else if (0 == strcmp (argv[1], "GridConnectivityType_t")) {
        cnt = NofValidGridConnectivityTypes;
        names = GridConnectivityTypeName;
    }
    else if (0 == strcmp (argv[1], "PointSetType_t")) {
        cnt = NofValidPointSetTypes;
        names = PointSetTypeName;
    }
    else if (0 == strcmp (argv[1], "GoverningEquationsType_t")) {
        cnt = NofValidGoverningEquationsTypes;
        names = GoverningEquationsTypeName;
    }
    else if (0 == strcmp (argv[1], "ModelType_t")) {
        cnt = NofValidModelTypes;
        names = ModelTypeName;
    }
    else if (0 == strcmp (argv[1], "BCType_t")) {
        cnt = NofValidBCTypes;
        names = BCTypeName;
    }
    else if (0 == strcmp (argv[1], "DataType_t")) {
        cnt = NofValidDataTypes;
        names = DataTypeName;
    }
    else if (0 == strcmp (argv[1], "ElementType_t")) {
        cnt = NofValidElementTypes;
        names = ElementTypeName;
    }
    else if (0 == strcmp (argv[1], "ZoneType_t")) {
        cnt = NofValidZoneTypes;
        names = ZoneTypeName;
    }
    else if (0 == strcmp (argv[1], "RigidGridMotionType_t")) {
        cnt = NofValidRigidGridMotionTypes;
        names = RigidGridMotionTypeName;
    }
    else if (0 == strcmp (argv[1], "ArbitraryGridMotionType_t")) {
        cnt = NofValidArbitraryGridMotionTypes;
        names = ArbitraryGridMotionTypeName;
    }
    else if (0 == strcmp (argv[1], "SimulationType_t")) {
        cnt = NofValidSimulationTypes;
        names = SimulationTypeName;
    }
    else if (0 == strcmp (argv[1], "WallFunctionType_t")) {
        cnt = NofValidWallFunctionTypes;
        names = WallFunctionTypeName;
    }
    else if (0 == strcmp (argv[1], "AreaType_t")) {
        cnt = NofValidAreaTypes;
        names = AreaTypeName;
    }
    else if (0 == strcmp (argv[1], "AverageInterfaceType_t")) {
        cnt = NofValidAverageInterfaceTypes;
        names = AverageInterfaceTypeName;
    }
    else {
        Tcl_AppendResult (interp, "type \"", argv[1], "\" not known", NULL);
        return TCL_ERROR;
    }

    for (n = 0; n < cnt; n++)
        Tcl_AppendElement (interp, names[n]);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_open (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int mode, cgfile;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filename mode", NULL);
        return TCL_ERROR;
    }
    if (argv[2][0] == 'r' || argv[2][0] == 'R')
        mode = MODE_READ;
    else if (argv[2][0] == 'w' || argv[2][0] == 'W')
        mode = MODE_WRITE;
    else if (argv[2][0] == 'm' || argv[2][0] == 'M')
        mode = MODE_MODIFY;
    else {
        Tcl_AppendResult (interp,
            "invalid mode: should be r[ead], w[rite] or m[odify]", NULL);
        return TCL_ERROR;
    }

    if (cg_open (argv[1], mode, &cgfile))
        return get_cg_error (interp, "cg_open");

    sprintf (buff, "%d", cgfile);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_version (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    float version;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_version (atoi(argv[1]), &version))
        return get_cg_error (interp, "cg_version");

    sprintf (buff, "%d", (int)(1000.0 * version + 0.5));
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_close (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_close (atoi(argv[1])))
        return get_cg_error (interp, "cg_close");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write CGNSBase_t Nodes					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nbases (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nbases;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " filenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nbases (atoi(argv[1]), &nbases))
        return get_cg_error (interp, "cg_nbases");

    sprintf (buff, "%d", nbases);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    int cell, phy;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_base_read (atoi(argv[1]), atoi(argv[2]), name, &cell, &phy))
        return get_cg_error (interp, "cg_base_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", cell);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", phy);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_base_id (atoi(argv[1]), atoi(argv[2]), &id))
        return get_cg_error (interp, "cg_base_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_base_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgbase;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basename celldim phydim", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "base", argv[2])) return TCL_ERROR;

    if (cg_base_write (atoi(argv[1]), argv[2],
            atoi(argv[3]), atoi(argv[4]), &cgbase))
        return get_cg_error (interp, "cg_base_write");

    sprintf (buff, "%d", cgbase);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Zone_t Nodes    					 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nzones (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nzones;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nzones (atoi(argv[1]), atoi(argv[2]), &nzones))
        return get_cg_error (interp, "cg_nzones");

    sprintf (buff, "%d", nzones);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char *p, name[33];
    int n, idim, sizes[9];
    int cgfile, cgbase, cgzone;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_zone_read (cgfile, cgbase, cgzone, name, sizes))
        return get_cg_error (interp, "cg_zone_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", sizes[0]);
    for (p = buff, n = 1; n < 3 * idim; n++) {
        p += strlen (p);
        sprintf (p, " %d", sizes[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_type (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    ZoneType_t type;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_zone_type (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &type))
        return get_cg_error (interp, "cg_zone_type");

    Tcl_AppendResult (interp, cg_zone_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_zone_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &id))
        return get_cg_error (interp, "cg_zone_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_zone_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, cgfile, cgbase, cgzone;
    int cdim, len, *sizes;
    ZoneType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonename sizes zonetype", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi (argv[1]);
    cgbase = atoi (argv[2]);
    if (check_name (interp, "zone", argv[3])) return TCL_ERROR;
    if (0 == strcmp (argv[5], "Structured"))
        type = Structured;
    else if (0 == strcmp (argv[5], "Unstructured"))
        type = Unstructured;
    else {
        Tcl_AppendResult (interp, "invalid zone type", NULL);
        return TCL_ERROR;
    }

    if (cg_cell_dim (cgfile, cgbase, &cdim))
        return get_cg_error (interp, "cg_zone_write");
    if (extract_data (interp, argv[4], Integer, &len, (void **)&sizes))
        return TCL_ERROR;
    if ((type == Structured && len != 3 * cdim) ||
        (type == Unstructured && len != 3)) {
        free (sizes);
        sprintf (buff, "must be %d", type == Structured ? 3 * cdim : 3);
        Tcl_AppendResult (interp,
            "size array length invalid - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_zone_write (cgfile, cgbase, argv[3], sizes, type, &cgzone);
    free (sizes);
    if (ierr) return get_cg_error (interp, "cg_zone_write");

    sprintf (buff, "%d", cgzone);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Family_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nfamilies (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nfam;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nfamilies (atoi(argv[1]), atoi(argv[2]), &nfam))
        return get_cg_error (interp, "cg_nfamilies");

    sprintf (buff, "%d", nfam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_family_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    int nboco, ngeo;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum", NULL);
        return TCL_ERROR;
    }

    if (cg_family_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            name, &nboco, &ngeo))
        return get_cg_error (interp, "cg_family_read");

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", nboco);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", ngeo);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_family_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfam;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familyname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "family", argv[3])) return TCL_ERROR;

    if (cg_family_write (atoi(argv[1]), atoi(argv[2]), argv[3], &cgfam))
        return get_cg_error (interp, "cg_family_write");

    sprintf (buff, "%d", cgfam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyName_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_famname_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_famname_read (name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_famname_read");
    }

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_famname_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " familyname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "family", argv[1])) return TCL_ERROR;

    if (cg_famname_write (argv[1]))
        return get_cg_error (interp, "cg_famname_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FamilyBC_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_fambc_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    BCType_t bctype;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum bcnum", NULL);
        return TCL_ERROR;
    }

    if (cg_fambc_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &bctype))
        return get_cg_error (interp, "cg_fambc_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_bc_type_name(bctype));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_fambc_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type, cgbc;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum bcname bctype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "fambc", argv[4]) ||
        get_type (interp, "BC", argv[5], NofValidBCTypes, BCTypeName, &type))
        return TCL_ERROR;

    if (cg_fambc_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], (BCType_t)type, &cgbc))
        return get_cg_error (interp, "cg_fambc_write");

    sprintf (buff, "%d", cgbc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryReference_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_geo_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int npart;
    char name[33], *filename, cadname[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum", NULL);
        return TCL_ERROR;
    }

    if (cg_geo_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &filename, cadname, &npart))
        return get_cg_error (interp, "cg_geo_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, filename);
    Tcl_AppendElement (interp, cadname);
    sprintf (buff, "%d", npart);
    Tcl_AppendElement (interp, buff);
    free (filename);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_geo_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cggeo;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geoname filename cadsystem", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "geo", argv[4]) ||
        check_name (interp, "cadsystem", argv[6])) return TCL_ERROR;
    if (strlen (argv[5]) < 1) {
        Tcl_AppendResult (interp, "filename not given", NULL);
        return TCL_ERROR;
    }

    if (cg_geo_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], argv[5], argv[6], &cggeo))
        return get_cg_error (interp, "cg_geo_write");

    sprintf (buff, "%d", cggeo);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GeometryEntity_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_part_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum partnum", NULL);
        return TCL_ERROR;
    }

    if (cg_part_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), name))
        return get_cg_error (interp, "cg_part_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_part_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgpart;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum familynum geonum partname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "part", argv[5])) return TCL_ERROR;

    if (cg_part_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), argv[5], &cgpart))
        return get_cg_error (interp, "cg_part_write");

    sprintf (buff, "%d", cgpart);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ngrids (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ngrids;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ngrids (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ngrids))
        return get_cg_error (interp, "cg_ngrids");

    sprintf (buff, "%d", ngrids);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_grid_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum gridnum", NULL);
        return TCL_ERROR;
    }

    if (cg_grid_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name))
        return get_cg_error (interp, "cg_grid_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_grid_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cggrid;
    char gridname[33];

    Tcl_ResetResult (interp);
    if (argc < 4 || argc > 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum [gridname]", NULL);
        return TCL_ERROR;
    }
    if (argc == 5) {
        if (check_name (interp, "grid", argv[4])) return TCL_ERROR;
        strcpy (gridname, argv[4]);
    }
    else
        strcpy (gridname, "GridCoordinates");

    if (cg_grid_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            gridname, &cggrid))
        return get_cg_error (interp, "cg_grid_write");

    sprintf (buff, "%d", cggrid);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridCoordinates_t/DataArray_t Nodes               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ncoords (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ncoor;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ncoords (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ncoor))
        return get_cg_error (interp, "cg_ncoords");

    sprintf (buff, "%d", ncoor);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordnum", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &type, name))
        return get_cg_error (interp, "cg_coord_info");

    Tcl_AppendElement (interp, cg_data_type_name(type));
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone;
    int n, index_dim, dims[3];
    int cnt, rmin[3], rmax[3];
    DataType_t datatype;
    void *coords;

    Tcl_ResetResult (interp);
    if (argc != 6 && argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordname datatype [rmin rmax]", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "coord", argv[4]) ||
        get_data_type (interp, argv[5], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_coord_dim (cgfile, cgbase, cgzone, &index_dim, dims))
        return get_cg_error (interp, "cg_coord_read");

    cnt = 1;
    if (argc == 8) {
        if (index_dim != sscanf (argv[6], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[7], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            cnt *= (rmax[n] - rmin[n] + 1);
        }
    }
    else {
        for (n = 0; n < index_dim; n++) {
            rmin[n] = 1;
            rmax[n] = dims[n];
            cnt *= rmax[n];
        }
    }

    if ((coords = malloc (cnt * sizeof_datatype(datatype))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for coordinates", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_read (cgfile, cgbase, cgzone, argv[4],
            datatype, rmin, rmax, coords)) {
        free (coords);
        return get_cg_error (interp, "cg_coord_read");
    }

    n = construct_data (interp, datatype, cnt, coords);
    free (coords);
    return n ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum coordnum", NULL);
        return TCL_ERROR;
    }

    if (cg_coord_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_coord_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_coord_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, cgfile, cgbase, cgzone, cgcoord;
    int ierr, cnt, index_dim, dims[3];
#if CGNS_VERSION >= 2400
    int rmin[3], rmax[3];
#endif
    DataType_t datatype;
    void *coords;

    Tcl_ResetResult (interp);
#if CGNS_VERSION >= 2400
    if (argc != 7 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum datatype coordname [rmin rmax] coords",
            NULL);
        return TCL_ERROR;
    }
#else
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum datatype coordname coords", NULL);
        return TCL_ERROR;
    }
#endif
    if (check_name (interp, "coord", argv[5]) ||
        get_data_type (interp, argv[4], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_coord_dim (cgfile, cgbase, cgzone, &index_dim, dims))
        return get_cg_error (interp, "cg_coord_read");

#if CGNS_VERSION >= 2400
    if (argc == 9) {
        if (index_dim != sscanf (argv[7], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[8], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            dims[n] = (rmax[n] - rmin[n] + 1);
        }
    }
#endif
    for (cnt = 1, n = 0; n < index_dim; n++)
        cnt *= dims[n];

    if (extract_data (interp, argv[argc-1], datatype, &n, &coords))
        return TCL_ERROR;
    if (n < cnt) {
        free (coords);
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient number of coordinate values - ", buff, NULL);
        return TCL_ERROR;
    }

#if CGNS_VERSION >= 2400
    if (argc == 9)
        ierr = cg_coord_partial_write (cgfile, cgbase, cgzone, datatype,
                   argv[5], rmin, rmax, coords, &cgcoord);
    else
#endif
        ierr = cg_coord_write (cgfile, cgbase, cgzone, datatype,
                   argv[5], coords, &cgcoord);
    free (coords);
    if (ierr) return get_cg_error (interp, "cg_coord_write");

    sprintf (buff, "%d", cgcoord);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Elements_t Nodes                                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nsections (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nsect;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nsections (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nsect))
        return get_cg_error (interp, "cg_nsections");

    sprintf (buff, "%d", nsect);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_section_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    ElementType_t type;
    int start, end, nbndry, parent;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum", NULL);
        return TCL_ERROR;
    }

    if (cg_section_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &type, &start, &end, &nbndry, &parent))
        return get_cg_error (interp, "cg_section_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_element_type_name(type));
    sprintf (buff, "%d", start);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", end);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nbndry);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", parent);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_elements_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    ElementType_t type;
    int ierr, nelem, getpar = (int)data;
    int cgfile, cgbase, cgzone, cgsect;
    int start, end, nbndry, haspar, size;
    int *elements, *parent;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_read (cgfile, cgbase, cgzone, cgsect,
            name, &type, &start, &end, &nbndry, &haspar))
        return get_cg_error (interp, "cg_elements_read");

    if (getpar && !haspar) return TCL_OK;
    nelem = end - start + 1;
    if (cg_ElementDataSize (cgfile, cgbase, cgzone, cgsect, &size))
        return get_cg_error (interp, "cg_elements_read");

    elements = (int *) malloc (size * sizeof(int));
    if (elements == NULL) {
        Tcl_AppendResult (interp, "malloc failed for elements", NULL);
        return TCL_ERROR;
    }
    if (haspar) {
        parent = (int *) malloc (4 * nelem * sizeof(int));
        if (parent == NULL) {
            free (elements);
            Tcl_AppendResult (interp, "malloc failed for parent data", NULL);
            return TCL_ERROR;
        }
    }

    if (cg_elements_read (cgfile, cgbase, cgzone, cgsect, elements, parent)) {
        free (elements);
        if (haspar) free (parent);
        return get_cg_error (interp, "cg_elements_read");
    }

    if (getpar)
        ierr = construct_data (interp, Integer, 4 * nelem, parent);
    else
        ierr = construct_data (interp, Integer, size, elements);
    free (elements);
    if (haspar) free (parent);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_section_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, type, cnt, start, end, nbndry, cgsect;
    void *elements;

    Tcl_ResetResult (interp);
    if (argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectname elementtype",
            " start end nbndry elements", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "section", argv[4]) ||
        get_type (interp, "element", argv[5], NofValidElementTypes,
            ElementTypeName, &type)) return TCL_ERROR;
    start = atoi(argv[6]);
    end = atoi(argv[7]);
    nbndry = atoi(argv[8]);
    if (extract_data (interp, argv[9], Integer, &cnt, &elements) ||
        count_elements (interp, (ElementType_t)type, cnt,
            (int *)elements, end - start + 1))
        return TCL_ERROR;

    ierr = cg_section_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
               argv[4], (ElementType_t)type, start, end, nbndry,
               elements, &cgsect);
    free (elements);
    if (ierr) return get_cg_error (interp, "cg_section_write");

    sprintf (buff, "%d", cgsect);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_parent_data_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    ElementType_t type;
    int ierr, cgfile, cgbase, cgzone, cgsect;
    int start, end, nbndry, haspar, size;
    void *parent;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum parentdata", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsect = atoi(argv[4]);

    if (cg_section_read (cgfile, cgbase, cgzone, cgsect,
            name, &type, &start, &end, &nbndry, &haspar))
        return get_cg_error (interp, "tcl_cg_parent_data_write");

    if (extract_data (interp, argv[5], Integer, &size, &parent))
        return TCL_ERROR;
    if (size < 4 * (end - start + 1)) {
        free (parent);
        sprintf (buff, "needs %d values", 4 * (end - start + 1));
        Tcl_AppendResult (interp,
            "insufficient parent data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_parent_data_write (cgfile, cgbase, cgzone, cgsect, parent);
    free (parent);
    if (ierr) return get_cg_error (interp, "cg_parent_data_write");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_npe (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type, npe;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " elementtype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "element", argv[1], NofValidElementTypes,
        ElementTypeName, &type)) return TCL_ERROR;

    if (cg_npe ((ElementType_t)type, &npe))
        return get_cg_error (interp, "cg_npe");

    sprintf (buff, "%d", npe);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ElementDataSize (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int size;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum sectnum", NULL);
        return TCL_ERROR;
    }

    if (cg_ElementDataSize (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &size))
        return get_cg_error (interp, "cg_ElementDataSize");

    sprintf (buff, "%d", size);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowSolution_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nsols (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nsol;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nsols (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nsol))
        return get_cg_error (interp, "cg_nsols");

    sprintf (buff, "%d", nsol);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    GridLocation_t location;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_sol_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location))
        return get_cg_error (interp, "cg_sol_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_grid_location_name(location));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_sol_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_coord_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_sol_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgsoln, location;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solname location", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "solution", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location)) return TCL_ERROR;

    if (cg_sol_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            argv[4], (GridLocation_t)location, &cgsoln))
        return get_cg_error (interp, "cg_sol_write");

    sprintf (buff, "%d", cgsoln);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write solution DataArray_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nfields (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nfield;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum", NULL);
        return TCL_ERROR;
    }

    if (cg_nfields (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &nfield))
        return get_cg_error (interp, "cg_nfields");

    sprintf (buff, "%d", nfield);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldnum", NULL);
        return TCL_ERROR;
    }

    if (cg_field_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), &type, name))
        return get_cg_error (interp, "cg_field_info");

    Tcl_AppendElement (interp, cg_data_type_name(type));
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgsoln;
    int n, index_dim, dims[3];
    int cnt, rmin[3], rmax[3];
    DataType_t datatype;
    void *field;

    Tcl_ResetResult (interp);
    if (argc != 7 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldname datatype [rmin rmax]",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "field", argv[5]) ||
        get_data_type (interp, argv[6], &datatype)) return TCL_ERROR;
    if (datatype != RealSingle && datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsoln = atoi(argv[4]);

    if (cg_solution_dim (cgfile, cgbase, cgzone, cgsoln, &index_dim, dims))
        return get_cg_error (interp, "cg_field_read");

    cnt = 1;
    if (argc == 9) {
        if (index_dim != sscanf (argv[6], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[7], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            cnt *= (rmax[n] - rmin[n] + 1);
        }
    }
    else {
        for (n = 0; n < index_dim; n++) {
            rmin[n] = 1;
            rmax[n] = dims[n];
            cnt *= rmax[n];
        }
    }

    if ((field = malloc (cnt * sizeof_datatype(datatype))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for field data", NULL);
        return TCL_ERROR;
    }

    if (cg_field_read (cgfile, cgbase, cgzone, cgsoln, argv[5],
            datatype, rmin, rmax, field)) {
        free (field);
        return get_cg_error (interp, "cg_field_read");
    }

    n = construct_data (interp, datatype, cnt, field);
    free (field);
    return n ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum fieldnum", NULL);
        return TCL_ERROR;
    }

    if (cg_field_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), &id))
        return get_cg_error (interp, "cg_field_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_field_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, cgfile, cgbase, cgzone, cgsoln, cgfield;
    int ierr, cnt, index_dim, dims[3];
#if CGNS_VERSION >= 2400
    int rmin[3], rmax[3];
#endif
    DataType_t datatype;
    void *field;

    Tcl_ResetResult (interp);
#if CGNS_VERSION >= 2400
    if (argc != 8 && argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum datatype",
            " fieldname [rmin rmax] field_data",
            NULL);
        return TCL_ERROR;
    }
#else
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum solnum datatype fieldname field_data",
            NULL);
        return TCL_ERROR;
    }
#endif
    if (check_name (interp, "field", argv[6]) ||
        get_data_type (interp, argv[5], &datatype)) return TCL_ERROR;
    if (datatype != Integer && datatype != RealSingle &&
        datatype != RealDouble) {
        Tcl_AppendResult (interp,
            "datatype not Integer, RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgsoln = atoi(argv[4]);

    if (cg_solution_dim (cgfile, cgbase, cgzone, cgsoln, &index_dim, dims))
        return get_cg_error (interp, "cg_field_read");

#if CGNS_VERSION >= 2400
    if (argc == 10) {
        if (index_dim != sscanf (argv[8], "%d%d%d",
                &rmin[0], &rmin[1], &rmin[2]) ||
            index_dim != sscanf (argv[9], "%d%d%d",
                &rmax[0], &rmax[1], &rmax[2])) {
            Tcl_AppendResult (interp,
                "invalid number rmin and/or rmax values", NULL);
            return TCL_ERROR;
        }
        for (n = 0; n < index_dim; n++) {
            if (rmin[n] < 1 || rmax[n] > dims[n] || rmin[n] > rmax[n]) {
                Tcl_AppendResult (interp,
                    "rmin and/or rmax value are invalid", NULL);
                return TCL_ERROR;
            }
            dims[n] = (rmax[n] - rmin[n] + 1);
        }
    }
#endif
    for (cnt = 1, n = 0; n < index_dim; n++)
        cnt *= dims[n];

    if (extract_data (interp, argv[argc-1], datatype, &n, &field))
        return TCL_ERROR;
    if (n < cnt) {
        free (field);
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient number of field values - ", buff, NULL);
        return TCL_ERROR;
    }

#if CGNS_VERSION >= 2400
    if (argc == 10)
        ierr = cg_field_partial_write (cgfile, cgbase, cgzone, cgsoln,
                   datatype, argv[6], rmin, rmax, field, &cgfield);
    else
#endif
        ierr = cg_field_write (cgfile, cgbase, cgzone, cgsoln,
                   datatype, argv[6], field, &cgfield);
    free (field);
    if (ierr) return get_cg_error (interp, "cg_field_write");

    sprintf (buff, "%d", cgfield);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write OversetHoles_t Nodes  				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nholes (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nhole;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nholes (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nhole))
        return get_cg_error (interp, "cg_nholes");

    sprintf (buff, "%d", nhole);
    Tcl_AppendResult (interp, buff, NULL);

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    GridLocation_t location;
    PointSetType_t ptsettype;
    int nptsets, npnts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }

    if (cg_hole_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location, &ptsettype, &nptsets, &npnts))
        return get_cg_error (interp, "cg_hole_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_grid_location_name(location));
    Tcl_AppendElement (interp, cg_point_set_type_name(ptsettype));
    sprintf (buff, "%d", nptsets);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", npnts);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cghole;
    int ierr, size, *pts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cghole = atoi(argv[4]);

    if (cg_hole_size (cgfile, cgbase, cgzone, cghole, &size))
        return get_cg_error (interp, "cg_hole_read");
    if ((pts = malloc (size * sizeof(int))) == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }

    if (cg_hole_read (cgfile, cgbase, cgzone, cghole, pts)) {
        free (pts);
        return get_cg_error (interp, "cg_hole_read");
    }

    ierr = construct_data (interp, Integer, size, pts);
    free (pts);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holenum", NULL);
        return TCL_ERROR;
    }
    if (cg_hole_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_hole_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_hole_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cghole;
    int ierr, location, ptype, nsets, npnts, cnt, idim;
    void *pnts;

    Tcl_ResetResult (interp);
    if (argc != 10) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum holename location",
            " ptsettype nptsets npts pntdata", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "hole", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location) ||
        get_type (interp, "ptset", argv[6], NofValidPointSetTypes,
            PointSetTypeName, &ptype)) return TCL_ERROR;
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_hole_write");

    if (extract_data (interp, argv[9], Integer, &cnt, &pnts))
        return TCL_ERROR;
    nsets = atoi(argv[7]);
    npnts = atoi(argv[8]);
    if (cnt < nsets * npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", nsets * npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_hole_write (cgfile, cgbase, cgzone, argv[4],
              (GridLocation_t)location, (PointSetType_t)ptype,
               nsets, npnts, pnts, &cghole);
    free (pnts);
    if (ierr) return get_cg_error (interp, "cg_hole_write");

    sprintf (buff, "%d", cghole);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nconns (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nconn;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nconns (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nconn))
        return get_cg_error (interp, "cg_nconns");

    sprintf (buff, "%d", nconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33], dname[33];
    GridLocation_t location;
    GridConnectivityType_t conntype;
    PointSetType_t ptsettype, dptsettype;
    ZoneType_t dztype;
    DataType_t datatype;
    int npts, dnpts;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }

    if (cg_conn_info (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), name, &location, &conntype, &ptsettype, &npts,
            dname, &dztype, &dptsettype, &datatype, &dnpts))
        return get_cg_error (interp, "cg_conn_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_grid_location_name(location));
    Tcl_AppendElement (interp, cg_grid_connectivity_type_name(conntype));
    Tcl_AppendElement (interp, cg_point_set_type_name(ptsettype));
    sprintf (buff, "%d", npts);
    Tcl_AppendElement (interp, buff);
    Tcl_AppendElement (interp, dname);
    Tcl_AppendElement (interp, cg_zone_type_name(dztype));
    Tcl_AppendElement (interp, cg_point_set_type_name(dptsettype));
    Tcl_AppendElement (interp, cg_data_type_name(datatype));
    sprintf (buff, "%d", dnpts);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgconn;
    int get_donor = (int)data;
    int ierr, cnt, size, dsize;
    DataType_t dtype;
    void *pnts, *donor = NULL;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgconn = atoi(argv[4]);

    if (cg_conn_dim (cgfile, cgbase, cgzone, cgconn,
            &cnt, &size, &dsize, &dtype))
        return get_cg_error (interp, "cg_conn_read");

    if (cnt == 0) return TCL_OK;
    if (get_donor && dsize == 0) return TCL_OK;

    pnts = malloc (cnt * sizeof(int));
    if (pnts == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }
    if (dsize) {
        donor = malloc (dsize * sizeof_datatype(dtype));
        if (donor == NULL) {
            free (pnts);
            Tcl_AppendResult (interp, "malloc failed for donor data", NULL);
            return TCL_ERROR;
        }
    }

    if (cg_conn_read (cgfile, cgbase, cgzone, cgconn, pnts, dtype, donor)) {
        free (pnts);
        if (donor != NULL) free (donor);
        return get_cg_error (interp, "cg_conn_read");
    }

    if (get_donor)
        ierr = construct_data (interp, dtype, dsize, donor);
    else
        ierr = construct_data (interp, Integer, cnt, pnts);
    free (pnts);
    if (donor != NULL) free (donor);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connnum", NULL);
        return TCL_ERROR;
    }
    if (cg_conn_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_conn_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgconn;
    int ierr, idim, cdim, cnt;
    int location, ctype, ptype, npnts;
    int dztype, dptype, ddtype, ndonor;
    void *pnts, *donor;

    Tcl_ResetResult (interp);
    if (argc != 16) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum connname location conntype",
            " ptsettype npnts ptdata donorname donorzonetype",
            " donorptsettype donordatatype ndonor donordata",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "conn", argv[4]) ||
        get_type (interp, "location", argv[5], NofValidGridLocation,
            GridLocationName, &location) ||
        get_type (interp, "conntype", argv[6], NofValidGridConnectivityTypes,
            GridConnectivityTypeName, &ctype) ||
        get_type (interp, "ptset", argv[7], NofValidPointSetTypes,
            PointSetTypeName, &ptype) ||
        check_name (interp, "donor", argv[10]) ||
        get_type (interp, "donorzone", argv[11], NofValidZoneTypes,
            ZoneTypeName, &dztype) ||
        get_type (interp, "donorptset", argv[12], NofValidPointSetTypes,
            PointSetTypeName, &dptype) ||
        get_type (interp, "donordata", argv[13], NofValidDataTypes,
            DataTypeName, &ddtype)) return TCL_ERROR;
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    npnts  = atoi(argv[8]);
    ndonor = atoi(argv[14]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_conn_write");
    if ((ZoneType_t)dztype == Unstructured)
        cdim = 1;
    else {
        if (cg_cell_dim (cgfile, cgbase, &cdim))
            return get_cg_error (interp, "cg_conn_write");
    }

    if (extract_data (interp, argv[9], Integer, &cnt, &pnts))
        return TCL_ERROR;
    if (cnt < npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    if (extract_data (interp, argv[15], (DataType_t)ddtype, &cnt, &donor))
        return TCL_ERROR;
    if (cnt < ndonor * cdim) {
        free (pnts);
        sprintf (buff, "needs %d values", ndonor * cdim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_conn_write (cgfile, cgbase, cgzone, argv[4],
              (GridLocation_t)location, (GridConnectivityType_t)ctype,
              (PointSetType_t)ptype, npnts, pnts, argv[10],
              (ZoneType_t)dztype, (PointSetType_t)dptype,
              (DataType_t)ddtype, ndonor, donor, &cgconn);
    free (pnts);
    free (donor);
    if (ierr) return get_cg_error (interp, "cg_conn_write");

    sprintf (buff, "%d", cgconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivity1to1_t Nodes in a zone            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n1to1 (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n1to1;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n1to1 (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &n1to1))
        return get_cg_error (interp, "cg_n1to1");

    sprintf (buff, "%d", n1to1);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, idim, cgfile, cgbase, cgzone;
    char *p, name[33], dname[33];
    int range[6], drange[6], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21num", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_1to1_read (cgfile, cgbase, cgzone, atoi(argv[4]),
            name, dname, range, drange, trans))
        return get_cg_error (interp, "cg_1to1_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, dname);
    sprintf (buff, "%d", range[0]);
    for (p = buff, n = 1; n < 2 * idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", range[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", drange[0]);
    for (p = buff, n = 1; n < 2 * idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", drange[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", trans[0]);
    for (p = buff, n = 1; n < idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", trans[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21num", NULL);
        return TCL_ERROR;
    }
    if (cg_1to1_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_1to1_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgconn;
    int n, *r, nr, idim;
    char *p, name[33], dname[33];
    int range[6], drange[6], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum one21name donorname",
            " range donorrange transform", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "one21", argv[4]) ||
        check_name (interp, "donor", argv[5])) return TCL_ERROR;
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_1to1_write");

    if (extract_data (interp, argv[6], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < 2 * idim) {
        free (r);
        sprintf (buff, "needs %d values", 2 * idim);
        Tcl_AppendResult (interp,
            "insufficient range data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        range[n] = r[n];
    free (r);

    if (extract_data (interp, argv[7], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < 2 * idim) {
        free (r);
        sprintf (buff, "needs %d values", 2 * idim);
        Tcl_AppendResult (interp,
            "insufficient donor range data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        drange[n] = r[n];
    free (r);

    if (extract_data (interp, argv[8], Integer, &nr, (void **)&r))
        return TCL_ERROR;
    if (nr < idim) {
        free (r);
        sprintf (buff, "needs %d values", idim);
        Tcl_AppendResult (interp,
            "insufficient transform data - ", buff, NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < nr; n++)
        trans[n] = r[n];
    free (r);

    if (cg_1to1_write (cgfile, cgbase, cgzone, argv[4], argv[5],
            range, drange, trans, &cgconn))
        return get_cg_error (interp, "cg_1to1_write");

    sprintf (buff, "%d", cgconn);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read all GridConnectivity1to1_t Nodes of a base                  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n1to1_global (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n1to1;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n1to1_global (atoi(argv[1]), atoi(argv[2]), &n1to1))
        return get_cg_error (interp, "cg_n1to1_global");

    sprintf (buff, "%d", n1to1);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}













/*-----------------------------------------------------------------------*/

/*
int cg_1to1_read_global(int fn, int B, char **connectname, char **zonename,
			char **donorname, int **range, int **donor_range, int **transform);
*/
static int tcl_cg_1to1_read_global (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}










/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BC_t Nodes                                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nbocos (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nboco;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_nbocos (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &nboco))
        return get_cg_error (interp, "cg_nbocos");

    sprintf (buff, "%d", nboco);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char *p, name[33];
    int n, idim, cgfile, cgbase, cgzone, cgboco;
    int npnts, ndataset, nindex[3], nflag;
    BCType_t bctype;
    PointSetType_t ptype;
    DataType_t dtype;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim) ||
        cg_boco_info (cgfile, cgbase, cgzone, cgboco, name,
            &bctype, &ptype, &npnts, nindex, &nflag, &dtype, &ndataset))
        return get_cg_error (interp, "cg_boco_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_bc_type_name(bctype));
    Tcl_AppendElement (interp, cg_point_set_type_name(ptype));
    sprintf (buff, "%d", npnts);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nindex[0]);
    for (p = buff, n = 1; n < idim; n++) {
        p += strlen(p);
        sprintf (p, " %d", nindex[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    Tcl_AppendElement (interp, cg_data_type_name(dtype));
    sprintf (buff, "%d", ndataset);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgboco;
    int ierr, pcnt, psize, ncnt;
    int get_nrmls = (int)data;
    DataType_t ntype;
    void *pnts, *nrml = NULL;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_boco_dim (cgfile, cgbase, cgzone, cgboco,
            &pcnt, &psize, &ncnt, &ntype))
        return get_cg_error (interp, "cg_boco_read");

    if (pcnt == 0) return TCL_OK;
    if (get_nrmls && ncnt == 0) return TCL_OK;

    pnts = malloc (pcnt * sizeof(int));
    if (pnts == NULL) {
        Tcl_AppendResult (interp, "malloc failed for points", NULL);
        return TCL_ERROR;
    }
    if (get_nrmls) {
        nrml = malloc (ncnt * sizeof_datatype(ntype));
        if (nrml == NULL) {
            free (pnts);
            Tcl_AppendResult (interp, "malloc failed for normals", NULL);
            return TCL_ERROR;
        }
    }

    if (cg_boco_read (cgfile, cgbase, cgzone, cgboco, pnts, nrml)) {
        free (pnts);
        if (nrml != NULL) free (nrml);
        return get_cg_error (interp, "cg_boco_read");
    }

    if (get_nrmls)
        ierr = construct_data (interp, ntype, ncnt, nrml);
    else
        ierr = construct_data (interp, Integer, pcnt, pnts);
    free (pnts);
    if (nrml != NULL) free (nrml);
    return ierr ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_id (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    double id;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    if (cg_boco_id (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), &id))
        return get_cg_error (interp, "cg_boco_id");

    sprintf (buff, "%g", id);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgfile, cgbase, cgzone, cgboco;
    int ierr, bctype, ptype, npnts, cnt, idim;
    void *pnts;

    Tcl_ResetResult (interp);
    if (argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcname bctype ptsettype npts pnts",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "bc", argv[4]) ||
        get_type (interp, "bc", argv[5], NofValidBCTypes,
            BCTypeName, &bctype) ||
        get_type (interp, "ptset", argv[6], NofValidPointSetTypes,
            PointSetTypeName, &ptype)) return TCL_ERROR;
    if (ptype != PointRange && ptype != ElementRange &&
        ptype != PointList  && ptype != ElementList) {
        Tcl_AppendResult (interp, "invalid ptset type", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    npnts  = atoi(argv[7]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &idim))
        return get_cg_error (interp, "cg_boco_write");

    if (extract_data (interp, argv[8], Integer, &cnt, &pnts))
        return TCL_ERROR;
    if (cnt < npnts * idim) {
        free (pnts);
        sprintf (buff, "needs %d values", npnts * idim);
        Tcl_AppendResult (interp,
            "insufficient point data - ", buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_boco_write (cgfile, cgbase, cgzone, argv[4],
              (BCType_t)bctype, (PointSetType_t)ptype,
               npnts, pnts, &cgboco);
    free (pnts);
    if (ierr) return get_cg_error (interp, "cg_boco_write");

    sprintf (buff, "%d", cgboco);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_boco_normal_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, cgfile, cgbase, cgzone, cgboco;
    int nflag, type, dim, psize, cnt, index[3];
    DataType_t ntype;
    void *normals;

    Tcl_ResetResult (interp);
    if (argc != 6 && argc != 8 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum normalindex",
            " [[normallistflag] normaldatatype normals]", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);
    cgzone = atoi(argv[3]);
    cgboco = atoi(argv[4]);

    if (cg_index_dim (cgfile, cgbase, cgzone, &dim))
        return get_cg_error (interp, "cg_boco_normal_write");

    if (dim > sscanf(argv[5], "%d%d%d", &index[0], &index[1], &index[2])) {
        sprintf (buff, "needs %d", dim);
        Tcl_AppendResult (interp,
            "insufficient number of normalindex values - ", buff, NULL);
        return TCL_ERROR;
    }

    nflag = type = 0;
    normals = NULL;
    if (argc >= 8)
        nflag = argc == 9 ? atoi(argv[6]) : 1;

    if (nflag) {
        if (get_type (interp, "normal data", argv[argc-2],
            NofValidDataTypes, DataTypeName, &type)) return TCL_ERROR;
        if (type != RealSingle && type != RealDouble) {
            Tcl_AppendResult (interp,
                "normal data type not RealSingle or RealDouble", NULL);
            return TCL_ERROR;
        }
        if (cg_physical_dim (cgfile, cgbase, &dim) ||
            cg_boco_dim (cgfile, cgbase, cgzone, cgboco,
                &cnt, &psize, &cnt, &ntype))
            return get_cg_error (interp, "cg_boco_normal_write");
        if (extract_data (interp, argv[argc-1], type, &cnt, &normals))
            return TCL_ERROR;
        if (cnt < psize * dim) {
            free (normals);
            sprintf (buff, "needs %d", psize * dim);
            Tcl_AppendResult (interp,
                "insufficient normal values - ", buff, NULL);
            return TCL_ERROR;
        }
    }

    ierr = cg_boco_normal_write (cgfile, cgbase, cgzone, cgboco,
           index, nflag, (DataType_t)type, normals);
    if (nflag) free (normals);
    if (ierr) return get_cg_error (interp, "cg_boco_normal_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCDataSet_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_dataset_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    BCType_t bctype;
    int dflag, nflag;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum dsnum", NULL);
        return TCL_ERROR;
    }

    if (cg_dataset_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), name, &bctype, &dflag, &nflag))
        return get_cg_error (interp, "cg_dataset_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_bc_type_name(bctype));
    sprintf (buff, "%d", dflag);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_dataset_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int bctype, cgds;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum datasetname bctype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "dataset", argv[5]) ||
        get_type (interp, "bc", argv[6], NofValidBCTypes,
            BCTypeName, &bctype)) return TCL_ERROR;

    if (cg_dataset_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), argv[5], (BCType_t)bctype, &cgds))
        return get_cg_error (interp, "cg_dataset_write");

    sprintf (buff, "%d", cgds);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ndset;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_bcdataset_info (&ndset))
        return get_cg_error (interp, "cg_bcdataset_info");

    sprintf (buff, "%d", ndset);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int bctype, bcdatatype;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " bcdatasetname bctype bcdatatype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "bcdataset", argv[1]) ||
        get_type (interp, "bc", argv[2], NofValidBCTypes,
            BCTypeName, &bctype) ||
        get_type (interp, "bcdata", argv[3], NofValidBCDataTypes,
            BCDataTypeName, &bcdatatype)) return TCL_ERROR;

    if (cg_bcdataset_write (argv[1], (BCType_t)bctype,
            (BCDataType_t)bcdatatype))
        return get_cg_error (interp, "cg_bcdataset_write");

    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bcdataset_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    BCType_t bctype;
    int dflag, nflag;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " index", NULL);
        return TCL_ERROR;
    }

    if (cg_bcdataset_read (atoi(argv[1]), name, &bctype, &dflag, &nflag))
        return get_cg_error (interp, "cg_bcdataset_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_bc_type_name(bctype));
    sprintf (buff, "%d", dflag);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%d", nflag);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCData_t Nodes                                    *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bcdata_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 7) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum dsnum bcdatatype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "bc", argv[6], NofValidBCDataTypes,
            BCDataTypeName, &type)) return TCL_ERROR;

    if (cg_bcdata_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]),
            atoi(argv[4]), atoi(argv[5]), (BCDataType_t)type))
        return get_cg_error (interp, "cg_bcdata_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DiscreteData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ndiscrete (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ndisc;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_ndiscrete (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), &ndisc))
        return get_cg_error (interp, "cg_ndiscrete");

    sprintf (buff, "%d", ndisc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_discrete_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum discretenum", NULL);
        return TCL_ERROR;
    }

    if (cg_discrete_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name))
        return get_cg_error (interp, "cg_discrete_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_discrete_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgdisc;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum discretename", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "discrete", argv[4])) return TCL_ERROR;

    if (cg_discrete_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], &cgdisc))
        return get_cg_error (interp, "cg_discrete_write");

    sprintf (buff, "%d", cgdisc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RigidGridMotion_t Nodes				 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n_rigid_motions (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nrm;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n_rigid_motions (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), &nrm))
        return get_cg_error (interp, "cg_n_rigid_motions");

    sprintf (buff, "%d", nrm);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rigid_motion_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    RigidGridMotionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum rigidmotionnum", NULL);
        return TCL_ERROR;
    }

    if (cg_rigid_motion_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name, &type))
        return get_cg_error (interp, "cg_rigid_motion_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_rigid_grid_motion_type_name(type));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rigid_motion_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgrm, type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum rigidmotionname rigidmotiontype", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "rigidmotion", argv[4]) ||
        get_type (interp, "rigidmotion", argv[5], NofValidRigidGridMotionTypes,
            RigidGridMotionTypeName, &type)) return TCL_ERROR;

    if (cg_rigid_motion_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], (RigidGridMotionType_t)type, &cgrm))
        return get_cg_error (interp, "cg_rigid_motion_write");
    
    sprintf (buff, "%d", cgrm);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ArbitraryGridMotion_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_n_arbitrary_motions (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nam;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    if (cg_n_arbitrary_motions (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), &nam))
        return get_cg_error (interp, "cg_n_arbitrary_motions");

    sprintf (buff, "%d", nam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_arbitrary_motion_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    ArbitraryGridMotionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum arbitarymotionnum", NULL);
        return TCL_ERROR;
    }

    if (cg_arbitrary_motion_read (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), name, &type))
        return get_cg_error (interp, "cg_arbitrary_motion_read");
    
    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_arbitrary_grid_motion_type_name(type));
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_arbitrary_motion_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int cgam, type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum arbitrarymotionname arbitrarymotiontype",
            NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "arbitrarymotion", argv[4]) ||
        get_type (interp, "arbitrarymotion", argv[5],
            NofValidArbitraryGridMotionTypes,
            ArbitraryGridMotionTypeName, &type)) return TCL_ERROR;

    if (cg_arbitrary_motion_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), argv[4], (ArbitraryGridMotionType_t)type, &cgam))
        return get_cg_error (interp, "cg_arbitrary_motion_write");
    
    sprintf (buff, "%d", cgam);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write SimulationType_t Node                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_simulation_type_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    SimulationType_t type;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_simulation_type_read (atoi(argv[1]), atoi(argv[2]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_simulation_type_read");
    }
    
    Tcl_AppendResult (interp, cg_simulation_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_simulation_type_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum simulationtype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "simulation", argv[3], NofValidSimulationTypes,
            SimulationTypeName, &type)) return TCL_ERROR;

    if (cg_simulation_type_write (atoi(argv[1]), atoi(argv[2]),
            (SimulationType_t)type))
        return get_cg_error (interp, "cg_simulation_type_write");
    
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BaseIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_biter_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, nsteps;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_biter_read (atoi(argv[1]), atoi(argv[2]), name, &nsteps);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_biter_read");
    }

    Tcl_AppendElement (interp, name);
    sprintf (buff, "%d", nsteps);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_biter_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum bitername nsteps", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "biter", argv[3])) return TCL_ERROR;

    if (cg_biter_write (atoi(argv[1]), atoi(argv[2]), argv[3], atoi(argv[4])))
        return get_cg_error (interp, "cg_biter_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ZoneIterativeData_t Node                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ziter_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_ziter_read (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_ziter_read");
    }

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ziter_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum zitername", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "ziter", argv[4])) return TCL_ERROR;

    if (cg_ziter_write (atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), argv[4]))
        return get_cg_error (interp, "cg_ziter_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Gravity_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_gravity_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, cgfile, cgbase, ierr, phys_dim;
    float vector[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    ierr = cg_gravity_read (cgfile, cgbase, vector);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_gravity_read");
    }

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");
    for (n = 0; n < phys_dim; n++) {
        sprintf (buff, "%g", vector[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gravity_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int len, cgfile, cgbase, phys_dim;
    float vector[3];

    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum vector", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_write");

    len = sscanf (argv[3], "%f%f%f", &vector[0], &vector[1], &vector[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for gravity vector - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_gravity_write (cgfile, cgbase, vector))
        return get_cg_error (interp, "cg_gravity_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Axisymmetry_t Nodes                               *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_axisym_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char *p;
    int ierr, n, cgfile, cgbase, phys_dim;
    float ref[3], axis[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    ierr = cg_axisym_read (cgfile, cgbase, ref, axis);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_axisym_read");
    }

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");
    sprintf (buff, "%g", ref[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", ref[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g", axis[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", axis[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_axisym_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int len, cgfile, cgbase, phys_dim;
    float ref[3], axis[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum refpoint axis", NULL);
        return TCL_ERROR;
    }
    cgfile = atoi(argv[1]);
    cgbase = atoi(argv[2]);

    if (cg_physical_dim (cgfile, cgbase, &phys_dim))
        return get_cg_error (interp, "cg_gravity_read");

    len = sscanf (argv[3], "%f%f%f", &ref[0], &ref[1], &ref[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for reference point - ", buff, NULL);
        return TCL_ERROR;
    }
    len = sscanf (argv[4], "%f%f%f", &axis[0], &axis[1], &axis[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for axis vector - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_axisym_write (cgfile, cgbase, ref, axis))
        return get_cg_error (interp, "cg_axisym_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write RotatingCoordinates_t Nodes                       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_rotating_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char *p;
    int ierr, n, phys_dim;
    float rate[3], center[3];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_rotating_read (rate, center);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_rotating_read");
    }

    if (cg_physical_dim (goFile, goBase, &phys_dim))
        return get_cg_error (interp, "cg_rotating_read");

    sprintf (buff, "%g", rate[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", rate[n]);
    }
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g", center[0]);
    for (p = buff, n = 1; n < phys_dim; n++) {
        p += strlen(p);
        sprintf (p, " %g", center[n]);
    }
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rotating_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int len, phys_dim;
    float rate[3], center[3];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " rate center", NULL);
        return TCL_ERROR;
    }
    if (!(goFile && goBase)) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (cg_physical_dim (goFile, goBase, &phys_dim))
        return get_cg_error (interp, "cg_rotating_write");

    len = sscanf (argv[1], "%f%f%f", &rate[0], &rate[1], &rate[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for rate - ", buff, NULL);
        return TCL_ERROR;
    }
    len = sscanf (argv[2], "%f%f%f", &center[0], &center[1], &center[2]);
    if (len < phys_dim) {
        sprintf (buff, "needs %d", phys_dim);
        Tcl_AppendResult (interp,
            "not enough values for center - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_rotating_write (rate, center))
        return get_cg_error (interp, "cg_rotating_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/WallFunction_t Nodes   	         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bc_wallfunction_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    WallFunctionType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_bc_wallfunction_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_bc_wallfunction_read");
    }

    Tcl_AppendResult (interp, cg_wall_function_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bc_wallfunction_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum wallfuctiontype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "wall function", argv[5],
        NofValidWallFunctionTypes, WallFunctionTypeName, &type))
        return TCL_ERROR;

    if (cg_bc_wallfunction_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), (WallFunctionType_t)type))
        return get_cg_error (interp, "cg_bc_wallfunction_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write BCProperty_t/Area_t Nodes                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_bc_area_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    char *p, name[33];
    AreaType_t type;
    float area;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_bc_area_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type, &area, name);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_bc_area_read");
    }

    Tcl_AppendElement (interp, cg_area_type_name(type));
    sprintf (buff, "%g", area);
    Tcl_AppendElement (interp, buff);
    for (p = name+strlen(name)-1; p >= name && isspace(*p); p--)
        ;
    *++p = 0;
    Tcl_AppendElement (interp, name);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_bc_area_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;
    float area;

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum zonenum bcnum areatype area regionname", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "area", argv[5],
        NofValidAreaTypes, AreaTypeName, &type))
        return TCL_ERROR;
    area = (float) atof(argv[6]);

    if (cg_bc_area_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), (AreaType_t)type, area, argv[7]))
        return get_cg_error (interp, "cg_bc_area_write");

    return TCL_OK;
}














/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridConnectivityProperty_t/Periodic_t Nodes       *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conn_periodic_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_conn_periodic_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), center, angle, trans);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conn_periodic_read");
    }
    sprintf (buff, "%g %g %g", center[0], center[1], center[2]);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g %g %g", angle[0], angle[1], angle[2]);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g %g %g", trans[0], trans[1], trans[2]);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_periodic_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum",
            " center angle translation", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[5], "%f%f%f", &center[0], &center[1], &center[2])) {
        Tcl_AppendResult (interp, "center needs 3 values", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[6], "%f%f%f", &angle[0], &angle[1], &angle[2])) {
        Tcl_AppendResult (interp, "angle needs 3 values", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[7], "%f%f%f", &trans[0], &trans[1], &trans[2])) {
        Tcl_AppendResult (interp, "translation needs 3 values", NULL);
        return TCL_ERROR;
    }

    if (cg_conn_periodic_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), center, angle, trans))
        return get_cg_error (interp, "cg_conn_periodic_write");
    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_periodic_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_1to1_periodic_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), center, angle, trans);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_1to1_periodic_read");
    }
    sprintf (buff, "%g %g %g", center[0], center[1], center[2]);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g %g %g", angle[0], angle[1], angle[2]);
    Tcl_AppendElement (interp, buff);
    sprintf (buff, "%g %g %g", trans[0], trans[1], trans[2]);
    Tcl_AppendElement (interp, buff);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_periodic_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    float angle[3], center[3], trans[3];

    Tcl_ResetResult (interp);
    if (argc != 8) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum",
            " center angle translation", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[5], "%f%f%f", &center[0], &center[1], &center[2])) {
        Tcl_AppendResult (interp, "center needs 3 values", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[6], "%f%f%f", &angle[0], &angle[1], &angle[2])) {
        Tcl_AppendResult (interp, "angle needs 3 values", NULL);
        return TCL_ERROR;
    }
    if (3 != sscanf (argv[7], "%f%f%f", &trans[0], &trans[1], &trans[2])) {
        Tcl_AppendResult (interp, "translation needs 3 values", NULL);
        return TCL_ERROR;
    }

    if (cg_1to1_periodic_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), center, angle, trans))
        return get_cg_error (interp, "cg_1to1_periodic_write");
    return TCL_OK;
}

#endif









/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *   Read and write GridConnectivityProperty_t/AverageInterface_t Nodes  *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conn_average_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_conn_average_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conn_average_read");
    }

    Tcl_AppendResult (interp, cg_average_interface_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conn_average_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum averagetype", NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < NofValidAverageInterfaceTypes; n++) {
        if (0 == strcmp (argv[5], AverageInterfaceTypeName[n])) {
            type = (AverageInterfaceType_t)n;
            break;
        }
    }
    if (n == NofValidAverageInterfaceTypes) {
        Tcl_AppendResult (interp, "invalid average interface type", NULL);
        return TCL_ERROR;
    }

    if (cg_conn_average_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), type))
        return get_cg_error (interp, "cg_conn_average_write");

    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_average_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 5) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum", NULL);
        return TCL_ERROR;
    }

    ierr = cg_1to1_average_read (atoi(argv[1]), atoi(argv[2]),
        atoi(argv[3]), atoi(argv[4]), &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_1to1_average_read");
    }

    Tcl_AppendResult (interp, cg_average_interface_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_1to1_average_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;
    AverageInterfaceType_t type;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            "filenum basenum zonenum interfacenum averagetype", NULL);
        return TCL_ERROR;
    }
    for (n = 0; n < NofValidAverageInterfaceTypes; n++) {
        if (0 == strcmp (argv[5], AverageInterfaceTypeName[n])) {
            type = (AverageInterfaceType_t)n;
            break;
        }
    }
    if (n == NofValidAverageInterfaceTypes) {
        Tcl_AppendResult (interp, "invalid average interface type", NULL);
        return TCL_ERROR;
    }

    if (cg_1to1_average_write (atoi(argv[1]), atoi(argv[2]),
            atoi(argv[3]), atoi(argv[4]), type))
        return get_cg_error (interp, "cg_1to1_average_write");

    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Variable Argument List Functions                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_goto (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;

    Tcl_ResetResult (interp);
    if (argc < 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " filenum basenum [label1 index1 [label2 index2 [...]]]", NULL);
        return TCL_ERROR;
    }
    if (argc > 2 * (MAX_GOTO_DEPTH + 1)) {
        Tcl_AppendResult (interp, "max goto depth exceeded", NULL);
        return TCL_ERROR;
    }
    goFile = atoi(argv[1]);
    goBase = atoi(argv[2]);
    goDepth = 0;
    for (n = 3; n < argc; n++) {
        if (0 == strcmp (argv[n], "end")) break;
        strncpy (goLabel[goDepth], argv[n], 32);
        goLabel[goDepth][32] = 0;
        if (++n >= argc) {
            Tcl_AppendResult (interp, "missing final index value", NULL);
            return TCL_ERROR;
        }
        goIndex[goDepth] = atoi(argv[n]);
        goDepth++;
    }

    if (goto_node())
        return get_cg_error (interp, "cg_goto");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gorel (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, cnt;
    char *lab;

    Tcl_ResetResult (interp);
    if (argc < 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " label1 index1 [label2 index2 [...]]", NULL);
        return TCL_ERROR;
    }
    if (!goFile && !goBase) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }

    for (n = 1; n < argc; n++) {
        lab = argv[n];
        if (0 == strcmp (lab, "end")) break;
        if (++n >= argc) {
            Tcl_AppendResult (interp, "missing final index value", NULL);
            return TCL_ERROR;
        }
        cnt = atoi(argv[n]);
        if (0 == strcmp (lab, "..")) {
            if (cnt < 1) cnt = 1;
            goDepth -= cnt;
            if (goDepth < 0) goDepth = 0;
        }
        else if (strcmp (lab, ".")) {
            if (goDepth == MAX_GOTO_DEPTH) {
                Tcl_AppendResult (interp,
                    "maximum depth of goto exceeded", NULL);
                return TCL_ERROR;
            }
            strncpy (goLabel[goDepth], lab, 32);
            goLabel[goDepth][32] = 0;
            goIndex[goDepth++] = cnt;
        }
    }

    if (goto_node())
        return get_cg_error (interp, "cg_gorel");
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_where (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    if (!goFile && !goBase) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    sprintf (buff, "%d %d", goFile, goBase);
    Tcl_AppendResult (interp, buff, NULL);
    for (n = 0; n < goDepth; n++) {
        Tcl_AppendElement (interp, goLabel[n]);
        sprintf (buff, "%d", goIndex[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ConvergenceHistory_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_convergence_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, niter;
    char *normdefs;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_convergence_read (&niter, &normdefs);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_convergence_read");
    }

    sprintf (buff, "%d", niter);
    Tcl_AppendElement (interp, buff);
    if (normdefs != NULL) {
        Tcl_AppendElement (interp, normdefs);
        free (normdefs);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_convergence_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc < 2 || argc > 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " niters [normdefinition]", NULL);
        return TCL_ERROR;
    }

    if (cg_convergence_write (atoi(argv[1]),
        argc == 3 ? argv[2] : ""))
        return get_cg_error (interp, "cg_convergence_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write ReferenceState_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_state_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, niter;
    char *desc;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_state_read (&desc);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_state_read");
    }

    if (desc != NULL) {
        Tcl_AppendResult (interp, desc, NULL);
        free (desc);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_state_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc > 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " [statedescription]", NULL);
        return TCL_ERROR;
    }

    if (cg_state_write (argc == 2 ? argv[1] : ""))
        return get_cg_error (interp, "cg_state_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write FlowEquationSet_t Nodes                           *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_equationset_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, ed, gef, gmf, vmf, tcmf, tcf, tmf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_read (&ed, &gef, &gmf, &vmf, &tcmf, &tcf, &tmf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_read");
    }

    sprintf (buff, "%d %d %d %d %d %d %d", ed, gef, gmf, vmf, tcmf, tcf, tmf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#if CGNS_VERSION >= 2100

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_chemistry_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, trf, ckf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_chemistry_read (&trf, &ckf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_chemistry_read");
    }

    sprintf (buff, "%d %d", trf, ckf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_elecmagn_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, emf, mmf, cmf;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_equationset_elecmagn_read (&emf, &mmf, &cmf);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_equationset_elecmagn_read");
    }

    sprintf (buff, "%d %d %d", emf, mmf, cmf);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#endif
#endif

/*-----------------------------------------------------------------------*/

static int tcl_cg_equationset_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " eqndimension", NULL);
        return TCL_ERROR;
    }

    if (cg_equationset_write (atoi(argv[1])))
        return get_cg_error (interp, "cg_equationset_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GoverningEquations_t Nodes                        *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_governing_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    GoverningEquationsType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_governing_read (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_governing_read");
    }

    Tcl_AppendResult (interp, cg_governing_equations_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_governing_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " equationntype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "equation", argv[1],
        NofValidGoverningEquationsTypes,
        GoverningEquationsTypeName, &type)) return TCL_ERROR;

    if (cg_governing_write ((GoverningEquationsType_t)type))
        return get_cg_error (interp, "cg_governing_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Diffusion Model Nodes                             *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_diffusion_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, idim, diff[6];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_diffusion_read (diff);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_diffusion_read");
    }

    if (posit_zone)
        ierr = cg_index_dim (goFile, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (goFile, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_diffusion_read");

    if (idim == 1)
        sprintf (buff, "%d", diff[0]);
    else if (idim == 2)
        sprintf (buff, "%d %d %d", diff[0], diff[1], diff[2]);
    else
        sprintf (buff, "%d %d %d %d %d %d", diff[0], diff[1], diff[2],
            diff[3], diff[4], diff[5]);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_diffusion_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int idim, cnt, ierr, diff[6];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " diffusionmodel", NULL);
        return TCL_ERROR;
    }
    if (!(goFile && posit_base)) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (posit_zone)
        ierr = cg_index_dim (goFile, posit_base, posit_zone, &idim);
    else
        ierr = cg_cell_dim (goFile, posit_base, &idim);
    if (ierr) return get_cg_error (interp, "cg_diffusion_write");

    if (idim == 1)
        cnt = 1;
    else if (idim == 2)
        cnt = 3;
    else if (idim == 3)
        cnt = 6;
    else {
        Tcl_AppendResult (interp, "couldn't determine index dimension", NULL);
        return TCL_ERROR;
    }
    if (cnt > sscanf (argv[1], "%d%d%d%d%d%d", &diff[0], &diff[1],
        &diff[2], &diff[3], &diff[4], &diff[5])) {
        sprintf (buff, "needs %d", cnt);
        Tcl_AppendResult (interp,
            "insufficient diffusion model values - ", buff, NULL);
        return TCL_ERROR;
    }

    if (cg_diffusion_write (diff))
        return get_cg_error (interp, "cg_diffusion_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GasModel_t, ViscosityModel_t,                     *
 *      ThermalConductivityModel_t, TurbulenceClosure_t,                 *
 *      TurbulenceModel_t, ThermalRelaxationModel_t,                     *
 *      ChemicalKineticsModel_t, EMElectricFieldModel_t,                 *
 *      EMMagneticFieldModel_t Nodes                                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_model_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    ModelType_t type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " modellabel", NULL);
        return TCL_ERROR;
    }

    ierr = cg_model_read (argv[1], &type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_model_read");
    }

    Tcl_AppendResult (interp, cg_model_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_model_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " modellabel modeltype", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "model", argv[2], NofValidModelTypes,
        ModelTypeName, &type)) return TCL_ERROR;

    if (cg_model_write (argv[1], (ModelType_t)type))
        return get_cg_error (interp, "cg_model_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataArray_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_narrays (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int narray;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_narrays (&narray))
        return get_cg_error (interp, "cg_narrays");

    sprintf (buff, "%d", narray);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    int ndim, dims[12];
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " arraynum", NULL);
        return TCL_ERROR;
    }

    if (cg_array_info (atoi(argv[1]), name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, cg_data_type_name(type));
    sprintf (buff, "%d", ndim);
    Tcl_AppendElement (interp, buff);
    if (ndim == 0)
        Tcl_AppendElement (interp, "");
    else {
        int n;
        char *p = buff;
        sprintf (p, "%d", dims[0]);
        for (n = 1; n < ndim; n++) {
            p += strlen(p);
            sprintf (p, " %d", dims[n]);
        }
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    int na, ndim, dims[12];
    int count, bytes;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " arraynum", NULL);
        return TCL_ERROR;
    }
    na = atoi(argv[1]);

    if (cg_array_info (na, name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");
    if (get_data_size (interp, type, ndim, dims, &count, &bytes))
        return TCL_ERROR;
    array = malloc (count * bytes);
    if (array == NULL) {
        Tcl_AppendResult (interp, "malloc failed for array data", NULL);
        return TCL_ERROR;
    }

    if (cg_array_read (na, array)) {
        free (array);
        return get_cg_error (interp, "cg_array_read");
    }

    na = construct_data (interp, type, count, array);
    free (array);
    return na ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_read_as (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];
    int na, ndim, dims[12];
    int count, bytes;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " arraynum datatype", NULL);
        return TCL_ERROR;
    }
    na = atoi(argv[1]);

    if (cg_array_info (na, name, &type, &ndim, dims))
        return get_cg_error (interp, "cg_array_info");
    if (get_data_type (interp, argv[2], &type)) return TCL_ERROR;
    if (get_data_size (interp, type, ndim, dims, &count, &bytes))
        return TCL_ERROR;
    array = malloc (count * bytes);
    if (array == NULL) {
        Tcl_AppendResult (interp, "malloc failed for array data", NULL);
        return TCL_ERROR;
    }

    if (cg_array_read_as (na, type, array)) {
        free (array);
        return get_cg_error (interp, "cg_array_read_as");
    }

    na = construct_data (interp, type, count, array);
    free (array);
    return na ? TCL_ERROR : TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_array_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, len, ndim, *dims, count;
    DataType_t type;
    void *array;

    Tcl_ResetResult (interp);
    if (argc != 6) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " arrayname datatype ndim dims data", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "array", argv[1]) ||
        get_data_type (interp, argv[2], &type) ||
        extract_data (interp, argv[4], Integer, &ndim, (void **)&dims))
        return TCL_ERROR;
    if (ndim != atoi(argv[3])) {
        free (dims);
        Tcl_AppendResult (interp,
            "mismatch in ndim and dimension count", NULL);
        return TCL_ERROR;
    }
    if (get_data_size (interp, type, ndim, dims, &len, &count)) {
        free (dims);
        return TCL_ERROR;
    }
    if (extract_data (interp, argv[5], type, &count, &array)) {
        free (dims);
        return TCL_ERROR;
    }
    if (count != len) {
        free (dims);
        free (array);
        Tcl_AppendResult (interp,
            "mismatch in dimension count and data array length", NULL);
        return TCL_ERROR;
    }

    ierr = cg_array_write (argv[1], type, ndim, dims, array);
    free (dims);
    free (array);
    if (ierr)
        return get_cg_error (interp, "cg_array_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write UserDefinedData_t Nodes - new in version 2.1      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if CGNS_VERSION >= 2100

static int tcl_cg_nuser_data (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, nuser;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_nuser_data (&nuser);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_nuser_data");
    }

    sprintf (buff, "%d", nuser);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_user_data_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " userdatanum", NULL);
        return TCL_ERROR;
    }

    if (cg_user_data_read (atoi(argv[1]), name))
        return get_cg_error (interp, "cg_user_data_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_user_data_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " userdataname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "userdata", argv[1])) return TCL_ERROR;

    if (cg_user_data_write (argv[1]))
        return get_cg_error (interp, "cg_user_data_write");

    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write IntegralData_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_nintegrals (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nint;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nintegrals (&nint))
        return get_cg_error (interp, "cg_nintegrals");

    sprintf (buff, "%d", nint);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_integral_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33];

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " integralum", NULL);
        return TCL_ERROR;
    }

    if (cg_integral_read (atoi(argv[1]), name))
        return get_cg_error (interp, "cg_integral_read");

    Tcl_AppendResult (interp, name, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_integral_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " integralname", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "integral", argv[1])) return TCL_ERROR;

    if (cg_integral_write (argv[1]))
        return get_cg_error (interp, "cg_integral_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Rind_t Nodes                                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_rind_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, n, idim, rind[6];

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_rind_read (rind);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_rind_read");
    }

    if (cg_index_dim (goFile, posit_base, posit_zone, &idim))
        return get_cg_error (interp, "cg_rind_read");
    for (n = 0; n < 2*idim; n++) {
        sprintf (buff, "%d", rind[n]);
        Tcl_AppendElement (interp, buff);
    }
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_rind_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, idim, nr, *rind;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " rind", NULL);
        return TCL_ERROR;
    }
    if (!(goFile && posit_base && posit_zone)) {
        Tcl_AppendResult (interp, "position not set with cg_goto", NULL);
        return TCL_ERROR;
    }
    if (cg_index_dim (goFile, posit_base, posit_zone, &idim))
        return get_cg_error (interp, "cg_rind_write");
    if (extract_data (interp, argv[1], Integer, &nr, (void **)&rind))
        return TCL_ERROR;
    if (nr < 2*idim) {
        free (rind);
        sprintf (buff, "insufficient rind values - need %d", 2*idim);
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    ierr = cg_rind_write (rind);
    free (rind);
    if (ierr) return get_cg_error (interp, "cg_rind_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Descriptor_t Nodes                                *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ndescriptors (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ndesc;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_ndescriptors (&ndesc))
        return get_cg_error (interp, "cg_ndescriptors");

    sprintf (buff, "%d", ndesc);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_descriptor_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char name[33], *text;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " descnum", NULL);
        return TCL_ERROR;
    }

    if (cg_descriptor_read (atoi(argv[1]), name, &text))
        return get_cg_error (interp, "cg_descriptor_read");

    Tcl_AppendElement (interp, name);
    Tcl_AppendElement (interp, text);
    free (text);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_descriptor_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " descname desctext", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "descriptor", argv[1])) return TCL_ERROR;

    if (cg_descriptor_write (argv[1], argv[2]))
        return get_cg_error (interp, "cg_descriptor_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalUnits_t Nodes                          *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_units_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    MassUnits_t mass;
    LengthUnits_t length;
    TimeUnits_t time;
    TemperatureUnits_t temp;
    AngleUnits_t angle;
#if CGNS_VERSION >= 2400
    int nunits, full = (int)data;
    ElectricCurrentUnits_t current;
    SubstanceAmountUnits_t amount;
    LuminousIntensityUnits_t intensity;
#endif

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

#if CGNS_VERSION >= 2400
    ierr = cg_unitsfull_read (&mass, &length, &time, &temp, &angle,
        &current, &amount, &intensity);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_unitsfull_read");
    }
    if (cg_nunits (&nunits))
        return get_cg_error (interp, "cg_nunits");

    Tcl_AppendElement (interp, cg_mass_units_name(mass));
    Tcl_AppendElement (interp, cg_length_units_name(length));
    Tcl_AppendElement (interp, cg_time_units_name(time));
    Tcl_AppendElement (interp, cg_temperature_units_name(temp));
    Tcl_AppendElement (interp, cg_angle_units_name(angle));
    if (full || nunits > 5) {
        Tcl_AppendElement (interp, cg_electric_current_units_name(current));
        Tcl_AppendElement (interp, cg_substance_amount_units_name(amount));
        Tcl_AppendElement (interp, cg_luminous_intensity_units_name(intensity));
    }
#else
    ierr = cg_units_read (&mass, &length, &time, &temp, &angle);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_units_read");
    }

    Tcl_AppendElement (interp, cg_mass_units_name(mass));
    Tcl_AppendElement (interp, cg_length_units_name(length));
    Tcl_AppendElement (interp, cg_time_units_name(time));
    Tcl_AppendElement (interp, cg_temperature_units_name(temp));
    Tcl_AppendElement (interp, cg_angle_units_name(angle));
#endif
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_units_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int type;
    MassUnits_t mass;
    LengthUnits_t length;
    TimeUnits_t time;
    TemperatureUnits_t temp;
    AngleUnits_t angle;
#if CGNS_VERSION >= 2400
    int full = (int)data;
    ElectricCurrentUnits_t current;
    SubstanceAmountUnits_t amount;
    LuminousIntensityUnits_t intensity;
#endif

    Tcl_ResetResult (interp);
#if CGNS_VERSION >= 2400
    if (full && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " massunits lengthunits timeunits temperatureunits angleunits",
            " currentunits amountunits intensityunits", NULL);
        return TCL_ERROR;
    }
#endif
    if (argc != 6 && argc != 9) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " massunits lengthunits timeunits temperatureunits angleunits",
#if CGNS_VERSION >= 2400
            " [currentunits amountunits intensityunits]",
#endif
            NULL);
        return TCL_ERROR;
    }

    if (get_type (interp, "mass unit", argv[1],
        NofValidMassUnits, MassUnitsName, &type)) return TCL_ERROR;
    mass = (MassUnits_t)type;

    if (get_type (interp, "length unit", argv[2],
        NofValidLengthUnits, LengthUnitsName, &type)) return TCL_ERROR;
    length = (LengthUnits_t)type;

    if (get_type (interp, "time unit", argv[3],
        NofValidTimeUnits, TimeUnitsName, &type)) return TCL_ERROR;
    time = (TimeUnits_t)type;

    if (get_type (interp, "temperature unit", argv[4],
        NofValidTemperatureUnits, TemperatureUnitsName, &type))
        return TCL_ERROR;
    temp = (TemperatureUnits_t)type;

    if (get_type (interp, "angle unit", argv[5],
        NofValidAngleUnits, AngleUnitsName, &type)) return TCL_ERROR;
    angle = (AngleUnits_t)type;

#if CGNS_VERSION >= 2400
    if (full || argc == 9) {
        if (get_type (interp, "electric current unit", argv[6],
            NofValidElectricCurrentUnits, ElectricCurrentUnitsName, &type))
            return TCL_ERROR;
        current = (ElectricCurrentUnits_t)type;

        if (get_type (interp, "substance amount unit", argv[7],
            NofValidSubstanceAmountUnits, SubstanceAmountUnitsName, &type))
            return TCL_ERROR;
        amount = (SubstanceAmountUnits_t)type;

        if (get_type (interp, "luminous intensity unit", argv[8],
            NofValidLuminousIntensityUnits, LuminousIntensityUnitsName, &type))
            return TCL_ERROR;
        intensity = (LuminousIntensityUnits_t)type;

        if (cg_unitsfull_write (mass, length, time, temp, angle,
                current, amount, intensity))
            return get_cg_error (interp, "cg_unitsfull_write");

        return TCL_OK;
    }
#endif

    if (cg_units_write (mass, length, time, temp, angle))
        return get_cg_error (interp, "cg_units_write");

    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_nunits (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nunits;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nunits (&nunits))
        return get_cg_error (interp, "cg_nunits");

    sprintf (buff, "%d", nunits);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DimensionalExponents_t Nodes                      *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_exponents_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_exponents_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_exponents_info");
    }

    Tcl_AppendResult (interp, cg_data_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_exponents_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr, nexp;
    DataType_t type;
    double exps[8];
#if CGNS_VERSION >= 2400
    int full = (int)data;
#endif

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_exponents_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_exponents_info");
    }
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp, "invalid exponents data type", NULL);
        return TCL_ERROR;
    }
#if CGNS_VERSION >= 2400
    if (full)
        nexp = 8;
    else {
        if (cg_nexponents (&nexp))
            return get_cg_error (interp, "cg_nexponents");
    }
    if (cg_expfull_read ((void *)exps))
        return get_cg_error (interp, "cg_expfull_read");
#else
    if (cg_exponents_read ((void *)exps))
        return get_cg_error (interp, "cg_exponents_read");
    nexp = 5;
#endif

    if (construct_data (interp, type, nexp, (void *)exps))
        return TCL_ERROR;
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_exponents_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nexp;
    DataType_t type;
    void *exps;
#if CGNS_VERSION >= 2400
    int full = (int)data;
#endif

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " datatype exponents", NULL);
        return TCL_ERROR;
    }
    if (get_data_type (interp, argv[1], &type)) return TCL_ERROR;
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp,
            "data type not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    if (extract_data (interp, argv[2], type, &nexp, &exps))
        return TCL_ERROR;
    if (nexp == 5) {
#if CGNS_VERSION >= 2400
        if (full) {
            free (exps);
            Tcl_AppendResult (interp, "expecting 8 exponents", NULL);
            return TCL_ERROR;
        }
#endif
        if (cg_exponents_write (type, exps)) {
            free (exps);
            return get_cg_error (interp, "cg_exponents_write");
        }
    }
#if CGNS_VERSION >= 2400
    else if (nexp == 8) {
        if (cg_expfull_write (type, exps)) {
            free (exps);
            return get_cg_error (interp, "cg_expfull_write");
        }
    }
#endif
    else {
        free (exps);
#if CGNS_VERSION >= 2400
        if (full)
            sprintf (buff, "expecting 8 exponents");
        else
            sprintf (buff, "expecting 5 or 8 exponents");
#else
        sprintf (buff, "expecting 5 exponents");
#endif
        Tcl_AppendResult (interp, buff, NULL);
        return TCL_ERROR;
    }

    free (exps);
    return TCL_OK;
}

#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_nexponents (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int nexps;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_nexponents (&nexps))
        return get_cg_error (interp, "cg_nexponents");

    sprintf (buff, "%d", nexps);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataConversion_t Nodes                            *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_conversion_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_conversion_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conversion_info");
    }

    Tcl_AppendResult (interp, cg_data_type_name(type), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conversion_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int n, ierr;
    DataType_t type;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_conversion_info (&type);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_conversion_info");
    }
    if (type == RealSingle) {
        float vals[2];
        if (cg_conversion_read (vals))
            return get_cg_error (interp, "cg_conversion_read");
        sprintf (buff, "%g %g", vals[0], vals[1]);
    }
    else if (type == RealDouble) {
        double vals[2];
        if (cg_conversion_read (vals))
            return get_cg_error (interp, "cg_conversion_read");
        sprintf (buff, "%g %g", vals[0], vals[1]);
    }
    else {
        Tcl_AppendResult (interp, "invalid conversion data type", NULL);
        return TCL_ERROR;
    }

    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_conversion_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    DataType_t type;
    double conv[2];

    Tcl_ResetResult (interp);
    if (argc != 3) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " datatype conversion", NULL);
        return TCL_ERROR;
    }
    if (get_data_type (interp, argv[1], &type)) return TCL_ERROR;
    if (type != RealSingle && type != RealDouble) {
        Tcl_AppendResult (interp,
            "data type not RealSingle or RealDouble", NULL);
        return TCL_ERROR;
    }
    if (2 != sscanf (argv[2], "%lf%lf", &conv[0], &conv[1])) {
        Tcl_AppendResult (interp,
            "conversion scale and offset not given", NULL);
        return TCL_ERROR;
    }

    if (type == RealSingle) {
        float vals[2];
        vals[0] = (float)conv[0];
        vals[1] = (float)conv[1];
        ierr = cg_conversion_write (RealSingle, vals);
    }
    else
        ierr = cg_conversion_write (RealDouble, conv);
    if (ierr)
        return get_cg_error (interp, "cg_conversion_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write DataClass_t Nodes                                 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_dataclass_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    DataClass_t dataclass;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_dataclass_read (&dataclass);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_dataclass_read");
    }

    Tcl_AppendResult (interp, cg_data_class_name(dataclass), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_dataclass_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int dataclass;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " dataclass", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "dataclass", argv[1], NofValidDataClass,
            DataClassName, &dataclass)) return TCL_ERROR;

    if (cg_dataclass_write ((DataClass_t)dataclass))
        return get_cg_error (interp, "cg_dataclass_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write GridLocation_t Nodes                              *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_gridlocation_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ierr;
    GridLocation_t location;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_gridlocation_read (&location);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_gridlocation_read");
    }

    Tcl_AppendResult (interp, cg_grid_location_name(location), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_gridlocation_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int location;

    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " location", NULL);
        return TCL_ERROR;
    }
    if (get_type (interp, "grid location", argv[1], NofValidGridLocation,
            GridLocationName, &location)) return TCL_ERROR;

    if (cg_gridlocation_write ((GridLocation_t)location))
        return get_cg_error (interp, "cg_gridlocation_write");

    return TCL_OK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Read and write Ordinal_t Nodes                                   *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_ordinal_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int ordinal, ierr;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    ierr = cg_ordinal_read (&ordinal);
    if (ierr) {
        if (ierr == CG_NODE_NOT_FOUND) return TCL_OK;
        return get_cg_error (interp, "cg_ordinal_read");
    }

    sprintf (buff, "%d", ordinal);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ordinal_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " ordinal", NULL);
        return TCL_ERROR;
    }

    if (cg_ordinal_write (atoi(argv[1])))
        return get_cg_error (interp, "cg_ordinal_write");

    return TCL_OK;
}
























#if CGNS_VERSION >= 2400

/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_info (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_ptset_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_SetResult (interp, "not implemented", TCL_STATIC);
    return TCL_ERROR;
}

#endif















/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Link Handling Functions - new in version 2.1                     *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if CGNS_VERSION >= 2100

static int tcl_cg_is_link (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    int len;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    if (cg_is_link (&len))
        return get_cg_error (interp, "cg_is_link");

    sprintf (buff, "%d", len);
    Tcl_AppendResult (interp, buff, NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_link_read (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    char *filename, *pathname;

    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }

    if (cg_link_read (&filename, &pathname))
        return get_cg_error (interp, "cg_link_read");

    Tcl_AppendElement (interp, filename);
    free (filename);
    Tcl_AppendElement (interp, pathname);
    free (pathname);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_link_write (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 4) {
        Tcl_AppendResult (interp, "usage: ", argv[0],
            " nodename filename nameinfile", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "node", argv[1])) return TCL_ERROR;
    if (strlen (argv[3]) < 1) {
        Tcl_AppendResult (interp, "nameinfile not given", NULL);
        return TCL_ERROR;
    }

    if (cg_link_write (argv[1], argv[2], argv[3]))
        return get_cg_error (interp, "cg_link_write");

    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      General Delete Function						 *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#if CGNS_VERSION >= 2200

static int tcl_cg_delete_node (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 2) {
        Tcl_AppendResult (interp, "usage: ", argv[0], " nodename", NULL);
        return TCL_ERROR;
    }
    if (check_name (interp, "node", argv[1])) return TCL_ERROR;

    if (cg_delete_node (argv[1]))
        return get_cg_error (interp, "cg_delete_node");

    return TCL_OK;
}

#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *      Error Handling Functions                                         *
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int tcl_cg_get_error (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    Tcl_AppendResult (interp, cg_get_error(), NULL);
    return TCL_OK;
}

/*-----------------------------------------------------------------------*/

static int tcl_cg_error_exit (
#ifdef PROTOTYPE
    ClientData data, Tcl_Interp *interp, int argc, char **argv)
#else
    data, interp, argc, argv)
ClientData data;
Tcl_Interp *interp;
int argc;
char **argv;
#endif
{
    Tcl_ResetResult (interp);
    if (argc != 1) {
        Tcl_AppendResult (interp, "usage: ", argv[0], NULL);
        return TCL_ERROR;
    }
    cg_error_exit();
    return TCL_OK;
}

/*---------- CGNStcl_Init ---------------------------------------
 * Initialize and create the commands
 *--------------------------------------------------------------*/

#if defined(_WIN32) && defined(BUILD_DLL)
__declspec(dllexport)
#endif
int Cgnstcl_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand (interp, "cg_library_version", tcl_cg_library_version, 0, 0);
    Tcl_CreateCommand (interp, "cg_get_names", tcl_cg_get_names, 0, 0);

    Tcl_CreateCommand (interp, "cg_open", tcl_cg_open, 0, 0);
    Tcl_CreateCommand (interp, "cg_version", tcl_cg_version, 0, 0);
    Tcl_CreateCommand (interp, "cg_close", tcl_cg_close, 0, 0);

    Tcl_CreateCommand (interp, "cg_nbases", tcl_cg_nbases, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_read", tcl_cg_base_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_id", tcl_cg_base_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_base_write", tcl_cg_base_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nzones", tcl_cg_nzones, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_read", tcl_cg_zone_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_type", tcl_cg_zone_type, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_id", tcl_cg_zone_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_zone_write", tcl_cg_zone_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nfamilies", tcl_cg_nfamilies, 0, 0);
    Tcl_CreateCommand (interp, "cg_family_read", tcl_cg_family_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_family_write", tcl_cg_family_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_famname_read", tcl_cg_famname_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_famname_write", tcl_cg_famname_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_fambc_read", tcl_cg_fambc_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_fambc_write", tcl_cg_fambc_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_geo_read", tcl_cg_geo_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_geo_write", tcl_cg_geo_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_part_read", tcl_cg_part_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_part_write", tcl_cg_part_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ngrids", tcl_cg_ngrids, 0, 0);
    Tcl_CreateCommand (interp, "cg_grid_read", tcl_cg_grid_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_grid_write", tcl_cg_grid_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ncoords", tcl_cg_ncoords, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_info", tcl_cg_coord_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_read", tcl_cg_coord_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_id", tcl_cg_coord_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_coord_write", tcl_cg_coord_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nsections", tcl_cg_nsections, 0, 0);
    Tcl_CreateCommand (interp, "cg_section_read", tcl_cg_section_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_elements_read", tcl_cg_elements_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_parent_data_read", tcl_cg_elements_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_section_write", tcl_cg_section_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_parent_data_write", tcl_cg_parent_data_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_npe", tcl_cg_npe, 0, 0);
    Tcl_CreateCommand (interp, "cg_ElementDataSize", tcl_cg_ElementDataSize, 0, 0);

    Tcl_CreateCommand (interp, "cg_nsols", tcl_cg_nsols, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_info", tcl_cg_sol_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_id", tcl_cg_sol_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_sol_write", tcl_cg_sol_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nfields", tcl_cg_nfields, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_info", tcl_cg_field_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_read", tcl_cg_field_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_id", tcl_cg_field_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_field_write", tcl_cg_field_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nholes", tcl_cg_nholes, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_info", tcl_cg_hole_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_read", tcl_cg_hole_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_id", tcl_cg_hole_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_hole_write", tcl_cg_hole_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_nconns", tcl_cg_nconns, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_info", tcl_cg_conn_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_read", tcl_cg_conn_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_donor_read", tcl_cg_conn_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_conn_id", tcl_cg_conn_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_write", tcl_cg_conn_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n1to1", tcl_cg_n1to1, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_read", tcl_cg_1to1_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_id", tcl_cg_1to1_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_write", tcl_cg_1to1_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n1to1_global", tcl_cg_n1to1_global, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_read_global", tcl_cg_1to1_read_global, 0, 0);

    Tcl_CreateCommand (interp, "cg_nbocos", tcl_cg_nbocos, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_info", tcl_cg_boco_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_read", tcl_cg_boco_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_normal_read", tcl_cg_boco_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_boco_id", tcl_cg_boco_id, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_write", tcl_cg_boco_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_boco_normal_write", tcl_cg_boco_normal_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_dataset_read", tcl_cg_dataset_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_dataset_write", tcl_cg_dataset_write, 0, 0);

#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_bcdataset_info", tcl_cg_bcdataset_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_bcdataset_write", tcl_cg_bcdataset_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_bcdataset_read", tcl_cg_bcdataset_read, 0, 0);
#endif

    Tcl_CreateCommand (interp, "cg_bcdata_write", tcl_cg_bcdata_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ndiscrete", tcl_cg_ndiscrete, 0, 0);
    Tcl_CreateCommand (interp, "cg_discrete_read", tcl_cg_discrete_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_discrete_write", tcl_cg_discrete_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n_rigid_motions", tcl_cg_n_rigid_motions, 0, 0);
    Tcl_CreateCommand (interp, "cg_rigid_motion_read", tcl_cg_rigid_motion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rigid_motion_write", tcl_cg_rigid_motion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_n_arbitrary_motions", tcl_cg_n_arbitrary_motions, 0, 0);
    Tcl_CreateCommand (interp, "cg_arbitrary_motion_read", tcl_cg_arbitrary_motion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_arbitrary_motion_write", tcl_cg_arbitrary_motion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_simulation_type_read", tcl_cg_simulation_type_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_simulation_type_write", tcl_cg_simulation_type_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_biter_read", tcl_cg_biter_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_biter_write", tcl_cg_biter_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ziter_read", tcl_cg_ziter_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ziter_write", tcl_cg_ziter_write, 0, 0);

#if CGNS_VERSION >= 2200
    Tcl_CreateCommand (interp, "cg_gravity_read", tcl_cg_gravity_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_gravity_write", tcl_cg_gravity_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_axisym_read", tcl_cg_axisym_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_axisym_write", tcl_cg_axisym_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_rotating_read", tcl_cg_rotating_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rotating_write", tcl_cg_rotating_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_bc_wallfunction_read", tcl_cg_bc_wallfunction_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_bc_wallfunction_write", tcl_cg_bc_wallfunction_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_bc_area_read", tcl_cg_bc_area_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_bc_area_write", tcl_cg_bc_area_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_conn_periodic_read", tcl_cg_conn_periodic_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_periodic_write", tcl_cg_conn_periodic_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_conn_average_read", tcl_cg_conn_average_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conn_average_write", tcl_cg_conn_average_write, 0, 0);

#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_1to1_periodic_read", tcl_cg_1to1_periodic_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_periodic_write", tcl_cg_1to1_periodic_write, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_average_read", tcl_cg_1to1_average_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_1to1_average_write", tcl_cg_1to1_average_write, 0, 0);
#endif
#endif

    Tcl_CreateCommand (interp, "cg_goto", tcl_cg_goto, 0, 0);
    Tcl_CreateCommand (interp, "cg_gorel", tcl_cg_gorel, 0, 0);
    Tcl_CreateCommand (interp, "cg_where", tcl_cg_where, 0, 0);

    Tcl_CreateCommand (interp, "cg_convergence_read", tcl_cg_convergence_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_convergence_write", tcl_cg_convergence_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_state_read", tcl_cg_state_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_state_write", tcl_cg_state_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_equationset_read", tcl_cg_equationset_read, 0, 0);
#if CGNS_VERSION >= 2100
    Tcl_CreateCommand (interp, "cg_equationset_chemistry_read", tcl_cg_equationset_chemistry_read, 0, 0);
#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_equationset_elecmagn_read", tcl_cg_equationset_elecmagn_read, 0, 0);
#endif
#endif
    Tcl_CreateCommand (interp, "cg_equationset_write", tcl_cg_equationset_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_governing_read", tcl_cg_governing_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_governing_write", tcl_cg_governing_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_diffusion_read", tcl_cg_diffusion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_diffusion_write", tcl_cg_diffusion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_model_read", tcl_cg_model_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_model_write", tcl_cg_model_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_narrays", tcl_cg_narrays, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_info", tcl_cg_array_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_read", tcl_cg_array_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_read_as", tcl_cg_array_read_as, 0, 0);
    Tcl_CreateCommand (interp, "cg_array_write", tcl_cg_array_write, 0, 0);

#if CGNS_VERSION >= 2100
    Tcl_CreateCommand (interp, "cg_nuser_data", tcl_cg_nuser_data, 0, 0);
    Tcl_CreateCommand (interp, "cg_user_data_read", tcl_cg_user_data_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_user_data_write", tcl_cg_user_data_write, 0, 0);
#endif

    Tcl_CreateCommand (interp, "cg_nintegrals", tcl_cg_nintegrals, 0, 0);
    Tcl_CreateCommand (interp, "cg_integral_read", tcl_cg_integral_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_integral_write", tcl_cg_integral_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_rind_read", tcl_cg_rind_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_rind_write", tcl_cg_rind_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ndescriptors", tcl_cg_ndescriptors, 0, 0);
    Tcl_CreateCommand (interp, "cg_descriptor_read", tcl_cg_descriptor_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_descriptor_write", tcl_cg_descriptor_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_units_read", tcl_cg_units_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_units_write", tcl_cg_units_write, 0, 0);
#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_nunits", tcl_cg_nunits, 0, 0);
    Tcl_CreateCommand (interp, "cg_unitsfull_read", tcl_cg_units_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_unitsfull_write", tcl_cg_units_write, (ClientData)1, 0);
#endif

    Tcl_CreateCommand (interp, "cg_exponents_info", tcl_cg_exponents_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_exponents_read", tcl_cg_exponents_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_exponents_write", tcl_cg_exponents_write, 0, 0);
#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_nexponents", tcl_cg_nexponents, 0, 0);
    Tcl_CreateCommand (interp, "cg_expfull_read", tcl_cg_exponents_read, (ClientData)1, 0);
    Tcl_CreateCommand (interp, "cg_expfull_write", tcl_cg_exponents_write, (ClientData)1, 0);
#endif

    Tcl_CreateCommand (interp, "cg_conversion_info", tcl_cg_conversion_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_conversion_read", tcl_cg_conversion_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_conversion_write", tcl_cg_conversion_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_dataclass_read", tcl_cg_dataclass_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_dataclass_write", tcl_cg_dataclass_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_gridlocation_read", tcl_cg_gridlocation_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_gridlocation_write", tcl_cg_gridlocation_write, 0, 0);

    Tcl_CreateCommand (interp, "cg_ordinal_read", tcl_cg_ordinal_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ordinal_write", tcl_cg_ordinal_write, 0, 0);

#if CGNS_VERSION >= 2400
    Tcl_CreateCommand (interp, "cg_ptset_info", tcl_cg_ptset_info, 0, 0);
    Tcl_CreateCommand (interp, "cg_ptset_read", tcl_cg_ptset_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_ptset_write", tcl_cg_ptset_write, 0, 0);
#endif

#if CGNS_VERSION >= 2100
    Tcl_CreateCommand (interp, "cg_is_link", tcl_cg_is_link, 0, 0);
    Tcl_CreateCommand (interp, "cg_link_read", tcl_cg_link_read, 0, 0);
    Tcl_CreateCommand (interp, "cg_link_write", tcl_cg_link_write, 0, 0);
#endif

#if CGNS_VERSION >= 2200
    Tcl_CreateCommand (interp, "cg_delete_node", tcl_cg_delete_node, 0, 0);
#endif

    Tcl_CreateCommand (interp, "cg_get_error", tcl_cg_get_error, 0, 0);
    Tcl_CreateCommand (interp, "cg_error_exit", tcl_cg_error_exit, 0, 0);

    return TCL_OK;
}

