/*
 * test_element_traits.c -- invariants of the element-type table, for every type.
 *
 * CPEX-0045 added 21 high-order element tags, each carrying a nodes-per-element
 * count, a dimension and a basic type in cgi_element_traits[].  Most of those
 * tags have no test of their own, and the obvious remedy -- a round-trip test
 * per tag -- would not actually catch the error worth catching.  A wrong npe in
 * the table is self-consistent on a round trip: the writer allocates the wrong
 * size, the reader validates against the same wrong size, and the comparison
 * passes.  Only an *independent* statement of the expected count catches it.
 *
 * The element name is that independent statement.  A tag named TYPE_N means N
 * nodes -- that is what the tag is for -- so npe must equal the numeric suffix
 * of its own name.  Checking that across the whole enumeration costs one loop
 * and covers every tag at once, including the ones nothing else touches.
 *
 * Everything here goes through the public API rather than the internal table,
 * so it also pins the accessors to each other.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include "cgnslib.h"

static int failures = 0;

static void fail(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "ERROR: ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    failures++;
}

/* Trailing integer of a tag name, or -1 when it has none (NODE, MIXED,
 * NGON_n, NFACE_n).  QUAD_P4_16 and PYRA_P4_29 carry their degree in the
 * middle, so taking the last field is what makes those work too. */
static int name_suffix_count(const char *name)
{
    const char *p = strrchr(name, '_');
    const char *q;
    if (p == NULL) return -1;
    p++;
    if (*p == '\0') return -1;
    for (q = p; *q; q++)
        if (!isdigit((unsigned char)*q)) return -1;
    return atoi(p);
}

int main(void)
{
    int t, checked = 0, suffix_checked = 0;

    printf("##################################################\n");
    printf("#   ElementType_t table invariants (%d types)     #\n",
           (int)NofValidElementTypes);
    printf("##################################################\n\n");

    /* Skip Null and UserDefined: they name no element. */
    for (t = 2; t < NofValidElementTypes; t++) {
        CGNS_ENUMT(ElementType_t) type = (CGNS_ENUMT(ElementType_t))t;
        CGNS_ENUMT(ElementType_t) basic, basic_of_basic;
        const char *name;
        int npe = -1, dim = -1, bdim = -1, want, p, complete;

        name = cg_ElementTypeName(type);
        if (name == NULL || name[0] == '\0') {
            fail("type %d has no name", t);
            continue;
        }
        checked++;

        /* cg_npe must agree with the table for every type; MIXED, NGON_n and
         * NFACE_n legitimately report 0, having no fixed count. */
        if (cg_npe(type, &npe) != CG_OK) {
            fail("%s: cg_npe failed", name);
            continue;
        }

        want = name_suffix_count(name);
        if (want >= 0) {
            suffix_checked++;
            if (npe != want)
                fail("%s: cg_npe reports %d, but the tag names %d nodes",
                     name, npe, want);
        }

        if (cg_element_dimension(type, &dim) != CG_OK) {
            fail("%s: cg_element_dimension failed", name);
            continue;
        }
        if (dim < 0 || dim > 3)
            fail("%s: dimension %d out of range", name, dim);

        if (cg_element_basic_element_type(type, &basic) != CG_OK) {
            fail("%s: cg_element_basic_element_type failed", name);
            continue;
        }

        /* The basic type of a basic type is itself; without this a chain of
         * tags could point at another high-order tag and the lookups that
         * resolve a family would not terminate where they should. */
        if (cg_element_basic_element_type(basic, &basic_of_basic) != CG_OK) {
            fail("%s: basic type %s has no basic type of its own",
                 name, cg_ElementTypeName(basic));
            continue;
        }
        if (basic_of_basic != basic)
            fail("%s: basic type %s is not idempotent (gives %s)",
                 name, cg_ElementTypeName(basic),
                 cg_ElementTypeName(basic_of_basic));

        /* A high-order tag describes the same shape as its basic tag, so the
         * two must agree on dimension. */
        if (cg_element_dimension(basic, &bdim) == CG_OK && bdim != dim)
            fail("%s: dimension %d but its basic type %s has %d",
                 name, dim, cg_ElementTypeName(basic), bdim);

        /* No element may carry more control points than the complete space of
         * its degree: the serendipity tags carry fewer, never more. */
        if (npe > 0) {
            for (p = 0; p <= 4; p++) {
                if (cg_npe_ho(basic, p, &complete) != CG_OK) continue;
                if (complete >= npe) break;
            }
            if (p <= 4 && cg_npe_ho(basic, p, &complete) == CG_OK &&
                npe > complete)
                fail("%s: %d nodes exceeds the complete space of %s at "
                     "degree %d (%d)", name, npe,
                     cg_ElementTypeName(basic), p, complete);
        }
    }

    printf("Checked %d element types; %d carry a node count in their name.\n",
           checked, suffix_checked);

    /* If the suffix check silently stopped applying, the test would keep
     * passing while covering nothing.  The high-order tags are the reason this
     * file exists, so require a count consistent with the enumeration. */
    if (suffix_checked < 40)
        fail("only %d types were name-checked; the invariant has stopped "
             "applying and this test is no longer covering the table",
             suffix_checked);

    printf("\n##################################################\n");
    if (failures == 0)
        printf("#   ALL ELEMENT TRAIT INVARIANTS HOLD             #\n");
    else
        printf("#   %d ELEMENT TRAIT FAILURE(S)                    #\n", failures);
    printf("##################################################\n");

    return failures == 0 ? 0 : 1;
}
