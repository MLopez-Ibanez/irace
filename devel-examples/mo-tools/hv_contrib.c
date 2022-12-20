#include "hv.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/* It does not actually compute the contribution but HV_total - HV_i,
   where HV_total is the total HV and HV_i is the contribution of the
   point, that is, it actually computes the HV minus the point i. */
double *
hv_contrib (const double *points, int dim, int size, const double * ref,
            const bool * uev)
{
    bool keep_uevs = uev != NULL;

    double * hv = malloc (sizeof(double) * size);

    // FIXME: Avoid duplicating the points! Play tricks with pointers?
    double * data = malloc (sizeof(double) * (size + 1) * dim);

    // FIXME: Avoid so many memcpy, remove points from the top and add
    // them to the end.
    for (int i = 0; i < size; i++) {
        int pos = 0;
//        const double * pointi = &points[i * dim];
        for (int j = 0; j < size; j++) {
            const double * pointj = &points[j * dim];
            if (i == j) continue;
            memcpy (data + pos, pointj, sizeof(double) * dim);
            pos += dim;    
        }

        if (keep_uevs && uev[i]) {
            //assert (pointi[0] == ubound[0] || pointi[1] == ubound[1]);
            hv[i] = 0.0;
        } else 
            hv[i] = fpli_hv(data, dim, size - 1, ref);
    }
    free (data);
    return hv;
}

