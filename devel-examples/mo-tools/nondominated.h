#ifndef NONDOMINATED_H
#define NONDOMINATED_H

#include "common.h"
#include <assert.h>

enum objs_agree_t { AGREE_MINIMISE = -1, AGREE_NONE = 0, AGREE_MAXIMISE = 1 };

static inline int
find_nondominated_set_ (const double *points, int dim, int size,
                        const signed char *minmax, const signed char agree,
                        bool *nondom, bool find_dominated_p)
{
    int j, k, d;
    int new_size = 0;

    for (k = 0; k < size - 1; k++){
        for (j = k + 1; j < size; j++){
            bool j_leq_k, k_leq_j;
            const double *pk;
            const double *pj;

            if (!nondom[k])
                break;
            if (!nondom[j])
                continue;

            k_leq_j = j_leq_k = true;

            pk = points + k * dim;
            pj = points + j * dim;

            /* FIXME: As soon as j_leq_k and k_leq_j become false,
               neither k or j will be removed, so break.  */
            /* FIXME: Do not handle agree here, assume that objectives
               have been fixed already to agree on
               minimization/maximization.  */
            if (agree < 0) {
                for (d = 0; d < dim; d++) {
                    j_leq_k = j_leq_k && (pj[d] <= pk[d]);
                    k_leq_j = k_leq_j && (pk[d] <= pj[d]);
                }
            } else if (agree > 0) {
                for (d = 0; d < dim; d++) {
                    j_leq_k = j_leq_k && (pj[d] >= pk[d]);
                    k_leq_j = k_leq_j && (pk[d] >= pj[d]);
                }
            } else {
                for (d = 0; d < dim; d++) {
                    if (minmax[d] < 0) {
                        j_leq_k = j_leq_k && (pj[d] <= pk[d]);
                        k_leq_j = k_leq_j && (pk[d] <= pj[d]);
                    } else if (minmax[d] > 0) {
                        j_leq_k = j_leq_k && (pj[d] >= pk[d]);
                        k_leq_j = k_leq_j && (pk[d] >= pj[d]);
                    }
                }
            }
            /* k is removed if it is weakly dominated by j. j is
               removed if it is dominated by k. */
            nondom[k] = !j_leq_k;
            nondom[j] = (!k_leq_j || j_leq_k);

            assert(nondom[k] || nondom[j]); /* both cannot be removed.  */

            if (find_dominated_p && (!nondom[k] || !nondom[j])) {
                return nondom[k] ? j : k;
            }

        }
    }

    if (find_dominated_p) return -1;

    for (k = 0; k < size; k++)
        if (nondom[k]) new_size++;

    return new_size;
}

static inline int
find_dominated_point (const double *points, int dim, int size,
                      const signed char *minmax)
{
    int k, pos;
    bool *nondom =  malloc (sizeof(bool) * size);

    for (k = 0; k < size; k++)
        nondom[k] = true;

    pos = find_nondominated_set_ (points, dim, size, minmax,
                                  AGREE_NONE, nondom, 
                                  /* find_dominated_p = */true);
    free (nondom);
    return pos;
}

static inline int
find_nondominated_set_agree (const double *points, int dim, int size,
                             const signed char *minmax, const signed char agree,
                             bool *nondom)
{
    return find_nondominated_set_ (points, dim, size, minmax, agree, nondom, 
                                   false);
}

static inline int
find_nondominated_set (const double *points, int dim, int size,
                       const signed char *minmax, bool *nondom)
{
    return find_nondominated_set_ (points, dim, size, minmax, AGREE_NONE,
                                   nondom, false);
}

static inline void
normalise (double *points, int dim, int size,
           const signed char *minmax, signed char agree,
           const double lower_range, const double upper_range,
           const double *lbound, const double *ubound)
{
    int k, d;
    const double range = upper_range - lower_range;
    double *diff;

    diff = malloc (dim * sizeof(double));
    for (d = 0; d < dim; d++) {
        diff[d] = ubound[d] - lbound[d];
        if (diff[d] == 0.0) // FIXME: Should we use approximate equality?
            diff[d] = 1; // FIXME: Do we need to handle agree?
    }

    for (k = 0; k < size; k++) {
        double *p = points + k * dim;
        for (d = 0; d < dim; d++)
            if ((agree > 0 && minmax[d] < 0)
                || (agree < 0 && minmax[d] > 0))
                p[d] = lower_range + range * (ubound[d] + p[d]) / diff[d];
            else
                p[d] = lower_range + range * (p[d] - lbound[d]) / diff[d];
    }

    free (diff);
}


#endif /* NONDOMINATED_H */
