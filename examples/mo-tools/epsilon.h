#ifndef EPSILON_H
#define EPSILON_H

#include "common.h"
#include "io.h"
#include <stdio.h>
#include <stdlib.h>
#include <float.h>

#define INFINITY (DBL_MAX)
#define NEG_INFINITY (-DBL_MAX)

/* 
   IMPLEMENTATION NOTE: Given objective vectors a and b,
   I_epsilon(a,b) is computed in the case of minimization as a/b for
   the multiplicative case (respectively, a - b for the additive
   case), whereas in the case of maximization it is computed as b/a
   for the multiplicative case (respectively, b - a for the additive
   case). This allows computing a single value for mixed optimization
   problems, where some objectives are to be maximized while others
   are to be minimized. Moreover, a lower value corresponds to a
   better approximation set, independently of the type of problem
   (minimization, maximization or mixed). However, the meaning of the
   value is different for each objective type. For example, imagine
   that f1 is to be minimized and f2 is to be maximized, and the
   multiplicative epsilon computed here for I_epsilon(A,B) = 3. This
   means that A needs to be multiplied by 1/3 for all f1 values and by
   3 for all f2 values in order to weakly dominate B.

   This also means that the computation of the multiplicative version
   for negative values doesn't make sense.
 */
static inline double 
epsilon_mult (int dim, const signed char *minmax,
              const double *points_a, int size_a,
              const double *points_b, int size_b)
{
    int a, b, d;
    double epsilon = 0;

    for (b = 0; b < size_b; b++) {
        double epsilon_min = INFINITY;
        for (a = 0; a < size_a; a++) {
            double epsilon_max = 0;
            for (d = 0; d < dim; d++) {
                double epsilon_temp;
                if (points_a[a * dim + d] <= 0 || points_b[b * dim + d] <= 0) {
                    errprintf ("cannot calculate multiplicative epsilon indicator with values <= 0\n.");
                    exit (EXIT_FAILURE);
                }
                if (minmax[d] < 0)
                    epsilon_temp =  points_a[a * dim + d] / points_b[b * dim + d];
                else if (minmax[d] > 0)
                    epsilon_temp =  points_b[b * dim + d] / points_a[a * dim + d];
                else 
                    epsilon_temp =  1;
                
                if (epsilon_temp < 0) {
                    errprintf ("cannot calculate multiplicative epsilon indicator with different signedness\n.");
                    exit (EXIT_FAILURE);
                }
                epsilon_max = MAX (epsilon_max, epsilon_temp);
            }
            epsilon_min = MIN (epsilon_min, epsilon_max);
        }
        epsilon = MAX (epsilon, epsilon_min);
    }
    return epsilon;
}

static inline double
epsilon_additive (int dim, const signed char *minmax,
                  const double *points_a, int size_a,
                  const double *points_b, int size_b)
{
    int a, b, d;
    double epsilon = NEG_INFINITY;

    for (b = 0; b < size_b; b++) {
        double epsilon_min = INFINITY;
        for (a = 0; a < size_a; a++) {
            double epsilon_max = NEG_INFINITY;
            for (d = 0; d < dim; d++) {
                double epsilon_temp;
                if (minmax[d] < 0)
                    epsilon_temp = points_a[a * dim + d] - points_b[b * dim + d];
                else if (minmax[d] > 0)
                    epsilon_temp = points_b[b * dim + d] - points_a[a * dim + d];
                else 
                    epsilon_temp = 0;
                epsilon_max = MAX (epsilon_max, epsilon_temp);
            }
            epsilon_min = MIN (epsilon_min, epsilon_max);
        }
        epsilon = MAX (epsilon, epsilon_min);
    }
    return epsilon;
}

/* FIXME: this can be done much faster. For example, the diff needs to
   be calculated just once and stored on a temporary array diff[].  */
static inline int
epsilon_additive_ind (int dim, const signed char *minmax,
                      const double *points_a, int size_a,
                      const double *points_b, int size_b)
{
    double eps_ab, eps_ba;

    eps_ab = epsilon_additive (dim, minmax, points_a, size_a, points_b, size_b);
    eps_ba = epsilon_additive (dim, minmax, points_b, size_b, points_a, size_a);

    DEBUG2 (printf ("eps_ab = %g, eps_ba = %g\n", eps_ab, eps_ba));

    if (eps_ab <= 0 && eps_ba > 0)
        return -1;
    else if (eps_ab > 0 && eps_ba <= 0)
        return 1;
    else
        return 0;
}

#endif /* EPSILON_H */
