/*************************************************************************

 ndsort: Perform nondominated sorting in a list of points.

 ---------------------------------------------------------------------
                       Copyright (c) 2011
          Manuel Lopez-Ibanez  <manuel.lopez-ibanez@ulb.ac.be>

 This program is free software (software libre); you can redistribute
 it and/or modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, you can obtain a copy of the GNU
 General Public License at <http://www.gnu.org/copyleft/gpl.html>

 ----------------------------------------------------------------------
  IMPORTANT NOTE: Please be aware that the fact that this program is
  released as Free Software does not excuse you from scientific
  propriety, which obligates you to give appropriate credit! If you
  write a scientific paper describing research that made substantive
  use of this program, it is your obligation as a scientist to
  acknowledge its use.  Moreover, as a personal note, I would
  appreciate it if you would email manuel.lopez-ibanez@ulb.ac.be with
  citations of papers referencing this work so I can mention them to
  my funding agent and tenure committee.
 ---------------------------------------------------------------------

 Literature: 

*************************************************************************/

#include "io.h"
#include "nondominated.h" // for normalise()

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h> // for isspace()

#include <unistd.h>  // for getopt()
#include <getopt.h> // for getopt_long()
#include <math.h>  // for INFINITY

char *program_invocation_short_name = "ndsort";

static void usage(void)
{
    printf("\n"
           "Usage: %s [OPTIONS] [FILE...]\n\n", program_invocation_short_name);

    printf(
"Perform nondominated sorting in a list of points.                        \n\n"

"Options:\n"
" -h, --help          give  this summary and exit.                          \n"
"     --version       print version number and exit.                        \n"
" -v, --verbose       print some information (time, number of points, etc.) \n"
" -q, --quiet         print as little as possible                           \n"
//" -H, --hypervolume   use hypervolume contribution to break ties            \n"
" -k, --keep-uevs     keep uniquely extreme values                          \n"
" -o, --obj [+|-]...  specify whether each objective should be              \n"
"                     minimised (-) or maximised (+)                        \n"
"\n");
}

static void version(void)
{
    printf("%s version %s (optimised for %s)\n\n",
           program_invocation_short_name, VERSION, MARCH);
    printf(
"Copyright (C) 2011"
"\nManuel Lopez-Ibanez  <manuel.lopez-ibanez@ulb.ac.be>\n"
"\n"
"This is free software, and you are welcome to redistribute it under certain\n"
"conditions.  See the GNU General Public License for details. There is NO   \n"
"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
"\n"        );
}

static bool verbose_flag = false;

static inline void
handle_read_data_error (int err, const char *filename)
{
    switch (err) {
    case 0: /* No error */
        break;

    case READ_INPUT_FILE_EMPTY:
        errprintf ("%s: no input data.", filename);
        exit (EXIT_FAILURE);

    case READ_INPUT_WRONG_INITIAL_DIM:
        errprintf ("check the argument of -o, --obj\n.");
    default:
        exit (EXIT_FAILURE);
    }
}

struct point_2d_front {
    double p[2];
    unsigned int i;
    unsigned int f;
};

static int
point_2d_front_cmp (const void * a, const void * b)
{   
    const double *pa = ((const struct point_2d_front *)a)->p;
    const double *pb = ((const struct point_2d_front *)b)->p;

    const double x0 = pa[0];
    const double x1 = pa[1];
    const double y0 = pb[0];
    const double y1 = pb[1];

    /* FIXME: Use ?: */
   if (x0 < y0)
        return -1;
    else if (x0 > y0)
        return 1;
    else if (x1 < y1)
        return -1;
    else if (x1 > y1)
        return 1;
    else
        return 0;
}

/*
   Nondominated sorting in 2D in O(n log n) from:

   M. T. Jensen. Reducing the run-time complexity of multiobjective
   EAs: The NSGA-II and other algorithms. IEEE Transactions on
   Evolutionary Computation, 7(5):503–515, 2003.

   FIXME: Could we avoid creating a copy of the points?
*/
static int *
pareto_ranking (const double *points, int dim, int size)
{
    int k;

    assert (dim == 2);

    struct point_2d_front *data = malloc(sizeof(struct point_2d_front) * size);
    for (k = 0; k < size; k++) {
        data[k].p[0] = points[k * dim];
        data[k].p[1] = points[k * dim + 1];
        data[k].i = (unsigned) k;
        data[k].f = 0;
    }

#if DEBUG >= 2
#define PARETO_RANKING_2D_DEBUG
    double *help_0 = malloc (size * sizeof(double));
    double *help_1 = malloc (size * sizeof(double));
    int *   help_i = malloc (size * sizeof(int));

    for (k = 0; k < size; k++) {
        help_0[k] = data[k].p[0];
        help_1[k] = data[k].p[1];
        help_i[k] = data[k].i;
    }
    fprintf(stderr, "%s():\n-------------------\n>>INPUT:", __FUNCTION__);
    fprintf(stderr, "\nIndex: "); vector_int_fprintf_fmt (stderr, help_i, size, "%4d"); 
    fprintf(stderr, "\n[0]  : "); vector_fprintf (stderr, help_0, size);
    fprintf(stderr, "\n[1]  : "); vector_fprintf (stderr, help_1, size);
#endif

    qsort (data, size, sizeof(struct point_2d_front), point_2d_front_cmp);

#ifdef PARETO_RANKING_2D_DEBUG 
    for (k = 0; i < size; k++) {
       help_0[k] = data[k].p[0];
       help_1[k] = data[k].p[1];
       help_i[k] = data[k].i;
    }
    fprintf(stderr, "%s():\n-------------------\n>>SORTED:", __FUNCTION__);
    fprintf(stderr, "\nIndex: "); vector_int_fprintf_fmt (stderr, help_i, size, "%4d"); 
    fprintf(stderr, "\n[0]  : "); vector_fprintf (stderr, help_0, size);
    fprintf(stderr, "\n[1]  : "); vector_fprintf (stderr, help_1, size);
#endif

    int n_front = 0;
    int * front_last = malloc (size * sizeof(int));
    front_last[0] = 0;
    data[0].f = 0; /* The first point is in the first front. */

    for (k = 1; k < size; k++) {
        double p0 = data[k].p[0];
        double p1 = data[k].p[1];
        if (p1 < data[front_last[n_front]].p[1]) {
            int low = 0;
            int high = n_front + 1;
            while (low < high) {
                int mid = low + ((high - low)/2);
                assert (mid <= n_front);
                if (p1 >= data[front_last[mid]].p[1])
                    low = mid + 1;
                else
                    high = mid;
            }
            assert (low <= n_front);
            assert (p1 < data[front_last[low]].p[1]);
            front_last[low] = k;
            data[k].f = low;
        } else if (p1 == data[front_last[n_front]].p[1]
                   && p0 == data[front_last[n_front]].p[0]) {
            front_last[n_front] = k;
            data[k].f = n_front;
        } else {
            n_front++;
            front_last[n_front] = k;
            data[k].f = n_front;
        }
    }
    free (front_last);

#ifdef PARETO_RANKING_2D_DEBUG
    {
        n_front++; // count max + 1
        int f, i;
        int *front_size = calloc(nfront, sizeof(int));
        int ** front = calloc(nfront, sizeof(int *));
        for (k = 0; k < size; k++) {
            f = data[k].f;
            if (front_size[f] == 0) {
                front[f] = malloc (size * sizeof(int));
            }
            front[f][front_size[f]] = k;
            front_size[f]++;
        }
        int *order = malloc (size * sizeof(int));
        f = 0, k = 0, i = 0;
        do {
            order[i] = front[f][k];
            fprintf (stderr, "\nfront[%d][%d] = %d = { %g , %g, %d, %d }",
                     f, k, front[f][k],
                     data[front[f][k]].p[0], data[front[f][k]].p[1],
                     data[front[f][k]].i, data[front[f][k]].f);
            i++, k++;
            if (k == front_size[f]) { f++; k = 0; }
        } while (f != n_front);

        for (f = 0; f < n_front; f++)
            free(front[f]);
        free(front);
        free(front_size);

        for (k = 0; i < size; k++) {
            help_0[k] = data[order[k]].p[0];
            help_1[k] = data[order[k]].p[1];
            help_i[k] = data[order[k]].i;
        }
        fprintf(stderr, "%s():\n-------------------\n>>OUTPUT:", __FUNCTION__);
        fprintf(stderr, "\nIndex: "); vector_int_fprintf_fmt (stderr, help_i, size, "%4d"); 
        fprintf(stderr, "\n[0]  : "); vector_fprintf (stderr, help_0, size);
        fprintf(stderr, "\n[1]  : "); vector_fprintf (stderr, help_1, size);

        free (order);

    }
    free (help_0);
    free (help_1);
    free (help_i);
    exit(1);
#endif    

    int * rank = malloc(size * sizeof(int));
    for (k = 0; k < size; k++) {
        rank[data[k].i] = data[k].f + 1;
    }
    free (data);
    return rank;
}

static void
fprint_rank (FILE * stream, const int * rank, int size) 
{
    int k;
    for (k = 0; k < size; k++) {
        fprintf (stream, "%d\n", rank[k]);
    }
}

static void fprint_vector_double (FILE * stream, const double * vec, int size)
{
    for (int k = 0; k < size; k++)
        fprintf (stream, "%g\n", vec[k]);
}

#include "hv_contrib.h"

static bool *
calculate_uev (bool *uev, const double *points, int dim, int size,
               const double *lbound, const double *ubound)
{
    if (uev == NULL) {
        uev = malloc (sizeof(bool) * size);
    }

    for (int j = 0; j < size; j++)
        uev[j] = false;

    for (int i = 0; i < dim; i++) { 
        assert (ubound[i] > -INFINITY);
        assert (lbound[i] < INFINITY);
        for (int j = 0; j < size; j++) {
            if (points[j * dim + i] == ubound[i]) {
                uev[j] = true;
                break;
            }
        }
        for (int j = 0; j < size; j++) {
            if (points[j * dim + i] == lbound[i]) {
                uev[j] = true;
                break;
            }
        }
    }
    return uev;
}

int main(int argc, char *argv[])
{
    int nsets = 0;
    int *cumsizes = NULL;
    double *points = NULL;
    int dim = 0;
    const char *filename;
    int numfiles;
    const signed char *minmax = NULL;
    bool only_rank_flag = false;
//    bool hypervolume_flag = false;
    bool keep_uevs_flag = false;

    int opt; /* it's actually going to hold a char */
    int longopt_index;
    /* see the man page for getopt_long for an explanation of these fields */
    static struct option long_options[] = {
        {"help",       no_argument,       NULL, 'h'},
        {"version",    no_argument,       NULL, 'V'},
        {"verbose",    no_argument,       NULL, 'v'},
        {"quiet",      no_argument,       NULL, 'q'},
//        {"hypervolume",no_argument,       NULL, 'H'},
        {"keep-uevs",  no_argument,       NULL, 'k'},
        {"rank",       no_argument,       NULL, 'r'},
        {"obj",        required_argument, NULL, 'o'},

        {NULL, 0, NULL, 0} /* marks end of list */
    };

    while (0 < (opt = getopt_long(argc, argv, "hVvqkro:",
                                  long_options, &longopt_index))) {
        switch (opt) {
        case 'V': // --version
            version();
            exit(EXIT_SUCCESS);

        case 'q': // --quiet
            verbose_flag = false;
            break;

        case 'v': // --verbose
            verbose_flag = true;
            break;

        case 'r': // --rank
            only_rank_flag = true;
            break;

        case 'k': // --keep-uevs
            keep_uevs_flag = true;
            break;

        case 'o': // --obj
            minmax = read_minmax (optarg, &dim);
            if (minmax == NULL) {
                fprintf(stderr, "%s: invalid argument '%s' for -o, --obj\n",
                        program_invocation_short_name,optarg);
                exit(EXIT_FAILURE);
            }
            break;

        case '?':
            // getopt prints an error message right here
            fprintf(stderr, "Try `%s --help' for more information.\n",
                    program_invocation_short_name);
            exit(EXIT_FAILURE);
        case 'h':
            usage();
            exit(EXIT_SUCCESS);
        default: // should never happen
            abort();
        }
    }

    numfiles = argc - optind;
    
    if (numfiles <= 0) {/* No input files: read stdin.  */
        filename = NULL;
    } else if (numfiles == 1) {
        filename = argv[optind];
    } else {
        errprintf ("more than one input file not handled yet.");
        exit(EXIT_FAILURE);
    }
    
    /* FIXME: Instead of this strange call, create a wrapper read_data_robust. */
    handle_read_data_error (
        read_data (filename, &points, &dim, &cumsizes, &nsets), filename);
    const int size = cumsizes[0] = cumsizes[nsets - 1];
    nsets = 1;
    if (!filename) filename = stdin_name;

    /* Default minmax if not set yet.  */
    if (minmax == NULL)
        minmax = read_minmax (NULL, &dim);

    if (verbose_flag) {
        printf ("# file: %s\n", filename);
        printf ("# points: %d\n", size);
    }

    int * rank = pareto_ranking (points, dim, size);


    if (only_rank_flag) {

        fprint_rank (stdout, rank, size);

    } else {
        bool *uev = NULL;
        static const double upper_range = 0.9;
        static const double lower_range = 0.0;

        double * order = malloc (sizeof(double) * size);
        int max_rank = 0;
        for (int k = 0; k < size; k++) {
            if (rank[k] > max_rank) max_rank = rank[k];
            order[k] = rank[k];
        }

        double * data = malloc (sizeof(double) * size * dim);
        double * lbound = malloc(sizeof(double) * dim);
        double * ubound = malloc(sizeof(double) * dim);
        double * ref = malloc(sizeof(double) * dim);
        
        for (int d = 0; d < dim; d++) 
            ref[d] = 1.0;
        
        max_rank = 1;
        for (int i = 1; i <= max_rank; i++) {
            for (int d = 0; d < dim; d++) {
                lbound[d] = INFINITY;
                ubound[d] = -INFINITY;
            }
            int data_size = 0;
            for (int k = 0; k < size; k++) {
                if (rank[k] != i) continue;
                const double *src = points + k * dim;
                memcpy (data + data_size * dim, src, sizeof(double) * dim);
                data_size++;
                for (int d = 0; d < dim; d++) {
                    if (lbound[d] > src[d]) lbound[d] = src[d];
                    if (ubound[d] < src[d]) ubound[d] = src[d];
                }
            }

            uev = calculate_uev (uev, data, dim, data_size, lbound, ubound);

            normalise (data, dim, data_size, minmax, AGREE_NONE,
                       lower_range, upper_range,
                       lbound, ubound);
            
            double *hv = hv_contrib (data, dim, data_size, ref, 
                                     keep_uevs_flag ? uev : NULL);
            // FIXME: This could also be done maintaining a list of
            // indices into the complete list of points.
            for (int k = 0, j = 0; k < size; k++) {
                if (rank[k] != i) continue;
                order[k] += hv[j++]; // FIXME: hv is not actually the HV contribution of j, but the hypervolume minus its contribution.
            }
            free (hv);
        }
        free (data);
        free (lbound);
        free (ubound);
        free (ref);
        fprint_vector_double (stdout, order, size);
        free (order);
    }

    free (rank);
    free (cumsizes);
    free (points);

    return 0;
}
