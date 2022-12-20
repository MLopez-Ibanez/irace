/*************************************************************************

 Calculates the number of Pareto sets from one file that
 dominate the Pareto sets of the other files.

 ---------------------------------------------------------------------

                       Copyright (c) 2007-2014
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
 General Public License at:
                 http://www.gnu.org/copyleft/gpl.html
 or by writing to:
           Free Software Foundation, Inc., 59 Temple Place,
                 Suite 330, Boston, MA 02111-1307 USA

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

 [1] Eckart Zitzler, Lothar Thiele, Marco Laumanns, Carlos M. Fonseca
     and Viviane Grunert da Fonseca. "Performance assessment of
     multiobjective optimizers: an analysis and review," Evolutionary
     Computation, IEEE Transactions on , vol.7, no.2, pp. 117-132,
     April 2003.

 [2] Manuel Lopez-Ibanez, Luis Paquete, and Thomas Stutzle. Hybrid
     population-based algorithms for the bi-objective quadratic
     assignment problem.  Journal of Mathematical Modelling and
     Algorithms, 5(1):111-137, April 2006.

*************************************************************************/

#include "epsilon.h"
#include "nondominated.h"

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h> // for isspace()

#include <unistd.h>  // for getopt()
#include <getopt.h> // for getopt_long()

char *program_invocation_short_name = "dominatedsets";

void usage(void)
{
    printf("\n"
           "Usage: %s [OPTIONS] [FILE...]\n\n", program_invocation_short_name);

    printf(
"Calculates the number of Pareto sets from one file that                    \n"
"dominate the Pareto sets of the other files.                             \n\n"

"Options:\n"
" -h, --help          give  this summary and exit.                          \n"
"     --version       print version number and exit.                        \n"
" -v, --verbose       print some information (time, number of points, etc.) \n"
" -q, --quiet         print as little as possible                           \n"
" -p, --percentages   print results also as percentages.                    \n"
"     --no-check      do not check nondominance of sets (faster but unsafe).\n"
" -o, --obj [+|-]...  specify whether each objective should be              \n"
"                     minimised (-) or maximised (+)                        \n"
"\n");
}

void version(void)
{
    printf("%s version %s (optimised for %s)\n\n",
           program_invocation_short_name, VERSION, MARCH);
    printf(
"Copyright (C) 2007-2014"
"\nManuel Lopez-Ibanez  <manuel.lopez-ibanez@ulb.ac.be>\n"
"\n"
"This is free software, and you are welcome to redistribute it under certain\n"
"conditions.  See the GNU General Public License for details. There is NO   \n"
"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
"\n"        );
}

static bool verbose_flag = false;
static bool percentages_flag = false;
static bool check_flag = true;

void 
print_results (char **filenames, int numfiles, int *nruns, int **results)
{
    int k,j;
    int max_col_len = 0;
    int max_filename_len = 0;
    int max_result = 0;
    char buffer[32];

    /* longest filename.  */
    for (k = 0; k < numfiles; k++)
        max_filename_len = MAX (max_filename_len, (int) strlen (filenames[k]));

    /* longest number.  */
    for (k = 0; k < numfiles; k++)
        for (j = 0; j < numfiles; j++)
            max_result = MAX (max_result, results[k][j]);
    snprintf(buffer, 32, "%d", max_result);
    buffer[31] = '\0';
    max_col_len = MAX (max_filename_len, (int) strlen(buffer));
    
    printf("\n\n"
           "Number of times that <row> is better than <column>:\n");

    /* Header row.  */
    printf ("\n%*s", max_filename_len, "");
    for (k = 0; k < numfiles; k++) {
        printf (" %*s", max_col_len, filenames[k]);
    }
    
    for (k = 0; k < numfiles; k++) {
        printf("\n%*s", max_filename_len, filenames[k]);
        for (j = 0; j < numfiles; j++) {
            if (k == j)
                printf (" %*s", max_col_len, "--");
            else
                printf (" %*d", max_col_len, results[k][j]);
        }
    }

    printf ("\n");
    
    if (percentages_flag) {
        max_col_len = MAX (max_col_len, (int) strlen("100.0"));

        printf("\n\n"
               "Percentage of times that <row> is better than <column>:\n");

        /* Header row.  */
        printf ("\n%*s", max_filename_len, "");
        for (k = 0; k < numfiles; k++) {
            printf (" %*s", max_col_len, filenames[k]);
        }

        for (k = 0; k < numfiles; k++) {
            printf("\n%*s", max_filename_len, filenames[k]);
            for (j = 0; j < numfiles; j++) {
                if (k == j)
                    printf (" %*s", max_col_len, "--");
                else
                    printf (" %*.1f", max_col_len, 
                            results[k][j] * 100.0 / (nruns[k] * nruns[j]));
            }
        }
    }
    printf ("\n\n");

    printf ("Ranks:");
    for (k = 0; k < numfiles; k++) {
        int rank = 0;
        for (j = 0; j < numfiles; j++) {
            if (k == j) continue;
            rank += results[j][k];
        }
        printf (" %3d", rank);
    }
    printf ("\n");
}

static inline int 
dominance (int dim, const double *a, const double *b, const signed char *minmax)
/***********************************************
 return: O  -> if a == b
         1  -> if a dominates b
         -1 -> if a NOT dominates b

***********************************************/
{
    int d;

    /* If any objective is worse, A can't dominate B. */
    for (d = 0; d < dim; d++)
        if ((minmax[d] < 0 && a[d] > b[d])
            || (minmax[d] > 0 && a[d] < b[d])) 
            return -1;

    /* If any objective is better, then A dominates B. */
    for (d = 0; d < dim; d++) 
        if ((minmax[d] < 0 && a[d] < b[d])
            || (minmax[d] > 0 && a[d] > b[d]))
            return 1;

    return 0;
}

static void printf_point (const double *points, int i, int dim)
{
    int d;
    for (d = 0; d < dim; d++)
        printf( " %.4g", points[i * dim + d]);
    printf("\n");
}

static int set_dominates (int dim, const signed char *minmax,
                          const double *points_x, int size_x,
                          const double *points_y, int size_y)
{
    bool x_dominates_y = false, 
        x_weakly_dominates_y = false;

    for (int y = 0; y < size_y; y++) {
        x_weakly_dominates_y = false;
        for (int x = 0; x < size_x; x++) {
            DEBUG1 (
                printf ("X:");
                printf_point (points_x, x, dim);
                printf ("Y:");
                printf_point (points_y, y, dim);
                );
            int result = dominance (dim, &points_x[x * dim], 
                                    &points_y[y * dim], minmax);
            if (result == 1) {
                DEBUG1 (printf ("X dominates Y!\n"));
                x_weakly_dominates_y = true;
                x_dominates_y = true;
                break; 

            } else if (result == 0) {
                DEBUG1 (printf ("X weakly dominates Y!\n"));
                x_weakly_dominates_y = true;
                break; 
            }
        }
        if (!x_weakly_dominates_y)
            break;
    }

    if (!x_weakly_dominates_y)
        return 1;
    else if (size_x != size_y || x_dominates_y)
        return -1;
    else
        return 0;
}

int 
pareto_better (int dim, const signed char *minmax,
               const double *points_a, int size_a,
               const double *points_b, int size_b)
{
    int result = set_dominates (dim, minmax, points_a, size_a, points_b, size_b);
    if (result == 1) {
        DEBUG1 (printf ("Trying with B\n"));
        result = set_dominates (dim, minmax, points_b, size_b, points_a, size_a);
        result = -result;
        if (result != 1) {
            DEBUG1(printf("A || B\n"));
            result = 0;
        }
    }

    int result2 = epsilon_additive_ind (dim, minmax, points_a, size_a, points_b, size_b);

    DEBUG2 (
        printf ("result = %d, result2 = %d\n", result, result2);
        for (int a = 0; a < size_a; a++) {
            printf_point (points_a, a, dim);
        }
        printf("\n\n");
        for (int b = 0; b < size_b; b++) {
            printf_point (points_b, b, dim);
        });
    if (result != result2) {
        printf ("result = %d  !=  result2 = %d\n", result, result2);
        abort();
    }
    return result;
}

void 
cmpparetos (int dim, const signed char *minmax,
            const double * points_a, int nruns_a, 
            const int *cumsizes_a, int *numbetter_a,
            const double * points_b, int nruns_b, 
            const int *cumsizes_b, int *numbetter_b)
{
    int a,b, result;
    int size_a;
    int size_b;

    *numbetter_a = 0;
    *numbetter_b = 0;

    for (a = 0, size_a = 0; a < nruns_a; a++) {
        for (b = 0, size_b = 0; b < nruns_b; b++) {
            result = 
                pareto_better (dim, minmax,
                               points_a + (dim * size_a), 
                               cumsizes_a[a] - size_a,
                               points_b + (dim * size_b), 
                               cumsizes_b[b] - size_b);
            if (result < 0)
                (*numbetter_a)++;
            else if (result > 0)
                (*numbetter_b)++;
            
            size_b = cumsizes_b[b];
        }
        size_a = cumsizes_a[a];
    }
}

int main(int argc, char *argv[])
{
    int *nruns = NULL;
    int **cumsizes = NULL;
    double **points = NULL;
    int dim = 0;
    char **filenames;
    int numfiles;
    int **results;
    const signed char *minmax = NULL;

    int k, n, j;

    int opt; /* it's actually going to hold a char */
    int longopt_index;
    /* see the man page for getopt_long for an explanation of these fields */
    static struct option long_options[] = {
        {"help",       no_argument,       NULL, 'h'},
        {"version",    no_argument,       NULL, 'V'},
        {"verbose",    no_argument,       NULL, 'v'},
        {"quiet",      no_argument,       NULL, 'q'},
        {"percentages",no_argument,       NULL, 'p'},
        {"no-check",   no_argument,       NULL, 'c'},
        {"obj",        required_argument, NULL, 'o'},

        {NULL, 0, NULL, 0} /* marks end of list */
    };

    while (0 < (opt = getopt_long(argc, argv, "hVvqpo:",
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

        case 'p': // --percentages
            percentages_flag = true;
            break;

        case 'c': // --no-check
            check_flag = false;
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

    if (numfiles <= 1) {
        fprintf(stderr, "%s: error: at least two input files are required.\n",
                program_invocation_short_name);
        usage();
        exit(EXIT_FAILURE);
    }

    filenames = malloc (sizeof(char *) * numfiles);
    points = malloc (sizeof(double *) * numfiles);
    nruns = malloc (sizeof(int) * numfiles);
    cumsizes = malloc (sizeof(int *) * numfiles);
    
    for (k = 0; k < numfiles; optind++, k++) {
        int err;

        filenames[k] = argv[optind];
        points[k] = NULL;
        cumsizes[k] = NULL;
        nruns[k] = 0;
        err = read_data (filenames[k], 
                         &points[k], &dim, &cumsizes[k], &nruns[k]);
        switch (err) {
        case 0: /* No error */
            break;
        case READ_INPUT_WRONG_INITIAL_DIM:
            errprintf ("check the argument of -o, --obj\n.");
        default:
            exit (EXIT_FAILURE);
        }
    }

    /* Default minmax if not set yet.  */
    if (minmax == NULL) 
        minmax = read_minmax (NULL, &dim);
    
    /* Print filename substitutions.  */
    for (k = 0; k < numfiles; k++) {
        char buffer[32];
        char *p;
        snprintf(buffer, 32, "f%d", k + 1);
        buffer[31] = '\0';
        p = malloc (sizeof(char) * (strlen(buffer) + 1));
        strncpy (p, buffer, strlen(buffer) + 1);

        printf ("# %s: %s\n", p, filenames[k]);
        filenames[k] = p;
    }
    printf ("\n");

    /* Print some info about input files.  */
    for (k = 0; k < numfiles; k++) {
        printf ("# %s: %d (%d", filenames[k], nruns[k], cumsizes[k][0]);
        for (n = 1; n < nruns[k]; n++)
            printf (", %d", cumsizes[k][n]); 
        printf (")\n");
    }

    /* Print some info.  */
    printf ("# objectives (%d): ", dim);
    for (k = 0; k < dim; k++) {
        printf ("%c", (minmax[k] < 0) ? '-' 
                : (minmax[k] > 0) ? '+' : 'i');
    }
    printf ("\n");
    
    if (check_flag) {
        bool check_failed = false;

        for (k = 0; k < numfiles; k++) {
            int size = 0;
            for (n = 0; n < nruns[k]; n++) {
                int failed_pos 
                    = find_dominated_point (&points[k][dim * size], dim,
                                            cumsizes[k][n] - size, minmax);
                if (failed_pos >= 0) {
                    fprintf (stderr, 
                             "%s: %s: set %d: point %d is dominated.\n",
                             program_invocation_short_name, 
                             filenames[k], n, failed_pos);
                    check_failed = true;
                }
                size  = cumsizes[k][n];
            }
        }
        if (check_failed) {
            errprintf ("input must be a collection of nondominated sets.");
            exit (EXIT_FAILURE);
        }
    }
    
    results = malloc (sizeof(int) * numfiles * numfiles
                      + sizeof(int *) * numfiles);

    for (k = 0; k < numfiles; k++) {
        results[k] = (int *) (results + numfiles) + k * numfiles;
        for (j = 0; j < numfiles; j++)
            results[k][j] = -1;
    }

    for (k = 0; k < numfiles; k++)
        for (j = k + 1; j < numfiles; j++)
            cmpparetos (dim, minmax,
                        points[k], nruns[k], cumsizes[k], &(results[k][j]),
                        points[j], nruns[j], cumsizes[j], &(results[j][k]));

    print_results (filenames, numfiles, nruns, results);

    if (verbose_flag) {
    }

    return EXIT_SUCCESS;
}


