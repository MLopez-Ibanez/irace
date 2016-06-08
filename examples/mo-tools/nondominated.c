/*************************************************************************

 nondominated:

 ---------------------------------------------------------------------

                       Copyright (c) 2007, 2008
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


*************************************************************************/
#include "io.h"
#include "nondominated.h"

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h> // for isspace()
#include <float.h>

#include <unistd.h>  // for getopt()
#include <getopt.h> // for getopt_long()
#include <math.h> // for log10()

char *program_invocation_short_name = "nondominated";
static int verbose_flag = false;
static bool union_flag = false;
static bool check_flag = true;
static bool filter_flag = false;
static bool normalise_flag = false;
static bool force_bounds_flag = false;
static const char *suffix = "_dat";

static void 
usage(void)
{
    printf("\n"
           "Usage:\n"
           "       %s [OPTIONS] [FILES] \n"
           "       %s [OPTIONS] < [INPUT] > [OUTPUT]\n\n",
           program_invocation_short_name, program_invocation_short_name);

    printf(
"Obtain information and perform some operations on the nondominated sets "
"given as input. \n\n"

"Options:\n"
" -h, --help          print this summary and exit;                          \n"
"     --version       print version number and exit;                        \n"
" -v, --verbose       print some extra information;                         \n"
" -q, --quiet         print the minimum information;                        \n"
"     --no-check      do not check nondominance of sets (faster but unsafe);\n"
" -o, --obj=[+|-]...  specify whether each objective should be minimised (-)\n"
"                     or maximised (+). By default all are minimised;       \n"
" -u, --upper-bound POINT defines an upper bound to check, e.g. \"10 5 30\";\n"
" -l, --lower-bound POINT defines a lower bound to check;\n"
" -U, --union         consider each file as a whole approximation set,      \n"
"                     (by default, approximation sets are separated by an   \n"
"                     empty line within a file);                            \n"
" -s, --suffix=STRING suffix to add to output files. Default is \"%s\".     \n"
"                     The empty string means overwrite the input file.      \n"
"                     This is ignored when reading from stdin because output\n"
"                     is sent to stdout.                                    \n"
"\n"
" The following options OVERWRITE output files:\n"
" -a, --agree=<max|min> transform objectives so all are maximised (or       \n"
"                       minimised). See also the option --obj.              \n"
" -f, --filter        check and filter out dominated points;                \n"
" -b, --force-bound   remove points that do not satisfy the bounds;         \n"
" -n, --normalise RANGE normalise all objectives to a range, e.g., \"1 2\". \n"
"                       If bounds are given with -l and -u, they are used   \n"
"                       for the normalisation.                              \n"
" -L, --log=[1|0]...  specify whether each objective should be transformed  \n"
"                     to logarithmic scale (1) or not (0).                  \n"
"\n", suffix);
}

static void 
version(void)
{
    printf("%s version %s (optimised for %s)\n\n",
           program_invocation_short_name, VERSION, MARCH);
    printf(
"Copyright (C) 2007, 2008, 2009"
"\nManuel Lopez-Ibanez  <manuel.lopez-ibanez@ulb.ac.be>\n"
"\n"
"This is free software, and you are welcome to redistribute it under certain\n"
"conditions.  See the GNU General Public License for details. There is NO   \n"
"warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n"
"\n"        );
}


static inline void
vector_fprintf (FILE *stream, const double *vector, int size)
{
    int k;
    for (k = 0; k < size; k++)
        fprintf (stream, " " point_printf_format, vector[k]);
}

static inline void
vector_printf (const double *vector, int size)
{
    vector_fprintf (stdout, vector, size);
}

static bool
read_range (char *str, double *lower, double *upper)
{
    char *endp;

    *lower = strtod (str, &endp);
    if (str == endp) return false;
    str = endp;

    *upper = strtod (str, &endp);
    if (str == endp) return false;
    str = endp;

    // not end of string: error
    while (*str != '\0') {
        if (!isspace(*str)) return false;
        str++;
    }

    return true;
}

/* FIXME: Handle "1 X 3", so X means: use the minimum/maximum for this
   dimension. */
static double *
read_point (char * str, int *nobj)
{
    double * point;
    char * endp;
    char * cursor;

    int k = 0, size = 10;

    point = malloc (size * sizeof(double));
    endp = str;

    do {
        cursor = endp;
        if (k == size) {
            size += 10;
            point = realloc (point, size * sizeof(double));
        }
        point[k] = strtod (cursor, &endp);
        k++;
    } while (cursor != endp);

    // not end of string: error
    while (*cursor != '\0') {
        if (!isspace(*cursor)) return NULL;
        cursor++;
    }

    // no number: error
    if (k == 1) return NULL;

    *nobj = k - 1;
    return point;
}

static void
data_bounds (double **minimum, double **maximum,
             const double *data, int nobj, int rows)
{
    int n, k, r;

#ifndef INFINITY
#define INFINITY DBL_MAX
#endif
#define NEG_INFINITY -DBL_MAX

    if (*minimum == NULL) {
        *minimum = malloc (nobj * sizeof(double));
        for (k = 0; k < nobj; k++)
            (*minimum)[k] = INFINITY;
    }

    if (*maximum == NULL) {
        *maximum = malloc (nobj * sizeof(double));
        for (k = 0; k < nobj; k++)
            (*maximum)[k] = NEG_INFINITY;
    }

    for (k = 0, r = 0; r < rows; r++) {
        for (n = 0; n < nobj; n++, k++) {
            if ((*minimum)[n] > data[k])
                (*minimum)[n] = data[k];
            if ((*maximum)[n] < data[k])
                (*maximum)[n] = data[k];
        }
    }
}

static inline void
handle_read_data_error (int err)
{
    switch (err) {
    case 0: /* No error */
        break;
    case READ_INPUT_WRONG_INITIAL_DIM:
        errprintf ("check the argument of either" 
                   " -o, --obj, -u, --upper or -l, --lower.\n");
    default:
        exit (EXIT_FAILURE);
    }
}

static inline bool
any_less_than (const double *a, const double *b, int dim)
{
    int d;
    for (d = 0; d < dim; d++)
        if (a[d] < b[d])
            return true;

    return false;
}

static void
file_bounds (const char *filename, double **minimum_p, double **maximum_p,
             int *dim_p)
{
    double *data = NULL;
    int *cumsizes = NULL;
    int nruns = 0;
    int dim = *dim_p;

    handle_read_data_error (
        read_data (filename, &data, &dim, &cumsizes, &nruns));

    data_bounds (minimum_p, maximum_p, data, dim, cumsizes[nruns - 1]);

    *dim_p = dim;

    free (data);
    free (cumsizes);
}

static void
logarithm_scale (double *points, int dim, int size,
                 const bool *logarithm)
{
    int k, d;

    assert (logarithm);

    for (d = 0; d < dim; d++) {
        if (!logarithm[d]) continue;
        for (k = 0; k < size; k++)
            points[k * dim + d] = log10(points[k * dim + d]);
    }
}

static void
agree_objectives (double *points, int dim, int size,
                  const signed char *minmax, const signed char agree)
{
    int k, d;

    for (d = 0; d < dim; d++)
        if ((agree > 0 && minmax[d] < 0)
            || (agree < 0 && minmax[d] > 0))
            for (k = 0; k < size; k++)
                points[k * dim + d] = -(points[k * dim + d]);
}


static bool
force_bounds (double *points, int dim, int *cumsizes, int nsets,
              const double *lbound, const double *ubound)
{
    int outbounds_found = -1;
    bool *outbounds;
    int n, n2, k;
    int size = cumsizes[nsets - 1];
    int *ssizes;

    outbounds = malloc (sizeof(bool) * size);

    for (n = size - 1; n >= 0; n--) {
        if (any_less_than (&points[n * dim], lbound, dim)
            || any_less_than (ubound, &points[n * dim], dim)) {
            outbounds[n] = true;
            outbounds_found = n;
        } else 
            outbounds[n] = false;
    }

    if (outbounds_found < 0) {
        if (verbose_flag >= 2) 
            fprintf (stderr, "# out of bounds: 0\n");
        free (outbounds);
        return false;
    }

    ssizes = malloc (sizeof(int) * nsets);
    ssizes[0] = cumsizes[0];
    for (k = 1; k < nsets; k++)
        ssizes[k] = cumsizes[k] - cumsizes[k-1];

    /* Find the set of the first out-of-bounds point.  */
    for (k = 0; outbounds_found >= cumsizes[k]; k++);

    /* Delete it.  */
    ssizes[k]--;

    /* Delete the rest of them.  */
    for (n = outbounds_found, n2 = outbounds_found + 1; k < nsets; k++) {
        while (n2 < cumsizes[k]) {
            if (outbounds[n2]) {
                n2++; ssizes[k]--;
            } else {
                memcpy (&points[n * dim], &points[n2 * dim], 
                        sizeof(double) * dim);
                n++, n2++;
            }
        }
    }

    if (verbose_flag >= 2) 
        fprintf (stderr, "# out of bounds: %d\n", n2 - n);
    
    cumsizes[0] = ssizes[0];
    for (k = 1; k < nsets; k++)
        cumsizes[k] = ssizes[k] + cumsizes[k-1];

    free (ssizes);
    free (outbounds);
    return true;
}

static bool
check_nondominated (const char * filename, const double *points,
                    int dim, const int *cumsizes, int nruns,
                    const signed char *minmax, const signed char agree,
                    bool **nondom_p)
{
    int n, cumsize;
    bool first_time = true;
    bool dominated_found = false;
    bool *nondom;

    nondom = nondom_p ? *nondom_p : NULL;

    if (nondom == NULL) {
        nondom = malloc (sizeof(bool) * cumsizes[nruns - 1]);
        for (n = 0; n <  cumsizes[nruns - 1]; n++)
            nondom[n] = true;
    }

    for (n = 0, cumsize = 0; n < nruns; cumsize = cumsizes[n], n++) {
        int old_size = cumsizes[n] - cumsize;
        int new_size 
            = find_nondominated_set_agree (&points[dim * cumsize], dim,
                                           old_size, minmax, agree,
                                           &nondom[cumsize]);

        if (verbose_flag >= 2) {
            if (first_time) {
                fprintf (stderr, "# filename\tset\tsize\tnondom\tdom\n");
                first_time = false;
            }
            fprintf (stderr, "%s\t%d\t%d\t%d\t%d\n",
                     filename, n+1, old_size, new_size, old_size - new_size);
        } else if (verbose_flag && new_size < old_size) {
            if (first_time) {
                fprintf (stderr, "filename\tset\tdom\n");
                first_time = false;
            }
            fprintf (stderr, "%s\t%d\t%d dominated\n",
                     filename, n+1, old_size - new_size);
        }

        if (new_size < old_size) {
            dominated_found = true;
        }
        else if (new_size > old_size) {/* This can't happen.  */
            errprintf ("a bug happened in %s: new_size > old_size!\n",
                       __FUNCTION__);
            exit (EXIT_FAILURE);
        }
    }

    if (nondom_p)
        *nondom_p = nondom;

    return dominated_found;
}

static void
print_file_info (FILE *stream, const char *filename,
                 int dim, const signed char *minmax)
{
    int n;

    /* Print some info about input files.  */
    fprintf (stream, "# file: %s\n", filename);
    fprintf (stream, "# objectives (%d): ", dim);
    for (n = 0; n < dim; n++) {
        fprintf (stream, "%c", (minmax[n] < 0) ? '-'
                 : ((minmax[n] > 0) ? '+' : 'i'));
    }
    fprintf (stream, "\n");
}

static void
print_output_header (FILE *stream, const char *filename,
                     int dim, const signed char *minmax,
                     signed char agree,
                     double lrange, double urange,
                     const double *lbound, const double *ubound,
                     const bool *logarithm)
{
    int n;

    print_file_info (stream, filename, dim, minmax);

    fprintf (stream, "# agree: %s\n", (agree < 0) ? "min"
             : ((agree > 0) ? "max" : "no"));

    if (logarithm) {
        fprintf (stream, "# logarithm: ");
        for (n = 0; n < dim; n++) {
            fprintf (stream, "%c", logarithm[n] ? '1' : '0');
        }
        fprintf (stream, "\n");
    }

    if (normalise_flag)
        fprintf (stream, "# range: " 
                 point_printf_format " " point_printf_format
                 "\n", lrange, urange);

    fprintf (stream, "# lower bound: ");
    vector_fprintf (stream, lbound, dim);
    fprintf (stream, "\n");
    fprintf (stream, "# upper bound: ");
    vector_fprintf (stream, ubound, dim);
    fprintf (stream, "\n");
}

static void
print_input_info (FILE *stream, const char *filename,
                  int dim, const int *cumsizes, int nruns,
                  const signed char *minmax,
                  const double *minimum, const double *maximum)
{
    int n;

    print_file_info (stream, filename, dim, minmax);

    fprintf (stream, "# sets: %d\n", nruns);
    fprintf (stream, "# sizes: %d", cumsizes[0]);
    for (n = 1; n < nruns; n++)
        fprintf (stream, ", %d", cumsizes[n] - cumsizes[n - 1]);
    fprintf (stream, "\n");
    fprintf (stream, "# points: %d\n", cumsizes[nruns - 1]);

    fprintf (stream, "# minimum:");
    vector_fprintf (stream, minimum, dim);
    fprintf (stream, "\n");
    fprintf (stream, "# maximum:");
    vector_fprintf (stream, maximum, dim);
    fprintf (stream, "\n");
}

static bool
process_file (const char *filename,
              const signed char *minmax, int *dim_p, signed char agree,
              double lrange, double urange,
              double *lbound, double *ubound,
              double **minimum_p, double **maximum_p,
              bool check_minimum, bool check_maximum,
              const bool *logarithm)
{
    bool *nondom = NULL;
    bool dominated_found = false;
    bool logarithm_flag = false;

    double *points = NULL;
    int d;
    int dim = *dim_p;
    int *cumsizes = NULL;
    int nsets = 0;

    double *minimum;
    double *maximum;
    double *log_lbound = NULL;
    double *log_ubound = NULL;


    handle_read_data_error (
        read_data (filename, &points, &dim, &cumsizes, &nsets));
    if (!filename) filename = stdin_name;

    if (union_flag) {
        cumsizes[0] = cumsizes[nsets - 1];
        nsets = 1;
    }

    /* Default minmax if not set yet.  */
    if (minmax == NULL)
        minmax = read_minmax (NULL, &dim);

    minimum = NULL, maximum = NULL;    
    data_bounds (&minimum, &maximum, points, dim, cumsizes[nsets - 1]);

    if (verbose_flag >= 2)
        print_input_info (stderr, filename, dim, cumsizes, nsets, minmax,
                          minimum, maximum);

    if (lbound == NULL)
        lbound = minimum;
    else if (check_minimum && !force_bounds_flag 
             && any_less_than (minimum, lbound, dim)) {
        errprintf ("%s: found vector smaller than lower bound:", filename);
        vector_fprintf (stderr, minimum, dim);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }

    if (ubound == NULL)
        ubound = maximum;
    else if (check_maximum && !force_bounds_flag 
             && any_less_than (ubound, maximum, dim)) {
        errprintf ("%s: found vector larger than upper bound:", filename);
        vector_fprintf (stderr, maximum, dim);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }

    if (force_bounds_flag) {
        force_bounds (points, dim, cumsizes, nsets, lbound, ubound);
    }

    if (logarithm) {
        log_lbound = malloc(sizeof(double) * dim);
        log_ubound = malloc(sizeof(double) * dim);

        memcpy (log_lbound, lbound, sizeof(double) * dim);
        memcpy (log_ubound, ubound, sizeof(double) * dim);

        for (d = 0; d < dim; d++) {
            if (!logarithm[d]) continue;
            log_lbound[d] = log10(lbound[d]);
            log_ubound[d] = log10(ubound[d]);
            logarithm_flag = true;
        }

        if (logarithm_flag) {
            lbound = log_lbound;
            ubound = log_ubound;
            logarithm_scale (points, dim, cumsizes[nsets - 1], logarithm);
        }
    }

    if (agree)
        agree_objectives (points, dim, cumsizes[nsets - 1], minmax, agree);

    if (normalise_flag)
        normalise (points, dim, cumsizes[nsets - 1], minmax, agree, 
                   lrange, urange, lbound, ubound);

    /* Check sets.  */
    if (check_flag || filter_flag)
        if (check_nondominated (filename, points, dim, cumsizes, nsets,
                                minmax, agree, filter_flag ? &nondom : NULL))
            dominated_found = true;

    if (verbose_flag >= 2)
        fprintf (stderr, "# nondominated: %s\n", 
                 dominated_found ? "FALSE" : "TRUE");

    /* Write out nondominated sets.  */
    if (filter_flag || agree || normalise_flag || force_bounds_flag
        || logarithm_flag) {

        if (filename != stdin_name) {
            char *outfilename;
            FILE *outfile;
            int outfilename_len = strlen(filename) + strlen(suffix) + 1;

            outfilename = malloc (sizeof(char) * outfilename_len);
            strcpy (outfilename, filename);
            strcat (outfilename, suffix);

            outfile = fopen (outfilename, "w");
            if (outfile == NULL) {
                errprintf ("%s: %s\n", outfilename, strerror(errno));
                exit(EXIT_FAILURE);
            }

            print_output_header (outfile, filename, dim, minmax, agree,
                                 lrange, urange, lbound, ubound,
                                 logarithm);

            if (filter_flag && dominated_found)
                write_sets_filtered (outfile, points, dim, cumsizes, nsets,
                                     nondom);
            else 
                write_sets (outfile, points, dim, cumsizes, nsets);

            fclose (outfile);
            if (verbose_flag)
                fprintf (stderr, "# %s -> %s\n", filename, outfilename);
            free (outfilename);
        }
        else {
            print_output_header (stdout, filename, dim, minmax, agree,
                                 lrange, urange, lbound, ubound,
                                 logarithm);

            if (filter_flag && dominated_found) 
                write_sets_filtered (stdout, points, dim, cumsizes, nsets, 
                                     nondom);
            else
                write_sets (stdout, points, dim, cumsizes, nsets);

            if (verbose_flag)
                fprintf (stderr, "# %s -> %s\n", filename, "<stdout>");
        }
    }

    if (nondom) 
        free (nondom);
    free (points);
    free (cumsizes);
    if (log_lbound) free (log_lbound);
    if (log_ubound) free (log_ubound);

    *minimum_p = minimum;
    *maximum_p = maximum;
    *dim_p = dim;

    if (verbose_flag >= 2)
        fprintf (stderr, "#\n");

    return dominated_found;
}

int main(int argc, char *argv[])
{
    signed char agree = 0;
    double lower_range = 0.0;
    double upper_range = 0.0;
    double *lower_bound = NULL;
    double *upper_bound = NULL;
    double *minimum = NULL;
    double *maximum = NULL;

    int numfiles;
    const signed char *minmax = NULL;
    const bool *logarithm = NULL;
    int dim = 0;
    bool dominated_found = false;
    int k;

    int opt; /* it's actually going to hold a char */
    int longopt_index;
    /* see the man page for getopt_long for an explanation of these fields */
    static struct option long_options[] = {
        {"help",       no_argument,       NULL, 'h'},
        {"version",    no_argument,       NULL, 'V'},
        {"verbose",    no_argument,       NULL, 'v'},
        {"quiet",      no_argument,       NULL, 'q'},

        {"no-check",   no_argument,       NULL, 'c'},
        {"filter",     no_argument,       NULL, 'f'},
        {"force-bounds",no_argument,      NULL, 'b'},
        {"obj",        required_argument, NULL, 'o'},
        {"agree",      required_argument, NULL, 'a'},
        {"normalise",  required_argument, NULL, 'n'},
        {"upper-bound",required_argument, NULL, 'u'},
        {"lower-bound",required_argument, NULL, 'l'},
        {"union",      no_argument,       NULL, 'U'},
        {"suffix",     required_argument, NULL, 's'},
        {"log",        required_argument, NULL, 'L'},

        {NULL, 0, NULL, 0} /* marks end of list */
    };

    while (0 < (opt = getopt_long (argc, argv, "hVvqfo:a:n:u:l:Us:b",
                                   long_options, &longopt_index))) {
        switch (opt) {
        case 'V': // --version
            version();
            exit(EXIT_SUCCESS);

        case 'q': // --quiet
            verbose_flag = 0;
            break;

        case 'v': // --verbose
            verbose_flag = 2;
            break;

        case 'c': // --no-check
            check_flag = false;
            break;

        case 'f': // --filter
            filter_flag = true;
            check_flag  = true;
            break;

        case 'b': // --force-bounds
            force_bounds_flag = true;
            break;

        case 'U': // --union
            union_flag = true;
            break;

        case 'o': // --obj
            minmax = read_minmax (optarg, &dim);
            if (minmax == NULL) {
                errprintf ("invalid argument '%s' for -o, --obj"
                           ", it should be a sequence of '+' or '-'\n", optarg);
                exit(EXIT_FAILURE);
            }
            break;

        case 'a': // --agree
            if (!strcmp (optarg, "max"))
                agree = 1;
            else if (!strcmp (optarg, "min"))
                agree = -1;
            else {
                errprintf ("invalid argument '%s' for -a, --agree"
                           ", it should be either \'min\' or \'max\'\n",
                           optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case 'n': // --normalise
            normalise_flag = true;
            if (!read_range (optarg, &lower_range, &upper_range)) {
                errprintf ("invalid range '%s' for -n, --normalise"
                           ", use for example -n \"1 2\"\n", optarg);
                exit (EXIT_FAILURE);

            } else if (lower_range >= upper_range) {
                errprintf ("lower range must be smaller than upper range"
                           " for -n, --normalise\n");
                exit (EXIT_FAILURE);
            }
            break;

        case 'u': // --upper-bound
            upper_bound = read_point (optarg, &dim);
            if (upper_bound == NULL) {
                errprintf ("invalid upper bound point '%s'", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case 'l': // --lower-bound
            lower_bound = read_point (optarg, &dim);
            if (lower_bound == NULL) {
                errprintf ("invalid lower bound point '%s'", optarg);
                exit (EXIT_FAILURE);
            }
            break;

        case 's': // --suffix
            suffix = optarg;
            break;

        case 'L': // --log
            logarithm = read_bitvector (optarg, &dim);
            if (logarithm == NULL) {
                errprintf ("invalid argument to --log '%s'", optarg);
                exit (EXIT_FAILURE);
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

    if (lower_bound && upper_bound 
        && any_less_than (upper_bound, lower_bound, dim)) {
        errprintf ("upper bound must be higher than lower bound.");
        exit (EXIT_FAILURE);
    }

    numfiles = argc - optind;

    if (numfiles <= 0) /* No input files: read stdin.  */
        return process_file (NULL, minmax, &dim, agree,
                             lower_range, upper_range,
                             lower_bound, upper_bound,
                             &minimum, &maximum,
                             /*check_minimum=*/true, /*check_maximum=*/true,
                             logarithm);
    else if (numfiles == 1)
        return process_file (argv[optind], minmax, &dim, agree,
                             lower_range, upper_range,
                             lower_bound, upper_bound,
                             &minimum, &maximum,
                             /*check_minimum=*/true, /*check_maximum=*/true,
                             logarithm);
    else {
        
        if (!lower_bound || !upper_bound) {
            /* Calculate the bounds among all input files.  */
            minimum = NULL, maximum = NULL;
            for (k = 0; k < numfiles; k++)
                file_bounds (argv[optind + k], &minimum, &maximum, &dim);
        
            k = 0;
        } else {
            /* If the bounds were given, initialize minimum and maximum.  */
            if (process_file (argv[optind], minmax, &dim, agree,
                              lower_range, upper_range,
                              lower_bound, upper_bound,
                              &minimum, &maximum,
                              /*check_minimum=*/true, /*check_maximum=*/true,
                              logarithm))
                dominated_found = true;
            k = 1;
        }

        for (; k < numfiles; k++) {
            int n;
            double *tmp_maximum = NULL;
            double *tmp_minimum = NULL;

            if (process_file (argv[optind + k], minmax, &dim, agree,
                              lower_range, upper_range,
                              (lower_bound) ? lower_bound : minimum, 
                              (upper_bound) ? upper_bound : maximum,
                              &tmp_minimum, &tmp_maximum,
                              lower_bound != NULL, upper_bound != NULL,
                              logarithm))
                dominated_found = true;

            /* If the bounds were given, the real minimum and maximum
               must be calculated as we process the files.  */
            if (lower_bound && upper_bound)
                for (n = 0; n < dim; n++) {
                    if (minimum[n] > tmp_minimum[n])
                        minimum[n] = tmp_minimum[n];
                    if (maximum[n] < tmp_maximum[n])
                        maximum[n] = tmp_maximum[n];
                }
            free (tmp_minimum);
            free (tmp_maximum);
        }
        if (verbose_flag) {
            printf ("# Total files: %d\n", numfiles);
            printf ("# Total minimum:");
            vector_printf (minimum, dim);
            printf ("\n");
            printf ("# Total maximum:");
            vector_printf (maximum, dim);
            printf ("\n");
            printf ("# Nondominated: %s\n", dominated_found ? "FALSE": "TRUE");
        }
    }

    return dominated_found;
}
