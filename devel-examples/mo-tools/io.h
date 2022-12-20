#ifndef _EAF_IO_H_
#define _EAF_IO_H_

#include "common.h"

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

#define point_printf_format "%-16.15g"

void 
errprintf(const char * template,...) 
/* enables the compiler to check the format string against the
   parameters */  __attribute__ ((format(printf, 1, 2)));

void warnprintf(const char *template,...)
/* enables the compiler to check the format string against the
   parameters */  __attribute__ ((format(printf, 1, 2)));

/* Error codes for read_data.  */
#define READ_INPUT_FILE_EMPTY -1
#define READ_INPUT_WRONG_INITIAL_DIM -2

int
read_data (const char *filename, double **data_p, 
           int *nobjs_p, int **cumsizes_p, int *nsets_p);

void
write_point (FILE *outfile, const double * data, int dim);

int 
write_sets (FILE *outfile, const double *data, int ncols, 
            const int *cumsizes, int nruns);
int 
write_sets_filtered (FILE *outfile, const double *data, int ncols, 
                     const int *cumsizes, int nruns, 
                     const bool *write_p);

static inline const signed char *
read_minmax (const char *str, int *nobj)
{
    signed char * minmax;
    int len;
    int i;
    bool all_ignored = true;

    if (str == NULL) { /* Default all minimised.  */
        assert (*nobj > 0);
        minmax = malloc (sizeof(signed char) * *nobj);
        for (i = 0; i < *nobj; i++)
            minmax[i] = -1;
        return minmax;
    }

    len = strlen (str);
    minmax = malloc (sizeof(signed char) * len);
    for (i = 0; i < len; i++) {
        switch (str[i])
        {
        case '+':
            minmax[i] = 1;
            all_ignored = false;
            break;
        case '-':
            minmax[i] = -1;
            all_ignored = false;
            break;
        case '0':
        case 'i':
            minmax[i] = 0;
            break;
        default: /* something unexpected was found */
            return NULL;
            break;
        }
    }

    if (all_ignored == true) {
        warnprintf ("all objectives ignored because of --obj=%s\n", str);
        exit (EXIT_SUCCESS);
    }

    *nobj = len;
    return minmax;
}

static inline const bool *
read_bitvector (const char *str, int *nobj)
{
    bool * vec;
    int len;
    int i;

    if (str == NULL) { /* Default all false.  */
        assert (*nobj > 0);
        vec = malloc (sizeof(bool) * *nobj);
        for (i = 0; i < *nobj; i++)
            vec[i] = false;
        return vec;
    }

    len = strlen (str);
    vec = malloc (sizeof(bool) * len);
    for (i = 0; i < len; i++) {
        switch (str[i])
        {
        case '1':
            vec[i] = true;
            break;
        case '0':
            vec[i] = false;
            break;
        default: /* something unexpected was found */
            return NULL;
        }
    }
    *nobj = len;
    return vec;
}

static const char stdin_name[] = "<stdin>";

#endif
