#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>

#define program_invocation_short_name "target-runner-dummy"

static void
fatal_error(const char *format,...)
{
    va_list ap;
    fprintf(stderr, "%s: fatal error: ", program_invocation_short_name);
    va_start(ap,format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}

static bool
str_equal(const char *a, const char * b)
{
    return(strncmp(a, b, strlen(b) + 1) == 0);
}

static bool
starts_with(const char *s, const char *prefix)
{
    size_t lenpre = strlen(prefix),
           lenstr = strlen(s);
    return lenstr < lenpre ? false : memcmp(prefix, s, lenpre) == 0;
}

static double
rand01(void)
{
    return rand() / (RAND_MAX + 1.);
}

long read_long(const char *s)
{
    errno = 0;    /* To distinguish success/failure after call */
    char *endptr;
    long i = strtol(s, &endptr, 10);
    if (errno != 0) {
        perror("error: reading number");
        exit(EXIT_FAILURE);
    }
    if (*endptr != '\0') {
        fatal_error("'%s' is not a number", s);
    }
    return i;
}

bool read_bool(const char *s)
{
    return (bool) read_long(s);
}

int main(int argc, char *argv[])
{
    if (argc < 6) {
        fatal_error(
"Not enough arguments (%d given)! Usage:\n"
"  target-runner-dummy ID_CONFIGURATION ID_INSTANCE SEED INSTANCE [BOUND] \n"
"                      [--time 0|1] [--reject 0|1] [--reject-rate RATE] \n"
"                      [--p_int INTEGER] [--p_real REAL] [--fail 0|1] [--opt-time 0|1]",
                    argc);
    }
    //int id_configuration = atoi(argv[1]);
    //const char * id_instance = argv[2];
    unsigned int seed = atol(argv[3]);
    long instance = read_long(argv[4]);
    double bound = -1;
    int idx = 5;
    if (!starts_with(argv[idx], "--")) {
        // There is a bound.
        bound = atof(argv[idx]);
        idx++;
    }
    
    double reject_rate = 0.1;
    bool reject = false,
        print_time = false,
        time_is_cost = false;
    int p_int = -1;
    double p_real = -1;
    
    for (; idx < argc; idx++) {
        const char *param = argv[idx++];
        if (idx >= argc) {
            fatal_error("Missing value for parameter %s", param);
        }
        const char *value = argv[idx];
        //printf("param: %s\n", param);
        //printf("value: %s\n", value);
        if (str_equal(param, "--time")) {
            print_time = read_bool(value);
        } else if (str_equal(param, "--reject")) {
            reject = read_bool(value);
        } else if (str_equal(param, "--reject-rate")) {
            reject_rate = atof(value);
        } else if (str_equal(param, "--p_int")) {
            p_int = read_long(value);
        } else if (str_equal(param, "--p_real")) {
            p_real = atof(value);
        } else if (str_equal(param, "--fail")) {
            if (read_bool(value)) {
                fatal_error("Asked to fail");
            }
        } else if (str_equal(param, "--opt-time")) {
            time_is_cost = read_bool(value);
        }
    }

    if (reject) {
        srand((unsigned int) (seed + instance + (p_int * p_real)));
        if (rand01() < reject_rate) {
            printf("Inf\n");
            exit(EXIT_SUCCESS);
        }
    }
    
    srand(seed);
    double cost = instance + (p_int * p_real) + rand01();
    double time = (time_is_cost) ? cost : (
        (bound > 0) ? (bound+1) * rand01() + 0.00001 : instance + (int) (10 * rand01())
        );
        
    if (bound > 0 && time > bound) {
        time = bound;
    }
    
    if (print_time) {
        printf("%g %g\n", cost, time);
    } else {
        printf("%g\n", cost);
    }
    return EXIT_SUCCESS;
}
