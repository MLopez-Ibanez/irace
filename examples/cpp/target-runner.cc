/*************
  This is an unfinished example of a target-runner written in C++.
*********/
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cmath>
#include <random>
#include <climits>
#include <cerrno>

char *program_name;

double instance_f1(double a)
{
    return 10 * a;
}

double instance_f2(double a)
{
    return 5 * a;
}

void error(const char *msg)
{
    fprintf(stderr, "%s: %s\n", program_name, msg);
    exit (EXIT_FAILURE);
}

long int safe_strtol(const char *str, const char *errmsg)
{
    // FIXME: C++ way to do this?
    char *endptr;
    errno = 0;    /* To distinguish success/failure after call */
    int base = 10;
    long int val = strtol(str, &endptr, base);
    /* Check for various possible errors */
    if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
        || (errno != 0 && val == 0)) {
        perror(errmsg);
        exit(EXIT_FAILURE);
    }
    if (endptr == str) {
        fprintf(stderr, "%s: no number was found (%s)\n", errmsg, str);
        exit(EXIT_FAILURE);
    }
    
    if (*endptr != '\0')
        fprintf(stderr, "%s: the string does not exactly match a number,"
                " further characters after number: %s\n", errmsg, endptr);
    return val;
}

int main(int argc, char**argv)
{
    program_name = argv[0];
    
    if (argc < 6) {
        error("Not enough parameters");
    }

    // FIXME: These could be used to generate a unique temporary file
    char *config_id = argv[1];
    char *instance_id = argv[2];
    int seed = (int) safe_strtol(argv[3], "parsing seed");
    std::string instance(argv[4]);

    // Value of parameter x
    double x;
    for (int i = 5; i < argc; i += 2) {
        std::string param(argv[i]);
        std::string value(argv[i+1]);
        if (param == "--param1") {
            // FIXME: This should use strtod and detect errors.
            x = atof(value.c_str());
        } else {
            error("Unrecognized parameter");
        }
    }

    // Simulate noise.
    std::default_random_engine generator(seed);
    std::uniform_real_distribution<double> noise(-0.1, 0.1);
    double cost;
    if (instance == "f1") {
        cost = noise(generator) +  instance_f1(x);
    } else if (instance == "f2") {
        cost = noise(generator) +  instance_f2(x);
    } else {
        error("Instance not known");
    }
    
    printf("%.15g\n", cost);
    
    return EXIT_SUCCESS;
}
