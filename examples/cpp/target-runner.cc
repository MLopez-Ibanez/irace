/*************
  This is an unfinished example of a target-runner written in C++.
*********/
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cmath>
#include <random>

char *program_name;

double instance_f1(double a)
{
    return 10 * a;
}

double instance_f2(double a)
{
    return -10 * a;
}

void error(const char *msg)
{
    fprintf(stderr, "%s: %s\n", program_name, msg);
    exit (EXIT_FAILURE);
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
    // FIXME: This should use strtoul to detect errors.
    int seed = (int) atoi(argv[3]);
    std::default_random_engine generator(seed);
    std::uniform_real_distribution<double> noise(-0.1, 0.1);

    std::string instance(argv[4]);

    double x = INFINITY;
    for (int i = 5; i < argc; i += 2) {
        std::string param(argv[i]);
        std::string value(argv[i+1]);
        if (param == "--param1") {
            // FIXME: This should use strtod to detect errors.
            x = atof(value.c_str());
        } else {
            error("Unrecognized parameter");
        }
    }

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
