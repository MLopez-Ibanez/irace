/* NSGA-II routine (implementation of the 'main' function) */

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <unistd.h>
# include <string.h>

# include "global.h"
# include "rand.h"
# include "problemdef.h"

char * problem;
void (* test_problem) (double *, double *, int **, double *, double *);
int nreal;
int nbin;
int nobj;
int ncon;
int popsize;
double pcross_real;
double pcross_bin;
double pmut_real;
double pmut_bin;
double eta_c;
double eta_m;
int ngen;
int neval, nevals;
int nbinmut;
int nrealmut;
int nbincross;
int nrealcross;
int *nbits;
double *min_realvar;
double *max_realvar;
double *min_binvar;
double *max_binvar;
int bitlength;
int choice;
int obj1;
int obj2;
int obj3;
int angle1;
int angle2;

int main (int argc, char **argv)
{
    int i;
    FILE *fpt3;
    FILE *gp;
    population *parent_pop;
    population *child_pop;
    population *mixed_pop;
    if (argc < 9)
    {
        printf("\n Usage ./nsga2r random_seed problem nvar nobj eval pop pcross pmut eta_c eta_m\n");
        exit(1);
    }
    seed = (double)atof(argv[1]);
    if (seed<=0.0 || seed>=1.0)
    {
        printf("\n Entered seed value is wrong, seed value must be in (0,1) \n");
        exit(1);
    }
    fpt3 = stdout; //fopen("best_pop.out","w");
    //fprintf(fpt3,"# This file contains the data of final feasible population (if found)\n");

    problem = argv[2];
    if (!strcmp(problem, "DTLZ1")) { test_problem = &dtlz1; } 
    else if (!strcmp(problem, "DTLZ2")) { test_problem = &dtlz2; }
    else if (!strcmp(problem, "DTLZ3")) { test_problem = &dtlz3; }
    else if (!strcmp(problem, "DTLZ4")) { test_problem = &dtlz4; }
    else if (!strcmp(problem, "DTLZ5")) { test_problem = &dtlz5; }
    else if (!strcmp(problem, "DTLZ6")) { test_problem = &dtlz6; }
    else if (!strcmp(problem, "DTLZ7")) { test_problem = &dtlz7; }
	 
    else if (!strcmp(problem, "ZDT1")) { test_problem = &zdt1; } 
    else if (!strcmp(problem, "ZDT2")) { test_problem = &zdt2; } 
    else if (!strcmp(problem, "ZDT3")) { test_problem = &zdt3; } 
    else if (!strcmp(problem, "ZDT4")) { test_problem = &zdt4; } 
    else if (!strcmp(problem, "ZDT6")) { test_problem = &zdt6; } 
    
    else { fprintf(stderr, "Invalid problem to be optimized!\n"); abort(); }
    nreal = atoi(argv[3]);
    nobj = atoi(argv[4]);
    nevals=atoi(argv[5]);
    popsize = atoi(argv[6]);
    if (popsize<4 || (popsize%4)!= 0)
    {
        printf("\n population size read is : %d",popsize);
        printf("\n Wrong population size entered, hence exiting \n");
        exit (1);
    }
    ngen = 1000000;
    if (nevals<1)
    {
        printf("\n number of evaluations read is : %d",ngen);
        printf("\n Wrong number of evaluations entered, hence exiting \n");
        exit (1);
    }
    if (nobj<2)
    {
        printf("\n number of objectives entered is : %d",nobj);
        printf("\n Wrong number of objectives entered, hence exiting \n");
        exit (1);
    }
    ncon = 0;
    if (nreal<0)
    {
        printf("\n number of real variables entered is : %d",nreal);
        printf("\n Wrong number of variables entered, hence exiting \n");
        exit (1);
    }
    if (nreal != 0)
    {
        min_realvar = (double *)malloc(nreal*sizeof(double));
        max_realvar = (double *)malloc(nreal*sizeof(double));
        if (!strcmp(problem, "DTLZ1") || !strcmp(problem, "DTLZ2") || !strcmp(problem, "DTLZ3") || !strcmp(problem, "DTLZ4")
            || !strcmp(problem, "DTLZ5") || !strcmp(problem, "DTLZ6") || !strcmp(problem, "DTLZ7")
            || !strcmp(problem, "ZDT1") || !strcmp(problem, "ZDT2") || !strcmp(problem, "ZDT3") || !strcmp(problem, "ZDT6")) {
          for (i=0; i<nreal; i++) { min_realvar[i] = 0.0; max_realvar[i] = 1.0; }
        }
        else if (!strcmp(problem, "ZDT4")) {
          min_realvar[0] = 0.0; max_realvar[0] = 1.0;
          for (i=1; i<nreal; i++) { min_realvar[i] = -5.0; max_realvar[i] = 5.0; }
        } else {
            fprintf(stderr, "Forgot to update min/max for this problem!\n"); abort(); 
        }

        pcross_real = atof(argv[7]);
        if (pcross_real<0.0 || pcross_real>1.0)
        {
            printf("\n Probability of crossover entered is : %e",pcross_real);
            printf("\n Entered value of probability of crossover of real variables is out of bounds, hence exiting \n");
            exit (1);
        }
        pmut_real = atof(argv[8]);
        if (pmut_real<0.0 || pmut_real>1.0)
        {
            printf("\n Probability of mutation entered is : %e",pmut_real);
            printf("\n Entered value of probability of mutation of real variables is out of bounds, hence exiting \n");
            exit (1);
        }
        eta_c = atof(argv[9]);
        if (eta_c<=0)
        {
            printf("\n The value entered is : %e",eta_c);
            printf("\n Wrong value of distribution index for crossover entered, hence exiting \n");
            exit (1);
        }
        eta_m = atof(argv[10]);
        if (eta_m<=0)
        {
            printf("\n The value entered is : %e",eta_m);
            printf("\n Wrong value of distribution index for mutation entered, hence exiting \n");
            exit (1);
        }
    }
    nbin = 0;
    if (nbin<0)
    {
        printf ("\n number of binary variables entered is : %d",nbin);
        printf ("\n Wrong number of binary variables entered, hence exiting \n");
        exit(1);
    }
    if (nbin != 0)
    {
        nbits = (int *)malloc(nbin*sizeof(int));
        min_binvar = (double *)malloc(nbin*sizeof(double));
        max_binvar = (double *)malloc(nbin*sizeof(double));
        for (i=0; i<nbin; i++)
        {
            printf ("\n Enter the number of bits for binary variable %d : ",i+1);
            scanf ("%d",&nbits[i]);
            if (nbits[i] < 1)
            {
                printf("\n Wrong number of bits for binary variable entered, hence exiting");
                exit(1);
            }
            printf ("\n Enter the lower limit of binary variable %d : ",i+1);
            scanf ("%lf",&min_binvar[i]);
            printf ("\n Enter the upper limit of binary variable %d : ",i+1);
            scanf ("%lf",&max_binvar[i]);
            if (max_binvar[i] <= min_binvar[i])
            {
                printf("\n Wrong limits entered for the min and max bounds of binary variable entered, hence exiting \n");
                exit(1);
            }
        }
        printf ("\n Enter the probability of crossover of binary variable (0.6-1.0): ");
        scanf ("%lf",&pcross_bin);
        if (pcross_bin<0.0 || pcross_bin>1.0)
        {
            printf("\n Probability of crossover entered is : %e",pcross_bin);
            printf("\n Entered value of probability of crossover of binary variables is out of bounds, hence exiting \n");
            exit (1);
        }
        printf ("\n Enter the probability of mutation of binary variables (1/nbits): ");
        scanf ("%lf",&pmut_bin);
        if (pmut_bin<0.0 || pmut_bin>1.0)
        {
            printf("\n Probability of mutation entered is : %e",pmut_bin);
            printf("\n Entered value of probability  of mutation of binary variables is out of bounds, hence exiting \n");
            exit (1);
        }
    }
    if (nreal==0 && nbin==0)
    {
        printf("\n Number of real as well as binary variables, both are zero, hence exiting \n");
        exit(1);
    }
    choice=0;
    if (choice!=0 && choice!=1)
    {
        printf("\n Entered the wrong choice, hence exiting, choice entered was %d\n",choice);
        exit(1);
    }
    if (choice==1)
    {
        gp = popen(GNUPLOT_COMMAND,"w");
        if (gp==NULL)
        {
            printf("\n Could not open a pipe to gnuplot, check the definition of GNUPLOT_COMMAND in file global.h\n");
            printf("\n Edit the string to suit your system configuration and rerun the program\n");
            exit(1);
        }
        if (nobj==2)
        { 
            obj1 = 1;
            //printf("\n Enter the objective for X axis display : ");
            //scanf("%d",&obj1);
            if (obj1<1 || obj1>nobj)
            {
                printf("\n Wrong value of X objective entered, value entered was %d\n",obj1);
                exit(1);
            }
            //printf("\n Enter the objective for Y axis display : ");
            //scanf("%d",&obj2);

            obj2 = 2;
            if (obj2<1 || obj2>nobj)
            {
                printf("\n Wrong value of Y objective entered, value entered was %d\n",obj2);
                exit(1);
            }
            obj3 = -1;
        }
        else
        {
          choice = 3;
            // printf("\n #obj > 2, 2D display or a 3D display ?, enter 2 for 2D and 3 for 3D :");
            // scanf("%d",&choice);
            if (choice!=2 && choice!=3)
            {
                printf("\n Entered the wrong choice, hence exiting, choice entered was %d\n",choice);
                exit(1);
            }
            if (choice==2)
            {
                printf("\n Enter the objective for X axis display : ");
                scanf("%d",&obj1);
                if (obj1<1 || obj1>nobj)
                {
                    printf("\n Wrong value of X objective entered, value entered was %d\n",obj1);
                    exit(1);
                }
                printf("\n Enter the objective for Y axis display : ");
                scanf("%d",&obj2);
                if (obj2<1 || obj2>nobj)
                {
                    printf("\n Wrong value of Y objective entered, value entered was %d\n",obj2);
                    exit(1);
                }
                obj3 = -1;
            }
            else
            {
              obj1 = 1; obj2 = 2; obj3 = 3;
                // printf("\n Enter the objective for X axis display : ");
                // scanf("%d",&obj1);
                if (obj1<1 || obj1>nobj)
                {
                    printf("\n Wrong value of X objective entered, value entered was %d\n",obj1);
                    exit(1);
                }
                // printf("\n Enter the objective for Y axis display : ");
                // scanf("%d",&obj2);
                if (obj2<1 || obj2>nobj)
                {
                    printf("\n Wrong value of Y objective entered, value entered was %d\n",obj2);
                    exit(1);
                }
                // printf("\n Enter the objective for Z axis display : ");
                // scanf("%d",&obj3);
                if (obj3<1 || obj3>nobj)
                {
                    printf("\n Wrong value of Z objective entered, value entered was %d\n",obj3);
                    exit(1);
                }
                angle1 = 60;
                // printf("\n You have chosen 3D display, hence location of eye required \n");
                // printf("\n Enter the first angle (an integer in the range 0-180) (if not known, enter 60) :");
                // scanf("%d",&angle1);
                if (angle1<0 || angle1>180)
                {
                    printf("\n Wrong value for first angle entered, hence exiting \n");
                    exit(1);
                }
                angle2 = 30;
                // printf("\n Enter the second angle (an integer in the range 0-360) (if not known, enter 30) :");
                // scanf("%d",&angle2);
                if (angle2<0 || angle2>360)
                {
                    printf("\n Wrong value for second angle entered, hence exiting \n");
                    exit(1);
                }
            }
        }
    }
    //printf("\n Input data successfully entered, now performing initialization \n");
    //fprintf(fpt3,"# of objectives = %d, # of constraints = %d, # of real_var = %d, # of bits of bin_var = %d, constr_violation, rank, crowding_distance\n",nobj,ncon,nreal,bitlength);
    nbinmut = 0;
    nrealmut = 0;
    nbincross = 0;
    nrealcross = 0;
    parent_pop = (population *)malloc(sizeof(population));
    child_pop = (population *)malloc(sizeof(population));
    mixed_pop = (population *)malloc(sizeof(population));
    allocate_memory_pop (parent_pop, popsize);
    allocate_memory_pop (child_pop, popsize);
    allocate_memory_pop (mixed_pop, 2*popsize);
    randomize();
    initialize_pop (parent_pop);
    //printf("\n Initialization done, now performing first generation");
    decode_pop(parent_pop);
    evaluate_pop (parent_pop);
    assign_rank_and_crowding_distance (parent_pop);
    fprintf(stderr, "# position parameters: %d\n# distance parameters: %d\n", (int) (nreal / 1.2), (int) nreal - (int) (nreal / 1.2));
    fflush(stdout);
    if (choice!=0)    onthefly_display (parent_pop,gp,1);
    sleep(1);
    ngen=1;
    while (nevals - neval > 0)
    {
        selection (parent_pop, child_pop);
        mutation_pop (child_pop);
        decode_pop(child_pop);
        evaluate_pop(child_pop);
        merge (parent_pop, child_pop, mixed_pop);
        fill_nondominated_sort (mixed_pop, parent_pop);
        /* Comment following four lines if information for all
        generations is not desired, it will speed up the execution */
        //report_pop(parent_pop,fpt4);
        if (choice!=0)    onthefly_display (parent_pop,gp,ngen);
        //usleep(3000);
        //printf("\n gen = %d",i); 
        ngen++;
    }
    //printf("\n Generations finished, now reporting solutions");
    fprintf(fpt3, "# Total iterations: %d\n", ngen);
    fprintf(fpt3, "# Total evaluations: %d\n", neval);
    report_pop(parent_pop,fpt3);
    fflush(stdout);
    fflush(fpt3);
    //fclose(fpt3);
    if (choice!=0)
    {
        pclose(gp);
    }
    if (nreal!=0)
    {
        free (min_realvar);
        free (max_realvar);
    }
    if (nbin!=0)
    {
        free (min_binvar);
        free (max_binvar);
        free (nbits);
    }
    deallocate_memory_pop (parent_pop, popsize);
    deallocate_memory_pop (child_pop, popsize);
    deallocate_memory_pop (mixed_pop, 2*popsize);
    free (parent_pop);
    free (child_pop);
    free (mixed_pop);
    //printf("\n Routine successfully exited \n");
    return (0);
}
