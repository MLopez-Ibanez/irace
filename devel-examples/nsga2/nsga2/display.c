/* Routines to display the population information using gnuplot */

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>
# include <unistd.h>
# include <stdbool.h>

# include "global.h"
# include "rand.h"
bool reverse_x = true;
bool reverse_y = false;

/* Function to display the current population for the subsequent generation */
void onthefly_display (population *pop, FILE *gp, int ii)
{
  if (choice == 3) {
    if (!reverse_x) {
      if (angle2 <= 180) angle2++;
      else reverse_x = true;
    }
    if (reverse_x) {
      if (angle2 > 0) angle2--;
      else reverse_x = false;
    }
    if (!reverse_y) {
      if (angle1 <= 90) angle1++;
      else reverse_y = true;
    }
    if (reverse_y) {
      if (angle1 > 30) angle1--;
      else reverse_y = false;
    }
  }
    int i;
    int flag;
    FILE *fpt;
    fpt = fopen("plot.out","w");
    flag = 0;
    for (i=0; i<popsize; i++)
    {
        if (pop->ind[i].constr_violation==0)
        {
            if (choice!=3)
                fprintf(fpt,"%e\t%e\n",pop->ind[i].obj[obj1-1],pop->ind[i].obj[obj2-1]);
            else
                fprintf(fpt,"%e\t%e\t%e\n",pop->ind[i].obj[obj1-1],pop->ind[i].obj[obj2-1],pop->ind[i].obj[obj3-1]);
            fflush(fpt);
            flag = 1;
        }
    }
    if (flag==0)
    {
        printf("\n No feasible soln in this pop, hence no display");
    }
    else
    {
        if (choice!=3)
            fprintf(gp,"set title 'Generation #%d'\n unset key\n plot 'plot.out' w points pointtype 6 pointsize 1, 'ref.srt2' w line\n",ii);
        else
            fprintf(gp,"set title 'Generation #%d'; set view %d,%d; unset key; set grid; set hidden3d; splot 'plot.out' w points pointtype 6 pointsize 1, 'ref.srt2' w points\n",ii,angle1,angle2);
            // fprintf(gp,"set title 'Generation #%d'; set view %d,%d; unset key; set multiplot layout 1,2; splot 'plot.out' w points pointtype 6 pointsize 1; set dgrid3d 30,30; set hidden3d; splot 'ref.srt2' w line\n",ii,angle1,angle2);
        fflush(gp);
    }
    fclose(fpt);
    return;
}
