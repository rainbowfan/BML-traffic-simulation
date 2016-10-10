#include<R.h>
#include<string.h>

void 
Movecars(int *grid,int *newgrid, int *row, int *col, int *PosRed, int *PosBlue, int *NumRed, int *NumBlue, int *NumStep, double *velocity)
{
	int x=0;
	int y=0;
	int time, red, blue;

	for(time=1;time<=*NumStep;time++){
                 
        memcpy(grid, newgrid, sizeof(int)* row[0] * col[0]);

		if(time%2!=0){
              
                        int canmoveBlue = 0;

			for(blue=0;blue<*NumBlue;blue++){

				x=PosBlue[blue];
				y=PosBlue[blue+*NumBlue];
				if((x==*row)&&grid[x+*row*(y-2)]==1){
					newgrid[x+*row*(y-1)-1]=1;
					newgrid[x+*row*(y-2)]=3;
					canmoveBlue++;
                    PosBlue[blue] = 1;
				}
				else if((x != *row)&&grid[x+*row*(y-1)]==1){

					newgrid[x+*row*(y-1)-1]=1;
					newgrid[x+*row*(y-1)]=3;
					canmoveBlue++;
                    PosBlue[blue] = x+1;
				}
			}
			
			velocity[time-1]= (double)canmoveBlue/(double)*NumBlue;
		}

		else{
			int canmoveRed = 0;
			for(red=0;red<*NumRed;red++){
				x=PosRed[red];
				y=PosRed[red+*NumRed];

				if((y==*col)&&grid[x-1]==1){
					newgrid[x+*row*(y-1)-1]=1;
					newgrid[x-1]=2;
					canmoveRed++;
                    PosRed[red+*NumRed] = 1;
				}
				else if((y != *col)&&grid[x+*row*(y-1)-1+*row]==1){
					newgrid[x+*row*(y-1)-1+*row]=2;
					newgrid[x+*row*(y-1)-1]=1;
					canmoveRed++;
                    PosRed[red+*NumRed] = y+1;
				}
			}

		        velocity[time-1]=(double)canmoveRed/(double)*NumRed;
		}
	}
}