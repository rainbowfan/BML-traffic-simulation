\name{MoveCars}
\alias{MoveCars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Move cars
}
\description{
MoveCars is a function which returns a list containing a new grid (a matrix) and cars' average velocity after 1 step. Since in this model, red cars and blue cars move alternatively. The average velocity is defined as the ratio of unblocked cars and total number of cars that are supposed to move at that step. 
}
\usage{
MoveCars(grid, color = "red")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
        a BLM grid matrix in which cars are located in certain cells. CreateGrid function returns grid.
}
  \item{color}{
        a character specified by either "red" or "blue".
}
}
\details{
This function shows the predefined rules in BML model. Red cars and blue cars move alternatively. Red cars move horizontally rightward while blue cars move vertically upward. If blue cars move to the top edge of the grid, it will start from bottom but still moves in the same column. If red cars move to the right edge of the grid, it will start from the left next time but keeps moving in the same row. If next positions are occupied, cars will not move.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{comp1 }{new BLM GRID matrix after one step.}
  \item{comp2 }{average velocity after one step.}
%% ...
}

\examples{

## The function is currently defined as
function (grid, color = "red") 
{
    cars = location_car(grid)
    index = which(cars$color == color)
    rows = cars[index, 1]
    cols = cars[index, 2]
    if (color == "red") {
        nextrows = rows
        nextcols = cols + 1L
        nextcols[nextcols > ncol(grid)] = 1L
    }
    else {
        nextrows = rows + 1L
        nextrows[nextrows > nrow(grid)] = 1L
        nextcols = cols
    }
    nextlocation = cbind(nextrows, nextcols)
    emptycheck = grid[nextlocation] == ""
    grid[nextlocation[emptycheck, , drop = FALSE]] = color
    grid[cbind(rows, cols)[emptycheck, , drop = FALSE]] = ""
    unblocked_car = sum(emptycheck)
    tot_car = nrow(nextlocation)
    v = unblocked_car/tot_car
    
    returnlist = list(grid, v)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
