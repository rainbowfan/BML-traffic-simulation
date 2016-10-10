\name{CreateGrid}
\alias{CreateGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
CreateGrid
}
\description{
In this function, grids are specified by its rows and columns.
}
\usage{
CreateGrid(r, c, ncars = c(red, blue))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r }{
       a number specifying the number of rows of a grid.
}
  \item{c}{
       a number specifying the number of columns of a grid.
}
  \item{ncars}{
       a vector specifying the number of red cars and the number of blue cars on the grid.
}
}
\details{
If total number of cars is not less than the total number of cells on the grid, the function will stop. 
}
\value{
This function will return a BML grid matrix in which each car is located in a random position in the matrix.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
## The function is currently defined as
function (r, c, ncars = c(red, blue)) 
{
    if (sum(ncars) >= r * c) 
        stop("overload")
    grid = matrix("", nrow = r, ncol = c)
    set.seed(1)
    pos = sample(1:(r * c), sum(ncars))
    grid[pos] = rep(c("red", "blue"), c(ncars[1], ncars[2]))
    class(grid) = c("BMLgrid", class(grid))
    
    return(grid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
