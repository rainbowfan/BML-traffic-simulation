\name{summary.BMLgrid}
\alias{summary.BMLgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of BMLgrid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{summary}{BMLgrid}(BMLgrid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ }{
Class of the input should be 'BMLgrid'
}
}
\details{
This function returns a summary including dimension of the grid, total number of steps, average velocity, total number of blue cars, red cars, color of the moving cars and the density of cars,
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{comp1}{dimension of the BML grid}
  \item{comp2}{total number of steps}
  \item{comp3 }{average velocity}
  \item{comp4 }{total number of blue cars}
  \item{comp5 }{total number of red cars}
  \item{comp6}{color of the moving cars}
  \item{comp7}{density of cars}
}

\examples{
## The function is currently defined as
function (x) 
{
    dim_grid = dim(x[[1]])
    Numsteps = x[[3]]
    velocity = x[[2]]
    blue_cars = sum(x[[1]] == "blue")
    red_cars = sum(x[[1]] == "red")
    density = (blue_cars + red_cars)/prod(dim_grid)
    moving_car = ifelse(x[[3]]\%\%2 == 0, "red", "blue")
    out = list(Grid_Dimension = dim_grid, Current_Steps = Numsteps, 
        Current_Velocity = velocity, Tot.Blue_Cars = blue_cars, 
        Tot.Red_Cars = red_cars, Current.Moving_car = moving_car, 
        Density = density)
    
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
