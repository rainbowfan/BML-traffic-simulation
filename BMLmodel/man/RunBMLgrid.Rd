\name{RunBMLgrid}
\alias{RunBMLgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RunBMLgrid
}
\description{
This function returns new positions of red and blue cars after a certain number of steps specified by callers.
}
\usage{
RunBMLgrid(r, c, ncars = c(red, blue), NumSteps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
        the number of rows in the grid matrix.
}
  \item{c}{
        the number of columns in the grid matrix.
}
  \item{ncars}{
        character vector specifying the number of red cars and the number of blue cars.
}
  \item{NumSteps}{
        a number specifying steps.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{comp1}{new grid matrix after n steps}
  \item{comp2}{average velocity of cars after n steps}
  \item{comp3}{Steps specified by callers, this value will be used in summary function in this package} 
}

\examples{
## The function is currently defined as
function (r, c, ncars = c(red, blue), NumSteps) 
{
    grid = CreateGrid(r, c, ncars)
    colors = rep(c("blue", "red"), ceiling(NumSteps/2))
    locations = MoveCars(grid, color = colors[1])[[1]]
    velocity = MoveCars(grid, color = colors[1])[[2]]
    for (i in 2:NumSteps) {
        locations = MoveCars(locations, color = colors[i])[[1]]
        velocity = MoveCars(locations, color = colors[i])[[2]]
    }
    list(locations, velocity, NumSteps)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
