\name{location_car}
\alias{location_car}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Location of cars
}
\description{
This function is used to decide whether cars can advance (whether its next position is empty).
}
\usage{
location_car(grid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid }{       
               a dataframe specified by CreateGrid function. 
}
}
\details{
First two columns of the output specifies the position of each car. The third color shows the color of the car.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\examples{
## The function is currently defined as
function (grid) 
{
    i = row(grid)[grid != ""]
    j = col(grid)[grid != ""]
    pos = cbind(i, j)
    data.frame(i = i, j = j, colors = grid[pos])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

