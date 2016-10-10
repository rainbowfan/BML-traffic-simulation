\name{plot.BMLgrid}
\alias{plot.BMLgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.BMLgrid
}
\description{
This plot function returns grid where red and blue cars are located at different positions.
}
\usage{
\method{plot}{BMLgrid}(BMLgrid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid }{
       a BML grid matrix in which cars are located in different positions.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
    z = matrix(match(grid, c("", "red", "blue")), nrow(loca), 
        ncol(loca))
    image(t(z), col = c("white", "red", "blue"), axes = FALSE)
    box()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line