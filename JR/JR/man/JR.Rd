\name{JR}
\alias{JR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
R-interface to J
}
\description{
Runs the J-scripts from the given input file and prints the output to a given output file.
}
\usage{
JR(input="j.par",output="JR.log",print=TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
The input file in the current R working directory (paths to other folders do not to work yet)
}
  \item{output}{
The output file written in the current R working directory (paths to other folders do not to work yet)
}
  \item{print}{
Logical, should the output be written to screen in addition to the file.
}
}

\value{
A named list with elements error and output. Error tells whether the J run crashed (error = TRUE) 
and output gives the name of output file.
}

\author{
R-interface by Lauri Mehtatalo <\email{lauri.mehtatalo@luke.fi}> 
based on Fortran codes by Juha Lappi <\email{juha.lappi.sjk@gmail.com}>.
}

\details{
The source codes can be found at \url{https://github.com/lukefi/J}
}

%\examples{
%JR()
%}

\keyword{optimization, forest planning, simulator}