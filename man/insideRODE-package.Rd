\name{insideRODE-package}
\alias{insideRODE-package}
\alias{insideRODE}
\docType{package}
\title{
insideRODE build in function with ODE solver and C/FORTRAN interface to nlme
}
\description{
insideRODE build-in function with Ordinary Differential Equation solver and C/FORTRAN interface to nlme,
including nlmLSODA, nlmODE, nlmVODE,nlmLSODE for general ODE;
 cfLSODA, cfLSODE, cfODE, cfVODE solver for C/FORTRAN based ODE.
WE USE SEPERATED FILE TO GENERATE FUNCTIONS. V1.0 can read dllname from dynload
sent it to cfFUNCTIONS. This package depends on the package of nlmeODE
from Christoffer W. Tornoe. This package updated the package from ODESOLVE
into deSolve package, and implement the interface to c/fortran code.
It will greatly enhance the performance of R and nlme.
}
\details{
\tabular{ll}{
Package: \tab insideRODE\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-03-23\cr
License: \tab LGPL(>2.0) \cr
LazyLoad: \tab yes\cr
}

}
\author{
YUZHUO PAN, XIAOYU YAN

Maintainer: YUZHUO PAN <yuzhuo.pan@gmail.com>

}
\references{

}

\keyword{ package }
\seealso{  nlme, nlmeODE, deSolve, lattice

}
\examples{
\dontrun{
## show examples
example(EXnlmLSODA)
example(EXnlmLSODE)
example(EXnlmODE)
example(EXnlmVODE)
example(EXcfLSODA)
example(EXcfLSODE)
example(EXcfODE)
example(EXcfVODE)
## run demos
demo("testfile")     # partial differential equations

}

}
\keyword{ package }