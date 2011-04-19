\name{insideRODE-package}
\alias{insideRODE-package}
\alias{insideRODE}
\docType{package}
\title{
insideRODE includes buildin functions with deSolve solver and C/FORTRAN interfaces to nlme, together with compiled codes.}
\description{
insideRODE build-in function with Ordinary Differential Equation solver and C/FORTRAN interface to nlme,
including nlmLSODA, nlmODE, nlmVODE,nlmLSODE for general ODE;
 cfLSODA, cfLSODE, cfODE, cfVODE solver for C/FORTRAN based ODE.
WE USE SEPERATED FILE TO GENERATE FUNCTIONS. V1.0 can read dllname from dynload, sent them to cf FUNCTIONS. 
insideRODE package also includes buildin functions from deSolve, compiled functions from compiler, 
and C/FORTRAN code interfaces to nlme. It includes nlmLSODA, nlmODE, nlmVODE,nlmLSODE for general purpose; 
cfLSODA,cfLSODE, cfODE, cfVODE call C/FORTRAN compiled dll functions.
Ver2.0 add sink()function into example it helps to directly combine c/fortran source code in R files. 
Finally, with new compiler package, we generated compiled functions: nlmODEcp, nlmVODEcp, nlmLSODEcp,nlmLSODAcp 
and cpODE, cpLSODA, cpLSODE, cpVODE. They will help to increase speed.
This package depends on the package of nlmeODE
from Christoffer W. Tornoe. This package updated the package from ODESOLVE
into deSolve package, and implement the interface to c/fortran code.
It will greatly enhance the performance of R and nlme.
}
\details{
\tabular{ll}{
Package: \tab insideRODE\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2011-04-19\cr
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
\seealso{  nlme, nlmeODE, deSolve, lattice, compiler

}
\examples{
\dontrun{
## show examples
example(EXnlmLSODA)
example(EXnlmLSODE)
example(EXnlmODE)
example(EXnlmVODE)
example(EXnlmLSODAcp)#compiled code with compiler
example(EXnlmLSODEcp)
example(EXnlmVODEcp)
example(EXnlmODEcp)
example(EXcfLSODA)# compiled dll function
example(EXcfLSODE)
example(EXcfODE)
example(EXcfVODE)
example(EXcpODE)  #compiled code with compiler
example(EXcpLSODE)
example(EXcpVODE)
example(EXcpLSODA)
## run demos
demo("testfile")     # differential equations 

}

}
\keyword{ package }