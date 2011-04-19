\name{cfVODE}
\alias{cfVODE}

\title{Solver for Ordinary Differential Equations (VODE)
       and compiled functions(c/fortran) to be used in NLME}

\description{ Generate functions for NLME Solves using a system of ordinary differential equations; a wrapper
around the implemented ODE solvers,The \R function \code{vode} provides an interface to the FORTRAN ODE
  solver of the same name, written by Peter N. Brown, Alan C. Hindmarsh
  and George D. Byrne.
  }

\usage{
cfVODE(model, data, LogParms = TRUE, JAC = FALSE, SEQ = FALSE,rtol = 1e-4, atol = 1e-4, tcrit = NULL, dllname = NULL, hmin = 0, hmax = Inf)
}
\arguments{
  \item{model }{either an \R-function that computes the values of the
    derivatives in the ODE system (the \emph{model definition}) at time
    t.The return value of \code{model} should be a list. See package
    \code{"nlmeODE"} for more details.
  }
  \item{data }{nlme GroupedData format.
  }
  \item{LogParms }{ transform parameters into log scale
  }
  \item{JAC }{A JAC set FALSE. This time we can implement this parts.
  }
  \item{SEQ }{A SEQ set FALSE.
  }
  \item{rtol }{relative error tolerance, either a scalar or an array as
    long as \code{y}. See details.
  }
  \item{atol }{absolute error tolerance, either a scalar or an array as
    long as \code{y}. See details.
  }
  \item{tcrit }{if not \code{NULL}, then \code{lsoda} cannot integrate
    past \code{tcrit}. The FORTRAN routine \code{lsoda} overshoots its
    targets (times points in the vector \code{times}), and interpolates
    values for the desired time points.  If there is a time beyond which
    integration should not proceed (perhaps because of a singularity),
    that should be provided in \code{tcrit}.
  }
  \item{dllname }{a string giving the name of the shared library
    (without extension) that contains all the compiled function or
    subroutine definitions refered to in \code{func} and
    \code{jacfunc}. See package \code{"deSolve"}.
  }

  \item{hmin }{an optional minimum value of the integration stepsize. In
    special situations this parameter may speed up computations with the
    cost of precision. Don't use \code{hmin} if you don't know why!
  }
  \item{hmax }{an optional maximum value of the integration stepsize. If
    not specified, \code{hmax} is set to the largest difference in
    \code{times}, to avoid that the simulation possibly ignores
    short-term events. If 0, no maximal size is specified.
  }
}
\examples{
#######################################################
#use c code
#see example cfLSODA
#######################################################


}
\seealso{
}
\note{
}
\keyword{math}
