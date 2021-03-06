\name{nlmODEcpcp}
\alias{nlmODEcp}

\title{General Solver for Ordinary Differential Equations
       and Generate functions to be used in NLME, this version
       just use default function. The future version will provide buildin
       methods such as "lsoda", "lsode", "lsodes", "lsodar", "vode", "daspk",
           "euler", "rk4", "ode23", "ode45", "radau",
           "bdf", "bdf_d", "adams", "impAdams", "impAdams_d".
}

\description{ Generate functions for NLME Solves using a system of ordinary differential equations; a wrapper
around the implemented ODE solvers}

\usage{
nlmODEcp(model, data, LogParms = TRUE, JAC = FALSE, SEQ = FALSE,rtol = 1e-4, atol = 1e-4, tcrit = NULL, hmin = 0, hmax = Inf)
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
####################################################################
#general model from nlmeODE package
#nlmODEcp SOLVER, USE ACCORDING FUNCTIONS
####################################################################
}
\seealso{
}
\note{
}
\keyword{math}


