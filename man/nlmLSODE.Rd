\name{nlmLSODE}         
\alias{nlmLSODE}

\title{
      LSODE Solver for NLME
}
\description{
  Use Solver for Ordinary Differential Equations (ODE),
  Switching Automatically Between Stiff and Non-stiff Methods
  and Generate functions to be used in NLME

}
\usage{
nlmLSODE(model, data, LogParms = TRUE, JAC = FALSE, SEQ = FALSE,rtol = 1e-4, atol = 1e-4, tcrit = NULL, hmin = 0, hmax = Inf)
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
#nlmLSODE SOLVER, USE ACCORDING FUNCTIONS
####################################################################
rm(list=ls())
require(insideRODE)

data(Theoph)# examples from nlmeODE
TheophODE <- Theoph
TheophODE$Dose[TheophODE$Time!=0] <- 0
TheophODE$Cmt <- rep(1,dim(TheophODE)[1])

# model files
OneComp <- list(DiffEq=list(
                            dy1dt = ~ -ka*y1 ,
                            dy2dt = ~ ka*y1-ke*y2),
                ObsEq=list(
                            c1 = ~ 0,
                            c2 = ~ y2/CL*ke),
                Parms=c("ka","ke","CL"),
                States=c("y1","y2"),
                Init=list(0,0))

TheophModel <- nlmLSODE(OneComp,TheophODE) #ode solver
system.time(Theoph.nlme <- nlme(conc ~ TheophModel(ka,ke,CL,Time,Subject),
data = TheophODE, fixed=ka+ke+CL~1, random = pdDiag(ka+CL~1),
start=c(ka=0.5,ke=-2.5,CL=-3.2),
control=list(returnObject=TRUE,msVerbose=TRUE),
verbose=TRUE))

plot(augPred(Theoph.nlme,level=0:1))

}
\seealso{
}
\note{
}
\keyword{math}
