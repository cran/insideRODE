\name{cpLSODA}
\alias{cpLSODA}
\title{
      LSODA Solver for NLME using compiled code(c or fortran)
}
\description{
  Use Solver for Ordinary Differential Equations (ODE),
  Switching Automatically Between Stiff and Non-stiff Methods
  and Generate functions to be used in NLME

}
\usage{
    cpLSODA(model, data, LogParms = TRUE, JAC = FALSE, SEQ = FALSE,rtol = 1e-4, atol = 1e-4, tcrit = NULL, dllname = NULL, hmin = 0, hmax = Inf)
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
#cpLSODA SOLVER
#######################################################
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

TheophModel <- nlmLSODA(OneComp,TheophODE) #ode solver

#######################################################
#example 
#sink functions
#cpLSODE
#######################################################
#sink("mymod.c")
cat("
/* file mymod.c */
#include <R.h>
#include <math.h>
static double parms[3];
#define ka parms[0]
#define ke parms[1]
#define CL parms[2]

/* initializer */
void initmod(void (* odeparms)(int *, double *))
{
   int N=2;
   odeparms(&N, parms);
}

/* names for states and derivatives */
#define y1 y[0]
#define y2 y[1]
#define dy1 ydot[0]
#define dy2 ydot[1]
#define c1 yout[0]
#define c2 yout[1]

/* Derivatives and 1 output variable */
void derivs (int *neq, double *t, double *y, double *ydot, double *yout, int *ip)
{
    dy1 = -exp(ka)*y1;
    dy2 = exp(ka)*y1-exp(ke)*y2;
    c1 = 0.0;
    c2 = y2/exp(CL)*exp(ke);
}

/* END file mymod1.c */
",fill=TRUE)
#sink()
#system("RCMD SHLIB mymod.c")
#dllname<-dyn.load("mymod.dll")[[1]]


#TheophModelc <- cpLSODA(OneComp,TheophODE,dllname=dllname)

#cpLSODA,cfLSODE, cpLSODA, cfVODE SOLVER
#sink("mymodff.f")
cat("
c file mymodf.f
        subroutine initmod(odeparms)
        external odeparms
        double precision parms(3)
        common /myparms/parms
        call odeparms(2, parms)
        return
        end
        subroutine derivs (neq, t, y, ydot, yout, ip)
        double precision t, y, ydot, ka, ke, CL
        integer neq, ip(*)
        dimension y(2), ydot(2), yout(2)
        common /myparms/ka,ke,CL
        ydot(1) = -exp(ka)*y(1)
        ydot(2) = exp(ka)*y(1)-exp(ke)*y(2)
        yout(1) = 0
        yout(2) = y(2)/exp(CL)*exp(ke)
        return
        end
",fill=TRUE)
#sink(file = NULL, append = FALSE, type = c("output"),split = FALSE)

}
\seealso{
}
\note{
}
\keyword{math}

