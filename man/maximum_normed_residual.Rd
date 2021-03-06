% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mnr.R
\name{maximum_normed_residual}
\alias{maximum_normed_residual}
\title{Detect outliers using the maximum normed residual method}
\usage{
maximum_normed_residual(data = NULL, x, alpha = 0.05)
}
\arguments{
\item{data}{a data.frame}

\item{x}{the variable in the data.frame for which to find the MNR
or a vector if \code{data=NULL}}

\item{alpha}{the significance level for the test. Defaults to 0.05}
}
\value{
an object of class \code{mnr}
This object has the following fields:
\itemize{
\item \code{call} the expression used to call this function
\item \code{data} the original data used to compute the MNR
\item \code{alpha} the value of alpha given by the user
\item \code{mnr} the computed MNR test statistic
\item \code{crit} the critical value given the sample size and the
significance level
\item \code{outliers} a data.frame containing the \code{index} and
\code{value} of each of the identified outliers
\item \code{n_outliers} the number of outliers found
}
}
\description{
This function detects outliers using the maximum normed residual
method described in CMH-17-1G. This method identifies a value
as an outlier if the absolute difference between the value and
the sample mean divided by the sample standard deviation
exceeds a critical value.
}
\details{
\code{data} is an optional argument. If \code{data} is given, it
should be a
\code{data.frame} (or similar object). When \code{data} is specified, the
value of \code{x} is expected to be a variable within \code{data}. If
\code{data} is not specified, \code{x} must be a vector.

The maximum normed residual test is a test for outliers. The test statistic
is given in CMH-17-1G. Outliers are identified in the returned object.

The maximum normed residual test statistic is defined as:

\deqn{MNR = max \frac{\left| x_i - \bar{x} \right|}{s} }{
  MNR = max | x_i- x_bar | / s }

When the value of the MNR test statistic exceeds the critical value
defined in Section 8.3.3.1 of CMH-17-1G, the corresponding value
is identified as an outlier. It is then removed from the sample, and
the test statistic is computed again and compared with the critical
value corresponding with the new sample. This process is repeated until
no values are identified as outliers.
}
\examples{
library(dplyr)

carbon.fabric.2 \%>\%
  filter(test=="FC" & condition=="ETW2" & batch=="A") \%>\%
  maximum_normed_residual(strength)

## Call:
## maximum_normed_residual(data = ., x = strength)
##
## MNR =  1.958797  ( critical value = 1.887145 )
##
## Outliers ( alpha = 0.05 ):
##   Index  Value
##       6  44.26

carbon.fabric.2 \%>\%
  filter(test=="FC" & condition=="ETW2" & batch=="B") \%>\%
  maximum_normed_residual(strength)

## Call:
## maximum_normed_residual(data = ., x = strength)
##
## MNR =  1.469517  ( critical value = 1.887145 )
##
## No outliers detected ( alpha = 0.05 )

}
\references{
“Composite Materials Handbook, Volume 1. Polymer Matrix Composites
Guideline for Characterization of Structural Materials,” SAE International,
CMH-17-1G, Mar. 2012.
}
