% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equiv.R
\name{glance.equiv_change_mean}
\alias{glance.equiv_change_mean}
\title{Glance at a \code{equiv_change_mean} object}
\usage{
\method{glance}{equiv_change_mean}(x, ...)
}
\arguments{
\item{x}{a \code{equiv_change_mean} object returned from
\code{\link[=equiv_change_mean]{equiv_change_mean()}}}

\item{...}{Additional arguments. Not used. Included only to match generic
signature.}
}
\value{
A one-row \code{\link[tibble:tibble]{tibble::tibble()}} with the following
columns:
\itemize{
\item \code{alpha} the value of alpha passed to this function
\item \code{n_sample} the number of observations in the sample for which
equivalency is being checked. This is either the value \code{n_sample}
passed to this function or the length of the vector \code{data_sample}.
\item \code{mean_sample} the mean of the observations in the sample for
which equivalency is being checked. This is either the value
\code{mean_sample} passed to this function or the mean of the vector
\code{data-sample}.
\item \code{sd_sample} the standard deviation of the observations in the
sample for which equivalency is being checked. This is either the value
\code{mean_sample} passed to this function or the standard deviation of
the vector \code{data-sample}.
\item \code{n_qual} the number of observations in the qualification data
to which the sample is being compared for equivalency. This is either
the value \code{n_qual} passed to this function or the length of the
vector \code{data_qual}.
\item \code{mean_qual} the mean of the qualification data to which the
sample is being compared for equivalency. This is either the value
\code{mean_qual} passed to this function or the mean of the vector
\code{data_qual}.
\item \code{sd_qual} the standard deviation of the qualification data to
which the sample is being compared for equivalency. This is either the
value \code{mean_qual} passed to this function or the standard deviation
of the vector \code{data_qual}.
\item \code{modcv} logical value indicating whether the equivalency
calculations were performed using the modified CV approach
\item \code{sp} the value of the pooled standard deviation. If
\code{modecv = TRUE}, this pooled standard deviation includes the
modification to the qualification CV.
\item \code{t0} the test statistic
\item \code{t_req} the t-value for \eqn{\alpha / 2} and
\eqn{df = n1 + n2 -2}
\item \code{threshold_min} the minimum value of the sample mean that would
result in a pass
\item \code{threshold_max} the maximum value of the sample mean that would
result in a pass
\item \code{result} a character vector of either "PASS" or "FAIL"
indicating the result of the test for change in mean
}
}
\description{
Glance accepts an object of type \code{equiv_change_mean}
and returns a \code{\link[tibble:tibble]{tibble::tibble()}} with
one row of summaries.

Glance does not do any calculations: it just gathers the results in a
tibble.
}
\examples{
x0 <- rnorm(30, 100, 4)
x1 <- rnorm(5, 91, 7)
eq <- equiv_change_mean(data_qual = x0, data_sample = x1, alpha = 0.01)
glance(eq)

## # A tibble: 1 x 14
##   alpha n_sample mean_sample sd_sample n_qual mean_qual sd_qual modcv
##   <dbl>    <int>       <dbl>     <dbl>  <int>     <dbl>   <dbl> <lgl>
## 1  0.01        5        85.8      9.93     30      100.    3.90 FALSE
## # ... with 6 more variables: sp <dbl>, t0 <dbl>, t_req <dbl>,
## #   threshold_min <dbl>, threshold_max <dbl>, result <chr>

}
\seealso{
\code{\link[=equiv_change_mean]{equiv_change_mean()}}
}
