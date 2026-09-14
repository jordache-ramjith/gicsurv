\name{calculation_analysis}
\alias{calculation_analysis}
\alias{output_analysis}
\alias{pred_analysis}
\title{Compatibility wrappers for the initial gicsurv API}
\description{
Wrappers retained to ease migration from the initial package. New code should
use \code{gic_fit()}, \code{gic_coefficients()} and
\code{predict_gicsurv()} directly.
}
\usage{
calculation_analysis(data, time1, time2, time3, status, covs = NULL,
  K = 10L, ...)

output_analysis(data = NULL, results, covs = NULL, K = NULL, ...)

pred_analysis(results, data = NULL, time1 = NULL, time2 = NULL,
  time3 = NULL, status = NULL, covs = NULL, maxtime, nboot = 1000L,
  K = NULL, ...)
}
\arguments{
  \item{data}{A data frame.}
  \item{time1, time2, time3, status, covs, K}{See \code{gic_fit()}.}
  \item{results}{A fitted \code{gicsurv_fit} object.}
  \item{maxtime}{Maximum prediction time.}
  \item{nboot}{Number of parameter simulations.}
  \item{...}{Additional arguments passed to the corresponding new function.}
}
\value{A fitted object, coefficient table or prediction table, respectively.}
