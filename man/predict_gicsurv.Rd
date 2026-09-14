\name{predict_gicsurv}
\alias{predict_gicsurv}
\title{Predict baseline hazard and survival functions}
\description{
Computes baseline hazard, cumulative hazard and survival estimates. Simulation
intervals use the active-set covariance conditional on the selected smoothing
parameter.
}
\usage{
predict_gicsurv(object, times, n_sim = 1000L, conf_level = 0.95,
  seed = 1986L, stratify_by = NULL)
}
\arguments{
  \item{object}{A fitted \code{gicsurv_fit} object.}
  \item{times}{Nonnegative prediction times.}
  \item{n_sim}{Number of parameter simulations; zero omits intervals.}
  \item{conf_level}{Confidence level.}
  \item{seed}{Random seed.}
  \item{stratify_by}{Optional fitted covariate. Continuous covariates are evaluated at their 25th and 75th percentiles; categorical covariates at their observed levels.}
}
\value{A data frame of baseline estimates and, when requested, intervals.}
