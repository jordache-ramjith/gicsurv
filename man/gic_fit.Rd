\name{gic_fit}
\alias{gic_fit}
\alias{print.gicsurv_fit}
\title{Fit a generalised interval-censored proportional hazards model}
\description{
Fits a nonnegative penalised B-spline baseline hazard and selects the
smoothing parameter using the raw maximum of a stratified K-fold
cross-validation criterion.
}
\usage{
gic_fit(data, time1, time2, time3, status, covs = NULL, K = 10L,
  start_family = c("uniform", "beta"), start_shape1 = 1,
  start_shape2 = 1, cv_folds = 5L,
  log_lambda_grid = -1:18,
  fold_seed = 1986L, quad_n = 20L, center_continuous = FALSE)

\method{print}{gicsurv_fit}(x, ...)
}
\arguments{
  \item{data}{A data frame.}
  \item{time1, time2, time3}{Names of the three GIC time variables.}
  \item{status}{Name of the indicator coded 1 for an observed secondary event and 0 for right censoring.}
  \item{covs}{Optional character vector of covariate names.}
  \item{K}{Number of cubic B-spline basis coefficients.}
  \item{start_family}{Within-start-interval density: \code{"uniform"} or \code{"beta"}.}
  \item{start_shape1, start_shape2}{Positive scaled-Beta shape parameters.}
  \item{cv_folds}{Number of cross-validation folds.}
  \item{log_lambda_grid}{Candidate log smoothing parameters.}
  \item{fold_seed}{Seed for reproducible fold allocation.}
  \item{quad_n}{Number of Gauss--Legendre nodes for start-time integration.}
  \item{center_continuous}{Whether numeric covariates with more than five distinct values are centred at their analysis-sample means.}
  \item{x}{A fitted \code{gicsurv_fit} object.}
  \item{...}{Additional arguments.}
}
\value{An object of class \code{gicsurv_fit}.}
