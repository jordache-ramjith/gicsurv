\name{gic_start_time_sensitivity}
\alias{gic_start_time_sensitivity}
\title{Sensitivity analysis for the assumed within-start-interval distribution}
\usage{
gic_start_time_sensitivity(
  object,
  shape1_values = seq(1, 2, by = 0.1),
  shape2_values = seq(1, 2, by = 0.1),
  retain_fits = TRUE, progress = NULL
)
}
\arguments{
  \item{object}{A fitted \code{gicsurv_fit} object returned by \code{gic_fit()}. Its analysis data, time variables, covariates, spline dimension, centring option and computational settings are reused automatically.}
  \item{shape1_values,shape2_values}{Fixed scaled-Beta shape grids. Both parameters must lie between 1 and 5.}
  \item{retain_fits}{Whether to retain fitted model objects for curve comparisons.}
  \item{progress}{Optional callback called after each grid member.}
}
\value{An object of class \code{gicsurv_start_sensitivity}, containing one-row-per-grid-member summaries, covariate effects, cross-validation profiles, the best held-out scenario, and optionally fitted objects.}
\description{Repeats the full GIC fit over pre-specified scaled-Beta within-start-interval densities, while retaining all specifications of a supplied fitted analysis. Each grid member obtains its own cross-validated smoothing parameter and final fit.}
\details{This is a sensitivity analysis, not a procedure for estimating the unobserved start-time density from interval-censored observations. The two Beta shape parameters are restricted to [1,5]: values below 1 create a density that diverges at an interval boundary, whereas substantially larger values may represent overly concentrated positions that are weakly supported by routinely scheduled data. Beta(1,1) is the Uniform analysis.}
\examples{
\dontrun{
fit <- gic_fit(data, time1 = "t1", time2 = "t2", time3 = "t3",
               status = "clearance", covs = c("age", "sex"))
sens <- gic_start_time_sensitivity(
  fit,
  shape1_values = seq(1, 2, by = .25),
  shape2_values = seq(1, 2, by = .25)
)
sens$summary
sens$coefficients
sens$best
}
}
