\name{gic_coefficients}
\alias{gic_coefficients}
\title{Covariate-effect table}
\description{Returns coefficients, standard errors, hazard ratios, confidence intervals and p-values.}
\usage{
gic_coefficients(object, conf_level = 0.95, formatted = FALSE)
}
\arguments{
  \item{object}{A fitted \code{gicsurv_fit} object.}
  \item{conf_level}{Confidence level.}
  \item{formatted}{Whether to return formatted character columns.}
}
\value{A data frame.}
