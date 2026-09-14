# gicsurv

`gicsurv` fits penalised B-spline proportional hazards models for
generalised interval-censored time-to-event data. The elapsed time of
interest runs from a start event to a secondary event. Either event may be
observed exactly or only between visits, and the secondary event may be
right-censored. These observation patterns can be mixed within one dataset.

The package includes a programmatic R interface and a Shiny application.

## Intended setting

The method was developed for epidemiological and clinical studies with
routinely scheduled follow-up. Visit timing is assumed to be non-informative
for the latent event times, conditional on the fitted covariates. When an
unscheduled, clinically triggered visit is followed by an intervention that
interrupts natural follow-up, the secondary-event time should generally be
right-censored at that intervention.

## Statistical model

The package uses:

- a proportional hazards model for covariate effects;
- a nonnegative cubic B-spline representation of the baseline hazard;
- an integrated squared second-derivative roughness penalty;
- directly constrained nonnegative spline coefficients;
- maximum held-out log-likelihood from stratified K-fold cross-validation to
  select the smoothing parameter;
- a cached likelihood engine that precomputes fixed quadrature and spline
  basis calculations without changing the likelihood or smoothing criterion;
- an active-set Hessian with feasible finite-difference steps for inference;
- either a uniform or a prespecified scaled-Beta density for the latent start
  event within its observed start interval.

The uniform assumption is local: it applies only within each person's
observed start interval. It does **not** assume that starts occur uniformly
from enrolment or study entry. The scaled-Beta option is intended primarily
for prespecified sensitivity analyses.

## Installation

Install or update the current GitHub version with:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("jordache-ramjith/gicsurv")
```

Restart R after updating the package. To overwrite an existing installation
with the current GitHub version, add `force = TRUE` to `install_github()`.

Install a local source archive with:

```r
install.packages("gicsurv_0.1.0.tar.gz", repos = NULL, type = "source")
```

During package development, the source directory can be installed with:

```r
install.packages("gicsurv", repos = NULL, type = "source")
```

## Required data structure

The input data must contain four time/status variables.

| Variable | Definition |
|---|---|
| `time1` | Right boundary of the start-event interval; use `0` when the start is exactly observed. |
| `time2` | Lower boundary of the secondary-event interval, or the right-censoring time. |
| `time3` | Upper boundary of the secondary-event interval; it may be missing or infinite for right-censored observations. |
| `status` | `1` when the secondary event was observed and `0` when it was right-censored. |

Additional numeric or categorical columns may be included as covariates.

## Launch the Shiny application

```r
gicsurv::gicsurv()
```

The application supports CSV, XLS and XLSX input and provides:

- definitions and validation of the required data fields;
- optional centring of continuous covariates;
- uniform and scaled-Beta within-start-interval analyses;
- covariate-effect estimates and formatted Word/CSV downloads;
- hazard and survival curves, including covariate-stratified curves;
- smoothing-selection and optimisation diagnostics;
- PDF, CSV and Word downloads;
- an in-app mathematical description of all seven likelihood contributions.
- a fixed scaled-Beta sensitivity-analysis tab for comparing pre-specified
  within-start-interval assumptions.

## Fit a model from R

```r
fit <- gicsurv::gic_fit(
  data = my_data,
  time1 = "time1",
  time2 = "time2",
  time3 = "time3",
  status = "status",
  covs = c("age", "sex"),
  K = 10,
  center_continuous = TRUE
)
```

Extract the covariate effects:

```r
gicsurv::gic_coefficients(fit)
```

Estimate hazard and survival curves:

```r
curves <- gicsurv::predict_gicsurv(
  fit,
  times = seq(0, 100, length.out = 200),
  n_sim = 1000,
  seed = 1986
)
```

Stratify curves by a fitted covariate:

```r
age_curves <- gicsurv::predict_gicsurv(
  fit,
  times = seq(0, 100, length.out = 200),
  stratify_by = "age"
)
```

For continuous variables, the stratified profiles use the 25th and 75th
percentiles. For categorical variables, they use the fitted levels.

## Scaled-Beta sensitivity analysis

To prespecify a non-uniform location for the latent start event within its
observed interval, supply the scaled-Beta density and its two shape
parameters. Shape parameters should be chosen before examining the
secondary-event outcomes whenever possible.

```r
fit_beta <- gicsurv::gic_fit(
  data = my_data,
  time1 = "time1",
  time2 = "time2",
  time3 = "time3",
  status = "status",
  covs = c("age", "sex"),
  start_family = "beta",
  start_shape1 = 1,
  start_shape2 = 2
)
```

To compare a planned grid of fixed assumptions, use:

```r
sensitivity <- gicsurv::gic_start_time_sensitivity(
  fit,
  shape1_values = seq(1, 2, by = 0.25),
  shape2_values = seq(1, 2, by = 0.25)
)

sensitivity$summary
sensitivity$coefficients
```

The fitted object supplies the data, variables, covariates, spline dimension
and computational settings. The function fits the complete model, including a separate
cross-validated smoothing selection, for every supplied fixed-Beta density.
It is a sensitivity analysis, not an estimation method for the latent
start-time distribution. Shapes are restricted to 1--5 to avoid
boundary-singular or overly concentrated densities.

## Diagnostics

Before interpreting a fit, check:

- whether optimisation converged;
- whether inference was reported as valid;
- whether the chosen smoothing parameter is at the lower or upper boundary
  of the search grid;
- the cross-validated held-out log-likelihood profile;
- the number of spline coefficients estimated at the zero boundary.

A zero boundary coefficient means that its local nonnegative B-spline
component was not needed at the constrained optimum. This count is not an
`mgcv`-style effective degrees of freedom measure.

Standard errors and confidence intervals are conditional on the selected
smoothing parameter and fitted active boundary set.

## Reproducibility

Reports should record the package version, spline dimension, smoothing grid,
number of cross-validation folds, quadrature nodes, random seed,
within-start-interval density and any scaled-Beta shape parameters.

## Citation

Please cite the associated methodological paper when using `gicsurv`.
Complete bibliographic details will be added after publication.

## License

See `LICENSE` in the package source.
