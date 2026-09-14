# gicsurv 0.1.0

- Replaced the original squared-coefficient smoothing shortcut with the
  revised nonnegative penalised B-spline estimator.
- Added raw-maximum cross-validation smoothing selection.
- Added active-set inference and diagnostics.
- Added prespecified scaled-Beta within-start-interval sensitivity analyses.
- Rebuilt the Shiny interface and corrected the event-status documentation.
- Added mean-centering of continuous covariates and covariate-stratified
  hazard and survival curves.
- Added formatted Word downloads for covariate effects and diagnostics.
- Added data-definition help, a scaled-Beta density preview, an independent
  README and a rendered mathematics tab.
- Added the dark interface, live elapsed-time display, bouncing-ball fitting
  overlay and smoothing-selection diagnostic plot.
- Added a cached likelihood engine that precomputes fixed quadrature rules and
  B-spline basis matrices. Validation against the reference engine retained
  the selected smoothing value and scientifically equivalent estimates while
  reducing benchmark fitting time approximately 9-fold for exact/IC and
  21-fold for DIC data.
