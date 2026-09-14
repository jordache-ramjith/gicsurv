#' Fit the generalised interval-censored proportional hazards model
#'
#' Fits a nonnegative penalised B-spline baseline hazard. The smoothing
#' parameter is selected by stratified K-fold cross-validation using the raw
#' maximum held-out log-likelihood. Standard errors condition on the selected
#' smoothing parameter and use the active-set Hessian.
#'
#' @param data A data frame.
#' @param time1,time2,time3 Names of the three GIC time variables.
#' @param status Name of the event indicator, coded 1=event and
#'   0=right-censored.
#' @param covs Optional character vector of covariate names.
#' @param K Number of B-spline basis coefficients.
#' @param start_family Within-start-interval density: `"uniform"` or `"beta"`.
#' @param start_shape1,start_shape2 Beta shape parameters, used only when
#'   `start_family="beta"`.
#' @param cv_folds Number of cross-validation folds.
#' @param log_lambda_grid Candidate log smoothing parameters.
#' @param fold_seed Seed used to construct reproducible folds.
#' @param quad_n Gauss--Legendre nodes for start-time integration.
#' @param center_continuous Centre numeric covariates with more than five
#'   distinct observed values at their analysis-sample means.
#' @return An object of class `gicsurv_fit`.
#' @export
gic_fit <- function(
    data, time1, time2, time3, status, covs = NULL, K = 10L,
    start_family = c("uniform", "beta"), start_shape1 = 1,
    start_shape2 = 1, cv_folds = 5L,
    log_lambda_grid = -1:18,
    fold_seed = 1986L, quad_n = 20L, center_continuous = FALSE) {
  start_family <- match.arg(start_family)
  K <- as.integer(K)
  if (K < 4L) stop("K must be at least 4 for a cubic B-spline basis.")
  if (start_family == "beta" &&
      (!is.finite(start_shape1) || !is.finite(start_shape2) ||
       start_shape1 <= 0 || start_shape2 <= 0)) {
    stop("Beta shape parameters must be finite and greater than zero.")
  }
  raw_data <- as.data.frame(data, check.names = FALSE)
  analysis_columns <- unique(c(time1, time2, status, covs))
  analysis_raw <- raw_data[stats::complete.cases(raw_data[, analysis_columns, drop = FALSE]), , drop = FALSE]
  model_data <- raw_data
  continuous_covariates <- character()
  covariate_centres <- numeric()
  covariate_info <- list()
  if (length(covs)) {
    for (variable in covs) {
      values <- analysis_raw[[variable]]
      finite_unique <- if (is.numeric(values)) {
        unique(values[is.finite(values)])
      } else character()
      is_continuous <- is.numeric(values) && length(finite_unique) > 5L
      if (is_continuous) {
        centre <- mean(values, na.rm = TRUE)
        continuous_covariates <- c(continuous_covariates, variable)
        covariate_centres[variable] <- centre
        if (isTRUE(center_continuous)) model_data[[variable]] <- raw_data[[variable]] - centre
        covariate_info[[variable]] <- list(
          type = "continuous", centre = centre,
          p25 = unname(stats::quantile(values, 0.25, na.rm = TRUE)),
          p75 = unname(stats::quantile(values, 0.75, na.rm = TRUE))
        )
      } else {
        levels <- if (is.factor(values)) levels(values) else sort(unique(values[!is.na(values)]))
        covariate_info[[variable]] <- list(type = "categorical", levels = levels)
      }
    }
  }
  fit <- fit_gic_smoothing(
    data = model_data, covs = covs, time1 = time1, time2 = time2,
    time3 = time3, status = status, K = K, degree = 3L,
    smoothing = "cv_max", positivity = "direct",
    start_family = start_family, start_shape1 = start_shape1,
    start_shape2 = start_shape2, log_lambda_grid = log_lambda_grid,
    cv_folds = as.integer(cv_folds), fold_seed = as.integer(fold_seed),
    quad_n = as.integer(quad_n)
  )
  fit$input <- list(
    time1 = time1, time2 = time2, time3 = time3, status = status,
    covs = covs, K = K, cv_folds = cv_folds, fold_seed = fold_seed,
    quad_n = quad_n, center_continuous = isTRUE(center_continuous)
  )
  fit$raw_data <- analysis_raw
  fit$model_data <- model_data
  fit$continuous_covariates <- continuous_covariates
  fit$covariate_centres <- covariate_centres
  fit$covariate_info <- covariate_info
  class(fit) <- c("gicsurv_fit", class(fit))
  fit
}

gic_profile_design <- function(object, stratify_by = NULL) {
  covs <- object$input$covs
  p <- length(object$beta)
  if (!length(covs)) {
    return(list(X = matrix(numeric(), nrow = 1L, ncol = 0L),
                labels = "Overall", values = NA_real_))
  }
  reference <- object$raw_data[1L, covs, drop = FALSE]
  for (variable in covs) {
    info <- object$covariate_info[[variable]]
    if (identical(info$type, "continuous")) {
      reference[[variable]] <- if (isTRUE(object$input$center_continuous)) info$centre else 0
    } else {
      reference[[variable]] <- info$levels[1L]
    }
  }
  labels <- if (isTRUE(object$input$center_continuous)) {
    "Mean continuous values / reference categories"
  } else {
    "Zero continuous values / reference categories"
  }
  values <- NA
  profiles <- reference
  if (!is.null(stratify_by) && nzchar(stratify_by)) {
    info <- object$covariate_info[[stratify_by]]
    if (is.null(info)) stop("The stratification variable was not included in the fitted model.")
    if (identical(info$type, "continuous")) {
      values <- c(info$p25, info$p75)
      labels <- paste0(stratify_by, " = ", format(signif(values, 4), trim = TRUE))
    } else {
      values <- info$levels
      labels <- paste0(stratify_by, " = ", values)
    }
    profiles <- reference[rep(1L, length(values)), , drop = FALSE]
    profiles[[stratify_by]] <- values
  }
  model_profiles <- profiles
  if (isTRUE(object$input$center_continuous)) {
    for (variable in object$continuous_covariates) {
      model_profiles[[variable]] <- model_profiles[[variable]] -
        object$covariate_centres[[variable]]
    }
  }
  for (variable in covs) {
    info <- object$covariate_info[[variable]]
    if (identical(info$type, "categorical") &&
        (is.factor(object$raw_data[[variable]]) || is.character(object$raw_data[[variable]]))) {
      model_profiles[[variable]] <- factor(
        model_profiles[[variable]], levels = as.character(info$levels)
      )
    }
  }
  formula <- stats::as.formula(paste("~", paste(covs, collapse = "+")))
  design <- stats::model.matrix(formula, model_profiles)
  design <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  aligned <- matrix(0, nrow = nrow(profiles), ncol = p,
                    dimnames = list(NULL, object$prepared$coefficient_names))
  shared <- intersect(colnames(design), colnames(aligned))
  aligned[, shared] <- design[, shared, drop = FALSE]
  list(X = aligned, labels = labels, values = values, data = profiles)
}

#' @export
print.gicsurv_fit <- function(x, ...) {
  cat("Generalised interval-censored proportional hazards fit\n")
  cat("Smoothing selection:", x$smoothing, "\n")
  cat("Selected lambda:", format(x$lambda, digits = 5), "\n")
  cat("Converged:", x$fit_converged, "\n")
  cat("Inference valid:", x$inference_valid, "\n")
  if (length(x$beta)) print(gic_coefficients(x), row.names = FALSE)
  invisible(x)
}

#' Covariate-effect table
#'
#' @param object A fitted `gicsurv_fit` object.
#' @param conf_level Confidence level.
#' @param formatted Return presentation-formatted character columns.
#' @return A data frame of coefficients, standard errors and hazard ratios.
#' @export
gic_coefficients <- function(object, conf_level = 0.95, formatted = FALSE) {
  stopifnot(inherits(object, "gicsurv_fit"))
  if (!length(object$beta)) {
    return(data.frame(
      Variables = character(), coef = numeric(), SE = numeric(),
      HR = numeric(), lower = numeric(), upper = numeric(),
      zval = numeric(), pval = numeric()
    ))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  coefficient_names <- object$prepared$coefficient_names
  output <- data.frame(
    Variables = coefficient_names,
    coef = object$beta, SE = object$beta_se,
    stringsAsFactors = FALSE
  )
  output$HR <- exp(output$coef)
  output$lower <- exp(output$coef - z * output$SE)
  output$upper <- exp(output$coef + z * output$SE)
  output$zval <- output$coef / output$SE
  output$pval <- 2 * stats::pnorm(abs(output$zval), lower.tail = FALSE)
  if (formatted) {
    numeric_columns <- setdiff(names(output), "Variables")
    output[numeric_columns] <- lapply(output[numeric_columns], function(x) {
      format(x, digits = 3, nsmall = 3)
    })
  }
  output
}

gic_active_covariance <- function(object) {
  geometry <- active_set_geometry(
    object$fit, object$lambda, object$prepared, object$basis,
    object$g_start, quad_n = object$input$quad_n
  )
  if (!isTRUE(geometry$ok)) stop("A valid active-set covariance was not available.")
  covariance_free <- solve(geometry$H_free)
  total <- object$basis$K + length(object$beta)
  covariance <- matrix(0, total, total)
  covariance[geometry$free_indices, geometry$free_indices] <- covariance_free
  list(covariance = covariance, geometry = geometry)
}

#' Predict the baseline hazard and survival functions
#'
#' Confidence intervals use simulation from the active-set covariance,
#' conditional on the selected smoothing parameter. Boundary spline
#' coefficients remain fixed at zero and negative simulated free coefficients
#' are truncated to zero.
#'
#' @param object A fitted `gicsurv_fit` object.
#' @param times Times at which to predict.
#' @param n_sim Number of parameter simulations; zero omits intervals.
#' @param conf_level Confidence level.
#' @param seed Random seed.
#' @param stratify_by Optional fitted covariate. Continuous covariates are
#'   evaluated at their 25th and 75th percentiles; categorical covariates are
#'   evaluated at every observed level.
#' @return A data frame of baseline estimates and intervals.
#' @export
predict_gicsurv <- function(object, times, n_sim = 1000L,
                            conf_level = 0.95, seed = 1986L,
                            stratify_by = NULL) {
  stopifnot(inherits(object, "gicsurv_fit"))
  times <- as.numeric(times)
  if (any(!is.finite(times)) || any(times < 0)) {
    stop("Prediction times must be finite and nonnegative.")
  }
  profile <- gic_profile_design(object, stratify_by)
  baseline <- predict_gic_baseline(object, times)
  estimates <- do.call(rbind, lapply(seq_len(nrow(profile$X)), function(i) {
    lp <- if (length(object$beta)) sum(profile$X[i, ] * object$beta) else 0
    multiplier <- exp(lp)
    data.frame(
      profile = profile$labels[i], time = times,
      haz = baseline$hazard * multiplier,
      ch = baseline$cumulative_hazard * multiplier,
      surv = exp(-baseline$cumulative_hazard * multiplier),
      stringsAsFactors = FALSE
    )
  }))
  if (n_sim <= 0L) return(estimates)

  covariance_info <- gic_active_covariance(object)
  mean_parameters <- c(object$theta, object$beta)
  set.seed(seed)
  draws <- MASS::mvrnorm(
    n = as.integer(n_sim), mu = mean_parameters,
    Sigma = covariance_info$covariance, tol = 1e-8
  )
  if (is.vector(draws)) draws <- matrix(draws, nrow = 1L)
  K <- object$basis$K
  theta_draws <- pmax(draws[, seq_len(K), drop = FALSE], 0)
  hazard_basis <- object$basis$hazard_basis(times)
  cumulative_basis <- object$basis$cumulative_basis(times)
  baseline_hazard_draws <- hazard_basis %*% t(theta_draws)
  baseline_cumulative_draws <- cumulative_basis %*% t(theta_draws)
  beta_draws <- if (length(object$beta)) draws[, K + seq_along(object$beta), drop = FALSE] else
    matrix(numeric(), nrow = nrow(draws), ncol = 0L)
  probabilities <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  interval <- function(x) t(apply(x, 1L, stats::quantile,
                                  probs = probabilities, na.rm = TRUE))
  intervals <- lapply(seq_len(nrow(profile$X)), function(i) {
    lp_draws <- if (length(object$beta)) as.vector(beta_draws %*% profile$X[i, ]) else
      rep(0, nrow(draws))
    multiplier <- exp(lp_draws)
    hazard_draws <- sweep(baseline_hazard_draws, 2L, multiplier, "*")
    cumulative_draws <- sweep(baseline_cumulative_draws, 2L, multiplier, "*")
    survival_draws <- exp(-cumulative_draws)
    cbind(interval(hazard_draws), interval(cumulative_draws), interval(survival_draws))
  })
  ci <- do.call(rbind, intervals)
  colnames(ci) <- c("haz.lwr", "haz.upr", "ch.lwr", "ch.upr",
                    "surv.lwr", "surv.upr")
  estimates <- cbind(estimates, ci)
  estimates
}

#' Sensitivity analysis for the assumed within-start-interval density
#'
#' Repeats the full GIC analysis over a grid of pre-specified scaled-Beta
#' densities for an interval-censored start event. This is a sensitivity
#' analysis: the Beta shape parameters are not estimated from the observed
#' outcome data. Every grid member receives its own smoothing-parameter
#' selection and final refit.
#'
#' To avoid boundary-singular and excessively concentrated densities, the
#' public function restricts both shapes to the interval [1, 5]. A Uniform
#' within-start-interval analysis is the special case `shape1 = shape2 = 1`.
#'
#' @param object A fitted object returned by [gic_fit()]. The sensitivity
#'   analysis automatically reuses its data, variables, covariates and all
#'   computational settings.
#' @param shape1_values,shape2_values Numeric vectors of fixed Beta shape
#'   values, each between 1 and 5 inclusive.
#' @param retain_fits Logical; retain fitted objects for curve comparisons.
#' @param progress Optional function called after each completed grid member
#'   with `(completed, total, label)`.
#' @return An object of class `gicsurv_start_sensitivity` containing the grid,
#'   fit summary, covariate effects, cross-validation profiles and, when
#'   requested, fitted model objects.
#' @export
gic_start_time_sensitivity <- function(
    object,
    shape1_values = seq(1, 2, by = 0.1),
    shape2_values = seq(1, 2, by = 0.1),
    retain_fits = TRUE, progress = NULL) {
  if (!inherits(object, "gicsurv_fit")) {
    stop("object must be a fitted model returned by gic_fit().")
  }
  shape1_values <- sort(unique(as.numeric(shape1_values)))
  shape2_values <- sort(unique(as.numeric(shape2_values)))
  if (!length(shape1_values) || !length(shape2_values) ||
      any(!is.finite(shape1_values)) || any(!is.finite(shape2_values)) ||
      any(shape1_values < 1 | shape1_values > 5) ||
      any(shape2_values < 1 | shape2_values > 5)) {
    stop("Both scaled-Beta shape grids must contain finite values between 1 and 5.")
  }
  grid <- expand.grid(shape1 = shape1_values, shape2 = shape2_values,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid$shape_id <- paste0(
    "Beta(", formatC(grid$shape1, format = "f", digits = 2), ",",
    formatC(grid$shape2, format = "f", digits = 2), ")"
  )
  total <- nrow(grid)
  fits <- if (isTRUE(retain_fits)) vector("list", total) else NULL
  summary_rows <- vector("list", total)
  coefficient_rows <- vector("list", total)
  profile_rows <- vector("list", total)
  settings <- object$input
  for (i in seq_len(total)) {
    started <- proc.time()[["elapsed"]]
    fitted <- tryCatch(
      gic_fit(
        data = object$raw_data, time1 = settings$time1, time2 = settings$time2,
        time3 = settings$time3, status = settings$status, covs = settings$covs,
        K = settings$K,
        start_family = "beta", start_shape1 = grid$shape1[i],
        start_shape2 = grid$shape2[i], cv_folds = settings$cv_folds,
        log_lambda_grid = object$selection$cv_profile$log_lambda,
        fold_seed = settings$fold_seed, quad_n = settings$quad_n,
        center_continuous = settings$center_continuous
      ),
      error = identity
    )
    elapsed <- proc.time()[["elapsed"]] - started
    if (inherits(fitted, "error")) {
      summary_rows[[i]] <- data.frame(
        shape_id = grid$shape_id[i], shape1 = grid$shape1[i], shape2 = grid$shape2[i],
        fit_status = "failed", error = conditionMessage(fitted), elapsed_seconds = elapsed,
        selected_lambda = NA_real_, selected_log_lambda = NA_real_,
        cv_heldout_loglik = NA_real_, inference_valid = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      profile <- fitted$selection$cv_profile
      selected <- profile[which.min(abs(profile$log_lambda - log(fitted$lambda))), , drop = FALSE]
      summary_rows[[i]] <- data.frame(
        shape_id = grid$shape_id[i], shape1 = grid$shape1[i], shape2 = grid$shape2[i],
        fit_status = if (isTRUE(fitted$fit_converged)) "completed" else "nonconverged",
        error = NA_character_, elapsed_seconds = elapsed,
        selected_lambda = fitted$lambda, selected_log_lambda = log(fitted$lambda),
        cv_heldout_loglik = selected$test_loglik,
        inference_valid = isTRUE(fitted$inference_valid), stringsAsFactors = FALSE
      )
      coefficients <- gic_coefficients(fitted)
      coefficients$shape_id <- grid$shape_id[i]
      coefficients$shape1 <- grid$shape1[i]
      coefficients$shape2 <- grid$shape2[i]
      coefficient_rows[[i]] <- coefficients
      profile$shape_id <- grid$shape_id[i]
      profile$shape1 <- grid$shape1[i]
      profile$shape2 <- grid$shape2[i]
      profile_rows[[i]] <- profile
      if (isTRUE(retain_fits)) fits[[i]] <- fitted
    }
    if (is.function(progress)) progress(i, total, grid$shape_id[i])
  }
  summary <- do.call(rbind, summary_rows)
  coefficients <- do.call(rbind, Filter(Negate(is.null), coefficient_rows))
  cv_profiles <- do.call(rbind, Filter(Negate(is.null), profile_rows))
  valid <- summary[summary$fit_status == "completed" & is.finite(summary$cv_heldout_loglik), , drop = FALSE]
  best_index <- if (nrow(valid)) which.max(valid$cv_heldout_loglik) else NA_integer_
  best <- if (nrow(valid)) valid[best_index, , drop = FALSE] else valid
  uniform_id <- grid$shape_id[grid$shape1 == 1 & grid$shape2 == 1]
  if (isTRUE(retain_fits)) names(fits) <- grid$shape_id
  out <- list(
    call = match.call(), source_fit = object, grid = grid, summary = summary,
    coefficients = coefficients, cv_profiles = cv_profiles,
    best = best, uniform_shape_id = if (length(uniform_id)) uniform_id[1] else NA_character_,
    fits = fits
  )
  class(out) <- "gicsurv_start_sensitivity"
  out
}

# Compatibility wrappers retained for users of the initial package release.
#' @export
calculation_analysis <- function(data, time1, time2, time3, status, covs = NULL,
                                 K = 10L, ...) {
  gic_fit(data, time1, time2, time3, status, covs, K, ...)
}

#' @export
output_analysis <- function(data = NULL, results, covs = NULL, K = NULL, ...) {
  gic_coefficients(results, ...)
}

#' @export
pred_analysis <- function(results, data = NULL, time1 = NULL, time2 = NULL,
                          time3 = NULL, status = NULL, covs = NULL,
                          maxtime, nboot = 1000L, K = NULL, ...) {
  predict_gicsurv(results, seq(0, maxtime, length.out = 1000L),
                  n_sim = nboot, ...)
}
