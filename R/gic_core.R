# Smoothing-method validation functions for DGEM.2026.0012
#
# This file is deliberately self-contained. It implements:
#   * the corrected seven-pattern GIC likelihood;
#   * cubic B-spline hazards and the same second-derivative roughness penalty S;
#   * four positivity implementations;
#   * warm-started stratified K-fold CV with one-SE and raw-maximum rules;
#   * archived profile-Laplace and GFS functions for audit comparisons;
#   * common diagnostics and prediction summaries.
#
# IMPORTANT: direct bounds are primary. Exponential, softplus, and square maps
# are computational sensitivity parameterisations only.

required_packages <- c("splines2", "pracma", "numDeriv")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install required packages: ", paste(missing_packages, collapse = ", "))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

log1pexp <- function(x) {
  out <- numeric(length(x))
  high <- x > 30
  low <- x < -30
  mid <- !(high | low)
  out[high] <- x[high]
  out[low] <- exp(x[low])
  out[mid] <- log1p(exp(x[mid]))
  out
}

inverse_softplus <- function(theta) {
  theta <- pmax(theta, 1e-12)
  ifelse(theta > 30, theta, log(expm1(theta)))
}

theta_from_working <- function(a, method) {
  switch(
    method,
    direct = a,
    exponential = exp(pmin(a, 700)),
    softplus = log1pexp(a),
    square = a^2,
    stop("Unknown positivity method: ", method)
  )
}

working_from_theta <- function(theta, method) {
  theta <- pmax(theta, 1e-12)
  switch(
    method,
    direct = theta,
    exponential = log(theta),
    softplus = inverse_softplus(theta),
    square = sqrt(theta),
    stop("Unknown positivity method: ", method)
  )
}

gauss_legendre_integrate <- function(f, lower, upper, n = 20L) {
  if (!is.finite(lower) || !is.finite(upper) || lower > upper) {
    stop("Quadrature bounds must be finite and ordered.")
  }
  if (lower == upper) return(0)
  rule <- pracma::gaussLegendre(as.integer(n), lower, upper)
  sum(rule$w * f(rule$x))
}

make_start_density <- function(family = c("uniform", "beta"),
                               shape1 = 1, shape2 = 1) {
  family <- match.arg(family)
  if (shape1 <= 0 || shape2 <= 0) stop("Beta shapes must be positive.")
  force(family); force(shape1); force(shape2)
  function(w, lower, upper) {
    width <- upper - lower
    if (!is.finite(width) || width <= 0) {
      stop("The start-event interval must have positive finite width.")
    }
    z <- (w - lower) / width
    inside <- z >= 0 & z <= 1
    out <- numeric(length(w))
    if (family == "uniform") {
      out[inside] <- 1 / width
    } else {
      out[inside] <- stats::dbeta(
        z[inside], shape1 = shape1, shape2 = shape2
      ) / width
    }
    out
  }
}

classify_gic_pattern <- function(t1, t2, t3, status, tolerance = 1e-10) {
  event_observed <- status == 1
  exact_start <- abs(t1) <= tolerance
  exact_event <- event_observed && is.finite(t3) &&
    abs(t2 - t3) <= tolerance
  if (!event_observed) {
    return(if (exact_start) "exact_start_right" else "ic_start_right")
  }
  if (exact_start && exact_event) return("exact_exact")
  if (exact_start) return("exact_start_ic")
  if (exact_event) return("ic_start_exact")
  overlap <- abs(t2) <= tolerance && abs(t3 - t1) <= tolerance
  if (overlap) "dic_overlap" else "dic_no_overlap"
}

prepare_gic_data <- function(data, time1 = "time1", time2 = "time2",
                             time3 = "time3", status = "status",
                             covs = NULL) {
  required <- unique(c(time1, time2, time3, status, covs))
  missing_names <- setdiff(required, names(data))
  if (length(missing_names)) {
    stop("Missing variables: ", paste(missing_names, collapse = ", "))
  }
  complete_variables <- unique(c(time1, time2, status, covs))
  keep <- stats::complete.cases(data[, complete_variables, drop = FALSE])
  data <- data[keep, , drop = FALSE]
  if (!nrow(data)) stop("No complete observations remain.")

  t1 <- as.numeric(data[[time1]])
  t2 <- as.numeric(data[[time2]])
  t3 <- as.numeric(data[[time3]])
  event <- as.integer(data[[status]])
  if (!all(event %in% c(0L, 1L))) {
    stop("status must be coded 1=event and 0=right-censored.")
  }
  t3[event == 0L] <- Inf
  if (any(t1 < 0 | t2 < 0)) stop("Observed times must be nonnegative.")

  patterns <- vapply(
    seq_along(t1),
    function(i) classify_gic_pattern(t1[i], t2[i], t3[i], event[i]),
    character(1)
  )

  if (is.null(covs) || !length(covs)) {
    X <- matrix(numeric(0), nrow = nrow(data), ncol = 0L)
    coefficient_names <- character(0)
  } else {
    formula <- stats::as.formula(paste("~", paste(covs, collapse = "+")))
    mm <- stats::model.matrix(formula, data = data)
    X <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    coefficient_names <- colnames(X)
  }
  list(
    data = data, t1 = t1, t2 = t2, t3 = t3, status = event,
    pattern = patterns, X = X, coefficient_names = coefficient_names
  )
}

make_bspline_hazard_basis <- function(observed_times, K = 10L, degree = 3L,
                                      penalty_quad_n = 100L) {
  times <- observed_times[is.finite(observed_times) & observed_times >= 0]
  if (length(times) < 2L || max(times) <= 0) {
    stop("At least two finite nonnegative times with a positive maximum are required.")
  }
  boundary <- c(0, max(times))
  n_internal <- as.integer(K) - as.integer(degree) - 1L
  if (n_internal < 0L) stop("K must be at least degree + 1.")
  knots <- if (n_internal) {
    seq(boundary[1], boundary[2], length.out = n_internal + 2L)[
      -c(1L, n_internal + 2L)
    ]
  } else {
    numeric(0)
  }
  seed_basis <- splines2::bSpline(
    times, knots = knots, degree = as.integer(degree),
    intercept = TRUE, Boundary.knots = boundary
  )

  hazard_basis <- function(t, derivs = 0L) {
    t <- pmin(pmax(t, boundary[1]), boundary[2])
    splines2::bSpline(
      t, knots = knots, degree = degree, intercept = TRUE,
      Boundary.knots = boundary, derivs = as.integer(derivs)
    )
  }
  cumulative_basis <- function(t) {
    t <- pmin(pmax(t, boundary[1]), boundary[2])
    splines2::ibs(
      t, knots = knots, degree = degree, intercept = TRUE,
      Boundary.knots = boundary
    )
  }
  rule <- pracma::gaussLegendre(
    as.integer(penalty_quad_n), boundary[1], boundary[2]
  )
  d2 <- hazard_basis(rule$x, derivs = 2L)
  S_raw <- crossprod(d2, d2 * rule$w)
  S_raw <- (S_raw + t(S_raw)) / 2
  eig_raw <- eigen(S_raw, symmetric = TRUE, only.values = TRUE)$values
  tol_raw <- max(abs(eig_raw)) * ncol(S_raw) * .Machine$double.eps^0.75
  positive_raw <- eig_raw > tol_raw
  # With time measured in days, the raw second-derivative matrix can be tiny
  # and force lambda into the tens of millions. Normalising by the mean
  # positive eigenvalue changes only the numerical units of lambda:
  # lambda_scaled * S_scaled = lambda_raw * S_raw.
  S_scale <- mean(eig_raw[positive_raw])
  if (!is.finite(S_scale) || S_scale <= 0) {
    stop("Could not obtain a positive roughness-matrix scale.")
  }
  S <- S_raw / S_scale
  eig <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  tol <- max(abs(eig)) * ncol(S) * .Machine$double.eps^0.75
  positive <- eig > tol
  list(
    K = ncol(seed_basis), degree = degree, knots = knots,
    boundary = boundary, hazard_basis = hazard_basis,
    cumulative_basis = cumulative_basis, S = S, S_raw = S_raw,
    S_scale = S_scale,
    S_rank = sum(positive),
    S_logdet_positive = sum(log(eig[positive])),
    S_eigenvalues = eig
  )
}

make_survival_functions <- function(theta, basis) {
  if (length(theta) != basis$K || any(!is.finite(theta)) ||
      any(theta < 0)) {
    stop("theta must be finite, nonnegative, and match the basis dimension.")
  }
  hazard <- function(t, lp = 0) {
    as.vector(basis$hazard_basis(t) %*% theta) * exp(lp)
  }
  cumulative_hazard <- function(t, lp = 0) {
    as.vector(basis$cumulative_basis(t) %*% theta) * exp(lp)
  }
  survival <- function(t, lp = 0) exp(-cumulative_hazard(t, lp))
  density <- function(t, lp = 0) survival(t, lp) * hazard(t, lp)
  list(
    hazard = hazard, cumulative_hazard = cumulative_hazard,
    survival = survival, density = density
  )
}

gic_contribution <- function(pattern, t1, t2, t3, survival, density,
                             g_start, lp = 0, quad_n = 20L) {
  integrate_w <- function(fun) {
    gauss_legendre_integrate(
      function(w) g_start(w, 0, t1) * fun(w), 0, t1, n = quad_n
    )
  }
  value <- switch(
    pattern,
    dic_no_overlap = integrate_w(function(w) {
      survival(t2 - w, lp) - survival(t3 - w, lp)
    }),
    dic_overlap = integrate_w(function(w) {
      1 - survival(t1 - w, lp)
    }),
    ic_start_right = integrate_w(function(w) survival(t2 - w, lp)),
    ic_start_exact = integrate_w(function(w) density(t2 - w, lp)),
    exact_start_ic = survival(t2, lp) - survival(t3, lp),
    exact_start_right = survival(t2, lp),
    exact_exact = density(t2, lp),
    stop("Unknown censoring pattern: ", pattern)
  )
  if (!is.finite(value) || value < 0) return(NA_real_)
  value
}

gic_loglik_reference <- function(theta, beta, prepared, basis, g_start,
                                 quad_n = 20L,
                                 contribution_floor = 1e-12) {
  sf <- make_survival_functions(theta, basis)
  lp <- if (ncol(prepared$X)) {
    as.vector(prepared$X %*% beta)
  } else {
    rep(0, length(prepared$t1))
  }
  contributions <- vapply(seq_along(prepared$t1), function(i) {
    gic_contribution(
      pattern = prepared$pattern[i], t1 = prepared$t1[i],
      t2 = prepared$t2[i], t3 = prepared$t3[i],
      survival = sf$survival, density = sf$density,
      g_start = g_start, lp = lp[i], quad_n = quad_n
    )
  }, numeric(1))
  if (any(!is.finite(contributions)) || any(contributions <= 0)) return(-Inf)
  sum(log(pmax(contributions, contribution_floor)))
}

# Compile every quadrature rule and spline-basis evaluation that depends only
# on the observed data and the fixed basis. Optimisation then changes only
# theta and beta, so these expensive quantities do not need to be reconstructed
# at every likelihood evaluation.
compile_gic_likelihood <- function(prepared, basis, g_start, quad_n = 20L) {
  compile_interval <- function(pattern, t1, t2, t3) {
    rule <- pracma::gaussLegendre(as.integer(quad_n), 0, t1)
    w <- as.numeric(rule$x)
    weights <- as.numeric(rule$w) * as.numeric(g_start(w, 0, t1))
    out <- list(pattern = pattern, weights = weights)
    if (pattern == "dic_no_overlap") {
      out$C2 <- basis$cumulative_basis(t2 - w)
      out$C3 <- basis$cumulative_basis(t3 - w)
    } else if (pattern == "dic_overlap") {
      out$C <- basis$cumulative_basis(t1 - w)
    } else if (pattern == "ic_start_right") {
      out$C <- basis$cumulative_basis(t2 - w)
    } else if (pattern == "ic_start_exact") {
      out$C <- basis$cumulative_basis(t2 - w)
      out$H <- basis$hazard_basis(t2 - w)
    } else {
      stop("Unexpected interval-start pattern: ", pattern)
    }
    out
  }

  entries <- lapply(seq_along(prepared$t1), function(i) {
    pattern <- prepared$pattern[i]
    if (pattern %in% c(
      "dic_no_overlap", "dic_overlap", "ic_start_right", "ic_start_exact"
    )) {
      return(compile_interval(
        pattern, prepared$t1[i], prepared$t2[i], prepared$t3[i]
      ))
    }
    if (pattern == "exact_start_ic") {
      return(list(
        pattern = pattern,
        C2 = basis$cumulative_basis(prepared$t2[i]),
        C3 = basis$cumulative_basis(prepared$t3[i])
      ))
    }
    if (pattern == "exact_start_right") {
      return(list(
        pattern = pattern,
        C = basis$cumulative_basis(prepared$t2[i])
      ))
    }
    if (pattern == "exact_exact") {
      return(list(
        pattern = pattern,
        C = basis$cumulative_basis(prepared$t2[i]),
        H = basis$hazard_basis(prepared$t2[i])
      ))
    }
    stop("Unknown censoring pattern: ", pattern)
  })
  list(entries = entries, quad_n = as.integer(quad_n), K = basis$K)
}

cached_survival <- function(C, theta, exp_lp) {
  exp(-as.vector(C %*% theta) * exp_lp)
}

gic_loglik_cached <- function(theta, beta, prepared, basis, g_start,
                              quad_n = 20L,
                              contribution_floor = 1e-12) {
  cache <- prepared$likelihood_cache
  if (is.null(cache)) {
    stop("Cached likelihood requested without a compiled likelihood design.")
  }
  if (cache$K != basis$K || cache$quad_n != as.integer(quad_n)) {
    stop("Compiled likelihood design does not match the basis or quadrature.")
  }
  lp <- if (ncol(prepared$X)) {
    as.vector(prepared$X %*% beta)
  } else {
    rep(0, length(prepared$t1))
  }
  contributions <- vapply(seq_along(cache$entries), function(i) {
    entry <- cache$entries[[i]]
    exp_lp <- exp(lp[i])
    value <- switch(
      entry$pattern,
      dic_no_overlap = sum(entry$weights * (
        cached_survival(entry$C2, theta, exp_lp) -
          cached_survival(entry$C3, theta, exp_lp)
      )),
      dic_overlap = sum(entry$weights * (
        1 - cached_survival(entry$C, theta, exp_lp)
      )),
      ic_start_right = sum(
        entry$weights * cached_survival(entry$C, theta, exp_lp)
      ),
      ic_start_exact = {
        survival <- cached_survival(entry$C, theta, exp_lp)
        hazard <- as.vector(entry$H %*% theta) * exp_lp
        sum(entry$weights * survival * hazard)
      },
      exact_start_ic =
        cached_survival(entry$C2, theta, exp_lp) -
          cached_survival(entry$C3, theta, exp_lp),
      exact_start_right = cached_survival(entry$C, theta, exp_lp),
      exact_exact = {
        survival <- cached_survival(entry$C, theta, exp_lp)
        hazard <- as.vector(entry$H %*% theta) * exp_lp
        survival * hazard
      },
      stop("Unknown cached censoring pattern: ", entry$pattern)
    )
    if (length(value) != 1L || !is.finite(value) || value <= 0) NA_real_
    else value
  }, numeric(1))
  if (any(!is.finite(contributions)) || any(contributions <= 0)) return(-Inf)
  sum(log(pmax(contributions, contribution_floor)))
}

# Assemble the precomputed likelihood ingredients by observation pattern. This
# is algebraically identical to the generic cache, but has no R-level loop over
# individuals during each objective evaluation.
compile_pattern_vector_cache <- function(prepared, basis) {
  entries <- prepared$likelihood_cache$entries; patterns <- unique(prepared$pattern)
  batches <- lapply(patterns, function(pattern) {
    indices <- which(prepared$pattern == pattern); selected <- entries[indices]; out <- list(pattern = pattern, indices = indices)
    if (pattern == "dic_no_overlap") { out$weights <- do.call(rbind, lapply(selected, `[[`, "weights")); out$C2 <- do.call(rbind, lapply(selected, `[[`, "C2")); out$C3 <- do.call(rbind, lapply(selected, `[[`, "C3"))
    } else if (pattern %in% c("dic_overlap", "ic_start_right")) { out$weights <- do.call(rbind, lapply(selected, `[[`, "weights")); out$C <- do.call(rbind, lapply(selected, `[[`, "C"))
    } else if (pattern == "ic_start_exact") { out$weights <- do.call(rbind, lapply(selected, `[[`, "weights")); out$C <- do.call(rbind, lapply(selected, `[[`, "C")); out$H <- do.call(rbind, lapply(selected, `[[`, "H"))
    } else if (pattern == "exact_start_ic") { out$C2 <- do.call(rbind, lapply(selected, `[[`, "C2")); out$C3 <- do.call(rbind, lapply(selected, `[[`, "C3"))
    } else if (pattern == "exact_start_right") { out$C <- do.call(rbind, lapply(selected, `[[`, "C"))
    } else if (pattern == "exact_exact") { out$C <- do.call(rbind, lapply(selected, `[[`, "C")); out$H <- do.call(rbind, lapply(selected, `[[`, "H"))
    } else stop("Unknown censoring pattern: ", pattern)
    out
  })
  names(batches) <- patterns; list(batches = batches, K = basis$K, quad_n = prepared$likelihood_cache$quad_n)
}

gic_loglik_pattern_vectorised <- function(theta, beta, prepared, basis, contribution_floor = 1e-12) {
  cache <- prepared$pattern_vector_cache
  if (is.null(cache)) stop("Missing pattern-vectorised likelihood cache.")
  if (cache$K != basis$K) stop("Pattern-vectorised cache does not match basis.")
  lp <- if (ncol(prepared$X)) as.vector(prepared$X %*% beta) else rep(0, nrow(prepared$X)); exp_lp <- exp(lp); contributions <- numeric(length(lp)); survival <- function(C, multiplier) exp(-as.vector(C %*% theta) * multiplier)
  for (batch in cache$batches) { idx <- batch$indices; multiplier <- exp_lp[idx]
    if (batch$pattern %in% c("dic_no_overlap", "dic_overlap", "ic_start_right", "ic_start_exact")) { q <- ncol(batch$weights); expanded_lp <- rep(multiplier, each = q)
      if (batch$pattern == "dic_no_overlap") { s2 <- matrix(survival(batch$C2, expanded_lp), nrow = length(idx), byrow = TRUE); s3 <- matrix(survival(batch$C3, expanded_lp), nrow = length(idx), byrow = TRUE); contributions[idx] <- rowSums(batch$weights * (s2 - s3))
      } else if (batch$pattern == "dic_overlap") { s <- matrix(survival(batch$C, expanded_lp), nrow = length(idx), byrow = TRUE); contributions[idx] <- rowSums(batch$weights * (1 - s))
      } else if (batch$pattern == "ic_start_right") { s <- matrix(survival(batch$C, expanded_lp), nrow = length(idx), byrow = TRUE); contributions[idx] <- rowSums(batch$weights * s)
      } else { s <- matrix(survival(batch$C, expanded_lp), nrow = length(idx), byrow = TRUE); h <- matrix(as.vector(batch$H %*% theta) * expanded_lp, nrow = length(idx), byrow = TRUE); contributions[idx] <- rowSums(batch$weights * s * h) }
    } else if (batch$pattern == "exact_start_ic") contributions[idx] <- survival(batch$C2, multiplier) - survival(batch$C3, multiplier)
      else if (batch$pattern == "exact_start_right") contributions[idx] <- survival(batch$C, multiplier)
      else if (batch$pattern == "exact_exact") contributions[idx] <- survival(batch$C, multiplier) * (as.vector(batch$H %*% theta) * multiplier)
  }
  if (any(!is.finite(contributions)) || any(contributions <= 0)) return(-Inf); sum(log(pmax(contributions, contribution_floor)))
}

gic_loglik <- function(theta, beta, prepared, basis, g_start, quad_n = 20L, contribution_floor = 1e-12) {
  engine <- getOption("gicsurv.likelihood_engine", "pattern_vectorised"); use_cache <- isTRUE(getOption("gicsurv.use_likelihood_cache", TRUE))
  if (identical(engine, "pattern_vectorised") && use_cache && !is.null(prepared$pattern_vector_cache)) gic_loglik_pattern_vectorised(theta, beta, prepared, basis, contribution_floor) else if (use_cache) gic_loglik_cached(theta, beta, prepared, basis, g_start, quad_n, contribution_floor) else gic_loglik_reference(theta, beta, prepared, basis, g_start, quad_n, contribution_floor)
}

subset_prepared <- function(prepared, indices) {
  out <- list(
    data = prepared$data[indices, , drop = FALSE],
    t1 = prepared$t1[indices], t2 = prepared$t2[indices],
    t3 = prepared$t3[indices], status = prepared$status[indices],
    pattern = prepared$pattern[indices],
    X = prepared$X[indices, , drop = FALSE],
    coefficient_names = prepared$coefficient_names
  )
  if (!is.null(prepared$likelihood_cache)) {
    out$likelihood_cache <- prepared$likelihood_cache
    out$likelihood_cache$entries <- prepared$likelihood_cache$entries[indices]
  }
  out
}

penalized_nll_theta <- function(parameters, lambda, prepared, basis,
                                g_start, quad_n = 20L) {
  K <- basis$K
  theta <- parameters[seq_len(K)]
  beta <- parameters[-seq_len(K)]
  if (any(theta < 0) || any(!is.finite(parameters))) return(1e30)
  ll <- gic_loglik(theta, beta, prepared, basis, g_start, quad_n)
  if (!is.finite(ll)) return(1e30)
  roughness <- drop(crossprod(theta, basis$S %*% theta))
  -ll + 0.5 * lambda * roughness
}

penalized_nll_working <- function(parameters, lambda, prepared, basis,
                                  g_start, positivity, quad_n = 20L) {
  K <- basis$K
  theta <- theta_from_working(parameters[seq_len(K)], positivity)
  beta <- parameters[-seq_len(K)]
  penalized_nll_theta(
    c(theta, beta), lambda, prepared, basis, g_start, quad_n
  )
}

default_theta_start <- function(prepared, K) {
  finite_t2 <- prepared$t2[is.finite(prepared$t2) & prepared$t2 > 0]
  scale <- if (length(finite_t2)) stats::median(finite_t2) else 1
  rep(max(1 / scale, 1e-3), K)
}

fit_given_lambda <- function(lambda, prepared, basis, g_start,
                             positivity = c("direct", "exponential",
                                            "softplus", "square"),
                             start_theta = NULL, start_beta = NULL,
                             quad_n = 20L, theta_lower = 0,
                             control = list(maxit = 600, factr = 1e7,
                                            pgtol = 1e-7)) {
  positivity <- match.arg(positivity)
  if (!is.finite(lambda) || lambda <= 0) stop("lambda must be positive.")
  K <- basis$K
  p <- ncol(prepared$X)
  start_theta <- start_theta %||% default_theta_start(prepared, K)
  start_beta <- start_beta %||% rep(0, p)
  if (length(start_theta) != K || length(start_beta) != p) {
    stop("Starting values have incorrect dimensions.")
  }

  working_theta <- working_from_theta(start_theta, positivity)
  par0 <- c(working_theta, start_beta)
  if (positivity == "direct") {
    raw_fit <- stats::nlminb(
      start = par0, objective = penalized_nll_working,
      lambda = lambda, prepared = prepared, basis = basis,
      g_start = g_start, positivity = positivity, quad_n = quad_n,
      lower = c(rep(theta_lower, K), rep(-Inf, p)),
      upper = rep(Inf, K + p),
      control = list(
        iter.max = control$maxit %||% 600,
        eval.max = 3L * (control$maxit %||% 600),
        rel.tol = 1e-8, x.tol = 1e-8
      )
    )
    fit <- list(
      par = raw_fit$par, value = raw_fit$objective,
      convergence = raw_fit$convergence, message = raw_fit$message,
      iterations = raw_fit$iterations, evaluations = raw_fit$evaluations
    )
  } else {
    fit <- stats::optim(
      par = par0, fn = penalized_nll_working,
      lambda = lambda, prepared = prepared, basis = basis,
      g_start = g_start, positivity = positivity, quad_n = quad_n,
      method = "BFGS", control = list(maxit = control$maxit %||% 600,
                                      reltol = 1e-8)
    )
  }
  fit$working_par <- fit$par
  fit$theta <- theta_from_working(fit$par[seq_len(K)], positivity)
  fit$beta <- fit$par[-seq_len(K)]
  fit$theta_beta <- c(fit$theta, fit$beta)
  fit$lambda <- lambda
  fit$positivity <- positivity
  fit
}

fit_given_lambda_retry <- function(lambda, prepared, basis, g_start,
                                   positivity = "direct",
                                   start_theta = NULL, start_beta = NULL,
                                   quad_n = 20L, max_attempts = 5L) {
  K <- basis$K
  p <- ncol(prepared$X)
  default_theta <- default_theta_start(prepared, K)
  default_beta <- rep(0, p)
  warm_theta <- start_theta %||% default_theta
  warm_beta <- start_beta %||% default_beta
  candidate_starts <- list(
    list(theta = warm_theta, beta = warm_beta, label = "warm_or_default"),
    list(theta = 0.5 * default_theta, beta = default_beta,
         label = "half_default_hazard"),
    list(theta = 2 * default_theta, beta = default_beta,
         label = "double_default_hazard"),
    list(theta = default_theta, beta = default_beta, label = "default"),
    list(
      theta = pmax(warm_theta, 10 * .Machine$double.eps^0.5),
      beta = warm_beta, label = "interior_warm"
    )
  )
  candidate_starts <- candidate_starts[seq_len(min(
    as.integer(max_attempts), length(candidate_starts)
  ))]
  attempts <- vector("list", length(candidate_starts))
  fits <- vector("list", length(candidate_starts))
  for (i in seq_along(candidate_starts)) {
    candidate <- candidate_starts[[i]]
    fit <- tryCatch(
      fit_given_lambda(
        lambda, prepared, basis, g_start, positivity,
        candidate$theta, candidate$beta, quad_n
      ),
      error = function(e) structure(
        list(message = conditionMessage(e)), class = "fit_error"
      )
    )
    fits[[i]] <- fit
    attempts[[i]] <- data.frame(
      attempt = i, start = candidate$label,
      convergence = if (inherits(fit, "fit_error")) 99L else fit$convergence,
      objective = if (inherits(fit, "fit_error")) Inf else fit$value,
      message = if (inherits(fit, "fit_error")) fit$message else
        (fit$message %||% ""), stringsAsFactors = FALSE
    )
    if (!inherits(fit, "fit_error") && fit$convergence == 0L &&
        is.finite(fit$value)) break
  }
  attempts <- do.call(rbind, attempts[!vapply(attempts, is.null, logical(1))])
  valid <- which(vapply(fits, function(fit) {
    !is.null(fit) && !inherits(fit, "fit_error") &&
      is.finite(fit$value)
  }, logical(1)))
  if (!length(valid)) stop("All inner-fit attempts failed.")
  converged <- valid[vapply(fits[valid], function(fit) {
    fit$convergence == 0L
  }, logical(1))]
  eligible <- if (length(converged)) converged else valid
  values <- vapply(fits[eligible], `[[`, numeric(1), "value")
  best <- fits[[eligible[which.min(values)]]]
  best$retry_attempts <- attempts
  best$retry_count <- nrow(attempts) - 1L
  best
}

matrix_rank_psd <- function(matrix, tolerance = NULL) {
  values <- eigen(
    (matrix + t(matrix)) / 2, symmetric = TRUE, only.values = TRUE
  )$values
  if (!length(values)) return(0L)
  tolerance <- tolerance %||%
    max(abs(values)) * max(dim(matrix)) * .Machine$double.eps^0.75
  sum(values > tolerance)
}

log_pdet_psd <- function(matrix, tolerance = NULL) {
  values <- eigen(
    (matrix + t(matrix)) / 2, symmetric = TRUE, only.values = TRUE
  )$values
  if (!length(values)) return(0)
  tolerance <- tolerance %||%
    max(abs(values)) * max(dim(matrix)) * .Machine$double.eps^0.75
  positive <- values > tolerance
  if (!any(positive)) return(0)
  sum(log(values[positive]))
}

stable_logdet <- function(matrix, tolerance = NULL) {
  values <- eigen(
    (matrix + t(matrix)) / 2, symmetric = TRUE, only.values = TRUE
  )$values
  tolerance <- tolerance %||%
    max(abs(values)) * length(values) * .Machine$double.eps^0.6
  list(
    ok = all(is.finite(values)) && all(values > tolerance),
    value = if (all(is.finite(values)) && all(values > tolerance)) {
      sum(log(values))
    } else {
      -Inf
    },
    eigenvalues = values
  )
}

active_set_geometry <- function(fit, lambda, prepared, basis, g_start,
                                quad_n = 20L,
                                active_tolerance = 1e-7,
                                hessian_r = 4L,
                                hessian_step_fraction = 0.25,
                                hessian_step_max = 1e-4) {
  K <- basis$K
  p <- ncol(prepared$X)
  active_theta <- fit$theta <= active_tolerance
  free_theta <- which(!active_theta)
  free_indices <- c(free_theta, K + seq_len(p))
  if (!length(free_theta)) {
    return(list(ok = FALSE, reason = "all spline coefficients at boundary"))
  }
  fixed <- fit$theta_beta
  reduced_nll <- function(free_values) {
    parameters <- fixed
    parameters[free_indices] <- free_values
    penalized_nll_theta(
      parameters, lambda, prepared, basis, g_start, quad_n
    )
  }
  # numDeriv's default absolute Richardson step is 1e-4 for parameters near
  # zero. For a small but positive free spline coefficient, that default can
  # cross theta = 0 and evaluate the objective outside the feasible region.
  # Use a coordinate-specific centered step no larger than one quarter of the
  # coefficient's distance to the boundary. Regression coefficients are
  # unconstrained and retain the standard 1e-4 step.
  hessian_eps <- c(
    pmin(hessian_step_max,
         hessian_step_fraction * fit$theta[free_theta]),
    rep(hessian_step_max, p)
  )
  H_free <- numDeriv::hessian(
    reduced_nll, fit$theta_beta[free_indices],
    method = "Richardson",
    method.args = list(r = hessian_r, eps = hessian_eps)
  )
  determinant <- stable_logdet(H_free)
  S_free <- basis$S[free_theta, free_theta, drop = FALSE]
  list(
    ok = determinant$ok, reason = if (determinant$ok) "ok" else
      "free-parameter Hessian not positive definite",
    H_free = H_free, logdet_H = determinant$value,
    H_eigenvalues = determinant$eigenvalues,
    S_free = S_free, S_rank_free = matrix_rank_psd(S_free),
    S_logdet_free = log_pdet_psd(S_free),
    active_theta = active_theta, free_theta = free_theta,
    free_indices = free_indices, hessian_eps = hessian_eps,
    hessian_step_fraction = hessian_step_fraction,
    hessian_step_max = hessian_step_max
  )
}

laplace_at_fit <- function(fit, prepared, basis, g_start, quad_n = 20L,
                           active_tolerance = 1e-7) {
  geometry <- active_set_geometry(
    fit, fit$lambda, prepared, basis, g_start, quad_n,
    active_tolerance = active_tolerance
  )
  if (!geometry$ok) {
    return(list(value = -Inf, fit = fit, geometry = geometry,
                reason = geometry$reason))
  }
  ll <- gic_loglik(
    fit$theta, fit$beta, prepared, basis, g_start, quad_n
  )
  roughness <- drop(crossprod(fit$theta, basis$S %*% fit$theta))
  value <- ll - 0.5 * fit$lambda * roughness +
    0.5 * geometry$S_rank_free * log(fit$lambda) +
    0.5 * geometry$S_logdet_free -
    0.5 * geometry$logdet_H
  list(
    value = value, fit = fit, geometry = geometry,
    loglik = ll, roughness = roughness, reason = "ok"
  )
}

make_log_lambda_grid <- function(bounds = c(-1, 18), n = 20L) {
  seq(bounds[1], bounds[2], length.out = as.integer(n))
}

select_lambda_laplace <- function(prepared, basis, g_start,
                                  positivity = "direct",
                                  log_lambda_grid = make_log_lambda_grid(),
                                  quad_n = 20L,
                                  active_tolerance = 1e-7,
                                  refine = TRUE) {
  records <- vector("list", length(log_lambda_grid))
  fits <- vector("list", length(log_lambda_grid))
  start_theta <- NULL
  start_beta <- NULL
  for (i in seq_along(log_lambda_grid)) {
    log_lambda <- log_lambda_grid[i]
    fit <- tryCatch(
      fit_given_lambda(
        exp(log_lambda), prepared, basis, g_start, positivity,
        start_theta, start_beta, quad_n
      ),
      error = function(e) structure(list(message = conditionMessage(e)),
                                    class = "fit_error")
    )
    if (inherits(fit, "fit_error")) {
      records[[i]] <- data.frame(
        log_lambda = log_lambda, lambda = exp(log_lambda),
        criterion = -Inf, convergence = 99L, active_theta = NA_integer_,
        reason = fit$message
      )
      next
    }
    if (fit$convergence == 0L) {
      start_theta <- fit$theta
      start_beta <- fit$beta
    }
    la <- laplace_at_fit(
      fit, prepared, basis, g_start, quad_n, active_tolerance
    )
    fits[[i]] <- la
    records[[i]] <- data.frame(
      log_lambda = log_lambda, lambda = exp(log_lambda),
      criterion = la$value, convergence = fit$convergence,
      active_theta = sum(la$geometry$active_theta %||% NA),
      reason = la$reason
    )
  }
  profile <- do.call(rbind, records)
  finite <- which(is.finite(profile$criterion))
  if (!length(finite)) stop("No finite Laplace criterion was obtained.")
  best_index <- finite[which.max(profile$criterion[finite])]
  best <- fits[[best_index]]

  # Optional local profile refinement. The profile grid remains the audit trail.
  if (refine && best_index > 1L && best_index < length(log_lambda_grid)) {
    interval <- log_lambda_grid[c(best_index - 1L, best_index + 1L)]
    cache_start_theta <- best$fit$theta
    cache_start_beta <- best$fit$beta
    objective <- function(log_lambda) {
      fit <- fit_given_lambda(
        exp(log_lambda), prepared, basis, g_start, positivity,
        cache_start_theta, cache_start_beta, quad_n
      )
      la <- laplace_at_fit(
        fit, prepared, basis, g_start, quad_n, active_tolerance
      )
      if (is.finite(la$value)) -la$value else 1e50
    }
    refined_outer <- tryCatch(
      stats::optimize(objective, interval = interval, tol = 0.05),
      error = function(e) NULL
    )
    if (!is.null(refined_outer)) {
      refined_fit <- fit_given_lambda(
        exp(refined_outer$minimum), prepared, basis, g_start, positivity,
        cache_start_theta, cache_start_beta, quad_n
      )
      refined <- laplace_at_fit(
        refined_fit, prepared, basis, g_start, quad_n, active_tolerance
      )
      if (is.finite(refined$value) && refined$value > best$value) best <- refined
    }
  }
  list(
    method = "laplace_profile", positivity = positivity,
    lambda = best$fit$lambda, fit = best$fit,
    criterion = best$value, geometry = best$geometry,
    profile = profile, converged = best$fit$convergence == 0L,
    grid_best_index = best_index,
    lambda_at_lower_boundary = best_index == 1L,
    lambda_at_upper_boundary = best_index == length(log_lambda_grid),
    effective_complete_smoothing =
      best_index == length(log_lambda_grid) &&
      length(log_lambda_grid) >= 3L &&
      all(diff(tail(profile$criterion, 3L)) >= -1e-6)
  )
}

# Coarse-to-fine profile Laplace selection.  The statistical criterion is
# identical to select_lambda_laplace(); only the numerical search changes.
# Every evaluated point is cached, the nearest successful fit supplies warm
# starts, and optimize() refines only the bracket around the best coarse point.
select_lambda_laplace_adaptive <- function(
    prepared, basis, g_start, positivity = "direct",
    log_lambda_grid = make_log_lambda_grid(n = 7L), quad_n = 20L,
    active_tolerance = 1e-7, refine_tolerance = 0.05) {
  coarse_grid <- sort(unique(as.numeric(log_lambda_grid)))
  if (length(coarse_grid) < 3L) {
    stop("Adaptive profile Laplace requires at least three coarse points.")
  }
  cache <- new.env(parent = emptyenv())

  cache_key <- function(x) sprintf("%.8f", x)
  cached_values <- function() {
    keys <- ls(cache, all.names = TRUE)
    if (!length(keys)) return(list())
    lapply(keys, function(key) get(key, envir = cache, inherits = FALSE))
  }
  evaluate <- function(log_lambda) {
    log_lambda <- min(max(log_lambda, min(coarse_grid)), max(coarse_grid))
    key <- cache_key(log_lambda)
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
    prior <- cached_values()
    successful <- prior[vapply(prior, function(x) {
      !is.null(x$laplace) && is.finite(x$laplace$value)
    }, logical(1))]
    if (length(successful)) {
      distances <- vapply(
        successful, function(x) abs(x$log_lambda - log_lambda), numeric(1)
      )
      nearest <- successful[[which.min(distances)]]$laplace$fit
      start_theta <- nearest$theta
      start_beta <- nearest$beta
    } else {
      start_theta <- NULL
      start_beta <- NULL
    }
    fit <- tryCatch(
      fit_given_lambda_retry(
        exp(log_lambda), prepared, basis, g_start, positivity,
        start_theta, start_beta, quad_n
      ),
      error = function(e) structure(
        list(message = conditionMessage(e)), class = "fit_error"
      )
    )
    if (inherits(fit, "fit_error")) {
      result <- list(
        log_lambda = log_lambda, laplace = NULL,
        record = data.frame(
          log_lambda = log_lambda, lambda = exp(log_lambda),
          criterion = -Inf, convergence = 99L,
          active_theta = NA_integer_, reason = fit$message,
          stringsAsFactors = FALSE
        )
      )
    } else {
      la <- laplace_at_fit(
        fit, prepared, basis, g_start, quad_n, active_tolerance
      )
      result <- list(
        log_lambda = log_lambda, laplace = la,
        record = data.frame(
          log_lambda = log_lambda, lambda = exp(log_lambda),
          criterion = la$value, convergence = fit$convergence,
          active_theta = sum(la$geometry$active_theta %||% NA),
          reason = la$reason, stringsAsFactors = FALSE
        )
      )
    }
    assign(key, result, envir = cache)
    result
  }

  invisible(lapply(coarse_grid, evaluate))
  coarse_results <- lapply(coarse_grid, evaluate)
  coarse_criterion <- vapply(coarse_results, function(x) {
    if (is.null(x$laplace)) -Inf else x$laplace$value
  }, numeric(1))
  finite <- which(is.finite(coarse_criterion))
  if (!length(finite)) stop("No finite adaptive Laplace criterion was obtained.")
  coarse_best <- finite[which.max(coarse_criterion[finite])]

  if (coarse_best > 1L && coarse_best < length(coarse_grid)) {
    bracket <- coarse_grid[c(coarse_best - 1L, coarse_best + 1L)]
    objective <- function(log_lambda) {
      value <- evaluate(log_lambda)$laplace$value %||% -Inf
      if (is.finite(value)) -value else 1e50
    }
    refined <- tryCatch(
      stats::optimize(
        objective, interval = bracket, tol = refine_tolerance
      ),
      error = function(e) NULL
    )
    if (!is.null(refined)) invisible(evaluate(refined$minimum))
  }

  all_results <- cached_values()
  criteria <- vapply(all_results, function(x) {
    if (is.null(x$laplace)) -Inf else x$laplace$value
  }, numeric(1))
  finite <- which(is.finite(criteria))
  if (!length(finite)) stop("No finite adaptive Laplace criterion was obtained.")
  best_result <- all_results[[finite[which.max(criteria[finite])]]]
  best <- best_result$laplace
  profile <- do.call(rbind, lapply(all_results, `[[`, "record"))
  profile <- profile[order(profile$log_lambda), , drop = FALSE]
  best_index <- which.min(abs(profile$log_lambda - best_result$log_lambda))
  lower_boundary <- abs(best_result$log_lambda - min(coarse_grid)) <=
    refine_tolerance
  upper_boundary <- abs(best_result$log_lambda - max(coarse_grid)) <=
    refine_tolerance
  upper_rows <- tail(profile[is.finite(profile$criterion), , drop = FALSE], 3L)

  list(
    method = "laplace_adaptive", positivity = positivity,
    lambda = best$fit$lambda, fit = best$fit,
    criterion = best$value, geometry = best$geometry,
    profile = profile, converged = best$fit$convergence == 0L,
    grid_best_index = best_index,
    coarse_grid = coarse_grid, n_profile_evaluations = nrow(profile),
    lambda_at_lower_boundary = lower_boundary,
    lambda_at_upper_boundary = upper_boundary,
    effective_complete_smoothing = upper_boundary &&
      nrow(upper_rows) >= 3L &&
      all(diff(upper_rows$criterion) >= -1e-6)
  )
}

select_lambda_gfs <- function(prepared, basis, g_start,
                              positivity = "direct", lambda_start = 1,
                              quad_n = 20L, active_tolerance = 1e-7,
                              max_outer = 25L,
                              log_lambda_tolerance = 1e-2,
                              damping = 0.6,
                              max_log_step = 2,
                              lambda_bounds = exp(c(-12, 18))) {
  lambda <- lambda_start
  start_theta <- NULL
  start_beta <- NULL
  history <- vector("list", max_outer)
  final_geometry <- NULL
  for (iteration in seq_len(max_outer)) {
    fit <- fit_given_lambda_retry(
      lambda, prepared, basis, g_start, positivity,
      start_theta, start_beta, quad_n
    )
    if (fit$convergence != 0L) {
      stop("Inner fit failed during GFS iteration ", iteration, ".")
    }
    geometry <- active_set_geometry(
      fit, lambda, prepared, basis, g_start, quad_n, active_tolerance
    )
    if (!geometry$ok) stop(geometry$reason)
    n_free <- length(geometry$free_indices)
    penalty_hessian <- matrix(0, n_free, n_free)
    n_free_theta <- length(geometry$free_theta)
    penalty_hessian[seq_len(n_free_theta), seq_len(n_free_theta)] <-
      lambda * geometry$S_free
    effective_penalty_df <- sum(diag(
      solve(geometry$H_free, penalty_hessian)
    ))
    numerator <- geometry$S_rank_free - effective_penalty_df
    roughness <- drop(crossprod(fit$theta, basis$S %*% fit$theta))
    if (!is.finite(numerator) || numerator <= 0 ||
        !is.finite(roughness) || roughness <= 0) {
      stop("Invalid GFS update: numerator=", signif(numerator, 5),
           ", roughness=", signif(roughness, 5), ".")
    }
    raw_lambda <- min(max(numerator / roughness, lambda_bounds[1]),
                      lambda_bounds[2])
    proposed_log_lambda <-
      (1 - damping) * log(lambda) + damping * log(raw_lambda)
    log_step <- max(min(
      proposed_log_lambda - log(lambda), max_log_step
    ), -max_log_step)
    updated_lambda <- exp(log(lambda) + log_step)
    history[[iteration]] <- data.frame(
      iteration = iteration, lambda = lambda,
      updated_lambda = updated_lambda, roughness = roughness,
      penalty_rank_free = geometry$S_rank_free,
      effective_penalty_df = effective_penalty_df,
      active_theta = sum(geometry$active_theta),
      active_indices = paste(which(geometry$active_theta), collapse = ";"),
      log_step = log_step, retry_count = fit$retry_count %||% 0L,
      convergence = fit$convergence
    )
    final_geometry <- geometry
    if (abs(log(updated_lambda) - log(lambda)) < log_lambda_tolerance) {
      lambda <- updated_lambda
      start_theta <- fit$theta
      start_beta <- fit$beta
      history <- do.call(rbind, history[seq_len(iteration)])
      final_fit <- fit_given_lambda_retry(
        lambda, prepared, basis, g_start, positivity,
        start_theta, start_beta, quad_n
      )
      return(list(
        method = "laplace_gfs", positivity = positivity,
        lambda = lambda, fit = final_fit, geometry = final_geometry,
        history = history, converged = final_fit$convergence == 0L,
        iterations = iteration,
        lambda_at_lower_boundary =
          lambda <= lambda_bounds[1] * exp(log_lambda_tolerance),
        lambda_at_upper_boundary =
          lambda >= lambda_bounds[2] / exp(log_lambda_tolerance),
        effective_complete_smoothing =
          lambda >= lambda_bounds[2] / exp(log_lambda_tolerance)
      ))
    }
    lambda <- updated_lambda
    start_theta <- fit$theta
    start_beta <- fit$beta
  }
  history <- do.call(rbind, history)
  final_fit <- fit_given_lambda_retry(
    lambda, prepared, basis, g_start, positivity,
    start_theta, start_beta, quad_n
  )
  list(
    method = "laplace_gfs", positivity = positivity,
    lambda = lambda, fit = final_fit, geometry = final_geometry,
    history = history, converged = FALSE, iterations = max_outer
    , lambda_at_lower_boundary = lambda <= lambda_bounds[1] * 1.01
    , lambda_at_upper_boundary = lambda >= lambda_bounds[2] / 1.01
    , effective_complete_smoothing = lambda >= lambda_bounds[2] / 1.01
  )
}

make_stratified_folds <- function(pattern, K = 5L, seed = 1L) {
  set.seed(seed)
  folds <- integer(length(pattern))
  for (group in unique(pattern)) {
    idx <- which(pattern == group)
    folds[idx] <- sample(rep(seq_len(K), length.out = length(idx)))
  }
  folds
}

select_lambda_cv <- function(prepared, basis, g_start,
                             positivity = "direct",
                             log_lambda_grid = make_log_lambda_grid(n = 15L),
                             folds = 5L, fold_seed = 1L,
                             quad_n = 20L,
                             rule = c("one_se", "max")) {
  rule <- match.arg(rule)
  fold_id <- make_stratified_folds(prepared$pattern, folds, fold_seed)
  score_rows <- vector("list", length(log_lambda_grid) * folds)
  train_sets <- lapply(seq_len(folds), function(fold) {
    subset_prepared(prepared, fold_id != fold)
  })
  test_sets <- lapply(seq_len(folds), function(fold) {
    subset_prepared(prepared, fold_id == fold)
  })
  if (identical(getOption("gicsurv.likelihood_engine", "pattern_vectorised"), "pattern_vectorised") && isTRUE(getOption("gicsurv.use_likelihood_cache", TRUE))) { train_sets <- lapply(train_sets, function(x) { x$pattern_vector_cache <- compile_pattern_vector_cache(x, basis); x }); test_sets <- lapply(test_sets, function(x) { x$pattern_vector_cache <- compile_pattern_vector_cache(x, basis); x }) }
  warm_theta <- vector("list", folds)
  warm_beta <- vector("list", folds)
  fit_cache <- vector("list", length(log_lambda_grid))
  row <- 0L
  # Start at the smooth end and move down the path.  Adjacent fits within each
  # fold then use warm starts, substantially reducing repeated optimisation.
  evaluation_order <- order(log_lambda_grid, decreasing = TRUE)
  for (i in evaluation_order) {
    lambda <- exp(log_lambda_grid[i])
    for (fold in seq_len(folds)) {
      row <- row + 1L
      train <- train_sets[[fold]]
      test <- test_sets[[fold]]
      timed <- system.time({
        fit <- tryCatch(
          fit_given_lambda_retry(
            lambda, train, basis, g_start, positivity,
            start_theta = warm_theta[[fold]],
            start_beta = warm_beta[[fold]],
            quad_n = quad_n
          ),
          error = function(e) structure(list(message = conditionMessage(e)),
                                      class = "fit_error")
        )
      })
      if (inherits(fit, "fit_error")) {
        score_rows[[row]] <- data.frame(
          log_lambda = log_lambda_grid[i], lambda = lambda, fold = fold,
          n_test = length(test$t1), test_loglik = -Inf,
          test_loglik_per_observation = -Inf, convergence = 99L,
          elapsed = unname(timed["elapsed"]), reason = fit$message
        )
      } else {
        if (fit$convergence == 0L) {
          warm_theta[[fold]] <- fit$theta
          warm_beta[[fold]] <- fit$beta
        }
        test_ll <- gic_loglik(
          fit$theta, fit$beta, test, basis, g_start, quad_n
        )
        score_rows[[row]] <- data.frame(
          log_lambda = log_lambda_grid[i], lambda = lambda, fold = fold,
          n_test = length(test$t1), test_loglik = test_ll,
          test_loglik_per_observation = test_ll / length(test$t1),
          convergence = fit$convergence,
          elapsed = unname(timed["elapsed"]),
          reason = if (fit$convergence == 0L) "ok" else "inner fit failure"
        )
      }
      fit_cache[[i]][[fold]] <- fit
    }
  }
  fold_scores <- do.call(rbind, score_rows)
  split_scores <- split(fold_scores, fold_scores$log_lambda)
  totals <- do.call(rbind, lapply(split_scores, function(x) {
    valid <- all(is.finite(x$test_loglik_per_observation)) &&
      nrow(x) == folds && all(x$convergence == 0L)
    fold_mean <- if (valid) mean(x$test_loglik_per_observation) else -Inf
    fold_se <- if (valid && nrow(x) > 1L) {
      stats::sd(x$test_loglik_per_observation) / sqrt(nrow(x))
    } else if (valid) 0 else Inf
    data.frame(
      log_lambda = x$log_lambda[1], lambda = x$lambda[1],
      test_loglik = if (valid) sum(x$test_loglik) else -Inf,
      mean_test_loglik_per_observation = fold_mean,
      se_test_loglik_per_observation = fold_se,
      all_folds_converged = all(x$convergence == 0L),
      stringsAsFactors = FALSE
    )
  }))
  totals <- totals[order(totals$log_lambda), , drop = FALSE]
  row.names(totals) <- NULL
  finite <- which(is.finite(totals$mean_test_loglik_per_observation))
  if (!length(finite)) stop("No finite cross-validation score was obtained.")
  max_index <- finite[which.max(
    totals$mean_test_loglik_per_observation[finite]
  )]
  max_row <- totals[max_index, , drop = FALSE]
  one_se_threshold <- max_row$mean_test_loglik_per_observation -
    max_row$se_test_loglik_per_observation
  eligible <- finite[
    totals$mean_test_loglik_per_observation[finite] >= one_se_threshold
  ]
  selected_index <- if (rule == "one_se") {
    eligible[which.max(totals$log_lambda[eligible])]
  } else {
    max_index
  }
  selected <- totals[selected_index, , drop = FALSE]
  selected_grid_index <- which.min(abs(
    log_lambda_grid - selected$log_lambda
  ))
  selected_fold_fits <- fit_cache[[selected_grid_index]]
  converged_fold_fits <- selected_fold_fits[vapply(
    selected_fold_fits,
    function(x) !is.null(x) && !inherits(x, "fit_error") &&
      x$convergence == 0L && is.finite(x$value),
    logical(1)
  )]
  final_start <- if (length(converged_fold_fits)) {
    converged_fold_fits[[1L]]
  } else NULL
  final_fit <- fit_given_lambda_retry(
    selected$lambda, prepared, basis, g_start, positivity,
    start_theta = final_start$theta %||% NULL,
    start_beta = final_start$beta %||% NULL,
    quad_n = quad_n
  )
  list(
    method = paste0("cv_", rule), positivity = positivity,
    lambda = selected$lambda,
    fit = final_fit, fold_scores = fold_scores, cv_profile = totals,
    criterion = selected$mean_test_loglik_per_observation,
    converged = final_fit$convergence == 0L,
    grid_best_index = selected_index,
    cv_rule = rule, cv_max_log_lambda = max_row$log_lambda,
    one_se_threshold = one_se_threshold,
    lambda_at_lower_boundary =
      selected$log_lambda == min(log_lambda_grid),
    lambda_at_upper_boundary =
      selected$log_lambda == max(log_lambda_grid),
    effective_complete_smoothing =
      selected$log_lambda == max(log_lambda_grid) &&
      nrow(totals) >= 3L &&
      all(diff(tail(
        totals$mean_test_loglik_per_observation[order(totals$log_lambda)],
        3L
      )) >= -1e-6)
  )
}

fit_gic_smoothing <- function(
    data, covs = NULL, time1 = "time1", time2 = "time2",
    time3 = "time3", status = "status", K = 10L, degree = 3L,
    smoothing = c("cv_one_se", "cv_max", "laplace_profile",
                  "laplace_adaptive", "laplace_gfs"),
    positivity = c("direct", "exponential", "softplus", "square"),
    start_family = c("uniform", "beta"), start_shape1 = 1,
    start_shape2 = 1, log_lambda_grid = make_log_lambda_grid(),
    cv_folds = 5L, fold_seed = 1L, quad_n = 20L,
    active_tolerance = 1e-7) {
  smoothing <- match.arg(smoothing)
  positivity <- match.arg(positivity)
  start_family <- match.arg(start_family)
  prepared <- prepare_gic_data(data, time1, time2, time3, status, covs)
  observed_times <- c(prepared$t1, prepared$t2,
                      prepared$t3[is.finite(prepared$t3)])
  basis <- make_bspline_hazard_basis(observed_times, K, degree)
  g_start <- make_start_density(start_family, start_shape1, start_shape2)
  use_likelihood_cache <- isTRUE(getOption(
    "gicsurv.use_likelihood_cache", TRUE
  ))
  if (use_likelihood_cache) {
    prepared$likelihood_cache <- compile_gic_likelihood(
      prepared, basis, g_start, quad_n
    )
    if (identical(getOption("gicsurv.likelihood_engine", "pattern_vectorised"), "pattern_vectorised")) prepared$pattern_vector_cache <- compile_pattern_vector_cache(prepared, basis)
  }

  started <- proc.time()[["elapsed"]]
  selection <- switch(
    smoothing,
    laplace_profile = select_lambda_laplace(
      prepared, basis, g_start, positivity, log_lambda_grid,
      quad_n, active_tolerance
    ),
    laplace_adaptive = select_lambda_laplace_adaptive(
      prepared, basis, g_start, positivity, log_lambda_grid,
      quad_n, active_tolerance
    ),
    laplace_gfs = select_lambda_gfs(
      prepared, basis, g_start, positivity,
      lambda_start = exp(stats::median(log_lambda_grid)),
      quad_n = quad_n, active_tolerance = active_tolerance
    ),
    cv_one_se = select_lambda_cv(
      prepared, basis, g_start, positivity,
      log_lambda_grid = log_lambda_grid, folds = cv_folds,
      fold_seed = fold_seed, quad_n = quad_n, rule = "one_se"
    ),
    cv_max = select_lambda_cv(
      prepared, basis, g_start, positivity,
      log_lambda_grid = log_lambda_grid, folds = cv_folds,
      fold_seed = fold_seed, quad_n = quad_n, rule = "max"
    )
  )
  elapsed <- proc.time()[["elapsed"]] - started
  fit <- selection$fit
  geometry <- tryCatch(
    active_set_geometry(
      fit, fit$lambda, prepared, basis, g_start, quad_n, active_tolerance
    ),
    error = function(e) NULL
  )
  beta_se <- rep(NA_real_, length(fit$beta))
  if (!is.null(geometry) && geometry$ok && length(fit$beta)) {
    covariance_free <- tryCatch(solve(geometry$H_free), error = function(e) NULL)
    if (!is.null(covariance_free)) {
      beta_positions <- (length(geometry$free_theta) + 1L):
        nrow(geometry$H_free)
      beta_se <- sqrt(pmax(diag(covariance_free)[beta_positions], 0))
    }
  }
  fit_converged <- fit$convergence == 0L
  curvature_valid <- !is.null(geometry) && isTRUE(geometry$ok)
  inference_valid <- fit_converged && curvature_valid &&
    all(is.finite(fit$beta)) && all(is.finite(beta_se))

  # The raw CV maximum is obtained from the same fold fits as the one-SE rule.
  # Refit it once on the complete dataset so that the sensitivity comparison
  # does not repeat the expensive cross-validation path.
  cv_max <- NULL
  if (smoothing == "cv_one_se" &&
      is.finite(selection$cv_max_log_lambda %||% NA_real_)) {
    cv_max_lambda <- exp(selection$cv_max_log_lambda)
    cv_max_fit <- if (abs(log(cv_max_lambda) - log(selection$lambda)) < 1e-10) {
      fit
    } else {
      fit_given_lambda_retry(
        cv_max_lambda, prepared, basis, g_start, positivity,
        start_theta = fit$theta, start_beta = fit$beta, quad_n = quad_n
      )
    }
    cv_max_geometry <- tryCatch(
      active_set_geometry(
        cv_max_fit, cv_max_lambda, prepared, basis, g_start, quad_n,
        active_tolerance
      ),
      error = function(e) NULL
    )
    cv_max_beta_se <- rep(NA_real_, length(cv_max_fit$beta))
    if (!is.null(cv_max_geometry) && isTRUE(cv_max_geometry$ok) &&
        length(cv_max_fit$beta)) {
      cv_max_covariance <- tryCatch(
        solve(cv_max_geometry$H_free), error = function(e) NULL
      )
      if (!is.null(cv_max_covariance)) {
        cv_max_beta_positions <-
          (length(cv_max_geometry$free_theta) + 1L):
          nrow(cv_max_geometry$H_free)
        cv_max_beta_se <- sqrt(pmax(
          diag(cv_max_covariance)[cv_max_beta_positions], 0
        ))
      }
    }
    cv_max <- list(
      lambda = cv_max_lambda, fit = cv_max_fit,
      beta = cv_max_fit$beta, beta_se = cv_max_beta_se,
      active_theta = sum(cv_max_fit$theta <= active_tolerance),
      active_indices = which(cv_max_fit$theta <= active_tolerance),
      inference_valid = cv_max_fit$convergence == 0L &&
        !is.null(cv_max_geometry) && isTRUE(cv_max_geometry$ok) &&
        all(is.finite(cv_max_fit$beta)) && all(is.finite(cv_max_beta_se))
    )
  }
  out <- list(
    call = match.call(), smoothing = smoothing, positivity = positivity,
    likelihood_engine = if (!use_likelihood_cache) "reference" else getOption("gicsurv.likelihood_engine", "pattern_vectorised"),
    start_family = start_family, start_shape1 = start_shape1,
    start_shape2 = start_shape2, prepared = prepared, basis = basis,
    g_start = g_start, selection = selection, fit = fit,
    lambda = selection$lambda, theta = fit$theta, beta = fit$beta,
    S_scale = basis$S_scale,
    beta_se = beta_se, elapsed = elapsed,
    active_theta = sum(fit$theta <= active_tolerance),
    active_indices = which(fit$theta <= active_tolerance),
    min_theta = min(fit$theta),
    converged = isTRUE(selection$converged),
    fit_converged = fit_converged,
    curvature_valid = curvature_valid, cv_max = cv_max,
    inference_valid = inference_valid,
    lambda_at_lower_boundary =
      isTRUE(selection$lambda_at_lower_boundary),
    lambda_at_upper_boundary =
      isTRUE(selection$lambda_at_upper_boundary),
    effective_complete_smoothing =
      isTRUE(selection$effective_complete_smoothing)
  )
  class(out) <- "gic_smoothing_fit"
  out
}

predict_gic_baseline <- function(object, times) {
  stopifnot(inherits(object, "gic_smoothing_fit"))
  sf <- make_survival_functions(object$theta, object$basis)
  data.frame(
    time = times, hazard = sf$hazard(times),
    cumulative_hazard = sf$cumulative_hazard(times),
    survival = sf$survival(times)
  )
}

integrated_squared_error <- function(estimate, truth, times) {
  if (length(times) < 2L) return(NA_real_)
  error2 <- (estimate - truth)^2
  sum(diff(times) * (head(error2, -1L) + tail(error2, -1L)) / 2)
}

compact_fit_record <- function(object, scenario_id, replicate,
                               true_beta = NA_real_,
                               true_hazard = NULL,
                               true_survival = NULL,
                               evaluation_times = NULL,
                               analysis_start_family = object$start_family) {
  beta1 <- if (length(object$beta)) object$beta[1] else NA_real_
  se1 <- if (length(object$beta_se)) object$beta_se[1] else NA_real_
  hazard_ise <- survival_ise <- NA_real_
  if (!is.null(evaluation_times) && !is.null(true_hazard) &&
      !is.null(true_survival)) {
    prediction <- predict_gic_baseline(object, evaluation_times)
    hazard_ise <- integrated_squared_error(
      prediction$hazard, true_hazard(evaluation_times), evaluation_times
    )
    survival_ise <- integrated_squared_error(
      prediction$survival, true_survival(evaluation_times), evaluation_times
    )
  }
  cv_max_beta <- if (!is.null(object$cv_max) && length(object$cv_max$beta)) {
    object$cv_max$beta[1]
  } else NA_real_
  cv_max_beta_se <- if (!is.null(object$cv_max) &&
                        length(object$cv_max$beta_se)) {
    object$cv_max$beta_se[1]
  } else NA_real_
  data.frame(
    scenario_id = scenario_id, replicate = replicate,
    smoothing = object$smoothing, positivity = object$positivity,
    analysis_start_family = analysis_start_family,
    analysis_shape1 = object$start_shape1,
    analysis_shape2 = object$start_shape2,
    lambda = object$lambda, log_lambda = log(object$lambda),
    S_scale = object$S_scale,
    lambda_raw_equivalent = object$lambda / object$S_scale,
    beta = beta1, beta_se = se1, true_beta = true_beta,
    covered = if (is.finite(beta1) && is.finite(se1) &&
                  is.finite(true_beta)) {
      beta1 - 1.96 * se1 <= true_beta &&
        true_beta <= beta1 + 1.96 * se1
    } else NA,
    active_theta = object$active_theta,
    active_indices = paste(object$active_indices, collapse = ";"),
    min_theta = object$min_theta,
    elapsed = object$elapsed, converged = object$converged,
    fit_converged = object$fit_converged,
    curvature_valid = object$curvature_valid,
    inference_valid = object$inference_valid,
    lambda_at_lower_boundary = object$lambda_at_lower_boundary,
    lambda_at_upper_boundary = object$lambda_at_upper_boundary,
    effective_complete_smoothing = object$effective_complete_smoothing,
    hazard_ise = hazard_ise, survival_ise = survival_ise
    , cv_max_lambda = object$cv_max$lambda %||% NA_real_
    , cv_max_log_lambda = if (!is.null(object$cv_max)) {
      log(object$cv_max$lambda)
    } else NA_real_
    , cv_max_beta = cv_max_beta
    , cv_max_beta_se = cv_max_beta_se
    , cv_max_active_theta = object$cv_max$active_theta %||% NA_integer_
    , cv_max_active_indices = if (!is.null(object$cv_max)) {
      paste(object$cv_max$active_indices, collapse = ";")
    } else ""
    , cv_max_inference_valid = object$cv_max$inference_valid %||% NA
  )
}
