test_that("public fit and prediction API works for exact/right-censored data", {
  set.seed(7)
  n <- 40L
  x <- stats::rbinom(n, 1, 0.5)
  event_time <- stats::rexp(n, rate = 0.07 * exp(-0.3 * x))
  censor_time <- stats::rexp(n, rate = 0.035)
  event <- as.integer(event_time <= censor_time)
  observed <- pmin(event_time, censor_time)
  data <- data.frame(
    time1 = 0,
    time2 = observed,
    time3 = ifelse(event == 1, observed, NA_real_),
    status = event,
    x = x
  )

  fit <- gic_fit(
    data, "time1", "time2", "time3", "status", "x",
    K = 5, cv_folds = 3,
    log_lambda_grid = seq(-4, 6, length.out = 5), quad_n = 8
  )
  expect_s3_class(fit, "gicsurv_fit")
  expect_true(fit$fit_converged)
  expect_true(all(is.finite(fit$beta)))
  expect_equal(nrow(gic_coefficients(fit)), 1L)

  prediction <- predict_gicsurv(
    fit, seq(0, max(observed), length.out = 10), n_sim = 20
  )
  expect_equal(nrow(prediction), 10L)
  expect_true(all(prediction$surv >= 0 & prediction$surv <= 1))
})

test_that("continuous centring and covariate profiles are available", {
  set.seed(13)
  n <- 45L
  age <- stats::runif(n, 18, 60)
  group <- factor(sample(c("A", "B"), n, replace = TRUE))
  event_time <- stats::rexp(n, 0.06 * exp(-0.02 * (age - mean(age))))
  censor_time <- stats::rexp(n, 0.03)
  event <- as.integer(event_time <= censor_time)
  observed <- pmin(event_time, censor_time)
  data <- data.frame(time1 = 0, time2 = observed,
                     time3 = ifelse(event == 1, observed, NA_real_),
                     status = event, age = age, group = group)
  fit <- gic_fit(
    data, "time1", "time2", "time3", "status", c("age", "group"),
    K = 5, cv_folds = 3, log_lambda_grid = seq(-3, 5, length.out = 5),
    quad_n = 8, center_continuous = TRUE
  )
  expect_lt(abs(mean(fit$model_data$age, na.rm = TRUE)), 1e-10)
  age_curves <- predict_gicsurv(fit, seq(0, max(observed), length.out = 8),
                                n_sim = 10, stratify_by = "age")
  group_curves <- predict_gicsurv(fit, seq(0, max(observed), length.out = 8),
                                  n_sim = 10, stratify_by = "group")
  expect_equal(length(unique(age_curves$profile)), 2L)
  expect_equal(length(unique(group_curves$profile)), 2L)
})
