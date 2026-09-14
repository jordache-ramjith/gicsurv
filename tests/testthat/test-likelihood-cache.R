testthat::test_that("cached and reference likelihood engines agree", {
  data <- data.frame(
    time1 = c(4, 4, 4, 4, 0, 0, 0),
    time2 = c(8, 0, 8, 8, 8, 8, 8),
    time3 = c(11, 4, NA, 8, 11, NA, 8),
    status = c(1, 1, 0, 1, 1, 0, 1),
    x = c(0, 1, 0, 1, 0, 1, 0)
  )
  prepared <- gicsurv:::prepare_gic_data(
    data, "time1", "time2", "time3", "status", "x"
  )
  observed_times <- c(
    prepared$t1, prepared$t2, prepared$t3[is.finite(prepared$t3)]
  )
  basis <- gicsurv:::make_bspline_hazard_basis(
    observed_times, K = 7L, degree = 3L
  )
  theta <- seq(0.01, 0.025, length.out = basis$K)
  beta <- -0.3

  for (specification in list(
    list(family = "uniform", a = 1, b = 1),
    list(family = "beta", a = 1, b = 2)
  )) {
    g_start <- gicsurv:::make_start_density(
      specification$family, specification$a, specification$b
    )
    cached <- prepared
    cached$likelihood_cache <- gicsurv:::compile_gic_likelihood(
      cached, basis, g_start, quad_n = 12L
    )
    reference_value <- gicsurv:::gic_loglik_reference(
      theta, beta, prepared, basis, g_start, quad_n = 12L
    )
    cached_value <- gicsurv:::gic_loglik_cached(
      theta, beta, cached, basis, g_start, quad_n = 12L
    )
    testthat::expect_equal(cached_value, reference_value, tolerance = 1e-10)
  }
})

testthat::test_that("production likelihood uses the compiled engine by default", {
  testthat::expect_true(getOption("gicsurv.use_likelihood_cache", TRUE))
})
