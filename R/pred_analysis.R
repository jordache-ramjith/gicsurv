
#' baseline estimates calculator
#'
#' This function calculates baseline hazard, cumulative hazard and survival
#' estimates given the data and selected variables and after running of the 
#' estimation.
#'
#' @param data and variables and user input
#' @return Baseline estimates
#' @export



pred_analysis <- function(results,
                          data,
                          time1,
                          time2,
                          time3,
                          status,
                          covs,
                          maxtime,
                          nboot,
                          K) {
  
  
  if (is.numeric(data[[time1]]) == F) {
    data[[time1]] = as.numeric(gsub(',', '.', data[[time1]]))
  } else {
    data[[time1]] = data[[time1]]
  }
  
  if (is.numeric(data[[time2]]) == F) {
    data[[time2]] = as.numeric(gsub(',', '.', data[[time2]]))
  } else {
    data[[time2]] = data[[time2]]
  }
  
  if (is.numeric(data[[time3]]) == F) {
    data[[time3]] = as.numeric(gsub(',', '.', data[[time3]]))
  } else {
    data[[time3]] = data[[time3]]
  }
  
  if (is.numeric(data[[status]]) == F) {
    data[[status]] = as.numeric(gsub(',', '.', data[[status]]))
  } else {
    data[[status]] = data[[status]]
  }
  
  
  #defining the specifications
  
  K <- max(K, 10) #number of intervals we wish to create
  M <- length(covs) #number of covariates
  degree <- 3
  
  
  data = data[is.na(data[[time1]]) == FALSE,]
  data = data[is.na(data[[time2]]) == FALSE,]
  #data = data[is.na(data[[time3]])==FALSE,]
  data = data[is.na(data[[status]]) == FALSE,]
  if (!all(is.na(covs))) {
    covs = na.omit(covs)
    data = data[rowSums(is.na(as.matrix(
      data[, covs], nrow = M, ncol = nrow(data)
    ))) == 0,]
  }
  
  bs.smoother <- function(X) {
    B <- list()
    
    X = X[!is.infinite(X)]
    
    smoother <-
      mgcv::smooth.construct(mgcv::s(
        x,
        k = K,
        bs = "bs",
        m = c(degree, (degree - 1))
      ), data.frame(x = X), NULL)
    B$knots <- smoother$knots[-(1:3)]
    B$knots[1] <- 0
    
    B$bs = splines2::bSpline(
      X,
      knots = B$knots[-c(1, length(B$knots))],
      degree = degree,
      intercept = TRUE,
      Boundary.knots = c(B$knots[1], B$knots[length(B$knots)])
    )
    
    B$bsint <-
      splines2::ibs(
        X,
        knots = B$knots[-c(1, length(B$knots))],
        degree = degree,
        intercept = TRUE,
        Boundary.knots = c(B$knots[1], B$knots[length(B$knots)])
      )
    
    basisfd <-
      fda::create.bspline.basis(
        rangeval = c(B$knots[1], B$knots[length(B$knots)]),
        breaks = B$knots,
        norder = (degree + 1)
      )
    B$penmat <- fda::bsplinepen(basisfd, Lfd = 2)
    
    return(B)
  }
  
  
  #this might need to change
  stime = c(data[[time1]], data[[time2]], data[[time3]])
  stime = stime[is.finite(stime) & stime >= 0]
  mtime = max(stime)
  
  bsmooth <- bs.smoother(X = stime)
  smooth <- bsmooth$bsint
  hsmooth <- bsmooth$bs
  predict.b <- function(object, t){
    predict(object, t)[,1:K]
  }
  
  
  
  tau <- results$par[1:(K)]^2
  
  # Define hazard function (updated)
  hazard <- function(t, tau) {
    lambda_k <- predict.b(hsmooth, t) %*% tau
    return(lambda_k)  # Use matrix multiplication
  }
  
  
  # Define cumulative hazard function (updated)
  cumulative_hazard <- function(t, tau) {
    lambda_k <- predict.b(smooth, t) %*% tau
    return(lambda_k)
  }
  
  
  # Define survival function (updated)
  survival_function <- function(t, tau) {
    Lambda_t <- cumulative_hazard(t, tau)
    return(exp(-Lambda_t))
  }
  
  
  #we do not wish to include time=0
  pred = data.frame(time = seq(0,min(maxtime,mtime), length.out=1000))
  
  pred$haz = as.vector(hazard(pred$time, tau))
  pred$ch = as.vector(cumulative_hazard(pred$time, tau))
  pred$surv = as.vector(survival_function(pred$time, tau))
  
  cicoefs = MASS::mvrnorm(nboot, results$par, solve(results$hessian))
  cihaz = matrix(nrow = nrow(pred), ncol = nboot)
  cich = matrix(nrow = nrow(pred), ncol = nboot)
  cisurv = matrix(nrow = nrow(pred), ncol = nboot)
  for (i in 1:nboot) {
    cihaz[, i] = hazard(pred$time, cicoefs[i, 1:K]^2)
    cich[, i] = cumulative_hazard(pred$time, cicoefs[i, 1:K]^2)
    cisurv[, i] = survival_function(pred$time, cicoefs[i, 1:K]^2)
  }
  
  haz.ci = t(apply(
    cihaz,
    1,
    FUN = function(x){
      quantile(x, probs = c(0.025, 0.975))}
  ))
  ch.ci = t(apply(
    cich,
    1,
    FUN = function(x){
      quantile(x, probs = c(0.025, 0.975))}
  ))
  surv.ci = t(apply(
    cisurv,
    1,
    FUN = function(x){
      quantile(x, probs = c(0.025, 0.975))}
  ))
  
  pred$haz.lwr = haz.ci[, 1]
  pred$haz.upr = haz.ci[, 2]
  pred$ch.lwr = ch.ci[, 1]
  pred$ch.upr = ch.ci[, 2]
  pred$surv.lwr = surv.ci[, 1]
  pred$surv.upr = surv.ci[, 2]
  
  return(pred)
}