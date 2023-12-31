
#' Model run calculation
#'
#' This function runs the model and estimates the spline coefficients,
#' regression coefficients, and their covariance matrix.
#'
#' @param data and variables and user input
#' @return A complete output object
#' @export






calculation_analysis <-
  function(data,
           time1,
           time2,
           time3,
           status,
           covs,
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
    
    data = data[rowSums(is.na(as.matrix(
      data[, covs], nrow = M, ncol = nrow(data)
    ))) == 0,]
    
    
    if (nrow(data) == 0) {
      return.out = NULL
    } else {
      data$overlap = ifelse(data[[time1]] == data[[time3]] &
                              data[[time2]] == 0, 1, 0)
      
      data$exact = ifelse((data[[status]] == 1 &
                             (data[[time1]] == 0) &
                             (data[[time2]] == data[[time3]])), 1, 0)
      
      data$leftic = ifelse((data[[status]] == 1 &
                              (data[[time1]] != 0) &
                              (data[[time2]] == data[[time3]])), 1, 0)
      
      data$rightic = ifelse((data[[status]] == 1 &
                               (data[[time1]] == 0) &
                               (data[[time2]] != data[[time3]])), 1, 0)
      
      data$dic = ifelse((data[[status]] == 1 &
                           (data[[time1]] != 0) &
                           (data[[time2]] != data[[time3]])), 1, 0)
      
      data$right.st = ifelse((data[[status]] == 0 &
                                data[[time1]] != 0), 1, 0)
      data$right.ex = ifelse((data[[status]] == 0 &
                                data[[time1]] == 0), 1, 0)
      
      data$overlap2 = ifelse(data[[time2]] == 0, 1, 0)
      
      if (is.null(covs)==TRUE) {
        X = t(matrix(0, nrow = nrow(data), ncol = 1))
        M = 0
      } else {
        X = t(model.matrix(formula(paste(
          "~", paste(covs, collapse = "+")
        )), data = data)[,-1])
        rnames <-
          colnames(model.matrix(formula(paste(
            "~", paste(covs, collapse = "+")
          )), data = data))[-1]
        M = nrow(X)
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
      
      bsmooth <- bs.smoother(X = stime)
      smooth <- bsmooth$bsint
      hsmooth <- bsmooth$bs
      pen <- bsmooth$penmat[1:K, 1:K]
      penf = matrix(0, nrow = (K + M), ncol = (K + M))
      penf[1:K, 1:K] = pen
      
      
      predict.b <- function(object, t) {
        predict(object, t)[, 1:K]
      }
      
      
      
      # Full likelihood with more precise integration - to be used to estimate regression and spline coefficients
      
      loglik_u = function(par, gamma) {
        tau = par[1:K] ^ 2
        
        if (is.null(covs)==TRUE) {
          beta = NULL
          data[["lp"]] = rep(0, nrow(data))
        } else {
          beta = par[(K+1):(K+M)]
          data[["lp"]] = t(t(beta) %*% X)
        }
        
        
        #DIC
        data1 = data[data[[status]] == 1 &
                       data$overlap == 0 & data$dic == 1,]
        data2 = data[data[[status]] == 1 &
                       data$overlap == 1 & data$dic == 1,]
        data3 = data[data[[status]] == 0 & data$right.st == 1,]
        
        #IC event
        data4 = data[data[[status]] == 1 &
                       data$overlap2 == 0 & data$rightic == 1,]
        data5 = data[data[[status]] == 1 &
                       data$overlap2 == 1 & data$rightic == 1,]
        data6 = data[data[[status]] == 0 & data$right.ex == 1,]
        
        #IC start
        data7 = data[data[[status]] == 1 &
                       data$leftic == 1 & data$dic == 0,]
        
        #exact
        data8 = data[data[[status]] == 1 &
                       data$exact == 1 &
                       data$leftic == 0 & data$dic == 0,]
        
        # Define hazard function (updated)
        hazard <- function(t, lp) {
          lambda_k <- predict.b(hsmooth, t) %*% tau
          return(lambda_k * exp(lp))  # Use matrix multiplication
        }
        
        # Define cumulative hazard function (updated)
        cumulative_hazard <- function(t, lp) {
          lambda_k <- predict.b(smooth, t) %*% tau
          return(lambda_k * exp(lp))
        }
        
        
        # Define survival function (updated)
        survival_function <- function(t, lp) {
          Lambda_t <- cumulative_hazard(t, lp)
          return(exp(-Lambda_t))
        }
        
        density <- function(t, lp) {
          out <- survival_function(t, lp) * hazard(t, lp)
          return(out)
        }
        
        # Define the integral function
        mean_survival <- function(t1, t2, lp) {
          survival <- function(t, u, lp) {
            exp(-(predict.b(smooth, t - u) %*% tau) * exp(lp))
          }
          
          survy = vector(length = length(t1))
          
          for (i in 1:length(t1)) {
            survy[i] = stats::integrate(
              function(u){
                survival(t2[i], u, lp[i])},
              lower = 0,
              upper = t1[i],
              subdivisions = 10,
              rel.tol = .Machine$double.eps ^ .05
            )$value
          }
          return(survy)
        }
        
        
        
        if (nrow(data1) == 0) {
          L1 = 1
        } else {
          L1 = (1 / data1[[time1]]) * (
            mean_survival(data1[[time1]], data1[[time2]], data1[["lp"]]) - mean_survival(data1[[time1]], data1[[time3]], data1[["lp"]])
          )
          
        }
        
        
        if (nrow(data2) == 0) {
          L2 = 1
        } else {
          L2 = (1 / data2[[time1]]) * (data2[[time1]] - mean_survival(data2[[time1]], data2[[time1]], data2[["lp"]]))
          
        }
        
        if (nrow(data3) == 0) {
          L3 = 1
        } else {
          L3 = (1 / data3[[time1]]) * (mean_survival(data3[[time1]], data3[[time2]], data3[["lp"]]))
          
        }
        
        
        if (nrow(data4) == 0) {
          L4 = 1
        } else {
          L4 = survival_function(t = data4[[time2]], lp = data4[["lp"]]) - survival_function(t =
                                                                                               data4[[time3]], lp = data4[["lp"]])
        }
        
        if (nrow(data5) == 0) {
          L5 = 1
        } else {
          L5 = 1 - survival_function(t = data5[[time3]], lp = data5[["lp"]])
        }
        
        if (nrow(data6) == 0) {
          L6 = 1
        } else {
          L6 = survival_function(t = data6[[time2]], lp = data6[["lp"]])
        }
        
        
        if (nrow(data7) == 0) {
          L7 = 1
        } else {
          L7 = (1 / data7[[time1]]) * survival_function(t = data7[[time2]] - data7[[time1]], lp =
                                                          data7[["lp"]]) - survival_function(t = data7[[time2]], lp = data7[["lp"]])
        }
        
        
        if (nrow(data8) == 0) {
          L8 = 1
        } else {
          L8 = hazard(t = data8[[time2]], lp = data8[["lp"]]) * survival_function(t =
                                                                                    data8[[time2]], lp = data8[["lp"]])
        }
        
        
        ll = sum(log(L1)) + sum(log(L2)) + sum(log(L3)) + sum(log(L4)) +
          sum(log(L5)) + sum(log(L6)) + sum(log(L7)) + sum(log(L8)) - (exp(gamma) /
                                                                         2) * as.numeric(t(c(tau, beta)) %*% penf %*% c(tau, beta))
        
        if (is.finite(ll)) {
          nll = -ll
        } else{
          nll = exp(25)
        }
        
        return(nll)
        
      }
      
      #simplified likelihood expression where the integrals are solved using simpson's approximation. This simplication is used in the estimation of the penalty. Further, here we use a ridge penalty instead of the full penalty since we are treating the spline parameters as random effects.
      
      loglik_u2 = function(par, gamma) {
        tau = par[1:K] ^ 2
        
        if (is.null(covs)==TRUE) {
          beta = NULL
          data[["lp"]] = rep(0, nrow(data))
        } else {
          beta = par[(K+1):(K+M)]
          data[["lp"]] = t(t(beta) %*% X)
        }
        
        
        #DIC
        data1 = data[data[[status]] == 1 &
                       data$overlap == 0 & data$dic == 1,]
        data2 = data[data[[status]] == 1 &
                       data$overlap == 1 & data$dic == 1,]
        data3 = data[data[[status]] == 0 & data$right.st == 1,]
        
        #IC event
        data4 = data[data[[status]] == 1 &
                       data$overlap2 == 0 & data$rightic == 1,]
        data5 = data[data[[status]] == 1 &
                       data$overlap2 == 1 & data$rightic == 1,]
        data6 = data[data[[status]] == 0 & data$right.ex == 1,]
        
        #IC start
        data7 = data[data[[status]] == 1 &
                       data$leftic == 1 & data$dic == 0,]
        
        #exact
        data8 = data[data[[status]] == 1 &
                       data$exact == 1 &
                       data$leftic == 0 & data$dic == 0,]
        
        
        
        # Define hazard function (updated)
        hazard <- function(t, lp) {
          lambda_k <- predict.b(hsmooth, t) %*% tau
          return(lambda_k * exp(lp))  # Use matrix multiplication
        }
        
        # Define cumulative hazard function (updated)
        cumulative_hazard <- function(t, lp) {
          lambda_k <- predict.b(smooth, t) %*% tau
          return(lambda_k * exp(lp))
        }
        
        
        # Define survival function (updated)
        survival_function <- function(t, lp) {
          Lambda_t <- cumulative_hazard(t, lp)
          return(exp(-Lambda_t))
        }
        
        density <- function(t, lp) {
          out <- survival_function(t, lp) * hazard(t, lp)
          return(out)
        }
        
        # Define the integral function
        mean_survival <- function(t1, t2, lp) {
          survival <- function(t, u, lp) {
            exp(-(predict.b(smooth, t - u) %*% tau) * exp(lp))
          }
          
          survy = vector(length = length(t1))
          
          for (i in 1:length(t1)) {
            survy[i] = stats::integrate(
              function(u){
                survival(t2[i], u, lp[i])},
              lower = 0,
              upper = t1[i],
              subdivisions = 10,
              rel.tol = .Machine$double.eps ^ .05
            )$value
          }
          return(survy)
        }
        
        
        
        if (nrow(data1) == 0) {
          L1 = 1
        } else {
          ##simpson's approximation
          L1 = ((data1[[time3]] - data1[[time2]]) / 36) * (
            density(data1[[time2]], data1[["lp"]]) + 4 * density((data1[[time2]] - data1[[time1]] /
                                                                    2), data1[["lp"]]) + density((data1[[time2]] - data1[[time1]]), data1[["lp"]]) + 4 *
              density((data1[[time2]] + data1[[time3]]) / 2, data1[["lp"]]) + 16 * density((data1[[time1]] +
                                                                                              data1[[time3]] - data1[[time1]]) / 2, data1[["lp"]]) + 4 * density(((
                                                                                                data1[[time2]] + data1[[time3]]
                                                                                              ) / 2) - data1[[time1]], data1[["lp"]]) + density(data1[[time3]], data1[["lp"]]) + 4 *
              density((data1[[time3]] - data1[[time1]] / 2), data1[["lp"]]) + density((data1[[time3]] -
                                                                                         data1[[time1]]), data1[["lp"]])
          )
          
        }
        
        
        if (nrow(data2) == 0) {
          L2 = 1
        } else {
          L2 = (data2[[time1]] / 36) * (density(data2[[time1]], data2[["lp"]]) + 8 *
                                          density((data2[[time1]] / 4) , data2[["lp"]]) + 6 * density((data2[[time1]] /
                                                                                                         2) , data2[["lp"]]))
        }
        
        if (nrow(data3) == 0) {
          L3 = 1
        } else {
          L3 = (1 / 6) * (
            survival_function(data3[[time2]], data3[["lp"]]) + 4 * survival_function((data3[[time2]] -
                                                                                        data3[[time1]] / 2), data3[["lp"]]) + survival_function((data3[[time2]] -
                                                                                                                                                   data3[[time1]]), data3[["lp"]])
          )
        }
        
        
        
        if (nrow(data4) == 0) {
          L4 = 1
        } else {
          L4 = survival_function(t = data4[[time2]], lp = data4[["lp"]]) - survival_function(t =
                                                                                               data4[[time3]], lp = data4[["lp"]])
        }
        
        if (nrow(data5) == 0) {
          L5 = 1
        } else {
          L5 = 1 - survival_function(t = data5[[time3]], lp = data5[["lp"]])
        }
        
        if (nrow(data6) == 0) {
          L6 = 1
        } else {
          L6 = survival_function(t = data6[[time2]], lp = data6[["lp"]])
        }
        
        
        if (nrow(data7) == 0) {
          L7 = 1
        } else {
          L7 = (1 / data7[[time1]]) * survival_function(t = data7[[time2]] - data7[[time1]], lp =
                                                          data7[["lp"]]) - survival_function(t = data7[[time2]], lp = data7[["lp"]])
        }
        
        
        if (nrow(data8) == 0) {
          L8 = 1
        } else {
          L8 = hazard(t = data8[[time2]], lp = data8[["lp"]]) * survival_function(t =
                                                                                    data8[[time2]], lp = data8[["lp"]])
        }
        
        
        
        ll = sum(log(L1)) + sum(log(L2)) + sum(log(L3)) + sum(log(L4)) +
          sum(log(L5)) + sum(log(L6)) + sum(log(L7)) + sum(log(L8)) -  (exp(gamma) /
                                                                          2) * as.numeric(sum(tau ^ 2))
        
        if (is.finite(ll)) {
          nll = -ll
        } else{
          nll = exp(25)
        }
        
        return(nll)
        
      }
      
      
      
      
      set.seed(1986)
      
      par0 = c(runif(K,-0.1, 0.1), rep(-0.01, M))
      
      #the neg log-likelihood of the variance component for the random intercepts.
      
      mll <- function(gamma) {
        # Initial guess for lambda, tau and beta (a vector of values)
        dt = 0
        j = 1
        
        while (dt <= 0 & j <= 10) {
          set.seed(paste0(1986, j))
          par0 = c(runif(K,-0.1, 0.1), rep(-0.01, M))
          
          (
            est1 <-
              stats::optim(
                par0,
                loglik_u2,
                gamma = gamma,
                control = list(maxit = 100, trace = 1),
                method = "BFGS",
                hessian = TRUE
              )
          )
          
          dt = det(est1$hessian)
          j = j + 1
          
        }
        ##we can assume that the gradient of the estimated parameters for the given value of sigma here is zero.
        
        ##The hessian above is evaluated at a specific sigma value
        
        ll = -K * log(1 / sqrt(exp(gamma))) - est1$value  - 0.5 * determinant(est1$hessian, logarithm =
                                                                                TRUE)$modulus[[1]]
        
        
        if (is.finite(ll) & det(est1$hessian) > 0) {
          nll = -ll
        } else{
          nll = exp(25)
        }
        
        return(nll)
        
        
        
      }
      
      
      A = Sys.time()
      points = seq(-15, 25, length.out = K)
      dog = sapply(X = points, function(X){
        mll(X)})
      dts = data.frame(points, dog)
      dts = dts[dts$dog < exp(25),]
      mod1 = mgcv::gam(dog ~ s(points, k = nrow(dts)), data = dts)
      pred = data.frame(points = seq(-15, 25, length.out = 10000))
      pred$dog = mgcv::predict.gam(mod1, newdata = pred)
      gamma_rn = pred$points[which(pred$dog == min(pred$dog))]
      B = Sys.time()
      B - A
      
      
      dt = 0
      j = 1
      
      while (dt <= 0 & j <= 20) {
        set.seed(paste0(1986, j))
        par0 = c(stats::runif(K,-0.1, 0.1), rep(-0.01, M))
        (
          results <-
            stats::optim(
              par0,
              loglik_u,
              gamma = gamma_rn,
              control = list(maxit = 1000, trace = 1),
              method = "BFGS",
              hessian = TRUE
            )
        )
        dt = det(results$hessian)
        j = j + 1
        
      }
      
      return.out = results
      
    }
    
    return(return.out)
  }
