
#' coefficient estimates calculator
#'
#' This function calculates estimates of the covariate effects given the data
#' and selected variables and after running of the estimation.
#'
#' @param data model object and covariates
#' @return covariate effects
#' @export




output_analysis <- function(data,
                            results,
                            covs,
                            K) {
  X = t(model.matrix(formula(paste(
    "~", paste(covs, collapse = "+")
  )), data = data)[,-1])
  
  rnames <-
    colnames(model.matrix(formula(paste(
      "~", paste(covs, collapse = "+")
    )), data = data))[-1]
  M = nrow(X)
  
  output <- data.frame(Variables = rnames,
                       coef = results$par[(K + 1):(K + M)],
                       SE = sqrt(diag(solve(
                         results$hessian
                       ))[(K + 1):(K + M)])) %>% dplyr::mutate(
                         HR = exp(coef),
                         lower = exp(coef - 2 * SE),
                         upper = exp(coef + 2 * SE),
                         zval = coef / SE,
                         pval = 2 * pnorm(abs(zval), lower.tail = FALSE)
                       )
  
  
  output$coef <- format(output$coef, digits = 2, nsmall=2)
  output$SE <- format(output$SE, digits = 2, nsmall=2)
  output$HR <- format(output$HR, digits = 2, nsmall=2)
  output$lower <- format(output$lower, digits = 2, nsmall=2)
  output$upper <- format(output$upper, digits = 2, nsmall=2)
  output$zval <- format(output$zval, digits = 2, nsmall=2)
  output$pval <- format(output$pval, digits = 2, nsmall=4)  
  
  return(output)
}