#' warning indicator
#'
#' This function allows a pop-up if the variables have not been changed from the
#' default.
#'
#' @param data file and selected variables
#' @return An indicator 
#' @export



warning_calc <- function(data,
                         time1,
                         time2,
                         time3,
                         status){
  ttt=!all((data[[status]] %in% c(0,1))) 
  
  ttp = time1==time2 & time1==time3 & time2==time3 
  
  ttt = ttt|ttp
  
  return(ttt)
}