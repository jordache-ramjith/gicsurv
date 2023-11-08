
#' variable select UI
#'
#' This function builds the UI for the variable selection.
#'
#' @param session number
#' @return The UI objects for the variable selection
#' @export



varselect_ui <- function(id) {
  ns <- NS(id)
  var_choices <- ""
  tagList(
    selectInput(
      ns("time1"),
      "Select your time1 variable",
      choices = var_choices,
      selected = NULL
    ),
    selectInput(
      ns("time2"),
      "Select your time2 variable",
      choices = var_choices,
      selected = NULL
    ),
    selectInput(
      ns("time3"),
      "Select your time3 variable",
      choices = var_choices,
      selected = NULL
    ),
    selectInput(
      ns("status"),
      "Select your status variable",
      choices = var_choices,
      selected = NULL
    ),
    selectInput(
      ns("covs"),
      "Select your covariates",
      choices = var_choices,
      selected = NULL,
      multiple = TRUE
    )
  )
}
