
#' data import UI
#'
#' This function is the for creating the UI of the data import for the current
#' session.
#'
#' @param session number
#' @return The UI objects for the data import UI
#' @export




importUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    column(12,
           column(4,actionButton("swaptabs", "Data setup info",icon=icon("circle-info"), class = "btn-info")),
           column(
             8,
             fileInput(
               ns("file1"),
               "Choose XLSX/XLS File",
               multiple = FALSE,
               accept = c(".xlsx", ".xls")
             ),
           )),
    column(12,
           column(
             width = 3,
             varselect_ui("select"),
             numericInput(
               "K",
               "No. of knots",
               value = 10,
               min = 5,
               max = 30,
               step = 1
             ),
             actionButton("do4", "Run Analysis", icon("paper-plane"), class = "btn-primary")
           ),
           column(9,
                  DT::DTOutput(ns("contents"))))
    
  )
}