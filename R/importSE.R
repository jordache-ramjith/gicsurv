
#' import server
#'
#' This function imports the data and renders the data into a datatable.
#'
#' @param session id
#' @return import server objects
#' @export




importSE <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 dtreact <- reactive({
                   file <- input$file1
                   if (is.null(file)){return(NULL)}
                   
                   readxl::read_excel(file$datapath, na = c(NA, "", "na", "NA", "N/A", "n/a"))
                 })
                 
                 
                 output$contents <- DT::renderDT(
                   dtreact(), options = list(pageLength = 10, info = FALSE, lengthMenu = list(c(10,50,100,200, -1), c("10","50","100","200","All")),scrollX = TRUE ) 
                 )
                 
                 return(dtreact)
               })
  
}