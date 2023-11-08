
#' variable server
#'
#' This function builds the server object to update the variable selection
#' depending on the uploaded data.
#'
#' @param session number and data
#' @return updated variable selection
#' @export


varselect_server <- function(id, data) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(data(), {
                   updateSelectInput(session,
                                     "time1",
                                     choices = names(data()))
                   updateSelectInput(session,
                                     "time2",
                                     choices = names(data()))
                   updateSelectInput(session,
                                     "time3",
                                     choices = names(data()))
                   updateSelectInput(session,
                                     "status",
                                     choices = names(data()))
                   updateSelectInput(session,
                                     "covs",
                                     choices = names(data()))
                 })
                 
                 return(
                   list(
                     time1 = reactive({
                       input$time1
                     }),
                     time2 = reactive({
                       input$time2
                     }),
                     time3 = reactive({
                       input$time3
                     }),
                     status = reactive({
                       input$status
                     }),
                     covs = reactive({
                       input$covs
                     })
                   )
                 )
               })
}