#' Launch the gicsurv Shiny application
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @return No return value; launches the application.
#' @export
gicsurv <- function(...) {
  app_directory <- system.file("shiny-app", package = "gicsurv")
  if (!nzchar(app_directory)) stop("The bundled Shiny application was not found.")
  shiny::runApp(app_directory, ...)
}
