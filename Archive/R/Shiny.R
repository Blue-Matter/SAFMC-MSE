#' Run the SAFMC Shiny Application
#'
#' \code{Shiny} runs the Shiny Application
#' @export
Shiny <- function(...) {
  appDir <- system.file("shiny_app", package = "SAMSE")
  shiny::runApp(appDir, ...)
}
