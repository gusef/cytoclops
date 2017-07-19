#' Launch Cytoclops
#'
#' Run the Shiny App Cytoclops
#' @export
cytoclops <- function () {
  shiny::runApp(system.file('shiny',package="cytoclops"))
}
