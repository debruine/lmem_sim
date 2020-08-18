#' Launch Shiny App
#'
#' Launch a local copy of the shiny app that accompanies Simulating LMEM
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
app <- function(...) {
  appDir <- system.file("app", package = "lmem.sim")
  shiny::runApp(appDir, ...)
}

#' Open Simulating LMEM paper
#'
#' Open the manuscript
#' 
#' @param type The type of manuscript to open (pdf, html, or Rmd)
#'
#' @export
#'
paper <- function(type = c("pdf", "html", "Rmd")) {
  type <- match.arg(type)
  filename <- paste0("paper/01.AMPSS_LMEM.", type)
  f <- system.file(filename, package = "lmem.sim")
  browseURL(f)
}

