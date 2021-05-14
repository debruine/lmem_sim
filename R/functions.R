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

#' Open Simulating LMEM appendices
#' 
#' @param i Which appendix to open
#' @param filename Where to save the appendix
#'
#' @export
#'
appendix <- function(i = c("1a", "1b", "1c", "2", "3a", "3b"), filename = NULL) {
  i <- match.arg(i)
  dir <- system.file("appendices/", package = "lmem.sim")
  files <- list.files(dir, ".Rmd", full.names = TRUE)
  n <- grepl(paste0("appendix", i), files)
  f <- files[n][[1]]
  
  if (is.null(filename)) filename <- basename(f)
  
  file.copy(f, filename)
  utils::browseURL(filename)
}

