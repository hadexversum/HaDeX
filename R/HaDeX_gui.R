#' HaDeX Graphical User Interface
#'
#' @description Launches graphical user interface.
#' 
#' @importFrom shiny runApp
#' 
#' @param port The TCP port. See \code{\link[shiny]{runApp}}.
#' 
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' 
#' @export HaDeX_gui

HaDeX_gui <- function(port = getOption("shiny.port"))
  runApp(system.file("HaDeX", package = "HaDeX"), port = port)
