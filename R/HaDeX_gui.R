#' HaDeX Graphical User Interface
#'
#' Launches graphical user interface.
#' 
#' @param port The TCP port. See \code{\link[shiny]{runApp}}.
#' 
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' @export HaDeX_gui
#' @importFrom shiny runApp

HaDeX_gui <- function(port = getOption("shiny.port"))
  runApp(system.file("HaDeX", package = "HaDeX"), port = port)
