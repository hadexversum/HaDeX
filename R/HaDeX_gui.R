#' HaDeX Graphical User Interface
#'
#' Launches graphical user interface.
#'
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' @export HaDeX_gui
#' @importFrom shiny runApp

HaDeX_gui <- function()
  runApp(system.file("HaDeX", package = "HaDeX"))
