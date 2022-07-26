#' Checks if GUI package is installed
#' 
#' Indicates presence or absence of HaDeXGUI package.
#' 
#' @return logical value indicating availability of GUI package
is_GUI_installed <- function()
  length(find.package("HaDeXGUI", quiet = TRUE)) > 0

#' Installs GUI package from GitHub
#' 
#' @importFrom remotes install_github
#' @export
install_GUI <- function()
  remotes::install_github("hadexversum/HaDeXGUI")

handle_missing_GUI <- function() {
  if (interactive()) {
    response <- menu(
      c("yes", "no"), 
      title = "To be able to run HaDeX Graphical User Interface, you have to have the 'HaDeXGUI' package installed. It is available via GitHub. Install?")
    switch (
      response, 
      yes = tryCatch(
        install_GUI(),
        finally = if (!is_GUI_installed()) {
          warning("There was an error during an attempt to install 'HaDeXGUI' package.", call. = FALSE)
        } else {
          message("Package installed successfully. You can run the app now calling 'HaDeX_GUI()' once again.")
        }
      ),
      no = message("You cannot run the GUI without having installed 'HaDeXGUI'. You can do it by calling 'HaDeX::install_GUI()'."),
    )
  } else {
    message("To be able to run HaDeX Graphical User Interface, you have to have the 'HaDeXGUI' package installed. You can do it by calling 'HaDeX::install_GUI()'.")
  }
}

#' HaDeX Graphical User Interface
#'
#' @description Launches graphical user interface from HaDeXGUI package. If the
#'   GUI package is not installed, it asks user whether to install it
#' 
#' @importFrom shiny runApp
#' 
#' @param ... arguments to pass to \code{HaDeXGUI::run_app()}. 
#'    See \code{\link[HaDeXGUI]{run_app}}.
#' @param port number of port for the app to run on.
#' @param prod should the app run in production mode (TRUE) or development (FALSE)?
#' 
#' @section Warning : Any ad-blocking software may cause malfunctions.
#' 
#' @export
HaDeX_GUI <- function(port = httpuv::randomPort(), prod = TRUE, ...) {
  if (is_GUI_installed()) {
    opts <- options()
    on.exit(options(opts))
    options(
      shiny.port = port,
      golem.app.prod = prod
    )
    HaDeXGUI::run_app(options = options, ...)
  } else handle_missing_GUI()
}
