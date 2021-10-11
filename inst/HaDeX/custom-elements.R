HaDeX_plotTab <- function(title, settingsPanel, displayPanel) tabPanel(
  title = title,
  fillRow(
    class = "HaDeX-plot-tab-content",
    flex = c(3, 7),
    settingsPanel,
    displayPanel
  )
)

HaDeX_plotSettingsPanel <- function(...) wellPanel(
  class = "HaDeX-tab-content-element HaDeX-plot-settings-panel",
  fillCol(
    flex = NA,
    h4("Select parameters for the plot"),
    ...
  )
)

HaDeX_plotDisplayPanel <- function(...) div(
  class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
  fillCol(
    flex = NA,
    ...
  )
)

HaDeX_plotSettingsSection <- function(..., title = NULL) tagList(
  if (!is.null(title)) h5(title) else NULL,
  ...
)