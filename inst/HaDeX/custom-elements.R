HaDeX_plotTab <- function(title, settingsPanel, displayPanel) tabPanel(
  title = title,
  fillRow(
    class = "HaDeX-plot-tab-content",
    flex = c(3, 7),
    settingsPanel,
    displayPanel
  )
)

HaDeX_nonplotTab <- function(title, ...) tabPanel(
  title = title,
  class = "HaDeX-nonplot-tab-content",
  ...
)

HaDeX_plotSettingsPanel <- function(...) wellPanel(
  class = "HaDeX-tab-content-element HaDeX-plot-settings-panel",
  fillCol(
    flex = NA,
    h4(
      class = "HaDeX-plot-settings-panels-header",
      "Select parameters for the plot"
      ),
    ...
  )
)

HaDeX_plotDisplayPanel <- function(...) div(
  class = "HaDeX-tab-content-element HaDeX-plot-display-panel",
  fillCol(
    class = "HaDeX-plot-display-panel-container",
    flex = NA,
    ...
  )
)

HaDeX_plotSettingsSection <- function(..., title = NULL) tagList(
  if (!is.null(title)) h5(class = "HaDeX-plot-settings-panels-section-title", title) else NULL,
  ...
)

HaDeX_collapseButton <- function(title, target) tags$button(
  title,
  class = "btn btn-default collapse-btn",
  `data-toggle`= "collapse",
  `data-target`= target
)

HaDeX_collapsablePanel <- function(id, ...) tags$div(
  class = "hideable collapse",
  id = id,
  ...
)