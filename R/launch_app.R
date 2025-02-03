#' Launch the EpiLinx app
#'
#' @import lubridate
#' @import tidyverse
#' @import ggraph
#' @import igraph
#' @import reshape2
#' @import shiny
#' @import ggnetwork
#' @import sna
#' @import shinydashboard
#' @import shinycssloaders
#' @import DT
#' @import shinyWidgets
#' @import shinyjs
#' @import shinyBS
## TODO: rationalise imports
#'
#' @export
launch_app <- function(){
  shinyAppDir(system.file("app", package="EpiLinx"))
}
