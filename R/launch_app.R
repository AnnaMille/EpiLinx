#' Launch the EpiLinx app
#'
#'
#' @export
launch_app <- function(){
  shinyAppDir(system.file("app", package="EpiLinx"))
}
