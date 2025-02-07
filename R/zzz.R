## This special R file contains (optional) functions that are run when your package is loaded/attached:

.onAttach <- function(lib, pkg)
{
  ## TODO: this is a bit aggressive; also my system says it can't be honored!
  # Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
}
