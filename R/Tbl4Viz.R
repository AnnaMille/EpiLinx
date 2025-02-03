# Table of patient stays for visualisation

#' @param dt Data table.
#' @param names List of patient identifiers.
#' @param dates Sequence of dates.
#' @param location Column of either unit, hospital or ward.

Tbl4Viz <- function(mat, location) {
  tbl_map <- melt(as.table(mat))
  tbl_map$value <- as.factor(tbl_map$value)
  colnames(tbl_map) <- c("Patient", "Date", quo_name(location))
  tbl_map$Date <- as.Date(tbl_map$Date)
  
 
  return(tbl_map)
}
