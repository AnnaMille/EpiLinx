# Matrix of patient stays for visualisation

#' @param dt Data table.
#' @param names List of patient identifiers.
#' @param dates Sequence of dates.
#' @param location Column of either unit, hospital or ward.

Matrix4Viz <- function(dt, names, dates, location) {
  Mat <- matrix(data=NA, nrow=length(names), ncol = length(dates))
  rownames(Mat) <- names
  colnames(Mat) <- dates

    for (i in  1:nrow(dt)){
      tempDays=seq(min(as.Date(dt$InDate[i])),max(as.Date(dt$OutDate[i])), "day")
      colIndx=which(dates %in% tempDays)
      rowIndx =which(names %in% dt$patient[i])
      Mat[rowIndx,colIndx] = dt[[location]][i]
    }

    # Filter non-admitted patients
  ResMat <- Mat[rowSums(is.na(Mat))!=ncol(Mat),]
  if(nrow(Mat) == 1)   ## KTKA: Added for single row matrices
    ResMat <- t(Mat[rowSums(is.na(Mat))!=ncol(Mat),])
  colnames(ResMat) <- format(as.Date(dates, format= "%Y-%m-%d"), format= "%Y-%m-%d")

  return(ResMat)
}
