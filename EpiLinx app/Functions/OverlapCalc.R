# Iterate through df under the conditions of time, patient ID and center
# Append rows to new df, if more than 1 patient visit center at the same day(s)

#' @param dt Data table.
#' @param location Column of either unit, hospital or ward.

OverlapCalc <- function(dt, location) {
  location_dt <- NULL
  locs <- location
  for (i in 1:(nrow(dt)-1)){
    
    #Look through location_dt and hospital
    loc = ((1:nrow(dt))>i) & (!grepl(dt$patient[i],dt$patient))&
      (dt[[locs]][i]==dt[[locs]])&
      (((dt$InDate[i]>=dt$InDate)&(dt$InDate[i]<=dt$OutDate)) |
         ((dt$InDate>=dt$InDate[i])&(dt$InDate<=dt$OutDate[i]))|
         ((dt$OutDate[i]<=dt$OutDate)&(dt$OutDate[i]>=dt$InDate))|
         ((dt$OutDate<=dt$OutDate[i])&(dt$OutDate>=dt$InDate[i])))
    if (sum(na.omit(loc))>0){
    dt_append <- as.data.frame(list('Patient 1' =paste0(dt$patient[i]),
                                     'Patient 2' =paste0(dt$patient[loc]),
                                     'locs' = dt[[locs]][loc],
                                     'Start'=pmax(dt$InDate[i],dt$InDate[loc]),
                                     'End'=pmin(dt$OutDate[i],dt$OutDate[loc])
    ))
      location_dt <- rbind(dt_append,location_dt)
     rm(dt_append)
    }
    rm(loc)
  }
  
  # Calculate duration of overlaps and arrange table
  if (!is.null(location_dt)){
    names(location_dt)[names(location_dt)=="locs"] <- location
    location_dt$Duration_days <- as.integer(as.Date(location_dt$End)-as.Date(location_dt$Start)+1)
    location_dt <- location_dt[!duplicated(location_dt[c("Patient.1","Patient.2",location,"Start","End")]),]
    location_dt <- arrange(location_dt,desc(Start),desc(Patient.1))
    location_dt$Start <- as.character(location_dt$Start)
    location_dt$End <- as.character(location_dt$End)
  } else{
    location_dt <- NULL
  }

  
  return(location_dt)
}
