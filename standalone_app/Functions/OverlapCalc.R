# Iterate through df under the conditions of time, patient ID and center
# Append rows to new df, if more than 1 patient visit center at the same day(s)

#' @param dt Data table.
#' @param loc Column of either unit, hospital or ward.

OverlapCalc <- function(dt, loc, n_days) {
  
  dt$rownum <- 1:nrow(dt)
  
  if(n_days == 0){
    location_dt <- dt %>%
      inner_join(dt, by = loc, suffix=c(".1",".2")) %>%
      filter(InDate.1 <= OutDate.2  &
               OutDate.1  >= InDate.2 &
               patient.1 != patient.2 &
               rownum.1 < rownum.2) %>%
      rowwise %>%
      mutate(Start = max(InDate.1, InDate.2),
             End = min(OutDate.1, OutDate.2),
             Duration_days = as.integer(End-Start+1),
             Patient.1 = patient.1,
             Patient.2 = patient.2,
             Department = Department.1) %>%
      distinct(all_of(loc), Patient.1, Patient.2, Start, End, .keep_all = T) %>%
      select(Patient.1, Patient.2, all_of(loc), Department, Start, End, Duration_days) %>%
      arrange(desc(Start), desc(Patient.1)) %>%
      mutate(Start = as.character(Start), End = as.character(End))
  }else{
    location_dt <- dt %>%
      inner_join(dt, by = loc, suffix=c(".1",".2")) %>%
      filter(InDate.1 <= OutDate.2 + n_days &
               OutDate.1 + n_days >= InDate.2 &
               patient.1 != patient.2 &
               rownum.1 < rownum.2) %>%
      rowwise %>%
      mutate(
        Patient.1.End = max(OutDate.1),
        Patient.2.Start = min(InDate.2),
        Difference_days = as.integer(Patient.2.Start-Patient.1.End),
        Patient.1 = patient.1,
        Patient.2 = patient.2,
        Department = Department.1) %>%
      distinct(all_of(loc), Patient.1, Patient.2, Patient.2.Start, Patient.1.End, .keep_all = T) %>%
      select(Patient.1, Patient.2, all_of(loc), Department, Patient.1.End, Patient.2.Start, Difference_days) %>%
      filter(Difference_days > 0) %>%
      arrange(Difference_days) %>%
      mutate(Patient.2.Start = as.character(Patient.2.Start), Patient.1.End = as.character(Patient.1.End))
  }
  
  return(location_dt)
}

