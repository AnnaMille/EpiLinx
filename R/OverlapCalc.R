# Iterate through df under the conditions of time, patient ID and center
# Append rows to new df, if more than 1 patient visit center at the same day(s)

#' @param dt Data table.
#' @param loc Column of either unit, hospital or ward.

OverlapCalc <- function(dt, loc, n_days) {

  dt$rownum <- 1:nrow(dt)

  if(n_days == 0){
    location_dt <- dt %>%
      inner_join(dt, by = loc, suffix=c(".1",".2")) %>%
      filter(.data$InDate.1 <= .data$OutDate.2  &
               .data$OutDate.1  >= .data$InDate.2 &
               .data$patient.1 != .data$patient.2 &
               .data$rownum.1 < .data$rownum.2) %>%
      rowwise %>%
      mutate(Start = max(.data$InDate.1, .data$InDate.2),
             End = min(.data$OutDate.1, .data$OutDate.2),
             Duration_days = as.integer(.data$End-.data$Start+1),
             Patient.1 = .data$patient.1,
             Patient.2 = .data$patient.2,
             Department = .data$Department.1) %>%
      distinct(all_of(loc), .data$Patient.1, .data$Patient.2, .data$Start, .data$End, .keep_all = T) %>%
      select(.data$Patient.1, .data$Patient.2, all_of(loc), .data$Department, .data$Start, .data$End, .data$Duration_days) %>%
      arrange(desc(.data$Start), desc(.data$Patient.1)) %>%
      mutate(Start = as.character(.data$Start), End = as.character(.data$End))
  }else{
    location_dt <- dt %>%
      inner_join(dt, by = loc, suffix=c(".1",".2")) %>%
      filter(.data$InDate.1 <= .data$OutDate.2 + n_days &
               .data$OutDate.1 + n_days >= .data$InDate.2 &
               .data$patient.1 != .data$patient.2 &
               .data$rownum.1 < .data$rownum.2) %>%
      rowwise %>%
      mutate(
        Patient.1.End = max(.data$OutDate.1),
        Patient.2.Start = min(.data$InDate.2),
        Difference_days = as.integer(.data$Patient.2.Start-.data$Patient.1.End),
        Patient.1 = .data$patient.1,
        Patient.2 = .data$patient.2,
        Department = .data$Department.1) %>%
      distinct(all_of(loc), .data$Patient.1, .data$Patient.2, .data$Patient.2.Start, .data$Patient.1.End, .keep_all = T) %>%
      select(.data$Patient.1, .data$Patient.2, all_of(loc), .data$Department, .data$Patient.1.End, .data$Patient.2.Start, .data$Difference_days) %>%
      filter(.data$Difference_days > 0) %>%
      arrange(.data$Difference_days) %>%
      mutate(Patient.2.Start = as.character(.data$Patient.2.Start), Patient.1.End = as.character(.data$Patient.1.End))
  }

  return(location_dt)
}

