analyse_anticipation <- function(
  dt, morning=behavr::hours(c(6, 9, 12)), evening=behavr::hours(c(18, 21, 24))
){
  anticipation_period <- `:=` <- NULL

  dt[, anticipation_period := NA_character_]


  dt[, t_day := t %% behavr::days(1)]

  dt[
    t_day >= morning[1] & t_day < morning[2],
    anticipation_period := "morning_bl"
  ]
  dt[
    t_day >= morning[2] & t_day < morning[3],
    anticipation_period := "morning_sl"
  ]
  dt[
    t_day >= evening[1] & t_day < evening[2],
    anticipation_period := "evening_bl"
  ]
  dt[
    t_day >= evening[2] & t_day < evening[3],
    anticipation_period := "evening_sl"
  ]

  dt_anticipation <- dt[
    !is.na(anticipation_period),
    .(moving = sum(moving)),
    by = .(id, day, anticipation_period)
  ]

  anticipation_analysis <- NULL

  
  dt_anticipation <- dcast(
    dt_anticipation, id + day ~ anticipation_period,
    value.var = "moving"
  )


  dt_anticipation[, morning_anticipation := (morning_sl - morning_bl) / (morning_sl + morning_bl)]
  dt_anticipation[, evening_anticipation := (evening_sl - evening_bl) / (evening_sl + evening_bl)]

  anticipation_analysis <- dt_anticipation[,
    .(
      morning_anticipation = 100 * mean(morning_anticipation),
      evening_anticipation = 100 * mean(evening_anticipation)
    ),
    by = id
  ]

  return(anticipation_analysis)
}