analyse_morning_anticipation <- function(
  dt, output_dt,
  morning=c(6, 9, 12), evening=c(18, 21, 24)
){
  anticipation_period <- := <- NULL

  dt[, anticipation_period := NA]

  dt[
    t %% behavr::days(1) > morning[1] && t %% behavr::days(1) <= morning[2],
    anticipation_period := "morning_bl"
  ]
  dt[
    t %% behavr::days(1) > morning[2] && t %% behavr::days(1) <= morning[3],
    anticipation_period := "morning_sl"
  ]
  dt[
    t %% behavr::days(1) > evening[1] && t %% behavr::days(1) <= evening[2],
    anticipation_period := "evening_bl"
  ]
  dt[
    t %% behavr::days(1) > evening[2] && t %% behavr::days(1) <= evening[3],
    anticipation_period := "evening_sl"
  ]

  dt[, anticipation_period := NULL]

  A9_anticipation <- dcast(
    dt[,
      .(moving = sum(moving))
      by = .(id, day, anticipation_period)
    ],
    id + day ~ anticipation_period, value.var = "moving"
  )

  A9_anticipation[,
    .(
      morning_anticipation := (morning_sl - morning_bl) / (morning_sl + morning_bl),
      evening_anticipation := (evening_sl - evening_bl) / (evening_sl + evening_bl)
    )
  ]

  A9_anticipation[
    .(
      A9_morning_anticipation := 100 * mean(morning_anticipation),
      A9_evening_anticipation := 100 * mean(evening_anticipation)
    ),
    by = id
  ]

  output_dt$A9_morning_anticipation <- A9_anticipation$A9_morning_anticipation
  output_dt$A9_evening_anticipation <- A9_anticipation$A9_evening_anticipation

  return(output_dt)
}