#' Computes:
#'     * A6_sum_active_minutes_mean: Number of minutes in the activate state in the average day for every fly
#'     * A7_beam_crosses_per_active_minute_mean: Average number of beam crosses per active minute
#' 
#' Assumes time_window_length=10 so that count divided by 6 is number of mins
analyse_activity <- function(dt, output_dt) {

  . <- moving <- id <- day <- sum_active_minutes_day <- NULL

  A6_active_minute <- dt[,
    .(sum_active_minutes_day = sum(moving) / 6),
    by = .(id, day)
  ]

  A6_active_minute_mean <- A6_active_minute[,
    .(A6_sum_active_minutes = mean(sum_active_minutes_day)),
    by = id
  ]

  output_dt$A6_sum_active_minutes_mean <- A6_active_minute_mean$A6_sum_active_minutes
  output_dt[, A7_beam_crosses_per_active_minute_mean := A5_sum_beam_crosses_mean / A6_sum_active_minutes_mean]

  return(output_dt)
}