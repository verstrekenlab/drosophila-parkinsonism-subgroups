#' Compute:
#'      * A5_sum_beam_crosses_mean: Number of beam crosses per day on average for every fly
#' 
analyse_beam_crosses <- function(dt, output_dt) {

  . <- id <- t <- beam_crosses <- day <- NULL

  A5_beam_cross <- dt[,
    .(beam_crosses = sum(beam_crosses, na.rm = TRUE)),
    by = .(id, day)
  ]
  A5_sum_beam_crosses_mean <- A5_beam_cross[
    ,
    .(sum_beam_crosses_mean = mean(beam_crosses)),
    by = id
  ]
  output_dt$A5_sum_beam_crosses_mean <- A5_sum_beam_crosses_mean$sum_beam_crosses_mean

  return(output_dt)
}