#' Compute average max velocity for each fly throughout the experiment
analyse_mean_velocity <- function(dt, output_dt) {

  . <- moving <- max_velocity <- id <- NULL
  B1_dt <- dt[moving == TRUE, .(mean_velocity_if_awake = mean(max_velocity)), by = id]

  output_dt$B1_velocity_if_awake <- B1_dt$mean_velocity_if_awake
  return(output_dt)
}

#' @import data.table
#' @importFrom behavr days
average_speed_total <- function(dt, output_dt) {

  . <- sum_movement <- .N <- id <- NULL

  deltaT <- (
    max(dt$t) - min(dt$t)
  ) / behavr::days(1)


  #each ROI 5.5 cm = 0.055 m; by day
  B4_dt <- dt[, .(
    B4_total_distance = sum(sum_movement[2:.N]) * 0.055 / deltaT
  ), by = id]


  output_dt <- output_dt$B4_total_distance <- B4_dt$B4_total_distance
  return(output_dt)
}