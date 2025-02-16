#' Compute average max velocity for each fly throughout the experiment
analyse_mean_velocity <- function(dt, output_dt) {

  . <- moving <- max_velocity <- id <- NULL
  B1_dt <- dt[moving == TRUE, .(mean_velocity_if_awake = mean(max_velocity)), by = id]

  output_dt$B1_velocity_if_awake <- B1_dt$mean_velocity_if_awake
  return(output_dt)
}
