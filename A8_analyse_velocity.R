#' Compute average max velocity for each fly throughout the experiment
analyse_velocity <- function(dt, output_dt) {

  . <- moving <- max_velocity <- id <- .N <- NULL

  A8_velocity <- dt[,
    .(mean_velocity_if_awake = max_velocity * moving / .N),
    by = id
  ]
  output_dt$A8_velocity <- A8_velocity$mean_velocity_if_awake

  return(output_dt)
}
