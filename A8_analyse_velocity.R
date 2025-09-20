#' Compute average max velocity for each fly throughout the experiment
analyse_velocity <- function(dt, time_window_length) {

  . <- moving <- max_velocity <- id <- .N <- NULL

  velocity_analysis <- dt[,
    .(total_distance = sum(sum_movement), n = .N*time_window_length),
    by = .(asleep, id)
  ]

  velocity_analysis[, velocity := total_distance / n]

  return(velocity_analysis)
}
