#' Compute average max velocity for each fly throughout the experiment
analyse_velocity <- function(dt, time_window_length) {

  . <- sum_movement <- asleep <- total_distance <- total_distance <- n <- velocity <- id <- .N <- `:=` <- NULL

  velocity_analysis <- dt[,
    .(total_distance = sum(sum_movement), n = .N * time_window_length),
    by = .(asleep, phase, id)
  ]

  velocity_analysis[, velocity := total_distance / n]

  return(velocity_analysis)
}
