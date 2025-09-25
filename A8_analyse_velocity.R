#' Compute average max velocity for each fly throughout the experiment
analyse_velocity <- function(dt, time_window_length, by_phase=FALSE) {

  . <- sum_movement <- total_distance <- total_distance <- velocity <- seconds_passed <- .N <- `:=` <- NULL

  grouping_vars = c("asleep", "id")
  if (by_phase) grouping_vars <- c(grouping_vars, "phase")

  velocity_analysis <- dt[,
    .(
      total_distance = sum(sum_movement),
      seconds_passed = .N * time_window_length
    ),
    by = grouping_vars
  ]

  velocity_analysis[, velocity := total_distance / seconds_passed]
  velocity_analysis[, seconds_passed := NULL]

  return(velocity_analysis)
}
