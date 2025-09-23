#' Describe sleep behavior of animals using 4 features:
#' 0. Latency (time from phase change to onset of first bout)
#' 1. Duration of longest bout
#' 2. Number of bouts
#' 3. Mean bout duration
#' 4. Total sleep time
sleep_descriptor <- function(dt) {
  
  .N <- duration_mins <- . <- NULL
  
  bout_summary <- dt[,.(
    latency = t[1] %% behavr::hours(12),
    length_longest_bout = max(duration_mins),
    n_bouts = .N,
    mean_bout_length = mean(duration_mins),
    sum_sleep_minutes = sum(duration_mins)
  )]
  return(bout_summary)
}

sleep_descriptor_2 <- function(dt) {
  length_longest_bout <- n_bouts <- duration_mins <- . <- NULL

  bout_summary <- dt[,.(
    length_longest_bout = mean(length_longest_bout),
    n_bouts = mean(n_bouts),
    mean_bout_length = mean(duration_mins),
    sum_sleep_minutes = mean(duration_mins)
  )]
  return(bout_summary)
}

latency_descriptor <- function(dt_bouts) {

  asleep <- duration <- NULL
  # compute latency
  latency <- dt_bouts[, t[1] %% behavr::hours(12)]
  latency_to_longest_bout <- dt_bouts[
    which.max(duration),
    t %% behavr::hours(12)
  ]


  # compute latency to longest bout
  latency_analysis <- data.table::data.table(
    latency = latency,
    latency_to_longest_bout = latency_to_longest_bout
  )
  return(latency_analysis)
}

#TODO
# do we want the average latency to the longest bout across nights
# i.e. the mean of n numbers where n is the number of nights
# or do we look for the longest bout in the whole experiment and take its latency?

#TODO
# What do we do with the sleep-deprived flies?

analyse_latency <- function(dt_bouts) {
  
  latency_to_longest_bout <- `:=` <- . <- day <- .SD <- id <- phase <- latency <- asleep <- NULL

  dt_bouts[, day := floor(t / behavr::hours(24))]
  latency_analysis <- dt_bouts[, latency_descriptor(.SD), by = .(id, day, asleep, phase)]
  
  latency_analysis <- latency_analysis[,
    .(
      latency = mean(latency),
      latency_to_longest_bout = mean(latency_to_longest_bout)
    ),
    by = .(asleep, phase, id)
  ]

  return(latency_analysis)
}