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
analyse_latency <- function(dt_bouts) {
  
  latency_to_longest_bout <- `:=` <- . <- day <- .SD <- id <- phase <- latency <- asleep <- NULL

  latency_analysis <- dt_bouts[, latency_descriptor(.SD), by = .(asleep, phase, id, day)]
  
  latency_analysis <- latency_analysis[,
    .(
      latency = mean(latency),                                # across days
      latency_to_longest_bout = mean(latency_to_longest_bout) # across days
    ),
    by = .(asleep, phase, id)
  ]

  return(latency_analysis)
}