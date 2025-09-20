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

#TODO
# do we want the average latency to the longest bout across nights
# i.e. the mean of n numbers where n is the number of nights
# or do we look for the longest bout in the whole experiment and take its latency?

#TODO
# What do we do with the sleep-deprived flies?

analyse_latency_to_sleep <- function(dt, start_day_experiment) {
  
  asleep <- := <- NULL
  bout_dt <- sleepr::bout_analysis(asleep, dt_curated)[asleep == TRUE,]
  bout_dt[, asleep := NULL]

  metadata <- dt[, meta = TRUE]
  metadata <- merge(
    metadata,
    metadata[, .(first_day =  ifelse(optomotor == "YES", get_t_after_sd(.SD), start_day_experiment)), by = id],
    by = id,
    all = TRUE
  )
  data.table::setkey(metadata, id)
  behavr::setmeta(bout_dt, metadata)

  bout_dt[duration_mins := duration/60]
  bout_dt[, day := floor(t / behavr::days(1))]
  bout_dt[, phase_count := floor(t / behavr::days(.5))]
  bout_dt[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "day", "night")]

  temp_for_latency <- bout_dt[t >= behavr::days(xmv(first_day)), sleep_descriptor(.SD), by = .(id, day)]
  temp_for_latency_phases <- bout_dt[t >= behavr::days(xmv(first_day)), sleep_descriptor(.SD), by = .(id, phase_count)]
  temp_for_latency_phases[, phase := ifelse(phase_count %%2 == 0, "day", "night")]

  A4_latency <- temp_for_latency[, sleep_descriptor_2(.SD), by = id]
  A4_latency_day <- temp_for_latency_phases[, phase == "day", sleep_descriptor_2(.SD), by = id]
  A4_latency_night <- temp_for_latency_phases[, phase == "night", sleep_descriptor_2(.SD), by = id]

  output_dt <- list()
  output_dt$A4_sum_sleep_minutes <- A4_latency$sum_sleep_minutes
  output_dt$A4_n_bouts<-A4_latency$n_bouts
  output_dt$A4_mean_bout_length<-A4_latency$mean_bout_length
  output_dt$A4_length_longest_bout<-A4_latency$length_longest_bout
  output_dt$A4_sum_sleep_minutes_day<-A4_latency_day$sum_sleep_minutes
  output_dt$A4_sum_sleep_minutes_night<-A4_latency_night$sum_sleep_minutes_night
  output_dt$A4_n_bouts_night<-A4_latency_night$n_bouts
  output_dt$A4_mean_bout_length_night<-A4_latency_night$mean_bout_length
  output_dt$A4_length_longest_bout_night<-A4_latency_night$length_longest_bout
  output_dt$A4_latency<-A4_latency_night$latency
  output_dt$A4_latency_to_longest_bout_night<-A4_latency_night$latency_to_longest_bout_night
  output_dt <- as.data.table(output_dt)

  return(output_dt)

}