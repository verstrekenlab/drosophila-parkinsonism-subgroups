analyse_mean_fraction_of_sleep_day_vs_night <- function(dt) {

  . <- asleep <- id <- phase <- NULL

  sleep_fractions <- dt[, .(sleep_fraction = mean(asleep)), by = .(phase, id)]
  sleep_fractions$asleep <- TRUE
  return(sleep_fractions)

}