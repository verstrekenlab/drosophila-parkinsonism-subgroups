#' @import data.table
analyse_peak <- function(dt, output_dt, outlier_thr = 20) {

  . <- := <- .SD <- id <- day <- max_velocity <- NULL

  mean_max_velocity <- dt[
    t %% behavr::days(1) <= behavr::hours(1) |
      t %% behavr::days(1) > behavr::hours(23),
    .(mean_max_velocity = mean(max_velocity)),
    by = .(id, day)
  ]

  dt <- merge(
    dt,
    mean_max_velocity,
    by = c("id", "day")
  )

  A10_peak <- dt[max_velocity <= outlier_thr * mean_max_velocity, ]

  A10_peak <- A10_peak[
    ,
    .SD[which.max(max_velocity), ],
    by = .(id, day)
  ][, .(id, t, day)]

  A10_peak_mean <- A10_peak[,
    .(peak_time = mean(t)),
    by = id
  ]
  A10_peak_mean[,
    diff_to86400 := (behavr::days(1) - t) / 60
  ]

  output_dt$A10_peak_time <- A10_peak$peak_time
  output_dt$A10_diff_to_ZT0 <- A10_peak$diff_to86400

  return(output_dt)
}