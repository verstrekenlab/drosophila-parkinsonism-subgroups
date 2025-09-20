analyse_mean_fraction_of_sleep_day_vs_night <- function(dt) {

  . <- asleep <- id <- phase <- NULL

  summary_dt <- rejoin(
    dt[,
      .(
        A2_sleep_fraction_l = mean(asleep[phase == "L"]),
        A2_sleep_fraction_d = mean(asleep[phase == "D"])
      ),
      by = id
    ]
  )
  return(summary_dt)

}