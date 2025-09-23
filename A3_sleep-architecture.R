analyse_sleep_architecture <- function(dt_bouts) {

    duration <- asleep <- phase <- id <- .N <- . <- NULL
    architecture <- dt_bouts[, .(
        bout_duration = mean(duration),
        bout_count = .N
      ),
      by = .(id, phase, asleep)
    ]
    return(architecture)
}