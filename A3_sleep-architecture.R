analyse_sleep_architecture <- function(dt_curated) {

    dt_bouts <- sleepr::bout_analysis(var = asleep, data = dt_curated)
    dt_bouts[, phase := ifelse(t %% behavr::hours(24) > behavr::hours(12), "D", "L")]

    architecture <- dt_bouts[, .(mean(duration), count = .N), by = .(id, phase, asleep)]
    out <- architecture[asleep == TRUE & phase == "D",]
    return(out)    
}