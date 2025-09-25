## PD library in flies paper
################################
################################
# This file contains the functions needed to extract the sleep features used in Kaempf et al 2026
# The functions are organized in sections, which start with the header ## --



## - Parameters
start_day_experiment <- 1
stop_day_experiment <- 6
ethoscope_cache <- "/ethoscope_data/natalie_cache"
ROOT <- here::here()
time_window_length <- 10

## -- Ethoscope QC
#' Visualize data segregated by ethoscope
#' This is useful to spot potential batch effects driven by each ethoscope device
#' @import ggetho
#' @import ggplot2
ethoscope_QC <- function(dt, title, output_path){

  asleep <- NULL

  dt_bin <- behavr::rejoin(
    behavr::bin_apply_all(dt, y = "asleep", x_bin_length = behavr::mins(30), FUN = mean)
  )[, .(t, asleep, fly_no)]

  gg <- ggplot2::ggplot(data = dt_bin, mapping = ggplot2::aes(x = t, y = asleep, group = fly_no)) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations(height = 1, alpha = 0.3, outline = NA) +
    ggplot2::facet_grid(fly_no ~ .) +
    ggplot2::ggtitle(title)

  ggplot2::ggsave(
    filename = output_path,
    plot = gg
  )
}


## -- Merge behavr
#' Merge several behavr tables into a single dataframe
#'
#' This function merges two behavr tables,
#' represented by x and y, in such a way
#' that x gains all the columns in y not present in x
#' The merge is performed using the t column
#' Both tables should refer to the same individual
#' since this function is not id-aware
#' @param x first behavr table to merge
#' @param y second behavr table to merge
#' @importFrom dplyr full_join
#' @export
merge_behavr <- function(x, y, merge_meta=TRUE) {
  id <- NULL
  columns_x <- colnames(x)
  stopifnot("t" %in% colnames(x))
  stopifnot("t" %in% colnames(y))

  # merge the data
  data <- merge(x, y[, c("t", data.table::key(y), setdiff(colnames(y), columns_x)), with = F], by = c("t", data.table::key(y)))
  data <- data[, c("id", "t", setdiff(colnames(data), c("id", "t"))), with=F]

  if (merge_meta) {
    # merge the metadata
    meta_x <- x[, meta = T]
    meta_y <- y[, meta = T]
    metadata <- data.table::as.data.table(dplyr::full_join(meta_x, meta_y))

    # set keys
    data.table::setkey(data, id)
    data.table::setkey(metadata, id)

    # set the right order

    # restore the behavr table
    behavr::setmeta(data, metadata)
  }
  return(data)
}
#' @rdname merge_behavr
#' @details merge_behavr with multi individual tables
#' @export
merge_behavr_all <- function(x, y, merge_meta=TRUE) {

  id_ <- NULL

  x_id_column <- data.table::key(x)
  y_id_column <- data.table::key(y)
  x$id_ <- apply(MARGIN=1, X=x[, x_id_column, with=F], FUN = function(x) paste(x, collapse="-"))
  y$id_ <- apply(MARGIN=1, X=y[, y_id_column, with=F], FUN = function(x) paste(x, collapse="-"))

  unique_ids <- unique(x$id_[x$id_ %in% y$id_])

  data <- lapply(unique_ids, function(id_value) {
     res <- merge_behavr(
       x[id_ == id_value, ],
       y[id_ == id_value, ],
       merge_meta = merge_meta
     )
     res[, id_ := NULL]

  })

  if("behavr" %in% class(data[[1]])) {
    merged <- behavr::bind_behavr_list(data)
  } else {
    # the behavrs break when x and y have multikeys
    # but this restores the behavr objects
    merged <- do.call(rbind, data)
    data.table::setkey(merged, id)
    behavr::setmeta(merged, behavr::meta(x))
  }

  return(merged)
}



## -- Mean fraction sleep day night
analyse_mean_fraction_of_sleep_day_vs_night <- function(dt) {

  . <- asleep <- id <- phase <- NULL

  sleep_fractions <- dt[, .(sleep_fraction = mean(asleep)), by = .(phase, id)]
  sleep_fractions$asleep <- TRUE
  return(sleep_fractions)

}

## -- Sleep architecture
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


## -- Latency
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

## -- Velocity
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

## -- Anticipation
analyse_anticipation <- function(
  dt, morning=behavr::hours(c(6, 9, 12)), evening=behavr::hours(c(18, 21, 24))
){
  anticipation_period <- `:=` <- NULL

  dt[, anticipation_period := NA_character_]


  dt[, t_day := t %% behavr::days(1)]

  dt[
    t_day >= morning[1] & t_day < morning[2],
    anticipation_period := "morning_bl"
  ]
  dt[
    t_day >= morning[2] & t_day < morning[3],
    anticipation_period := "morning_sl"
  ]
  dt[
    t_day >= evening[1] & t_day < evening[2],
    anticipation_period := "evening_bl"
  ]
  dt[
    t_day >= evening[2] & t_day < evening[3],
    anticipation_period := "evening_sl"
  ]

  dt_anticipation <- dt[
    !is.na(anticipation_period),
    .(moving = sum(moving)),
    by = .(id, day, anticipation_period)
  ]

  anticipation_analysis <- NULL

  
  dt_anticipation <- data.table::dcast(
    dt_anticipation, id + day ~ anticipation_period,
    value.var = "moving"
  )


  dt_anticipation[, morning_anticipation := (morning_sl - morning_bl) / (morning_sl + morning_bl)]
  dt_anticipation[, evening_anticipation := (evening_sl - evening_bl) / (evening_sl + evening_bl)]

  anticipation_analysis <- dt_anticipation[,
    .(
      morning_anticipation = 100 * mean(morning_anticipation),
      evening_anticipation = 100 * mean(evening_anticipation)
    ),
    by = id
  ]

  return(anticipation_analysis)
}

log10x1000_inv <- function(x) { return(10 ^ (x / 1000))}



## -- Movement detector
#' Generic function to aggregate movement with some statistic
#' @param data  [data.table] containing behavioural variable from or one multiple animals.
#' When it has a key, unique values, are assumed to represent unique individuals (e.g. in a [behavr] table).
#' Otherwise, it analysis the data as coming from a single animal. `data` must have a column `t` representing time.
#' @param time_window_length number of seconds to be used by the motion classifier.
#' This corresponds to the sampling period of the output data.
#' @param func Aggregating function (max, min, median, mean, etc)
#' @param feature Name of a column in the sqlite3 file e.g. xy_dist_log10x1000
#' @param statistic Name of the column resulting from aggregation e.g. max_movement
#' @param score Name of the column providing a score i.e. category to the statistic e.g. micromovement
#' score is usually a binary variable i.e. TRUE/FALSE
#' @param preproc_FUN Optional, function to preprocess the input before computing the feature
#' (if the data needs some transformation like reverting xy_dist_log10x1000 back to a distance)
#' @param time_window_length Size of non overlapping time bins, in seconds
# @param threshold If the statistic is greater than this value, the score is TRUE, and 0 otherwise
#' @rdname custom_annotation_wrapper
#' @details movement_detector_enclosed takes:
#' \itemize{
#' \item{the name of an R summary function (mean, max, etc)}
#' \item{the name of a column in the future datasets to apply the function to}
#' \item{the name of the resulting summary column}
#' \item{the name of an alternative boolean column, which is set to TRUE if the summary column has a value greater than a threshold (default 1)}
#' \item{a preprocessing function to be applied to the column before the summary function is applied to it}
#' }
#' @example
#' max_movement_detector <- custom_annotation_wrapper(movement_detector_enclosed("max", "xy_dist_log10x1000", "max_movement", "micromovement", log10x1000_inv))
movement_detector_enclosed <- function(func, feature, statistic, score, preproc_FUN=NULL) {

  dt <- . <- NULL

  closure <- function(data, time_window_length=10, threshold=1) {

    message(paste0("Movement detector - ", func, " running.\ntime_window_length = ", time_window_length))
    func <- match.fun(func)
    # data$body_movement <- data$xy_dist_log10x1000
    d <- sleepr::prepare_data_for_motion_detector(data,
                                          c("t", feature, "x"),
                                          time_window_length,
                                          "has_interacted")

    d[, dt := c(NA, diff(t))]

    data.table::setnames(d, feature, "feature")
    # restore the distance from the log-transformed variable
    if (! is.null(preproc_FUN)) d[, feature := preproc_FUN(feature)]

    # Get a central summary value for variables of interest
    # for each window given by t_round
    # See prepare_data_for_motion_detector to learn
    # how is t_round computed
    # velocity_corrected -> max
    # has_interacted -> sum
    # beam_cross -> sum
    d_small <- d[, .(
      statistic = func(feature[1:.N])
    ), by = "t_round"]

    # Gist of the program!!
    # Score movement as TRUE/FALSE value for every window
    # Score is TRUE if max_velocity of the window is > 1
    # Score FALSE otherwise
    d_small[, score :=  ifelse(statistic > threshold, TRUE,FALSE)]
    data.table::setnames(d_small, "score", score)
    data.table::setnames(d_small, "statistic", statistic)

    # Set t_round as the representative time of the window
    # i.e. t becomes the begining of the window and not the t
    # of the first frame in the window
    return(d_small)
  }

  attr(closure, "needed_columns") <- function() {
    c("t", statistic, score)
  }
  attr(closure, "parameters") <- function() {
    return(names(formals(func)))
  }

  attr(closure, "variables") <- function() {
    statistic
  }

  return(closure)
}


#' Custom annotation from the dt_raw file
#'
#' This function gives aggregates a variable of interest in a custom way
#' All datapoints in every time_window_length seconds is aggregated into a single datapoint
#'
#' @param custom_function function used to produce the custom annotation
#' @param ... Extra arguments to be passed to `custom_function`.
#' @return a [behavr] table similar to `data` with additional variables/annotations.
#' The resulting data will only have one data point every `time_window_length` seconds.
#' @details
#' The default `time_window_length` is 300 seconds -- it is also known as the "5-minute rule".
#' custom_annotation_wrapper simplifies writing new annotation functions by leaving the shared functionality here
#' and the dedicated functionality to the new function.
#' This function adds to the functionality in the annotation function:
#' \itemize{
#' \item{Check a minimal amount of data is available and quit otherwise}
#' \item{Restore the name of the time column to remove the effects of binning}
#' \item{Check the amount of data after annotation is also enough (at least 1)}
#' \item{Apply a rolling interpolation of the labels to the data (assume the last available data point)}
#' }
#' It implements 3 attributes:
#' \itemize{
#' \item{needed_columns: A function that returns the columns needed by the function in its passed data}
#' \item{parameters: A function that returns the name of the parameters used by the function (including other functions' called by it)}
#' \item{variables: A function that returns the name of the newly produced columns by the function}
#' }
#' @export
custom_annotation_wrapper <- function(custom_function) {

  custom_annotation <- function(data,
                                time_window_length = 10, #s
                                ...
  ){
    moving = .N = is_interpolated  = .SD = asleep = NULL
    # all columns likely to be needed.
    columns_to_keep <- c("t", attr(custom_function, 'needed_columns')())


    wrapped <- function(d){
      if(nrow(d) < 100)
        return(NULL)
      # todo if t not unique, stop
      d_small <- custom_function(d, time_window_length, ...)
      data.table::setnames(d_small, "t_round", "t")

      if(data.table::key(d_small) != "t")
        stop("Key in output of motion_classifier_FUN MUST be `t'")

      if(nrow(d_small) < 1)
        return(NULL)
      # the times to  be queried
      time_map <- data.table::data.table(t = seq(from=d_small[1,t], to=d_small[.N,t], by=time_window_length),
                                         key = "t")
      missing_val <- time_map[!d_small]

      d_small <- d_small[time_map,roll=T]
      d_small[,is_interpolated := FALSE]
      d_small[missing_val,is_interpolated:=TRUE]
      d_small <- stats::na.omit(d[d_small,
                                  on=c("t"),
                                  roll=T])
      d_small <- d_small[, intersect(columns_to_keep, colnames(d_small)), with=FALSE]
      return(d_small)
    }

    if(is.null(data.table::key(data)))
      return(wrapped(data))
    data[,
         wrapped(.SD),
         by=data.table::key(data)]
  }

  attr(custom_annotation, "needed_columns") <- function() {attr(custom_function, 'needed_columns')()}
  attr(custom_annotation, "parameters") <- function() {
    args <- names(formals(custom_annotation))
    args <- c(args, attr(custom_function, "parameters")())
    args <- unique(args)
    args <- args[args != "..."]
    args <- args[args != "data"]
    return(args)
  }

  attr(custom_annotation, "variables") <- function() {
    attr(custom_function, "variables")()
  }

  return(custom_annotation)
}

sum_movement_detector <- custom_annotation_wrapper(movement_detector_enclosed("sum", "xy_dist_log10x1000", "sum_movement", "micromovement", log10x1000_inv))

analyse_ID_batch <- function(batch_id) {

  data_dir <- paste0(ROOT, "/ID", batch_id)
  dir.create(paste(data_dir, "/output", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/arousal", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/quality_control", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/freq_velocity_table", sep = ""), showWarnings = F)


  metadata <- data.table::fread(file.path(
    data_dir,
    paste("metadata_ID", batch_id, ".csv", sep = ""))
  )

  # clean empty rows
  metadata <- metadata[complete.cases(metadata), ]

  # make sure date is read as a character
  metadata$date <- as.character(metadata$date)
  metadata <- metadata[status == "OK", ][1:3, ]

  #4: Linking, link metadata with ethoscope
  metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = file.path(data_dir, "raw_data"))

  dt_sleep <- scopr::load_ethoscope(
    metadata,
    FUN = sleepr::sleep_annotation,
    reference_hour = NA,
    curate = FALSE,
    cache = ethoscope_cache,
    verbose = TRUE,
    velocity_correction_coef = 0.0042,
    min_time_immobile = 300,
    time_window_length = time_window_length
  )

  dt_movement <- scopr::load_ethoscope(
    metadata,
    FUN = sum_movement_detector,
    reference_hour = NA,
    curate = FALSE,
    cache = ethoscope_cache,
    verbose = TRUE,
    velocity_correction_coef = 0.0042,
    min_time_immobile = 300,
    time_window_length = time_window_length
  )

  dt <- merge_behavr_all(dt_sleep, dt_movement[, .(id, t, sum_movement)])


  dt_curated <- dt[t > behavr::days(start_day_experiment) & t < behavr::days(stop_day_experiment), ]

  # NOTE Wrapping in invisible()
  # because for some reason the data tables are getting printed
  invisible({
    dt_curated[, day := floor(t / behavr::days(1))]
    dt_curated[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]
    dt_bouts <- sleepr::bout_analysis(var = asleep, data = dt_curated)
    dt_bouts[, day := floor(t / behavr::days(1))]
    dt_bouts[, phase := ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D")]
    dt_bouts[, bout_id := seq_len(nrow(dt_bouts))]
  })

  ## Quality control per etoscope
  if (requireNamespace("ggetho", quietly = TRUE)) {
    ethoscope_codes <- unique(sapply(metadata$id, function(x) {
      substr(x, 21, 26)
    }))

    sapply(seq_along(ethoscope_codes), function(i) {
      ethoscope_ids <- unique(grep(
        pattern = ethoscope_codes[i], x = dt$id, value = TRUE
      ))

      output_plot <- file.path(
        file.path(data_dir, "output", "quality_control"),
        paste("ID", batch_id, "_graph_", ethoscope_codes[i], ".svg", sep = "")
      )

      ethoscope_QC(
        dt_curated[id %in% ethoscope_ids, ],
        title = ethoscope_codes[i],
        output_path = output_plot
      )
    })
  }
  #### -- Sleep fractions in D and L phases
  sleep_fractions <- analyse_mean_fraction_of_sleep_day_vs_night(dt_curated)
  sleep_fractions <- data.table::dcast(sleep_fractions, id ~ paste0("asleep_", phase), value.var = "sleep_fraction")


  #### -- Sleep architecture (bout count and duration)
  sleep_architecture <- analyse_sleep_architecture(dt_bouts)
  sleep_architecture <- sleep_architecture[phase == "D" & asleep == TRUE]
  sleep_architecture[, phase := NULL]
  sleep_architecture[, asleep := NULL]


  #### -- Latency to sleep
  sleep_latency <- analyse_latency(dt_bouts)
  sleep_latency <- sleep_latency[phase == "D" & asleep == TRUE]
  sleep_latency[, phase := NULL]
  sleep_latency[, asleep := NULL]

  #### -- Average velocity
  velocity_analysis <- analyse_velocity(dt_curated, time_window_length, by_phase=FALSE)
  velocity_analysis <- velocity_analysis[asleep == TRUE]
  velocity_analysis[, asleep := NULL]

  #### -- Anticipation analysis
  anticipation_analysis <- analyse_anticipation(dt_curated)


  output_dt <- list(sleep_fractions, sleep_architecture, sleep_latency, velocity_analysis, anticipation_analysis)


  output_dt <- Reduce(function(x, y) {
    merge(x, y, by = c("id"), all.x = TRUE, all.y = FALSE)
  }, output_dt)

  return(output_dt)
}
