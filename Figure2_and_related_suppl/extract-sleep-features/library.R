## PD library in flies paper
################################
################################
# This file contains the functions needed to extract the sleep features used in Kaempf et al 2026
# The functions are organized in sections, which start with the header ## --

library(data.table)
library(behavr)
library(scopr)
library(sleepr)


## - Parameters
start_day_experiment <- 1
stop_day_experiment <- 5
id_columns <- c("id", "genotype")
time_window_length <- 10

## -- Ethoscope QC
#' Visualize data segregated by ethoscope
#' This is useful to spot potential batch effects driven by each ethoscope device
#' Requires ggplot2 and ggetho
#' @import ggetho
#' @import ggplot2
ethoscope_QC <- function(dt, title, output_path){

  asleep <- NULL
  if (!requireNamespace("ggetho", quietly = TRUE)) {
    warning("Please install ggplot2 and ggetho to get ethoscope QC plots")
    return(NULL)
  }


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

average_over_days <- function(data, variables, grouping, days = c(1, 2, 3, 4), weighted = FALSE) {
   
  weighted_mean <- function(x) {
    if (length(x)==4) {
      return((x[1]*25 + x[2]*13 + x[3]*7+x[4]*3)/48)
    }
  }

  if (weighted) {
    dt <- data[day %in% days, lapply(.SD, weighted_mean), by = grouping, .SDcols = variables]
  } else {
    dt <- data[day %in% days, lapply(.SD, mean), by = grouping, .SDcols = variables]
  }
  return(dt)
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

#' @import data.table
export_table <- function(dt, feature, batch_id, parameter_index, output_dir) {
  dt <- data.table::copy(dt)
  dt$export_table <- dt[[feature]] 
  write.csv(
    x = dt[, .(export_table)],
    file = file.path(
      output_dir,
      paste0("ID", batch_id, "_", parameter_index, "_", feature, ".csv")
    )
  )
}

## -- Mean fraction sleep day night
analyse_mean_fraction_of_sleep_day_vs_night <- function(dt) {

  . <- asleep <- id <- phase <- NULL

  sleep_fractions <- dt[, .(sleep_fraction = mean(asleep)), by = c("phase", id_columns)]
  sleep_fractions <- data.table::dcast(sleep_fractions, as.formula(paste0(paste(id_columns, collapse = " + "), " ~ phase")), value.var = "sleep_fraction")

  return(sleep_fractions)
}

## -- Sleep architecture
analyse_sleep_architecture <- function(dt_bouts, ...) {

    duration <- .N <- . <- NULL
    architecture <- average_over_days(
      dt_bouts[, .(
          bout_duration = mean(duration) / 60, # to mins
          bout_count = .N
        ),
        by = c(id_columns, "phase", "asleep", "day")
      ],
      variables = c("bout_duration", "bout_count"),
      grouping = c(id_columns, "phase", "asleep"),
      ...
    )
  
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
    latency = latency / 60, # to mins
    latency_to_longest_bout = latency_to_longest_bout / 60 # to mins
  )
  return(latency_analysis)
}

analyse_latency <- function(dt_bouts, ...) {
  
  latency_to_longest_bout <- `:=` <- . <- day <- .SD <- phase <- latency <- asleep <- NULL

  latency_analysis <- dt_bouts[, latency_descriptor(.SD), by = c("asleep", "phase", id_columns, "day")]

  latency_analysis <- average_over_days(
    latency_analysis,
    variables = c("latency", "latency_to_longest_bout"),
    grouping = c("asleep", "phase", id_columns),
    ...
  )

  return(latency_analysis)
}

## -- Velocity
#' Compute average max velocity for each fly throughout the experiment
analyse_velocity <- function(dt, by_phase = FALSE, roi_width = 0.055, deltaT = behavr::days(4)) {

  . <- sum_movement <- total_distance <- velocity <- .N <- `:=` <- NULL

  grouping_vars <- id_columns
  if (by_phase) grouping_vars <- c(grouping_vars, "phase")

  delta <- function(x) {
    return(max(x) - min(x))
  }


  distance_analysis <- dt[,
    .(
      total_distance = sum(sum_movement[2:.N] * roi_width) / deltaT
    ),
    by = grouping_vars
  ]

  velocity_analysis <- dt[
    moving == TRUE,
    .(velocity_if_awake = mean(max_velocity)),
    by = grouping_vars
  ]

  out <- merge(
    velocity_analysis, distance_analysis[, c(id_columns, "total_distance"), with = FALSE],
    by = id_columns, all.x=TRUE, all.y=TRUE
  )

  return(out)
}

## -- Anticipation
analyse_anticipation <- function(
  dt, morning = behavr::hours(c(18, 21, 24)), ...
){
  anticipation_period <- `:=` <- NULL

  dt[, anticipation_period := NA_character_]


  dt[, t_day := t %% behavr::days(1)]

  dt[
    t_day %between% c(morning[1], morning[2]),
    anticipation_period := "period_1"
  ]
  dt[
    t_day %between% c(morning[2], morning[3]),
    anticipation_period := "period_2"
  ]

  dt_anticipation <- dt[
    !is.na(anticipation_period),
    .(
      moving = sum(moving)
    ),
    by = c(id_columns, "day", "anticipation_period")
  ]
  anticipation_analysis <- NULL


  dt_anticipation <- data.table::dcast(
    dt_anticipation,
    as.formula(paste0(paste(c(id_columns, "day"), collapse = " + "), " ~ anticipation_period")),
    value.var = "moving"
  )

  dt_anticipation[, morning_anticipation := (period_2 - period_1) / (period_2 + period_1)]

  anticipation_analysis <- average_over_days(
    dt_anticipation,
    variables = c("morning_anticipation"),
    grouping = id_columns,
    ...
  )
  anticipation_analysis[, morning_anticipation := 100 * morning_anticipation]

  # NOTE: We made the mistake of dividing by 3 days instead of 4 in the original analysis
  # This line is placed here for consistency
  # This error is not critical because the morning anticipation of all flies is scaled by the same number (4/3)
  # anticipation_analysis[, morning_anticipation := 4/3 * morning_anticipation]
  
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

    message(paste0("Movement detector - running.\ntime_window_length = ", time_window_length))
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

    key <- data.table::key(d)
    d_small <- d[, .(
      statistic = func(.SD)
    ), by = "t_round"]
    data.table::setkeyv(d_small, key)

    # Gist of the program!!
    # Score movement as TRUE/FALSE value for every window
    # Score is TRUE if max_velocity of the window is > 1
    # Score FALSE otherwise
    d_small[, score :=  ifelse(statistic > threshold, TRUE, FALSE)]
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

    stopifnot(!is.null(data.table::key(data)))
    key<-data.table::key(data)


    data[,
      wrapped(.SD),
      by = eval(key)
    ]
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

analyse_ID_batch <- function(batch_id, root, ethoscope_cache = NULL, testing=FALSE, weighted = FALSE) {

  data_dir <- paste0(root, "/ID", batch_id)
  output_dir <- file.path(data_dir, "output_cv3")
  dir.create(output_dir, showWarnings = F)
  # dir.create(paste(data_dir, "/output/arousal", sep = ""), showWarnings = F)
  dir.create(file.path(output_dir, "quality_control"), showWarnings = F)
  # dir.create(file.path(output_dir, "freq_velocity_table"), showWarnings = F)
  
  # NOTE: Why keeping one hour before and one hour after the day limits?
  t_0 <- days(start_day_experiment)-3600
  t_last <- days(stop_day_experiment)+3600

  ### -- Load metadata
  
  # Read the metadata: a description of which flies to load and information about them (genotype, age, etc)
  metadata <- data.table::fread(file.path(
    data_dir,
    paste("metadata_ID", batch_id, ".csv", sep = ""))
  )

  # Make sure date is read as a character
  metadata$date <- as.character(metadata$date)

  # Curate data
  metadata <- metadata[complete.cases(metadata), ]
  metadata <- metadata[status == "OK", ]
  if (testing) metadata <- metadata[1:3, ]

  # Link ethoscope metadata to specific sqlite (dbfiles) in the ethoscope_database
  # This function finds which sqlite file contains the data of each fly in the metadata
  metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = file.path(data_dir, "raw_data"))

  #### -- Load ethoscope data using the curated metadata
  # Decide whether each fly is asleep or not every 10 seconds
  # Sleep = immobility for 300 seconds or more (5 mins)
  # Movement = the fly moves from one frame to next more than 0.0042 units of the ROI width
  # The fly is asleep for 10 seconds if it spent those 10 seconds and at least
  # the immediate 300 seconds prior, in an immobile state
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

  # Sum the distance travelled by each fly every 10 seconds
  dt_movement <- scopr::load_ethoscope(
    metadata,
    FUN = sum_movement_detector,
    reference_hour = NA,
    curate = FALSE,
    cache = ethoscope_cache,
    verbose = TRUE,
    time_window_length = time_window_length
  )

  # Combine both variables into a single data frame
  dt <- merge_behavr_all(dt_sleep, dt_movement[, .(id, t, sum_movement)])

  # Keep only from t_0 to t_last
  # TODO: Can this be moved to min_time and max_time in load_ethoscope?
  dt_curated <- behavr::rejoin(dt[t %between% c(t_0, t_last), ])

  deltaT <- t_last - t_0
  stopifnot(max(dt_curated$t)-min(dt_curated$t) == deltaT)
  deltaT <- deltaT / behavr::days(1) # to days
  
  ### -- Run bout analysis and annotate day and phase columns
  ### 1) Annotate day and phase
  ### 2) Run sleep/wake bouts analysis:
  ###  This annotates when the bouts start, whether it is sleep or wake,
  ###  and how long they last
  ### 3) Annotate day and phase on bout table, and also bout identifier

  # NOTE Wrapping in invisible()
  # because for some reason the data tables are getting printed
  # when creating the new columns, garbling the printed output
  invisible({
    dt_curated[, day := floor(t / behavr::days(1))]
    dt_curated[, phase := factor(ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D"), levels = c("L", "D"))]


    dt_bouts <- dt_curated[, 
      sleepr::bout_analysis(var = asleep, data = .SD),
      by = id_columns
    ]
    dt_bouts[, day := floor(t / behavr::days(1))]
    dt_bouts[, phase := factor(ifelse(t %% behavr::hours(24) < behavr::hours(12), "L", "D"), levels = c("L", "D"))]
    dt_bouts[, bout_id := seq_len(nrow(dt_bouts))]
  })

  ### -- Quality control per etoscope
  ethoscope_codes <- unique(sapply(metadata$id, function(x) {
    substr(x, 21, 26)
  }))

  sapply(seq_along(ethoscope_codes), function(i) {
    ethoscope_ids <- unique(grep(
      pattern = ethoscope_codes[i], x = dt$id, value = TRUE
    ))

    output_plot <- file.path(
      file.path(output_dir, "quality_control"),
      paste("ID", batch_id, "_graph_", ethoscope_codes[i], ".svg", sep = "")
    )

    ethoscope_QC(
      dt_curated[id %in% ethoscope_ids, ],
      title = ethoscope_codes[i],
      output_path = output_plot
    )
  })

  #### -- Sleep fractions in D and L phases
  sleep_fractions <- analyse_mean_fraction_of_sleep_day_vs_night(dt_curated)
  setnames(sleep_fractions, "L", "sleep_fraction_day")
  setnames(sleep_fractions, "D", "sleep_fraction_night")

  #### -- Sleep architecture (bout count and duration)
  sleep_architecture <- analyse_sleep_architecture(dt_bouts, weighted = weighted)
  sleep_architecture <- sleep_architecture[phase == "D" & asleep == TRUE]
  sleep_architecture[, phase := NULL]
  sleep_architecture[, asleep := NULL]
  setnames(sleep_architecture, "bout_duration", "mean_bout_length_night")
  setnames(sleep_architecture, "bout_count", "n_bouts_night")
  
  #### -- Latency to sleep
  sleep_latency <- analyse_latency(dt_bouts, weighted = weighted)
  sleep_latency <- sleep_latency[phase == "D" & asleep == TRUE]
  sleep_latency[, phase := NULL]
  sleep_latency[, asleep := NULL]
  sleep_latency[, latency := NULL]
  setnames(sleep_latency, "latency_to_longest_bout", "latency_to_longest_bout_night")

  #### -- Average velocity
  velocity_analysis <- analyse_velocity(dt_curated, by_phase = FALSE, deltaT = deltaT)

  #### -- Anticipation analysis
  anticipation_analysis <- analyse_anticipation(dt_curated[day >= start_day_experiment], weighted = weighted)
  anticipation_analysis <- anticipation_analysis[, c(id_columns, "morning_anticipation"), with = FALSE]
 
  ### -- Put it all together
  output_dt <- list(sleep_fractions, sleep_latency, sleep_architecture, anticipation_analysis, velocity_analysis)
  output_dt <- Reduce(function(x, y) {
    merge(x, y, by = id_columns, all.x = TRUE, all.y = FALSE)
  }, output_dt)

  ### -- Export
  export_table(output_dt, "sleep_fraction_night", batch_id, "A2", output_dir = output_dir)
  export_table(output_dt, "latency_to_longest_bout_night", batch_id, "A4", output_dir = output_dir)
  export_table(output_dt, "mean_bout_length_night", batch_id, "A4", output_dir = output_dir)
  export_table(output_dt, "n_bouts_night", batch_id, "A4", output_dir = output_dir)
  export_table(output_dt, "morning_anticipation", batch_id, "A9", output_dir = output_dir)
  export_table(output_dt, "velocity_if_awake", batch_id, "B1", output_dir = output_dir)
  export_table(output_dt, "total_distance", batch_id, "B4", output_dir = output_dir)
  data.table::fwrite(x = output_dt, file = file.path(output_dir, paste("ID", batch_id, ".csv", sep = "")))

  return(output_dt)
}

