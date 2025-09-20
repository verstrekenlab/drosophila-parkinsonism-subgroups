
library(behavr)
library(scopr)
library(sleepr)
library(ggetho)



## - Parameters
ID <- "0061"
DATA_DIR <- paste("/home/vibflysleep/FlySleepLab_Dropbox/Antonio/PVLab/Natalie/PD/drosophila-parkinsonism-subgroups/ID",ID, sep="")

start_day_experiment <- 1
stop_day_experiment <- 5
a_print_graphs <- FALSE
a_print_quality_control <- TRUE
a_print_graphs_individual_arousal <- TRUE
arousal <- TRUE
a_calculate_velocity_after_awakening <- TRUE
a_remove_temp_dataTables <- TRUE
a_change_single_sleep_events <- FALSE
a_filter_values <- FALSE
result_dir <- file.path(DATA_DIR, "raw_data")
ethoscope_cache <- "/ethoscope_data/natalie_cache"

create_directory_structure <- function(data_dir) {
  # Instead use a local drive (not a network drive)
  dir.create(paste(data_dir, "/output", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/arousal", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/quality_control", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/freq_velocity_table", sep = ""), showWarnings = F)
}

get_t_after_sd <- function(meta_row) {
  as.numeric(
    as.Date(substr(meta_row$SD_end_datetime, 1, 10)) -
    as.Date(substr(meta_row$datetime, 1, 10))
  )
}

time_window_length <- 10
source("ethoscope_QC.R")
source("A1_mean_fraction_sleep.R")
source("A2_mean_fraction_sleep_day_night.R")
source("A3_sleep-architecture.R")
# source("A4_latency.R")
source("A5_beam_crosses.R")
source("B1_motor_parameters.R")
source("movement_detector.R")
create_directory_structure(DATA_DIR)

metadata <- data.table::fread(file.path(
  DATA_DIR,
  paste("metadata_ID", ID, ".csv", sep = ""))
)

# clean empty rows
metadata <- metadata[complete.cases(metadata), ]

# make sure date is read as a character
metadata$date <- as.character(metadata$date)
metadata <- metadata[status == "OK", ][1:3, ]

#4: Linking, link metadata with ethoscope
metadata <- scopr::link_ethoscope_metadata(metadata, result_dir = result_dir)

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

dt <- behavr::merge_behavr_all(dt_sleep, dt_movement[, .(id, t, sum_movement)])

dt_curated <- dt[t <= behavr::days(stop_day_experiment + 1), ]
dt_curated[, day := floor(t / behavr::days(1))]

## Quality control per etoscope
ethoscope_codes <- unique(sapply(metadata$id, function(x) {
  substr(x, 21, 26)
}))
output_folder <- file.path(DATA_DIR, "output", "quality_control")

lapply(seq_along(ethoscope_codes), function(i) {
  ethoscope_ids <- unique(grep(
    pattern = ethoscope_codes[i], x = dt$id, value = TRUE
  ))

  output_plot <- file.path(
    output_folder,
    paste("ID", ID, "_graph_", ethoscope_codes[i], ".svg", sep = "")
  )

  ethoscope_QC(
    dt_curated[id %in% ethoscope_ids, ],
    title = ethoscope_codes[i],
    output_path = output_plot
  )
})

output_folder <- file.path(DATA_DIR, "output")

####### sleep fraction analysis ######

#A2: mean fraction of sleep day vs night
sleep_fractions <- analyse_mean_fraction_of_sleep_day_vs_night(dt_curated)

#A3: Bout analysis and sleep architecture
## check difference to A4 mean duration!
sleep_architecture <- analyse_sleep_architecture(dt_curated)


# #A4 to calculate the "latency to sleep"
sleep_latency <- analyse_latency_to_sleep(
  dt_curated,
  start_day_experiment, stop_day_experiment
)

###motor performance
#A5: beam crosses in 24h
# output_dt <- analyse_beam_crosses(dt_curated, output_dt)

#A6 + A7 beam crosses per active minute
# output_dt <- analyse_activity(dt_curated, output_dt)

#A8 average velocity
print(head(dt_curated))
velocity_analysis <- analyse_velocity(dt_curated, time_window_length)


#A9 morning anticipation = (a-b)/(a+b),
# output_dt <- analyse_morning_anticipation(dt_curated, output_dt)
#  a = number of transitions in 6h to 9h
#  b = number of transitions in 3h to 6h

#A10 time of the peak
# add evening peak?
# output_dt <- analyse_peak(dt_curated, output_dt)


#B1: motor parameters
output_dt <- analyse_mean_velocity(dt_curated, output_dt)
output_dt <- average_speed_total(dt_curated, output_dt)
