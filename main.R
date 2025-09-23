
library(behavr)
library(scopr)
library(sleepr)
library(ggetho)



## - Parameters
ID <- "0061"
DATA_DIR <- paste("/home/vibflysleep/FlySleepLab_Dropbox/Antonio/PVLab/Natalie/PD/drosophila-parkinsonism-subgroups/ID",ID, sep="")
start_day_experiment <- 1
stop_day_experiment <- 6
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
source("merge_behavr.R")
source("A2_mean_fraction_sleep_day_night.R")
source("A3_sleep-architecture.R")
source("A4_latency.R")
source("A8_analyse_velocity.R")
source("A9_anticipation.R")
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
output_folder <- file.path(DATA_DIR, "output", "quality_control")
ethoscope_codes <- unique(sapply(metadata$id, function(x) {
  substr(x, 21, 26)
}))

sapply(seq_along(ethoscope_codes), function(i) {
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


#### -- Sleep fractions in D and L phases
sleep_fractions <- analyse_mean_fraction_of_sleep_day_vs_night(dt_curated)
sleep_fractions <- dcast(sleep_fractions, id ~ paste0("asleep_", phase), value.var = "sleep_fraction")


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

print(output_dt)