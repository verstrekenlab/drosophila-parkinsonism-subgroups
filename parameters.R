ID <- "0061"
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
DATA_DIR <- paste("/home/vibflysleep/FlySleepLab_Dropbox/Antonio/PVLab/Natalie/PD/drosophila-parkinsonism-subgroups/ID",ID, sep="")
result_dir <- file.path(DATA_DIR, "raw_data")
ethoscope_cache <- "/ethoscope_data/natalie_cache"
