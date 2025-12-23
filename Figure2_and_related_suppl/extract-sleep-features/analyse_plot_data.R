# Analyse the sleep trace of flies listed in metadata files under metadata/

library(data.table)
library(parallel)
library(behavr)
library(digest)

if(!file.exists("paths.txt")) {
  stop("Please make a file called paths.txt with the format:
  root,cache
  path_to_root,path_to_cache")
}
paths <- read.table("paths.txt", header = TRUE, sep = ",")
ROOT <- as.character(paths$root)
ethoscope_cache <- as.character(paths$cache)
x_bin_length_mins <- 30

columns <- c(
  "ID", "machine_name", "date", "region_id",
  "reference_hour", "genotype", "control_genotype", "status"
)

unique_fields <- c("region_id", "machine_name", "date")

load_data <- function(metadata, group) {

  . <- asleep <- id <- machine_name <- NULL
  region_id <- NULL
  row_id <- date <- duplicated_entry <- NULL

  stopifnot(length(unique(metadata$result_dir)) == 1)

  duplicated_table <- metadata[
    ,
    .(duplicated_entry = .N > 1),
    by = unique_fields
  ]
  metadata <- merge(
    metadata, duplicated_table,
    by = unique_fields
  )

  if (nrow(metadata[duplicated_entry == TRUE, ]) > 0) {
    data.table::fwrite(
      x = metadata[
        duplicated_entry == TRUE,
        unique_fields, with = FALSE
      ][order(date, machine_name, region_id), ],
      file = file.path("metadata", paste0(group, "_duplicated_entries.csv"))
    )
    metadata <- metadata[,
      .SD[1], by = unique_fields
    ][order(date, machine_name, region_id), ]
  }

  data.table::fwrite(
      x = metadata,
      file = file.path("metadata", paste0("metadata_", group, ".csv"))
    )

  number_of_flies <- nrow(metadata)
  metadata$row_id <- 1:number_of_flies
  metadata0 <- data.table::copy(metadata)

  suppressMessages(
    metadata <- scopr::link_ethoscope_metadata(
      x = metadata,
      result_dir = metadata$result_dir[1]
    )
  )

  if(nrow(metadata) != number_of_flies) {
    missing_rows <- metadata0$row_id[!(metadata0$row_id %in% metadata$row_id)]
    missing <- unique(metadata0[row_id %in% missing_rows,][, unique_fields, with = FALSE])
    print(paste0("### Flies missing in ", group, " ###"))
    print(missing)
  }

  dt <- scopr::load_ethoscope(
    metadata = metadata,
    FUN = sleepr::sleep_annotation,
    reference_hour = NA,
    curate = FALSE,
    cache = ethoscope_cache,
    verbose = TRUE,
    velocity_correction_coef = 0.0042,
    min_time_immobile = 300,
    time_window_length = 10
  )

  dt <- dt[t >= behavr::hours(6) & t < (behavr::days(5) + behavr::hours(6)), ]

  dt_bin <- behavr::bin_apply_all(
    data = dt, y = "asleep", FUN = mean,
    x_bin_length = behavr::mins(x_bin_length_mins)
  )
  dt_bin <- behavr::rejoin(
    dt_bin[, .(
      id,                                      # unique for each fly
      asleep = asleep * x_bin_length_mins,     # time spent asleep, in minutes
      t                                        # timestamp, every x_bin_length_mins mins
    )]
  )

  dt_bin[, file_info := sapply(file_info, function(x) x$path)]
  return(dt_bin)
}


main <- function() {

  status <- result_dir <- metadata_file <- ID <- group <- . <- NULL
  n <- asleep <- genotype <- std <- sem <- NULL

  metadatas <- list.files(
    path = "metadata",
    pattern = "^metadata.*csv",
    full.names = TRUE
  )
  metadatas <- grep(
    pattern = "ID0",
    invert = TRUE,
    x = metadatas,
    value = TRUE
  )
  print(metadatas)
  metadatas <- lapply(metadatas, function(path) {
    metadata <- data.table::fread(path)[, columns, with = FALSE]
    metadata <- metadata[status == "OK", ]
    metadata$metadata_file <- basename(path)
    return(metadata)
  })
  metadata <- do.call(rbind, metadatas)
  metadata <- metadata[,
    result_dir := file.path(ROOT, ID, "raw_data"),
    by = .(ID)
  ]
  data.table::fwrite(x = metadata, file = "metadata/all_metadata.csv")


  metadata[, group := paste(metadata_file, ID, sep = "_")]

  groups <- unique(metadata$group)
  n_jobs <- min(30, length(groups))
  message(paste0("Using ", n_jobs, " parallel processes to load the data"))


  dt_bins <- parallel::mclapply(
    groups,
    function(group_) {
      load_data(
        metadata[group == group_, ],
        group = group_
      )
    }, mc.cores = n_jobs
  )

  dt_bin <- do.call(rbind, dt_bins)
  setDT(dt_bin)
  data.table::fwrite(x = dt_bin, file = "plot_data.csv")

  dt_summ <- dt_bin[,
    .(
      n = .N, asleep = mean(asleep),
      std = sd(asleep),
      sem = sd(asleep) / sqrt(.N)
    ),
    by = .(metadata_file, genotype, t, day = floor(t / behavr::days(1)))
  ]

  dt_summ <- dt_summ[,
    .(
      n = mean(n), asleep = mean(asleep),
      std = mean(std), sem = mean(sem)
    ),
    by = .(metadata_file, genotype, zt = t %% behavr::days(1))
  ]
  data.table::fwrite(x = dt_summ, file = "plot_data_summ.csv")
}

main()