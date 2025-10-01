source("library.R")
library(parallel)

if(!file.exists("paths.txt")) {
  stop("Please make a file called paths.txt with the format:
  root,cache
  path_to_root,path_to_cache")
}

paths <- read.table("paths.txt", header = TRUE, sep = ",")
ROOT <- as.character(paths$root)
ethoscope_cache <- as.character(paths$cache)

BATCHES <- gsub(x = list.files(ROOT, pattern="ID"), pattern ="ID", replacement = "")[1]
N_JOBS <- 1

out <- parallel::mclapply(
  BATCHES,
  function(batch_id) {
    sleep_features <- analyse_ID_batch(
      batch_id,
      root = ROOT,
      ethoscope_cache = ethoscope_cache,
      testing = FALSE,
      weighted = FALSE
    )
    message(sprintf(
      "Batch %s done (%d/%d)",
      batch_id,
      which(BATCHES == batch_id),
      length(BATCHES)
    ))
    sleep_features
  },
  mc.cores = N_JOBS
)

sleep_features <- do.call(rbind, out)
data.table::fwrite(file = "2026_kaempf_sleep_features.csv", x = sleep_features)
