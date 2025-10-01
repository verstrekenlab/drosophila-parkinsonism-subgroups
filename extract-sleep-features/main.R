source("library.R")
# source("old_library.R")

library(parallel)


## Put these at the top of your script or in .Rprofile
options(keep.source = TRUE)          # keep line/column source refs
options(show.error.locations = TRUE) # print file:line on errors
options(error = traceback)      # or options(error = recover) for interactive debugging

ROOT <- "/media/vibflysleep/Elements/PVlab/Natalie/ethoscope_data/"
ethoscope_cache <- "/media/vibflysleep/Elements/PVlab/Natalie/ethoscope_data/natalie_cache/"

BATCHES <- gsub(x = list.files(ROOT, pattern="ID"), pattern ="ID", replacement = "")
N_JOBS <- 1

# out <- list()
# for (batch_id in BATCHES) {
#     sleep_features <- analyse_ID_batch_old(batch_id, testing= TRUE)
#     out <- c(out, list(sleep_features))
# }

# out <- list()
# count <- 0
# for (batch_id in BATCHES) {
#     sleep_features <- analyse_ID_batch(batch_id, root = ROOT, testing = FALSE, weighted = FALSE)
#     out <- c(out, list(sleep_features))
#     count <- count + 1
#     print(paste0("Batch ", batch_id, " done (", count, "/", length(BATCHES), ")"))
# }


out <- mclapply(
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
