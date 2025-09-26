source("library.R")
source("old_library.R")


## Put these at the top of your script or in .Rprofile
options(keep.source = TRUE)          # keep line/column source refs
options(show.error.locations = TRUE) # print file:line on errors
options(error = traceback)      # or options(error = recover) for interactive debugging


BATCHES <- c("0061")

# out <- list()
# for (batch_id in BATCHES) {
#     sleep_features <- analyse_ID_batch_old(batch_id, testing= TRUE)
#     out <- c(out, list(sleep_features))
# }

out <- list()
for (batch_id in BATCHES) {
    sleep_features <- analyse_ID_batch(batch_id, testing= TRUE)
    out <- c(out, list(sleep_features))
}

sleep_features <- do.call(rbind, out)
data.table::fwrite(file = "2026_kaempf_sleep_features.csv", x = sleep_features)
