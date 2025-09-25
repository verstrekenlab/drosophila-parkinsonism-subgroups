source("library.R")

BATCHES <- c("0061")

out <- list()
for (batch_id in BATCHES) {
    sleep_features <- analyse_ID_batch(batch_id, testing= TRUE)
    out <- c(out, list(sleep_features))
}

sleep_features <- do.call(rbind, out)
data.table::fwrite(file = "2026_kaempf_sleep_features.csv", x = sleep_features)
