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
    setkey(data, id)
    setkey(metadata, id)

    # set the right order

    # restore the behavr table
    setmeta(data, metadata)
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
    merged <- bind_behavr_list(data)
  } else {
    # the behavrs break when x and y have multikeys
    # but this restores the behavr objects
    merged <- do.call(rbind, data)
    setkey(merged, id)
    setmeta(merged, behavr::meta(x))
  }

  return(merged)
}

