#' Visualize data segregated by ethoscope
#' This is useful to spot potential batch effects driven by each ethoscope device
#' @import ggetho
#' @import ggplot2
ethoscope_QC <- function(dt, title, output_path){

  asleep <- NULL

  dt_bin <- behavr::rejoin(
    behavr::bin_apply_all(dt, y = "asleep", x_bin_length = behavr::mins(30), FUN = mean)
  )[, .(t, asleep, fly_no)]

  gg <- ggplot(data = dt_bin, mapping = aes(x = t, y = asleep, group = fly_no)) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations(height = 1, alpha = 0.3, outline = NA) +
    ggplot2::facet_grid(fly_no ~ .) +
    ggplot2::ggtitle(title)

  ggplot2::ggsave(
    filename = output_path,
    plot = gg
  )
}