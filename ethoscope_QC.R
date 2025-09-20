#' Visualize data segregated by ethoscope
#' This is useful to spot potential batch effects driven by each ethoscope device
#' @import ggetho
#' @import ggplot2
ethoscope_QC <- function(dt, title, output_path){

  asleep <- NULL

  gg <- ggetho::ggetho(dt, ggplot2::aes(y = asleep)) +
    ggetho::stat_pop_etho() +
    ggetho::stat_ld_annotations(height = 1, alpha = 0.3, outline = NA) +
    ggplot2::facet_grid(fly_no ~ .) +
    ggplot2::ggtitle(title)

  ggplot2::ggsave(
    filename = output_path,
    plot = gg
  )
}