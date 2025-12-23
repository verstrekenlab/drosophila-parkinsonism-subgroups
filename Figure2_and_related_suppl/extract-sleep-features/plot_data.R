# Generate plots using the data generated with analyse_plot_data.R

library(ggplot2)
library(patchwork)
library(data.table)
library(behavr)
library(behavr)
library(ggetho)
library(ggprism)
library(colorspace)

theme_plot <- ggprism::theme_prism() + theme(text = element_text(size = 12, unit(12, "pt")))
ggplot2::theme_set(theme_plot)


GENOTYPE_COLORS <- c(
  `Park1_21` = "#f81e1e",
  `Pink1[KO]_y;;` = "#f0820a",
  `iPla KO` = "#6b82f8",
  `Rab39 KO` = "#00b835",
  `;;pdf[KO]` = "#f8756b",
  w1118 = "black"

)

GENOTYPE_LABELS <- c(
  w1118           = expression(control),
  "Rab39 KO"      = expression(Rab39^{KO-WS/y}),
  "Pink1[KO]_y;;" = expression(Pink1^{KO-WS/y}),
  "Park1_21"      = expression(park^{1/Δ21}),
  "iPla KO"       = expression(iPLA2-VIA^{KO-WS}),
  ";;pdf[KO]"     = expression(pdf^{KO})
)

dt_bin <- data.table::fread("plot_data_summ.csv")
dt_bin[zt < behavr::hours(6), zt := zt + behavr::hours(24)]

ggs <- lapply(unique(dt_bin$metadata_file), function(m_f) {

  dt_plot <- dt_bin[metadata_file == m_f, ]


  limits <- unique(dt_plot$genotype)


  gg <- ggplot2::ggplot(
    data = dt_plot,
    mapping = ggplot2::aes(
      x = zt, y = asleep,
      ymin = asleep - sem,
      ymax = asleep + sem,
      color = genotype,
      fill = genotype
    )
  ) +
    # ggplot2::facet_grid(metadata_file ~ .) +
  ggetho::scale_x_hours(name = "ZT") +
  geom_line() +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = asleep - sem,
      ymax = asleep + sem,
      group = genotype
    ),
    color = NA,
    alpha = 0.3
  ) +
  scale_color_manual(
    name   = "Genotype",
    breaks = names(GENOTYPE_COLORS),
    values = GENOTYPE_COLORS,
    limits = limits,
    labels = GENOTYPE_LABELS[limits]
  ) +
  scale_fill_manual(
    name   = "Genotype",
    breaks = names(GENOTYPE_COLORS),
    values = GENOTYPE_COLORS,
    limits = limits,
    labels = GENOTYPE_LABELS[limits]
  ) +
  guides(
    # make ribbon swatch visible/pleasant in legend
    fill  = guide_legend(override.aes = list(alpha = 0.5)),
    # optional: tweak line thickness in legend
    color = guide_legend(override.aes = list(linewidth = 1.2))
  ) +
  ggetho::stat_ld_annotations() +
  ggplot2::labs(y = "Time asleep\nper 30 min bin (min)")
})

gg <- ggs[[1]] / ggs[[2]] / ggs[[3]]

suppressWarnings({
  ggsave(plot = gg, filename = "plot.svg", width = 100, height = 150, unit = "mm")
  ggsave(plot = gg, filename = "plot.png", width = 100, height = 150, unit = "mm")
})