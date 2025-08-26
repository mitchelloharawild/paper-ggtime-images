(((fct_order_cartesian / chr_order_cartesian)) |
(fct_order_polar / chr_order_polar) + plot_layout(widths = c(4, 4, 1,1)) & labs(x = NULL))

wrap_plots(
  A = fct_order_facet / chr_order_facet,
  B = fct_order_cartesian, C = chr_order_cartesian,
  D = fct_order_polar, E = chr_order_polar,
  ncol = 2, nrow = 3,
  widths = c(4, 1), heights = c(1, 4, 4),
  design = "AA\nBD\nCE"
)
