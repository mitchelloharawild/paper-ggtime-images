library(grid)

# Colours shared by annotated figures
highlight <- "#FB8C58"
wrong <- "#980000"
right <- "#1B7837"

# Draw a grob over a panel (annotation_custom() isn't compatible with time scales)
GeomGrob <- ggproto("GeomGrob", Geom, draw_panel = function(data, panel_params, coord, grob) grob)
annotate_grob <- function(grob) {
  layer(
    stat = "identity", position = "identity", geom = GeomGrob,
    data = data.frame(grob = 1), mapping = aes(), inherit.aes = FALSE,
    show.legend = FALSE, params = list(grob = grob)
  )
}

# A tick or cross in the top left corner, with an explanation beside it
annotate_mark <- function(valid, label) {
  colour <- if (valid) right else wrong
  x <- function(mm) unit(0.9 + mm, "mm")
  y <- function(mm) unit(1, "npc") - unit(0.9 + mm, "mm")
  mark <- if (valid) {
    polylineGrob(x(c(0, 0.7, 1.9)), y(c(1, 1.7, 0.1)), gp = gpar(col = colour, lwd = 1.5))
  } else {
    polylineGrob(
      x(c(0, 1.6, 0, 1.6)), y(c(0.1, 1.7, 1.7, 0.1)), id = c(1, 1, 2, 2),
      gp = gpar(col = colour, lwd = 1.5)
    )
  }
  annotate_grob(grobTree(mark, textGrob(
    label, unit(3.8, "mm"), unit(1, "npc") - unit(0.9, "mm"), hjust = 0, vjust = 1,
    gp = gpar(col = colour, fontsize = 6.5)
  )))
}

# An arrow to an error, positioned as proportions of the panel
annotate_arrow <- function(from, to, curvature) {
  annotate_grob(curveGrob(
    from[1], from[2], to[1], to[2], default.units = "npc",
    curvature = curvature, square = FALSE, ncp = 8,
    arrow = arrow(length = unit(1.1, "mm"), type = "closed"),
    gp = gpar(col = wrong, fill = wrong, lwd = 0.9)
  ))
}

# Panels side by side, with gray lines that map the shared code to all panels, and
# each title and its code to one panel. Headers are drawn in blank panels, so they
# are aligned with the panels below them
header_layout <- function(shared, titles, codes, plots) {
  blank <- function(...) {
    ggplot() +
      annotate_grob(grobTree(...)) +
      coord_cartesian(clip = "off") +
      theme_void() +
      theme(plot.margin = margin(0, 4, 0, 4))
  }
  code <- function(label, y) {
    textGrob(
      label, 0, unit(y, "mm"), hjust = 0, vjust = 0.5,
      gp = gpar(fontfamily = "DejaVu Sans Mono", fontsize = 5.1, col = "grey20")
    )
  }
  line <- function(y) {
    segmentsGrob(0, unit(y, "mm"), 1, unit(y, "mm"), gp = gpar(col = "grey60", lwd = 1.5))
  }
  n <- length(plots)
  headers <- lapply(seq_len(n), function(i) {
    title <- textGrob(titles[i], 0, unit(7, "mm"), hjust = 0, gp = gpar(fontsize = 8.5, fontface = "bold"))
    blank(title, code(codes[i], 3.4), line(1.2))
  })
  plots <- lapply(seq_len(n), function(i) {
    plots[[i]] +
      labs(x = NULL) +
      (if (i > 1) labs(y = NULL)) +
      theme(
        axis.text = element_text(size = 6),
        axis.text.y = if (i > 1) element_blank(),
        axis.title.y = element_text(size = 7),
        axis.ticks.x = element_line(colour = "black", linewidth = 0.3),
        axis.ticks.length.x = unit(0.8, "mm"),
        panel.grid.major = element_line(linewidth = 0.3),
        plot.margin = margin(1, 4, 1, 4)
      )
  })
  areas <- c(
    list(area(1, 1, 1, n)),
    lapply(seq_len(n), function(i) area(2, i)),
    lapply(seq_len(n), function(i) area(3, i))
  )
  wrap_plots(
    c(list(blank(code(shared, 2.8), line(0.7))), headers, plots),
    design = do.call(c, areas),
    heights = unit(c(4.5, 9.5, 1), c("mm", "mm", "null"))
  )
}
