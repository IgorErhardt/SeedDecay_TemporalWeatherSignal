# Consistent boxed panel letters for analysis and diagnostic graphics.
# Draw the box and letter separately so every box has the same dimensions
# without changing the original bold sans-serif letterforms.
panel_letter_box_side <- grid::unit(5.8, "mm")
panel_letter_inset <- grid::unit(1.3, "mm")

GeomPanelLetter <- ggplot2::ggproto(
  "GeomPanelLetter", ggplot2::Geom,
  required_aes = "label",
  default_aes = ggplot2::aes(),
  draw_key = ggplot2::draw_key_blank,
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    if (nrow(data) == 0L) return(grid::nullGrob())
    x_left <- panel_letter_inset
    y_top <- grid::unit(1, "npc") - panel_letter_inset
    grid::grobTree(
      grid::roundrectGrob(
        x = x_left, y = y_top,
        width = panel_letter_box_side, height = panel_letter_box_side,
        r = grid::unit(1.2, "mm"), just = c("left", "top"),
        gp = grid::gpar(fill = "#ECEFF1", col = "#20252B", lwd = 0.6)
      ),
      grid::textGrob(
        label = as.character(data$label[1]),
        x = x_left + panel_letter_box_side / 2,
        y = y_top - panel_letter_box_side / 2,
        gp = grid::gpar(col = "#20252B", fontfamily = "sans",
                        fontface = "bold", fontsize = 10.5)
      )
    )
  }
)

panel_letter_geom <- function(tags) {
  ggplot2::layer(
    data = tags, mapping = ggplot2::aes(label = label),
    stat = "identity", geom = GeomPanelLetter, position = "identity",
    inherit.aes = FALSE, show.legend = FALSE
  )
}

panel_letter_layer <- function(data, facet_column, labels = LETTERS) {
  groups <- unique(as.character(data[[facet_column]]))
  if (is.factor(data[[facet_column]])) {
    groups <- levels(data[[facet_column]])
  }
  stopifnot(length(groups) <= length(labels))
  tags <- data.frame(group = groups, label = labels[seq_along(groups)])
  names(tags)[1] <- facet_column
  if (is.factor(data[[facet_column]])) {
    tags[[facet_column]] <- factor(groups, levels = levels(data[[facet_column]]))
  }
  panel_letter_geom(tags)
}

single_panel_letter <- function(letter = "A") {
  panel_letter_geom(data.frame(label = letter))
}
