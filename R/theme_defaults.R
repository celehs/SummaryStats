# These are global internal constants for ggplot styling
theme_global <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 18),
    axis.title = ggplot2::element_text(size = 18),
    plot.title = ggplot2::element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_textbox_simple(size = 16, lineheight = 1.3),
    legend.text = ggplot2::element_text(size = 16),
    legend.title = ggplot2::element_text(size = 18, face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

theme_bar <- ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 18),
    axis.title = ggplot2::element_text(size = 18),
    plot.title = ggplot2::element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_textbox_simple(size = 16, lineheight = 1.3),
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    legend.position = "none"
  )

set_palette <- ggplot2::scale_color_brewer(palette = "Set2")
set_fill_palette <- ggplot2::scale_fill_brewer(palette = "Set2")

ggplot2::update_geom_defaults("line", list(linewidth = 1))
ggplot2::update_geom_defaults("point", list(size = 3))
