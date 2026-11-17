# ---------------------------------------------------------------------------
# theme.R — one place for every colour, so the app, the cards and the
# ggplot charts always match. Palette: dark plum dashboard with pink/violet.
# ---------------------------------------------------------------------------
DSE <- list(
  bg       = "#1d1029",  # page
  panel    = "#2b1a3f",  # tiles and chart panels
  panel_hi = "#3a2556",  # raised / hover
  line     = "#4c3470",  # hairlines, grid
  text     = "#f3eaff",
  muted    = "#b9a6d3",
  pink     = "#e0559a",
  violet   = "#9b6bdf",
  blush    = "#f2b6d0",
  blue     = "#4a90d9",
  up       = "#7fe0a8",  # positive return
  down     = "#ff8fa8"   # negative return
)

TIER_COLORS <- c(
  Legendary = DSE$pink,
  Rare      = DSE$violet,
  Common    = DSE$blue,
  Bust      = DSE$blush
)

dse_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = DSE$bg, fg = DSE$text,
    primary = DSE$pink, secondary = DSE$violet,
    success = DSE$up, danger = DSE$down,
    base_font = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Space Grotesk", local = FALSE),
    "card-bg" = DSE$panel,
    "border-color" = DSE$line,
    "border-radius" = "0.75rem"
  )
}

# ggplot theme for charts that sit inside the dark tiles.
theme_dse <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = DSE$text),
      axis.text = ggplot2::element_text(colour = DSE$muted),
      panel.grid.major = ggplot2::element_line(colour = DSE$line, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.position = "none"
    )
}
