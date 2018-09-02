# Define my own color pallet

drsimonj_colors <- c(
  'red'        = "#d11141",
  'green'      = "#00b159",
  'blue'       = "#00aedb",
  'orange'     = "#f37735",
  'yellow'     = "#ffc425",
  'light grey' = "#cccccc",
  'dark grey'  = "#8c8c8c",
  'light blue' = "#03A9F4",
  'purple'     = "#9C27B0",
  'teal'       = "#64FFDA",
  'lime'       = "#CDDC39",
  'amber'      = "#FFC107",
  'pink'       = "#EC407A")

drsimonj_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (drsimonj_colors)
  drsimonj_colors[cols]
}

drsimonj_palettes <- list(
  'main'    = drsimonj_cols("blue", "green", "yellow"),
  'cool'    = drsimonj_cols("blue", "teal", "green"),
  'hotpink'     = drsimonj_cols("blue", "purple", "pink", "red"),
  'hot' = drsimonj_cols("yellow", "orange", "red"),
  'mixed'   = drsimonj_cols("blue", "green", "yellow", "orange", "red"),
  'grey'    = drsimonj_cols("light grey", "dark grey")
)

drsimonj_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- drsimonj_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_drsimonj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drsimonj_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_drsimonj <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- drsimonj_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("drsimonj_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# Ways of use:
# scale_color_drsimonj(discrete = FALSE, palette = "cool")
# scale_color_drsimonj()
# scale_fill_drsimonj(palette = "mixed", guide = "none")