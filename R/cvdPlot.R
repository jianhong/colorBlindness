#' Show color-deficiency simulations of a plot
#' @description Plot the color-deficiency simulations for ggplot grob.
#' @param plot The grob to be plotted.
#' @importFrom ggplot2 last_plot
#' @importFrom grid editGrob
#' @importFrom cowplot plot_to_gtable plot_grid
#' @importFrom methods is
#' @details 
#' This function is modified from https://github.com/clauswilke/colorblindr
#' @examples 
#' colorBlindness:::cvdPlot(displayColors(safeColors))
#' colorBlindness:::cvdPlot(displayColors(paletteMartin))

cvdPlot <- function(plot = last_plot()){
  ori <- plot
  deu <- replaceColors(plot, "deuteranope")
  pro <- replaceColors(plot, "protanope")
  des <- replaceColors(plot, "desaturate")
  plot_grid(ori, deu, pro, des, scale = 0.9, 
            labels = c("normal vision", "deuteranopia (6%)", "protanopia (2%)", "desaturated (BW)"))
}

replaceColors <- function(grob, type){
  if(!is(grob, "grob")) grob <- plot_to_gtable(grob)
  if (!is.null(grob$gp)) {
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- cvdSimulator(grob$gp$col, type)
    }
    if (!is.null(grob$gp$fill)) {
      grob$gp$fill <- cvdSimulator(grob$gp$fill, type)
    }
  }
  
  if (length(grob$grobs)>0) {
    grob$grobs <- lapply(grob$grobs, replaceColors, type=type)
  }
  
  if (length(grob$children)>0) {
    grob$children <- lapply(grob$children, replaceColors, type=type)
  }
  
  if (is(grob, "rastergrob")) {
    r <- cvdSimulator(grob$raster, type=type)
    dim(r) <- dim(grob$raster)
    class(r) <- class(grob$raster)
    grob <- editGrob(grob, raster = r)
  }
  
  return(grob)
}
