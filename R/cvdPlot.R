#' Show color-deficiency simulations of a plot
#' @description Plot the color-deficiency simulations for ggplot grob.
#' @param plot The grob to be plotted.
#' @importFrom ggplot2 last_plot
#' @importFrom grid editGrob grid.grabExpr
#' @importFrom cowplot plot_to_gtable plot_grid
#' @importFrom methods is show
#' @importFrom graphics par
#' @importFrom gridGraphics grid.echo
#' @details 
#' This function is modified from https://github.com/clauswilke/colorblindr
#' @return 
#' An object of ggplot.
#' @export
#' @examples 
#' cvdPlot(displayColors(safeColors))
#' cvdPlot(displayColors(paletteMartin))

cvdPlot <- function(plot = last_plot()){
  expr <- substitute(plot)
  chk <- function(){#try to avoid to run the call, however, it can not be done for pheatmap, save for later
    if(is.call(expr)) return(TRUE)
    return(!inherits(plot, c("gg", "grob")))
  }
  if(!inherits(plot, c("gg", "grob"))){#convert to grid
    pin <- par("pin")
    tmpfile <- tempfile()
    sink(tmpfile, type="output")
    plot <- tryCatch(grid.grabExpr(show(plot), 
                                   warn = 0,
                                   wrap = TRUE,
                                   width = pin[1], 
                                   height = pin[2]),
                     error = function(e){
                       message("Cannot handle this plot. Error message:", e)
                     })
    if(is.null(plot)){
      if(is.call(expr)){
        plot <- tryCatch(grid.grabExpr(grid.echo(function() eval(expr)),
                                       warn = 0,
                                       wrap = TRUE,
                                       width = pin[1], 
                                       height = pin[2]),
                         error = function(e){
                           message("Cannot handle this plot. Error message:", e)
                         })
      }
    }
    sink()
    unlink(tmpfile)
    if(is.null(plot)){
      stop("Cannnot handle this plot")
    }
  }
  
  if(!is(plot, "grob")) plot <- plot_to_gtable(plot)
  
  ori <- replaceColors(plot, "none")
  deu <- replaceColors(plot, "deuteranope")
  pro <- replaceColors(plot, "protanope")
  des <- replaceColors(plot, "desaturate")
  plot_grid(ori, deu, pro, des, scale = 0.9, 
            labels = c("normal vision", "deuteranopia (6%)", "protanopia (2%)", "desaturated (BW)"))
}

replaceColors <- function(grob, type){
  if(type=="none") return(grob)
  if(type=="safe" && is(grob, "text")){
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- "#000000"
    }
  }
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
