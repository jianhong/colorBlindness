#' Show color-deficiency simulations of a plot
#' @description Plot the color-deficiency simulations for ggplot grob.
#' @param plot The grob to be plotted.
#' @param layout The sub-figure types. the choices are origin, deuteranope,
#' protanope, desaturate, and enhanced, enhanced.deuteranope, 
#' enhanced.protanope, enhanced.desaturate.
#' @importFrom ggplot2 last_plot
#' @importFrom grid editGrob grid.grabExpr unit gTree
#' @importFrom cowplot plot_to_gtable plot_grid
#' @importFrom methods is show
#' @importFrom graphics par
#' @importFrom gridGraphics grid.echo
#' @importFrom gtable gtable_col
#' @details 
#' This function is modified from <https://github.com/clauswilke/colorblindr>
#' @return 
#' An object of ggplot.
#' @export
#' @examples 
#' cvdPlot(displayColors(safeColors))
#' cvdPlot(displayColors(paletteMartin))

cvdPlot <- function(plot = last_plot(), 
                    layout=c("origin", "deuteranope", 
                             "protanope", "desaturate")){
  choices = c("origin"="none", "deuteranope"="deuteranope", 
              "protanope"="protanope", "desaturate"="desaturate",
              "enhanced"="safe", "enhanced.deuteranope"="enhanced.deuteranope", 
              "enhanced.protanope"="enhanced.protanope", 
              "enhanced.desaturate"="enhanced.desaturate")
  labels = c("none"="normal vision", "deuteranope"="deuteranopia (6%)", 
             "protanope"="protanopia (2%)", "desaturate"="desaturated (BW)",
             "safe"="enhanced vision", "enhanced.deuteranope"="enhanced deut", 
             "enhanced.protanope"="enhanced prot", 
             "enhanced.desaturate"="enhanced desat")
  layout <- match.arg(layout, 
                      choices = names(choices), 
                      several.ok = TRUE)
  layout <- choices[layout]
  
  expr <- substitute(plot)
  chk <- function(){
    #if(is.call(expr)) {#try to avoid to run the call, 
    # however, it can not be done for pheatmap, save for later
    #  return(TRUE)
    #}
    res <- !inherits(plot, c("gg", "grob", "gList"))
    return(res)
  }
  if(chk()){#convert to grid
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
                           message("Cannot handle this plot. Error message:", 
                                   e)
                         })
      }
    }
    sink()
    unlink(tmpfile)
    if(is.null(plot)){
      stop("Cannnot handle this plot")
    }
  }
  if(is(plot, "gList")){
    u <- unit(1, "null")
    plot <- gtable_col(NULL, list(gTree(children = plot)), u, u)
    plot$layout$clip <- "inherit"
  }
  if(!is(plot, "grob")) plot <- plot_to_gtable(plot)
  
  layout1 <- layout[!grepl("^enhanced", layout)]
  layout2 <- layout[grepl("^enhanced", layout)]
  if(length(layout1)>0){
    dat <- lapply(layout1, replaceColors, grob=plot)
  }else{
    dat <- list()
  }
  if(length(layout2)>0){
    grob <- replaceColors(plot, "safe")
    types <- sub("enhanced.", "", layout2)
    dat2 <- lapply(types, replaceColors, grob=grob)
    names(dat2) <- layout2
    dat <- c(dat, dat2)
  }
  dat$scale <- .9
  dat$labels <- labels[layout]
  do.call(plot_grid, dat)
}

