#' replace the colors for plots
#' @description replace the colors of plots to meet 
#' the requirment of publication.
#' Replacing red with magenta or green with turquoise.
#' Replacing all the colored symbols in the legend.
#' @param plot The grob to be plotted.
#' @return an object of gtable.
#' @export
#' @examples 
#' replacePlotColor(displayColors(c("Red", "Green", "blue")))

replacePlotColor <- function(plot){
  expr <- substitute(plot)
  #try to avoid to run the call, 
  #however, it can not be done for pheatmap, save for later
  chk <- function(){
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
  
  plot <- replaceColors(plot, "safe")
  return(plot)
}