#' convert plot to grob
#' @description use grid.grabExpr or plot_to_gtable to convert plot to grob
#' @param plot plots
#' @importFrom grid editGrob grid.grabExpr
#' @importFrom cowplot plot_to_gtable plot_grid
#' @importFrom methods is show
#' @importFrom graphics par
#' @importFrom gridGraphics grid.echo
#' 
grobify <- function(plot){
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
  return(plot)
}
