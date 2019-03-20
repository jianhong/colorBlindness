#' display colors
#' @description Display the given colors
#' @param col color set to display
#' @return NULL
#' @importFrom ggplot2 ggplot geom_raster scale_fill_manual scale_x_continuous theme element_blank element_text aes
#' @export
#' @examples
#' displayColors(safeColors)
#' displayColors(palette15)

displayColors <- function(col){
  d <- data.frame(x=seq_along(col), y=1, col=factor(col, levels = col))
  g <- ggplot(d, aes(x=x, y=y, fill=col)) + 
    geom_raster() + 
    scale_fill_manual(guide = FALSE, values = unname(col)) +
    scale_x_continuous(breaks=seq_along(col), labels = names(col)) + 
    theme(axis.text.y      = element_blank(),
          axis.line        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          axis.text.x = 
            element_text(family = "Helvetica", 
                         angle = 90, 
                         hjust = 1, 
                         vjust = .5))
  return(g)
}
