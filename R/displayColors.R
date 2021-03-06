#' display colors
#' @description Display the given colors
#' @rdname displayColors
#' @param col color set to display
#' @param ... parameters could be used by \link[ggplot2:geom_raster]{geom_tile}.
#' @return an \link[ggplot2:ggplot]{ggplot} object
#' @importFrom ggplot2 ggplot geom_tile scale_fill_manual scale_x_continuous 
#' theme element_blank element_text aes_string coord_equal
#' @export
#' @examples
#' displayColors(safeColors)
#' displayColors(paletteMartin)

displayColors <- function(col, ...){
  d <- data.frame(x=seq_along(col), y=1, col=factor(col, levels = unique(col)))
  g <- ggplot(d, aes_string(x='x', y='y', fill='col')) + 
    geom_tile(...) + coord_equal() +
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

#' @export
#' @rdname displayColors
#' @param types the type of color vision deficiency.
#' @examples 
#' displayAllColors(safeColors, color="white")
#' displayAllColors(paletteMartin, color="white")
#' 
displayAllColors <- function(col, 
                             types=c("deuteranope", "protanope", "desaturate"),
                             ...){
  types <- types[types %in% c("deuteranope", "protanope", "desaturate")]
  cols <- lapply(types, cvdSimulator, col=col)
  cols <- c(list(original=col), cols)
  names(cols) <- c("normal vision", types)
  l <- unlist(cols, use.names = FALSE)
  d <- data.frame(x=rep(seq_along(col), length(cols)), 
                  y=factor(rep(names(cols), lengths(cols)), 
                           levels = rev(names(cols))), 
                  col=factor(l, levels=unique(l)))
  g <- ggplot(d, aes_string(x='x', y='y', fill='col')) + 
    geom_tile(...) + coord_equal() +
    scale_fill_manual(guide = FALSE, values = l) +
    scale_x_continuous(breaks=seq_along(col), labels = names(col)) + 
    theme(axis.line        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank(),
          axis.text.x = 
            element_text(family = "Helvetica", 
                         angle = 90, 
                         hjust = 1, 
                         vjust = .5),
          axis.text.y = element_text(family = "Helvetica"))
  return(g)
}
