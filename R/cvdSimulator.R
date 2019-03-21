#' simulate color vision deficiency
#' @description Transformation of R colors by simulating color vision deficiencies.
#' @references 
#' Vienot, F., Brettel, H. and Mollon, J.D., 1999. Digital video colourmaps for 
#' checking the legibility of displays by dichromats. 
#' Color Research & Application. 24(4), pp.243-252.
#' @param col character. A vector of colors.
#' @param type Deficiency type, "protanope" or "deuteranope"
#' @details 
#' Here use Vienot's methods but not Gustavo's methods ( implemented in colorspace::simulate_cvd).
#' @importFrom colorspace desaturate
#' @export
#' @examples 
#' cvdSimulator(safeColors)
cvdSimulator <- function(col, type=c("deuteranope", "protanope", "desaturate")){
  type <- match.arg(type)
  if(type=="desaturate"){
    col <- desaturate(col, amount=1)
    if(is.matrix(col)) col <- rgb(t(col), maxColorValue = 255)
    return(col)
  }
  col <- relativePhotometricQuantities(col)
  col <- reduceColor(col, method=type)
  col <- RGB2LMS(col)
  col <- dichromatColor(col, method=type)
  col <- LMS2RGB(col)
  col <- RGB2rgb(col)
  return(col)
}
