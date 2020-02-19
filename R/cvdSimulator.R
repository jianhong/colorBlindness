#' simulate color vision deficiency
#' @description Transformation of R colors by simulating color vision deficiencies.
#' @references 
#' Vienot, F., Brettel, H. and Mollon, J.D., 1999. Digital video colourmaps for 
#' checking the legibility of displays by dichromats. 
#' Color Research & Application. 24(4), pp.243-252.
#' Sharma, G., Wu, W. and Dalal, E.N., 2005. The CIEDE2000 color-difference formula:
#'  Implementation notes, supplementary test data, and mathematical observations. 
#'  Color Research & Application 30.1, pp.21-30.
#' @param col character. A vector of colors.
#' @param type Deficiency type, "protanope" or "deuteranope"
#' @return colors.
#' @details 
#' Here use Vienot's methods but not Gustavo's methods ( implemented in colorspace::simulate_cvd).
#' @importFrom colorspace desaturate
#' @importFrom grDevices col2rgb
#' @export
#' @examples 
#' cvdSimulator(safeColors)
cvdSimulator <- function(col, type="deuteranope"){
  type <- match.arg(type, 
                    choices = c("deuteranope", "protanope", "desaturate", #public
                                "none", "safe")) #private
  switch(type,
         "none" = {
           #do nothing
         },
         "safe" = {
           # How to adjust the green-red and blue-yellow
           # move it to blue
           col1 <- col
           dim(col1) <- NULL
           col1 <- unique(col1)
           col2 <- closestColorLab2(col1)
           names(col2) <- col1
           col1 <- col2[col]
           dim(col1) <- dim(col)
           col <- col1
         },
         "desaturate"={
           col <- desaturate(col, amount=1)
           if(is.matrix(col)) col <- rgb(t(col), maxColorValue = 255)
         },
         {
           colcopy <- col
           col <- relativePhotometricQuantities(col)
           alpha <- col["alpha", ]
           col <- col[seq.int(3), , drop=FALSE]
           col <- reduceColor(col, method=type)
           col <- RGB2LMS(col)
           col <- dichromatColor(col, method=type)
           col <- LMS2RGB(col)
           col <- rbind(col, alpha=round(255*alpha))
           col <- RGB2rgb(col)
           col[is.na(colcopy)] <- NA
         })
  
  return(col)
}


