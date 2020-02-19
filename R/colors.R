#' safe colors
#' @description color blindness safe colors
#' @export BLACK
#' @rdname safeColors
BLACK = "#000000"
#' @export ORANGE
#' @rdname safeColors
ORANGE = "#E69F00"
#' @export SKY_BLUE
#' @rdname safeColors
SKY_BLUE = "#56B4E9"
#' @export BLUISH_GREEN
#' @rdname safeColors
BLUISH_GREEN = "#009E73"
#' @export YELLOW
#' @rdname safeColors
YELLOW = "#F0E442"
#' @export BLUE
#' @rdname safeColors
BLUE = "#0072B2"
#' @export VERMILLION
#' @rdname safeColors
VERMILLION = "#D55E00"
#' @export REDDISH_PURPLE
#' @rdname safeColors
REDDISH_PURPLE = "#CC79A7"

#' @export safeColors
#' @rdname safeColors
#' @references
#' Wong B. Points of view: Avoiding color. Nat Methods. 2011 Jul;8(7):525. PubMed PMID: 21850730.
#' Wong B. Points of view: Color blindness. Nat Methods 8, 441 (2011). https://doi.org/10.1038/nmeth.1618
#' @examples 
#' safeColors
safeColors <- c(black=BLACK, orange=ORANGE, skyBlue=SKY_BLUE,
                bluishGreen=BLUISH_GREEN, yellow=YELLOW,
                blue=BLUE, vermillion=VERMILLION,
                reddishPurple=REDDISH_PURPLE)
#' available color variable
#' @description export available color names
#' @return a character vector contain safe colors.
#' @export
#' @examples 
#' colorNames()
colorNames <- function(){
  return(c("BLACK", "ORANGE", "SKY_BLUE", "BLUISH_GREEN",
           "YELLOW", "VERMILLION", "REDDISH_PURPLE"))
}
#' available colors
#' @description export available colors
#' @return a character vector contain safe colors.
#' @export
#' @examples 
#' availableColors()
#' 
availableColors <- function(){
  return(safeColors)
}
