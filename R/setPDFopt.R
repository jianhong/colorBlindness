#' Auxiliary funciton to set width of pdf for journals
#' @description Set the pdf width and height for journals.
#' @param preSet The pre-setting of width,height,family,font for pdf.
#' Available choices: 1col, 1.5col, 2col.
#' @export
#' @import grDevices
#' @details 
#' The family will be Helvetica.
#' The font will be 8.
#' The width and height will be same. 
#' 1col=3.54 inches (9 cm.);
#' 1.5col=5 inches (12.7 cm.); 
#' 2col=7.25 inches (18.4 cm.). 
#' @examples 
#' setPDFopt("1col")
#' @references 
#' http://www.sciencemag.org/sites/default/files/Figure_prep_guide.pdf
#' 
setPDFopt <- function(preSet=c("1col", "1.5col", "2col")){
  preSet <- match.arg(preSet)
  preSet <- c("1col"=3.54, "1.5col"=5, "2col"=7.25)[preSet]
  pdf.options(width=preSet, height=preSet,
              family="Helvetica", fonts=8)
}
