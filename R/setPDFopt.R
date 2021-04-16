#' Auxiliary funciton to set width of pdf for journals
#' @description Set the pdf width and height for journals.
#' @param width columns.
#' @param presets The pre-setting of width,height,family,font for pdf.
#' Available choices: 0.5col, 1col, 1.5col, 2col.
#' @export
#' @import grDevices
#' @details 
#' The family will be Helvetica.
#' The font will be 8.
#' The width and height will be same. 
#' 
#' @return 
#' A named list of all the defaults. 
#' If any arguments are supplied the return values are 
#' the old values and the result has the visibility flag turned off.
#' @examples 
#' op <- setPDFopt("1col")
#' @references 
#' <http://www.sciencemag.org/sites/default/files/Figure_prep_guide.pdf>
#' 
#' <https://images.nature.com/full/nature-assets/aj/artworkguidelines.pdf>
#'  
#' 
#' 
setPDFopt <- function(width=c("1col", "1.5col", "0.5col", "2col"),
                      presets=PRESETS$science){
  width <- match.arg(width)
  width <- presets[width]
  pdf.options(width=width, height=width,
              family="Helvetica", pointsize=8)
}

#' Pre-sets of width
#' @description Pre-sets of width for figures.
#' @export PRESETS
#' @rdname setPDFopt
#' @details 
#' science:
#' 0.5col=1.78 inches (4.52 cm.);
#' 1col=3.54 inches (9 cm.);
#' 1.5col=5 inches (12.7 cm.); 
#' 2col=7.25 inches (18.4 cm.). 
#' nature:
#' 0.5col=2.28 inches (5.8 cm.);
#' 1col=3.39 inches (8.6 cm.);
#' 1.5col=4.76 inches (12.1 cm.); 
#' 2col=7 inches (17.8 cm.). 
#' cell:
#' 0.5col=1.78 inches (4.52 cm.);
#' 1col=3.35 inches (8.5 cm.);
#' 1.5col=4.49 inches (11.4 cm.); 
#' 2col=6.85 inches (17.4 cm.). 
#' CA: A Cancer Journal for Clinicians
#' 0.5col=1.62 inches (4.1 cm.);
#' 1col=3.25 inches (8.25 cm.);
#' 1.5col=3.87 inches (9.8 cm.); 
#' 2col=6.75 inches (17.1 cm.). 
#' 
PRESETS <- list(
  science=c("0.5col"=1.78, "1col"=3.54, "1.5col"=5, "2col"=7.25),
  nature=c("0.5col"=2.28, "1col"=3.39, "1.5col"=4.76, "2col"=7),
  cell=c("0.5col"=1.78, "1col"=3.35, "1.5col"=4.49, "2col"=6.85),
  CA=c("0.5col"=1.62, "1col"=3.25, "1.5col"=3.87, "2col"=6.75)
)
