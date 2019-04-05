#' simulate color vision deficiency
#' @description Transformation of R colors by simulating color vision deficiencies.
#' @references 
#' Vienot, F., Brettel, H. and Mollon, J.D., 1999. Digital video colourmaps for 
#' checking the legibility of displays by dichromats. 
#' Color Research & Application. 24(4), pp.243-252.
#' Sharma, G., Wu, W. and Dalal, E.N., 2005. The CIEDE2000 color‐difference formula:
#'  Implementation notes, supplementary test data, and mathematical observations. 
#'  Color Research & Application 30.1, pp.21-30.
#' @param col character. A vector of colors.
#' @param type Deficiency type, "protanope" or "deuteranope"
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
           # Replacing red with magenta or green with turquoise.
           # Red=#FF0000, magenta=#FF00FF
           # Gree=#00FF00, turquoise=#40E0D0
           # basic idea is that, add same red to blue, add similar green to blue;
           # the safe color is safeColors.
           # step 1. get the nearest 2 colors from paletteMartin.
           # step 2. calculate the distance of the given color to the 2 nrearest colors
           # step 3. adjust lab between them
           closestColor <- closestColorCIE2000
           col1 <- col
           dim(col1) <- NULL
           col1 <- unique(col1)
           col2 <- closestColor(col1)
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
           col <- reduceColor(col, method=type)
           col <- RGB2LMS(col)
           col <- dichromatColor(col, method=type)
           col <- LMS2RGB(col)
           col <- RGB2rgb(col)
           col[is.na(colcopy)] <- NA
         })
  
  return(col)
}

col2lab <- function(x){
  convertColor(t(col2rgb(x)), from = "sRGB", 
               to="Lab", scale.in = 255)
}
lab2hex <- function(x){
  rgb(convertColor(x, from = "Lab", to="sRGB"))
}
colDistRGB <- function(.ele, sc, c){
  .ele <- sc[, .ele, drop=FALSE]
  rbar = (c["red", ] + .ele["red", ])/2
  dR = (c["red", ] - .ele["red", ])^2
  dG = (c["green", ] - .ele["green", ])^2
  dB = (c["blue", ] - .ele["blue", ])^2
  sqrt((2+rbar/256)*dR + 4*dG + (2+(255 - rbar)/256)*dB)
}
colDistCIE2000 <- function(.ele, sc, c){ #http://www.brucelindbloom.com/index.html?Math.html
  .ele <- sc[.ele, , drop=FALSE]
  L1 <- .ele[, "L"]
  L2 <- c[, "L"]
  a1 <- .ele[, "a.x"]
  a2 <- c[, "a.x"]
  b1 <- .ele[, "b"]
  b2 <- c[, "b"]
  Lbar_ <- (L1 + L2)/2
  C1 <- sqrt(a1^2 + b1^2)
  C2 <- sqrt(a2^2 + b2^2)
  Cbar <- (C1 + C2)/2
  G <- (1-sqrt(Cbar^7/(Cbar^7+25^7)))/2
  a1_ <- a1*(1+G)
  a2_ <- a2*(1+G)
  C1_ <- sqrt(a1_^2 + b1^2)
  C2_ <- sqrt(a2_^2 + b2^2)
  Cbar_ <- (C1_ + C2_)/2
  at1 <- atan2(b1, a1_)*180/pi
  h1_ <- ifelse(at1<0, at1+360, at1)
  at2 <- atan2(b2, a2_)*180/pi
  h2_ <- ifelse(at2<0, at2+360, at2)
  Hbar_ <- ifelse(abs(h1_-h2_)>180, (h1_+h2_+360)/2, (h1_+h2_)/2)
  T <- 1 - 0.17 * cos(Hbar_ - 30) + 0.24 * cos(2*Hbar_) +
    0.32 * cos(3*Hbar_ + 6) - 0.20 * cos(4*Hbar_ - 63)
  dh_ <- ifelse(abs(h2_ - h1_)<= 180, h2_ - h1_, 
                ifelse(abs(h2_ - h1_) > 180 & h2_ <= h1_,
                       h2_ - h1_ + 360, h2_ - h1_ - 360))
  dL_ <- L2 - L1
  dC_ <- C2_ - C1_
  dH_ <- 2*sqrt(C1_*C2_)*sin(dh_/2)
  SL <- 1 + 0.015 * (Lbar_ - 50)^2/sqrt(20+(Lbar_ - 50)^2)
  SC <- 1 + 0.045 * Cbar_
  SH <- 1 + 0.015 * Cbar_*T
  dtheta <- 30 * exp(-((Hbar_-275)/25)^2)
  RC <- 2*sqrt(Cbar_^7/(Cbar_^7 + 25^7))
  RT <- -RC*sin(2*dtheta)
  KL <- KC <- KH <- 1
  sqrt((dL_/KL/SL)^2 + (dC_/KC/SC)^2 + (dH_/KH/SH)^2 + RT*(dC_/KC/SC)*(dH_/KH/SH))
}
closestColorRGB <- function(col){
  if(all(is.na(col))) return(col)
  safeCol <- c(safeColors, "white"="#FFFFFF")
  sc <- col2rgb(safeCol)
  c <- col2rgb(col)
  d <- lapply(colnames(sc), colDistRGB, sc=sc, c=c)
  d <- do.call(cbind, d)
  colnames(d) <- colnames(sc)
  maxV <- max(d)
  d1 <- d + 10^(nchar(maxV))*seq.int(nrow(d))
  d1 <- order(t(d1))
  d <- t(d)[d1]
  d <- matrix(d, ncol = 9, byrow = TRUE)
  d1 <- matrix(d1, ncol = 9, byrow = TRUE)
  d1 <- d1 - length(safeCol)*(seq.int(nrow(d1))-1)
  da <- col2lab(safeCol[d1[, 1]])
  db <- col2lab(safeCol[d1[, 2]])
  f <- d[, 1]/(d[, 1]+d[, 2])
  newcol <- round(f*(db - da)) + da
  newcol <- lab2hex(newcol)
  dim(newcol) <- dim(col)
  newcol[is.na(col)] <- NA
  return(newcol)
}
closestColorCIE2000 <- function(col){
  if(all(is.na(col))) return(col)
  safeCol <- c(safeColors, "white"="#FFFFFF")
  sc <- col2lab(safeCol)
  c <- col2lab(col)
  d <- lapply(names(safeCol), colDistCIE2000, sc=sc, c=c)
  d <- do.call(cbind, d)
  colnames(d) <- names(safeCol)
  maxV <- max(d)
  d1 <- d + 10^(nchar(maxV))*seq.int(nrow(d))
  d1 <- order(t(d1))
  d <- t(d)[d1]
  d <- matrix(d, ncol = 9, byrow = TRUE)
  d1 <- matrix(d1, ncol = 9, byrow = TRUE)
  d1 <- d1 - length(safeCol)*(seq.int(nrow(d1))-1)
  da <- col2lab(safeCol[d1[, 1]])
  db <- col2lab(safeCol[d1[, 2]])
  f <- d[, 1]/(d[, 1]+d[, 2])
  newcol <- round(f*(db - da)) + da
  newcol <- lab2hex(newcol)
  dim(newcol) <- dim(col)
  newcol[is.na(col)] <- NA
  return(newcol)
}
