replaceColors <- function(grob, type){
  if(type=="none") return(grob)
  if(type=="safe" && is(grob, "text")){
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- "#000000"
    }
  }
  if (!is.null(grob$gp)) {
    if (!is.null(grob$gp$col)) {
      grob$gp$col <- cvdSimulator(grob$gp$col, type)
    }
    if (!is.null(grob$gp$fill)) {
      grob$gp$fill <- cvdSimulator(grob$gp$fill, type)
    }
  }
  
  if (length(grob$grobs)>0) {
    grob$grobs <- lapply(grob$grobs, replaceColors, type=type)
  }
  
  if (length(grob$children)>0) {
    grob$children <- lapply(grob$children, replaceColors, type=type)
  }
  
  if (is(grob, "rastergrob")) {
    r <- cvdSimulator(grob$raster, type=type)
    dim(r) <- dim(grob$raster)
    class(r) <- class(grob$raster)
    grob <- editGrob(grob, raster = r)
  }
  
  return(grob)
}


relativePhotometricQuantities <- function(col){
  return((col2rgb(col, alpha = TRUE)/255)^2.2)
}

reduceColor <- function(col, method=c("protanope", "deuteranope")){
  method <- match.arg(method)
  return(switch(method,
                "protanope"=.992052*col+.003974,
                "deuteranope"=.957237*col+.0213814))
}

RGB2LMS <- function(col){
  return(matrix(c(
    17.8824, 43.5161, 4.11935,
    3.45565, 27.1554, 3.86714,
    .0299566, .184309, 1.46709
  ), nrow = 3, ncol = 3, byrow = TRUE) %*% col)
}

dichromatColor <- function(col, method=c("protanope", "deuteranope")){
  prot <- matrix(c(
    0, 2.02344, -2.52581,
    0, 1, 0,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  deut <- matrix(c(
    1, 0, 0,
    .494207, 0, 1.24827,
    0, 0, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  return(switch(method,
                "protanope"= prot %*% col,
                "deuteranope"= deut %*% col))
}

LMS2RGB <- function(col){
  return(matrix(c(
    .080944, -.130504, .116721,
    -.0102485, .0540194, -.113615,
    -.000365294, -.00412163, .693513
  ), nrow = 3, ncol = 3, byrow = TRUE) %*% col)
}

RGB2rgb <- function(col){
  if("alpha" %in% rownames(col)){
    alpha <- col["alpha", ]
  }else{
    alpha <- 255
  }
  col <- 255* (col[seq.int(3), , drop=FALSE]^(1/2.2))
  return(rgb(col[1, ], col[2, ], col[3, ], alpha = alpha, maxColorValue = 255))
}

XYZ2LMS <- function(XYZ){#nrow(XYZ) == 3
  stopifnot(nrow(XYZ)==3)
  return(matrix(c(
    0.7328, 0.4296, -0.1624,
    -0.7036, 1.6975, 0.0061,
    0.0030, 0.0136, 0.9834
  ), nrow = 3, ncol = 3, byrow = TRUE) %*% XYZ)
}

LMS2XYZ <- function(LMS){
  stopifnot(nrow(LMS)==3)
  return(matrix(c(
    1.096123821, -0.278869000, 0.1827452,
    0.454369042, 0.473533154, 0.0720978,
    -0.009627609, -0.005698031, 1.0153256
  ), nrow = 3, ncol = 3, byrow = TRUE) %*% LMS)
}

# Replacing red with magenta or green with turquoise.
# Red=#FF0000, magenta=#FF00FF
# Gree=#00FF00, turquoise=#40E0D0
# basic idea is that, add same red to blue, add similar green to blue;
# the safe color is safeColors.
# step 1. get the nearest 2 colors from paletteMartin.
# step 2. calculate the distance of the given color to the 2 nrearest colors
# step 3. adjust lab between them
col2lab <- function(x){
  col <- col2rgb(x, alpha = TRUE)
  cbind(convertColor(t(col[seq.int(3), , drop=FALSE]), from = "sRGB", 
                     to="Lab", scale.in = 255), alpha = col["alpha", ])
}
lab2hex <- function(x){
  col <- x[, seq.int(3)]
  alpha <- 255
  if(ncol(x)>3){
    alpha <- x[, "alpha"]
  }
  rgb(convertColor(col, from = "Lab", to="sRGB", scale.out = 255), 
      alpha = alpha, maxColorValue = 255)
}
colDistRGB <- function(.ele, sc, c){
  .ele <- sc[, .ele, drop=FALSE]
  rbar = (c["red", ] + .ele["red", ])/2
  dR = (c["red", ] - .ele["red", ])^2
  dG = (c["green", ] - .ele["green", ])^2
  dB = (c["blue", ] - .ele["blue", ])^2
  sqrt((2+rbar/256)*dR + 4*dG + (2+(255 - rbar)/256)*dB)
}
#http://www.brucelindbloom.com/index.html?Math.html
colDistCIE2000 <- function(.ele, sc, c){ 
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
  sqrt((dL_/KL/SL)^2 + (dC_/KC/SC)^2 + (dH_/KH/SH)^2 + 
         RT*(dC_/KC/SC)*(dH_/KH/SH))
}
safeCol <- c(safeColors,
             "white"="#FFFFFF")
closestColorRGB <- function(col){
  if(all(is.na(col))) return(col)
  sc <- col2rgb(safeCol, alpha = TRUE)
  c <- col2rgb(col, alpha = TRUE)
  d <- lapply(colnames(sc), colDistRGB, sc=sc, c=c)
  d <- do.call(cbind, d)
  colnames(d) <- colnames(sc)
  maxV <- max(d)
  d1 <- d + 10^(nchar(maxV))*seq.int(nrow(d))
  d1 <- order(t(d1))
  d <- t(d)[d1]
  d <- matrix(d, ncol = length(safeCol), byrow = TRUE)
  d1 <- matrix(d1, ncol = length(safeCol), byrow = TRUE)
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
  sc <- col2lab(safeCol)
  c <- col2lab(col)
  d <- lapply(names(safeCol), colDistCIE2000, sc=sc, c=c)
  d <- do.call(cbind, d)
  colnames(d) <- names(safeCol)
  maxV <- max(d)
  d1 <- d + 10^(nchar(maxV))*seq.int(nrow(d))
  d1 <- order(t(d1))
  d <- t(d)[d1]
  d <- matrix(d, ncol = length(safeCol), byrow = TRUE)
  d1 <- matrix(d1, ncol = length(safeCol), byrow = TRUE)
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

closestColorLab <- function(col){
  lab <- col2lab(col)
  lab.a <- lab[, 2]#lab[, "a"]
  lab.b <- lab[, "b"]
  lab[, "b"] <- lab[, "b"] + sign(lab.b) * lab.a
  lab[lab[, "b"]>127, "b"] <- 127
  lab[lab[, "b"]< -128, "b"] <- -128
  lab[, "L"] <- lab[, "L"] - sign(lab.b) * lab.a/2 + lab.b/4
  #lab[, "L"] <- lab[, "L"] + ifelse(lab.b>0, lab.b/4, 0)
  lab[lab[, "L"]<0, "L"] <- 0
  lab[lab[, "L"]>100, "L"] <- 100
  newcol <- lab2hex(lab)
  dim(newcol) <- dim(col)
  newcol[is.na(col)] <- NA
  return(newcol)
}

closestColorLab2 <- function(col){
  lab <- col2lab(col)
  lab.a <- lab[, 2]#lab[, "a"]
  lab.b <- lab[, "b"]
  ## red --> magenta=#FF00FF ## red lab.a>0 & lab.b>0, 
  ##                            increase a little L, decrase b
  ## green --> cyan ## green lab.a<0 & lab.b>0, 
  ##                  decrease b, increase L, increase a
  lab[, "L"] <- lab[, "L"] + ifelse(lab.a<0 & lab.b>0, abs(lab.a/2), 0)
  lab[, "b"] <- ifelse(lab.a<0 & lab.b>0, -lab.b, lab.b)
  lab[, "L"] <- lab[, "L"] + ifelse(lab.a>0 & lab.b>0, abs(lab.a/6), 0)
  lab[, "b"] <- ifelse(lab.a>0 & lab.b>0, lab[, "b"]+lab.a, lab[, "b"])
  #lab[, 2] <- ifelse(lab.a>0 & lab.b>0, lab.a/2, lab.a)
  lab[lab[, 2]>127, 2] <- 127
  lab[lab[, 2]< -128, 2] <- -128
  lab[lab[, "b"]>127, "b"] <- 127
  lab[lab[, "b"]< -128, "b"] <- -128
  lab[lab[, "L"]<0, "L"] <- 0
  lab[lab[, "L"]>100, "L"] <- 100
  newcol <- lab2hex(lab)
  dim(newcol) <- dim(col)
  newcol[is.na(col)] <- NA
  return(newcol)
}

closestColorRGB2 <- function(col){
  rgb <- col2rgb(col, alpha = TRUE)
  rgb["blue", ] <- rgb["blue", ] + rgb["red", ]/2 + rgb["green", ]/3
  rgb["red", ] <- rgb["red", ] + round(rgb["green", ]/2)
  rgb <- round(rgb)
  rgb[rgb>255] <- 255
  newcol <- rgb(t(rgb), maxColorValue=255)
  dim(newcol) <- dim(col)
  newcol[is.na(col)] <- NA
  return(newcol)
}