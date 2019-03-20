
relativePhotometricQuantities <- function(col){
  return((col2rgb(col)/255)^2.2)
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
  col <- 255* (col^(1/2.2))
  return(rgb(col[1, ], col[2, ], col[3, ], maxColorValue = 255))
}


