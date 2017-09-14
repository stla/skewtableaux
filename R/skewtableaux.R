.showSkewTableau <- function(skewtab){
  .C("showSkewTableauR", rlist=skewtab, l=length(skewtab),
     result="")$result
}
.dualSkewTableau <- function(skewtab){
  .C("dualSkewTableauR", rlist=skewtab, l=length(skewtab),
     result=list(0L))$result[[1L]]
}
.isValidSkewTableau <- function(skewtab){
  .C("isValidSkewTableauR", rlist=skewtab, l=length(skewtab),
     result=0L)$result
}
.skewTableauShape <- function(skewtab){ # skewtab must be valid
  .C("skewTableauShapeR", rlist=skewtab, l=length(skewtab),
     result=list(0L))$result[[1L]]
}

#' Dual skew tableau
#'
#' The dual of a skew tableau, that is, its mirror image to the main diagonal.
#'
#' @param skewtab an object of class \code{\link{skewtableau}} or that can be coerced to a \code{\link{skewtableau}}
#'
#' @return A skew tableau.
#' @export
#' @useDynLib SkewTableaux
#'
#' @examples
#' skewtab <- skewtableau(list(3,c(1,1)), list(1,c(2,3)))
#' dualSkewTableau(skewtab)
dualSkewTableau <- function(skewtab){
  if(!is.skewtableau(skewtab)){
    skewtab <- as.skewtableau(skewtab)
  }
  dskewtab <- .dualSkewTableau(skewtab)
  class(dskewtab) <- "skewtableau"
  dskewtab
}
