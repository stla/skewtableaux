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
  attr(dskewtab, "valid") <- attr(skewtab, "valid")
  class(dskewtab) <- "skewtableau"
  dskewtab
}

#' Shape of a skew tableau
#'
#' Returns the shape of a skew tableau.
#'
#' @param skewtab a (valid) skewtableau (object having class \code{\link{skewtableau}} or
#' that can be coerced to such an object)
#'
#' @return A skew partition (class \code{\link{skewpartition}}).
#' @export
#'
#' @examples
#' skewtab <- skewtableau(list(3,c(1,1)), list(1,c(2,3)))
#' skewTableauShape(skewtab)
skewTableauShape <- function(skewtab){
  if(!is.skewtableau(skewtab)){
    skewtab <- as.skewtableau(skewtab)
  }
  if(attr(skewtab, "valid")){
    skewpart <- .skewTableauShape(skewtab)
    names(skewpart) <- c("outer", "inner")
    class(skewpart) <- "skewpartition"
    return(skewpart)
  }else{
    stop("Invalid skew tableau.")
  }
}
