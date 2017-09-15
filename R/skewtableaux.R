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
.semiStandardSkewTableaux <- function(n, outer, inner){
  .C("semiStandardSkewTableauxR", n=n, outer=list(outer), inner=list(inner),
     result=list(0L))$result[[1L]]
}
.asciiSkewTableau <- function(skewtab){
  .C("asciiSkewTableauR", rlist=skewtab, l=length(skewtab), result="")$result
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

#' Semistandard skew tableaux
#'
#' List of semistandard skew tableaux of a given shape and filled with integers in \code{1:n}.
#'
#' @param n integer
#' @param skewpart a skewpartition (\code{\link{skewpartition}})
#'
#' @return A list of skew tableaux.
#' @export
#'
#' @examples
#' skewpart <- skewpartition(c(3,2), c(1,1))
#' semiStandardSkewTableaux(3, skewpart)
semiStandardSkewTableaux <- function(n, skewpart){ # quid si skewpart invalid ?
  if(!is.skewpartition(skewpart)){
    skewpart <- as.skewpartition(skewpart)
  }
  if(.isValidSkewPartition(skewpart)==0L){
    stop("`skewpart` must be a valid skew partition.")
  }
  out <- .semiStandardSkewTableaux(as.integer(n), skewpart$outer, skewpart$inner)
  out <- lapply(out, function(skewtab){
    class(skewtab) <- "skewtableau"
    attr(skewtab, "valid") <- TRUE
    skewtab
  })
  out
}

#' ASCII representation of a skew tableau
#'
#' ASCII representation of a skew tableau.
#'
#' @param skewtab an object of class \code{\link{skewtableau}} or that can be coerced to a \code{\link{skewtableau}}
#'
#' @return A character string.
#' @export
#'
#' @examples
#' skewtab <- skewtableau(list(3,c(1,1)), list(1,c(2,3)))
#' cat(asciiSkewTableau(skewtab))
asciiSkewTableau <- function(skewtab){
  if(!is.skewtableau(skewtab)){
    skewtab <- as.skewtableau(skewtab)
  }
  .asciiSkewTableau(skewtab)
}
