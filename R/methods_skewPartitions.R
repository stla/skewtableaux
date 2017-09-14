checkSkewPartition <- function(outer, inner){
  test1 <- is.numeric(outer)
  if(!test1){
    message("`outer` must be a numeric vector.")
    return(FALSE)
  }
  test2 <- is.numeric(inner)
  if(!test2){
    message("`inner` must be a numeric vector.")
    return(FALSE)
  }
  outer <- purrr::map_int(outer, as.integer)
  inner <- purrr::map_int(inner, as.integer)
  test <- .isSubPartitionOf(inner, outer)
  if(!test){
    message("`inner` is not a subpartition of `outer`.")
    return(FALSE)
  }
  TRUE
}

#' @name skewpartition
#' @rdname skewpartition
#' @title The \code{skewpartition} objects
#' @description Creates and print \code{skewpartition} objects
#'
#' @param outer,inner partitions, \code{inner} must be a subpartition of \code{outer}
#' @param x an object to coerce to a \code{skewpartition}
#'
#' @return The \code{skewpartition} function constructs a skew partition by giving its
#' outer and inner partitions.
#'
#' @examples
#' sp <- skewpartition(c(6,4,1), c(3,2))
#' sp
#' unclass(sp)
#' as.skewpartition(list(c(6,4,1), c(3,2)))
#' # a partition not given in increasing order is automatically sorted:
#' skewpartition(c(4,6,1), c(3,2))
#' # the nonpositive elements are automatically removed:
#' skewpartition(c(6,4,1), c(-4,3,2))
NULL

#' @rdname skewpartition
#' @export
skewpartition <- function(outer, inner){
  if(!checkSkewPartition(outer, inner)){
    stop("Invalid partitions.")
  }
  outer <- purrr::map_int(outer, as.integer)
  inner <- purrr::map_int(inner, as.integer)
  if(.isPartition(outer)==0L){
    warning("`outer` is not a valid partition - automatic fix.")
    outer <- .mkPartition(outer)
  }
  if(.isPartition(inner)==0L){
    warning("`inner` is not a valid partition - automatic fix.")
    inner <- .mkPartition(inner)
  }
  out <- list(outer=outer, inner=inner)
  class(out) <- "skewpartition"
  out
}

#' @rdname skewpartition
#' @export
is.skewpartition <- function(x) inherits(x, "skewpartition")

#' @rdname skewpartition
#' @export
as.skewpartition <- function(x) UseMethod("as.skewpartition")

#' @rdname skewpartition
#' @export
as.skewpartition.skewpartition <- function(x) x

#' @rdname skewpartition
#' @export
as.skewpartition.list <- function(x){
  if(length(x) != 2L){
    stop("`x` must be a list of two numeric vectors.")
  }
  skewpartition(x[[1L]], x[[2L]])
}

#' @rdname skewpartition
#' @export
print.skewpartition <- function(x){
  print(.showSkewPartition(x$outer, x$inner))
}
