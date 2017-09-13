checkSkewTableau <- function(rlist){
  test <- is.list(rlist)
  test <- test && all(purrr::map_lgl(rlist, is.list))
  test <- test && all(as.logical(lapply(rlist, function(row){
    all(purrr::map_lgl(row, function(x) is.numeric(x)))
  })))
  test <- test && all(as.integer(lapply(rlist, function(row){
    purrr::map_int(row[1L], length)
  })) == 1L)
  test <- test && all(as.integer(lapply(rlist, length)) == 2L)
  test
}

#' @name skewtableau
#' @rdname skewtableau
#' @title The \code{skewtableau} objects
#' @description Creates and print skewtableau objects
#'
#' @param ... a series of lists, each of the form \code{list(offset, entries)} where
#' \code{offset} is an integer number and \code{entries} is a vector of integer numbers.
#' @param x an object to coerce to a \code{skewtableau}
#'
#' @return The \code{skewtableau} function constructs a skew tableau by giving its list of rows.
#'
#' @examples
#' st <- skewtableau(list(1,c(2,3)), list(4,c(5,6)))
#' st
#' unclass(st)
NULL

#' @rdname skewtableau
#' @export
skewtableau <- function(...){
  if(!checkSkewTableau(rlist <- list(...))){
    stop("Invalid input.")
  }
  rlist <- rapply(rlist, as.integer, how="replace")
  rlist <- lapply(rlist, function(row) setNames(row, c("offset", "entries")))
  class(rlist) <- "skewtableau"
  rlist
}

#' @rdname skewtableau
#' @export
is.skewtableau <- function(x) inherits(x, "skewtableau")

#' @rdname skewtableau
#' @export
as.skewtableau <- function(x) UseMethod("as.skewtableau")

#' @rdname skewtableau
#' @export
as.skewtableau.skewtableau <- function(x) x

#' @rdname skewtableau
#' @export
as.skewtableau.list <- function(x){
  do.call(skewtableau, x)
}

#' @rdname skewtableau
#' @export
print.skewtableau <- function(x){
  print(.showSkewTableau(x))
}
