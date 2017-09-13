.onLoad <- function(libname, pkgname) {
  library.dynam("SkewTableaux", pkgname, libname, now=TRUE)
  .C("HsStart")
  invisible()
}

.onUnLoad <- function(libpath) {
  library.dynam.unload("SkewTableaux", libpath)
  invisible()
}
