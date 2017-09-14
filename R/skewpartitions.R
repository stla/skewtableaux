.isValidSkewPartition <- function(skewpart){
  .C("isValidSkewPartitionR", outer=list(skewpart[[1]]), inner=list(skewpart[[2]]),
     result=0L)$result
}
.showSkewPartition <- function(outer, inner){
  .C("showSkewPartitionR", outer=list(outer), inner=list(inner),
     result="")$result
}

showSkewPartition <- function(skewpart){
  outer <- purrr::map_int(skewpart[[1L]], as.integer)
  if(.isPartition(outer)==0L){
    warning("`outer` is not a valid partition.")
  }
  inner <- purrr::map_int(skewpart[[2L]], as.integer)
  if(.isPartition(inner)==0L){
    warning("`inner` is not a valid partition.")
  }
  if(.isSubPartitionOf(inner, outer)==0){
    stop("`inner` is not a subpartition of `outer`.")
  }
  .showSkewPartition(outer, inner)
}
