.isPartition <- function(partition){
  .C("isPartitionR", partition=list(partition), result=0L)$result
}
.isSubPartitionOf <- function(partition1, partition2){
  .C("isSubPartitionOfR", partition1 = list(partition1), partition2=list(partition2),
     result=0L)$result
}
.mkPartition <- function(partition){
  .C("mkPartitionR", partition=list(partition), result=list(0L))$result[[1L]]
}

#' Subpartition test
#'
#' Tests whether the first partition is a subpartition of the second one.
#'
#' @param partition1 a partition
#' @param partition2 a partition
#'
#' @return A logical value.
#' @export
#'
#' @examples
#' isSubPartitionOf(c(3, 2), c(6, 3, 1))
isSubPartitionOf <- function(partition1, partition2){
  partition1 <- purrr::map_int(partition1, as.integer)
  if(.isPartition(partition1)==0L){
    warning("`partition1` is not a valid partition.")
  }
  partition2 <- purrr::map_int(partition2, as.integer)
  if(.isPartition(partition2)==0L){
    warning("`partition2` is not a valid partition.")
  }
  as.logical(.isSubPartitionOf(partition1, partition2))
}
