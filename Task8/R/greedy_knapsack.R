#' Title
#'
#' @param x data frame with two variables, w and v.
#' @param W integer containg the maximum weight of our knapsack
#'
#' @return list of two lists, value showing the max value of our optimal knapsack and the elements of that knapsack
#' @export
greedy_knapsack <- function(x, W){
  x$row <- 1:nrow(x) # create an index variable for x called row
  value <- 0
  v_w_ratio <- x[,2]/x[,1] # value to weight ratio
  sort_x <- x[order(v_w_ratio, decreasing = T),] # sort the x by ratio in decreasing order
  weight_NS <- 0
  items <- c()
  for (i in 1:nrow(x)){
    if(weight_NS + sort_x[i,1] >= W){# in order to only find the values that fits in the knapsack we skip all values that would go over the threshold.
      next()
    }
    weight_NS <- weight_NS + sort_x[i,1]# pack the total weight of the knapsack up until item i
    value <- value + sort_x[i,2]# total value of the knapsack up until item i
    items <- c(items, sort_x$row[i]) #save the x indexes of each item we put in the knapsack
  }
  a <- list("value" = value, "elements" = items)
  return(a)
}
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = T), v = runif(n=n,0, 10000))
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#

