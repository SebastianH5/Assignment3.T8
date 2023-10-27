#' Title
#'
#' @param x data frame with two variables, w and v.
#' @param W integer containg the maximum weight of our knapsack
#'
#' @return list of two lists, value showing the max value of our optimal knapsack and the elements of that knapsack
#' @export
brute_force_knapsack <- function(x, W){
  stopifnot(is.data.frame(x), ncol(x)==2, x$w >0, x$v > 0)
  i <- 2
  finallist <- list(list("value" = 0, "elements" = 0))
  max <- c(0)
  order_index <- order(x[,1], decreasing = F) # index that orders according to size of weight
  a <- x[order_index, 1]

  repeat{

    # when to break the combination loop
    if(sum(a[1:i]) > W) {break}

    w <- as.data.frame(combn(x[,1], i)) # pick combinations of two from the column with weights
    sumweights <- colSums(w)

    v <- as.data.frame(combn(x[,2], i)) # pick combinations of two from the column with values
    sumvalues <- colSums(v)
    underlimitindex <- which(sumweights <= W)
    allowedvalues <- sumvalues[underlimitindex] # the values that are below the maximum weight limit


    max[i] <- max(allowedvalues)

    maxvalueindex <- which(sumvalues == max[i])

    maxvaluecomb <- v[ ,maxvalueindex] # combination of vales, which is the max value in allowed weight


    # to find which index values that are in the original data frame that adds up to the max value in accordance with the allowed weight
    optimum_items <- c()
    j <- 1
    while (j <= length(maxvaluecomb)){
      optimum_items[j] <- which(x[,2] == maxvaluecomb[j])
      j <- j + 1
    }


    finallist[[i]] <- list("value" = max[i], "elements" = optimum_items)
    i <- i +1
  }

  max_index <- which.max(max)

  return(finallist[[max_index]])
}

#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = T), v = runif(n=n,0, 10000))
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
