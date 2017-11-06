#hi_low returns a vector where 1s represent a maximum or minumum, provided it has more than one occurrence.

hi_low <- function(vec) {
  include_vec <- rep(1,length(vec))                             # Assume we include all data points
  exclude.list = c(NaN,NA)                                      # Set exclusions
  minval <- min(vec[-which(vec %in% exclude.list)])             # Find the min
  maxval <- max(vec[-which(vec %in% exclude.list)])             # Find the max
  
  if (minval == maxval) return(include_vec)                     # If all values same, we're done here.
  
  count.of.mins <- sum(vec == minval, na.rm = T)                # Count minima
  count.of.maxs <- sum(vec == maxval, na.rm = T)                # Count maxima
  
  if (count.of.maxs > 1 && maxval != Inf) {                     # Assign the value we want to use to replace the maxima
    max.replacement.value <-  (count.of.maxs - 1)/count.of.maxs 
  } else {                                                         
    max.replacement.value <- 0
  }
  
  maxvector <- match(vec, maxval, incomparables = exclude.list) # Find maxima
  maxvector[maxvector == 1] <- max.replacement.value            # Replace the max position
  
  
  if (count.of.mins > 1) {                                      # Assign the value we want to use to replace the minima
    min.replacement.value <- (count.of.mins - 1)/count.of.mins
  } else {
    min.replacement.value <- 0
  }
  
  minvector <- match(vec, minval, incomparables = exclude.list) # Find minima
  minvector[minvector == 1] <- min.replacement.value            # Replace the min position
  
  hey_jude <- (!is.na(vec))*1                                   # Code to make sure NA and NaN are made 0
  include_vec <- pmin(include_vec, hey_jude, minvector, 
                      maxvector, na.rm = T)                     # Return a coalesced vector indicating if something is multimin/max
} 

#Examples follow

c1 <- c(0,0,1,NaN,Inf,3,4,4)
result <- hi_low(c1)

c2 <- c(0,0,0,0,0,0,0,0)
result <- hi_low(c2)

# !!! This returns a vector of all 0s, Is that really what we want?
c3 <- c(Inf,Inf,NA,NA,NaN,NaN,1,NA)
result <- hi_low(c3)

# !!! This would also be weird
c4 <- c(Inf,Inf,Inf)
result <- hi_low(c4)
