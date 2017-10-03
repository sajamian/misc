#hi_low returns the average of a vector, excluding invalid entries and min/max
hi_low <- function(vec) {
  vec <- vec[-which(vec %in% c(NaN,Inf))]           #Subset vec to exclude NaNs/Infs
  vec <- vec[-which(vec %in% c(min(vec),max(vec)))] #Subset vec to exclude mins/maxs
  mean(vec)
}

#Example follows
col <- c(0,0,1,NaN,Inf,3,4,4)
colavg <- hi_low(col)