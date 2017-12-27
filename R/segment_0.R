#' segment_0
#'
#' Define a funtion "segment0" that separate different shifts
#'
#' @importfrom data.table data.table
#' @param speed the speed is a vector of speeds to be segmented.
#' @param threshold the threshold that is needed to segment the speeds. (the unit of threshold is an hour)
#' @param time_diff the difference of time for each data point.
#'
#' @return shift number, which is a vector which indicates the separated shift numbers, with 0 indicated time of taking a rest.
#'
#' @export
segment_0 = function(speed, threshold, time_diff) {
  ## Replace very long single points
  speed[time_diff >= threshold] <- 0
  ## First, replacing stretches of less than "threshold" consecutive 0 speeds by 1s
  r1 = rle(speed != 0)
  r1$values <- replicate(length(r1$values), 1)
  r1$values <- cumsum(r1$values)
  order_tmp <- inverse.rle(r1)
  dat_tmp1 <- data.table::data.table(speed, order_tmp, time_diff)
  dat_tmp2 <- dat_tmp1[,.(sumdiff = sum(time_diff)), by = order_tmp]
  r2 = rle(speed != 0)
  r2$values[r2$values == 0 & dat_tmp2$sumdiff < threshold] <- TRUE
  r2 <- inverse.rle(r2)
  r2 <- rle(r2)
  ## Then numbering consecutive stretches of non-zero values
  r2$values[r2$values] = cumsum(r2$values[r2$values])
  return(inverse.rle(r2))
}


