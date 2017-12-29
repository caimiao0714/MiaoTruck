#' segment_1
#'
#' A version of "segment0" that could be passed onto a pipe function. Note: "segment0" is a function that separates different shifts
#'
#' @import data.table
#' @param data The data with speed and time_diff to be separated.
#' @param speed the variable name of speed to be segmented in the data.
#' @param time_diff the variable name of time_diff to weight the speed in the data.
#' @param threshold the threshold that is needed to segment the speeds. (the unit of threshold is an hour)
#' @return shift number, which is a vector which indicates the separated shift numbers, with 0 indicated time of taking a rest.
#'
#' @export
segment_1 = function(data, speed, time_diff, threshold) {
  ## Replace very long single points
  data$speed[data$time_diff >= threshold] <- 0
  ## First, replacing stretches of less than "threshold" consecutive 0 speeds by 1s
  r1 = rle(data$speed != 0)
  r1$values <- replicate(length(r1$values), 1)
  r1$values <- cumsum(r1$values)
  order_tmp <- inverse.rle(r1)
  dat_tmp1 <- data.table::data.table(speed = data$speed, order_tmp = order_tmp, time_diff = data$time_diff)
  dat_tmp2 <- dat_tmp1[,.(sumdiff = sum(time_diff)), by = order_tmp]
  r2 = rle(data$speed != 0)
  r2$values[r2$values == 0 & dat_tmp2$sumdiff < threshold] <- TRUE
  r2 <- inverse.rle(r2)
  r2 <- rle(r2)
  ## Then numbering consecutive stretches of non-zero values
  r2$values[r2$values] = cumsum(r2$values[r2$values])
  return(inverse.rle(r2))
}
