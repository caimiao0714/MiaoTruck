#' tidy_data
#'
#' This function cleans the driver data and returns the cleaned dataset.
#'
#' @param data the data to be cleaned.
#' @param delete_duplicate where the duplicated data points are to be deleted. The default value is TRUE, which means the duplicated data points will be deleted by default.
#'
#' @import lubridate
#' @import dplyr
#'
#' @export

tidy_data <- function(data, delete_duplicate = TRUE){

  data <- data[!is.na(data$DATIME_GMT),]
  data$DATEGMT_lub <- suppressWarnings(lubridate::mdy_hm(data$DATIME_GMT))
  data <- data[!is.na(data$DATEGMT_lub),]
  dat_nodup <- data
  ####data$c_datime <- lubridate::mdy_hm(data$c_datime)
  # order time points

  # deleting duplicate time points
  if(delete_duplicate){
    dat_nodup <- data[!duplicated(data$c_datime)|is.na(data$c_datime),]
    dat_nodup <- dat_nodup[!duplicated(dat_nodup$DATEGMT_lub),]
  }else{
    dat_nodup <- dat_nodup
  }
  dat_nodup <- arrange(dat_nodup, DATEGMT_lub)

  # filtering only the "hard breaks"
  # data <-
  #   data[!(data$EVT_TYP == "ROLL_STABILITY" |
  #   data$EVT_TYP == "HEADWAY") | is.na(data$EVT_TYP), ]
  dat_nodup <-
    dat_nodup[(dat_nodup$EVT_TYP != "ROLL_STABILITY" &
                 dat_nodup$EVT_TYP != "HEADWAY") | is.na(dat_nodup$EVT_TYP), ]

  #create indicating variable of "hard brakes"
  # data$hard_brake <- 0
  # data$hard_brake[data$EVT_TYP == "HARD_BRAKING"] <- 1
  dat_nodup$hard_brake <- 0
  # dat_nodup$hard_brake[dat_nodup$EVT_TYP == "HARD_BRAKING"] <- 1
  dat_nodup$hard_brake[!is.na(dat_nodup$EVT_TYP)] <- 1

  # create a new variable of time difference
  # data <- arrange(data, DATEGMT_lub)
  # data$time_diff <- data$DATEGMT_lub - lag(data$DATEGMT_lub)
  # data$time_diff[1] <- 0
  # data$time_diff <- as.numeric(data$time_diff)/60
  dat_nodup <- arrange(dat_nodup, DATEGMT_lub)
  dat_nodup$time_diff <- dat_nodup$DATEGMT_lub - lag(dat_nodup$DATEGMT_lub)
  dat_nodup$time_diff[1] <- 0
  dat_nodup$time_diff <- as.numeric(dat_nodup$time_diff)/60

  return(dat_nodup)
}
