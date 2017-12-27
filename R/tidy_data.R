#' tidy_data
#'
#' This function cleans the driver data and returns the cleaned dataset.
#'
#' @param data the data to be cleaned.
#' @param threshold_hour The threshold of hour to segment the speed data
#' @param thre_tem The upper threshold of temperature to decide extreme temperature conditions. The unit of thre_tem is 1 cesius degree. The default value is 0 cesius degree.
#' @param thre_visul The upper threshold of visibility distance to decide extreme temperature conditions. The unit of thre_visul is 1 meter. The default value is 1000 meters.
#' @param delete_duplicate where the duplicated data points are to be deleted. The default value is TRUE, which means the duplicated data points will be deleted by default.
#'
#' @importfrom lubridate mdy_hm hour
#' @import dplyr
#'
#' @export
tidy_data <- function(data, threshold_hour = 8, thre_tem = 0, thre_visul = 1000, delete_duplicate = TRUE){
  data$DATEGMT_lub <- suppressWarnings(lubridate::mdy_hm(data$DATIME))
  dat_nodup <- data[!is.na(data$DATEGMT_lub),]
  ####data$c_datime <- lubridate::mdy_hm(data$c_datime)
  # order time points

  # deleting duplicate time points
  if(delete_duplicate){
    dat_nodup <- data[(!duplicated(data$c_datime))|is.na(data$c_datime),]
    dat_nodup <- dat_nodup[!duplicated(dat_nodup$DATEGMT_lub),]
  }else{
    dat_nodup <- dat_nodup[!duplicated(dat_nodup$DATEGMT_lub),]
  }
  dat_nodup <- dplyr::arrange(dat_nodup, DATEGMT_lub)

  # filtering only the "hard breaks"
  # data <-
  #   data[!(data$EVT_TYP == "ROLL_STABILITY" |
  #   data$EVT_TYP == "HEADWAY") | is.na(data$EVT_TYP), ]
  dat_nodup <-
    dat_nodup[(dat_nodup$EVT_TYP != "ROLL_STABILITY" &
                 dat_nodup$EVT_TYP != "HEADWAY") | is.na(dat_nodup$EVT_TYP), ]

  # create indicating variable of "hard brakes"
  dat_nodup$hard_brake <- 0
  dat_nodup$hard_brake[dat_nodup$EVT_TYP == "HARD_BRAKING"] <- 1

  # create a new variable of time difference
  dat_nodup <- dplyr::arrange(dat_nodup, DATEGMT_lub)
  dat_nodup$time_diff <- dat_nodup$DATEGMT_lub - lag(dat_nodup$DATEGMT_lub)
  dat_nodup$time_diff[1] <- 0
  dat_nodup$time_diff <- as.numeric(dat_nodup$time_diff)/60 # the unit of time_diff is "an hour"

  # segment shift No.s
  dat_nodup$shift_no <- segment_0(dat_nodup$SPEED, threshold_hour, dat_nodup$time_diff)

  # calculating cumulative on-shift time for each observation
  dat_nodup$cumsum_shift_hours <- ave(dat_nodup$time_diff, dat_nodup$shift_no, FUN=cumsum)
  dat_nodup$cumsum_shift_hours <- round(dat_nodup$cumsum_shift_hours/60, 3)

  # calculating cumulative driving time
  dat_nodup <- dplyr::arrange(dat_nodup, DATEGMT_lub)
  dat_nodup$lag_time_hours <- (dat_nodup$DATEGMT_lub - dplyr::lag(dat_nodup$DATEGMT_lub))/60
  dat_nodup$lag_time_hours[1] <- 0.01
  dat_nodup$lag_time_hours[dat_nodup$lag_time_hours > 3] <- 0.25
  dat_nodup$lead_time_hours <- (dplyr::lead(dat_nodup$DATEGMT_lub) - dat_nodup$DATEGMT_lub)/60
  dat_nodup$lead_time_hours[length(dat_nodup$lead_time_hours)] <- 0.01
  dat_nodup$lead_time_hours[dat_nodup$lead_time_hours > 3] <- 0.25

  # Calculate driving time in that shift for each time point
  dat_nodup$drive_time_hours <- as.numeric(ifelse(dat_nodup$SPEED > 0, 0.5, 0) *(dat_nodup$lag_time_hours + dat_nodup$lead_time_hours))

  # Calculate cumulative driving time in that shift for each time point
  dat_nodup$cumsum_drive_hours <- round(ave(dat_nodup$drive_time_hours, dat_nodup$shift_no, FUN=cumsum), 3)

  # time of the day (hour of the day)
  dat_nodup$Hour <- lubridate::hour(dat_nodup$DATEGMT_lub)

  # precipitation - binary variable
  dat_nodup$precipitation <- 0
  dat_nodup$`AA1_ depth dimension`[is.na(dat_nodup$`AA1_ depth dimension`)] <- 0
  dat_nodup$precipitation[as.numeric(dat_nodup$`AA1_ depth dimension`) > 0] <- 1

  return(dat_nodup)
}
