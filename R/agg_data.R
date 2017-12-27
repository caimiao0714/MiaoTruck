#' agg_data
#'
#' This function provides a way to aggregate the data onto the hour level.
#'
#' @import dplyr
#'
#' @param data The data to be passed onto agg_data function
#' @inheritParams tidy_data
#'
#' @export
agg_data <- function(data, thre_tem, thre_visul){
  output_data <- tidy_data(dat_nodup) %>%
    filter(shift_no != 0) %>%
    select(shift_no, cumsum_drive_hours, SPEED, hard_brake, `AIR-TEMPERATURE-OBSERVATION air temperature`, precipitation, `VISIBILITY-OBSERVATION distance dimension`, driver_name) %>%
    mutate(`cumulative drive hours` = ceiling(cumsum_drive_hours)) %>%
    group_by(shift_no, `cumulative drive hours`) %>%
    summarise(N_obs = length(SPEED),
              p_0_speed = round(mean(SPEED == 0, na.rm = TRUE)*100, 2),
              temperature_less_thre = ifelse(sum(`AIR-TEMPERATURE-OBSERVATION air temperature`/10 < thre_tem, na.rm = TRUE) > 0, 1, 0),
              precipitation = ifelse(sum(precipitation)>0, 1, 0),
              cum_drive_time = max(cumsum_drive_hours),
              vis_less_thre = ifelse(sum(as.numeric(`VISIBILITY-OBSERVATION distance dimension`) < thre_visul, na.rm = TRUE)> 0, 1, 0),
              N_hard_brake = sum(hard_brake, na.rm = TRUE),
              driver_name = max(driver_name))

  out_dat$cum_temperature <- ave(out_dat$temperature_less_thre, out_dat$shift_no, FUN=cumsum)
  out_dat$cum_precipitation <- ave(out_dat$precipitation, out_dat$shift_no, FUN=cumsum)
  out_dat$cum_visibility <- ave(out_dat$vis_less_thre, out_dat$shift_no, FUN=cumsum)

  return(output_data)
}
