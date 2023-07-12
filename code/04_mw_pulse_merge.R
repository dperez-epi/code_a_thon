#Script to merge minimum wage increase and pulse data

mwpulse_raw <- pulse_data %>% 
  left_join(mw_crosswalk)
