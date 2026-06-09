
# Wave Number to Duration Label ------------------------------------------

cmie_wave_duration = function(wave_num, start_date = F){

  label_vec <- c(
  "Jan-Apr (2014)", "May-Aug (2014)", "Sep-Dec (2014)",
  "Jan-Apr (2015)", "May-Aug (2015)", "Sep-Dec (2015)",
  "Jan-Apr (2016)", "May-Aug (2016)", "Sep-Dec (2016)",
  "Jan-Apr (2017)", "May-Aug (2017)", "Sep-Dec (2017)",
  "Jan-Apr (2018)", "May-Aug (2018)", "Sep-Dec (2018)",
  "Jan-Apr (2019)", "May-Aug (2019)", "Sep-Dec (2019)",
  "Jan-Apr (2020)", "May-Aug (2020)", "Sep-Dec (2020)",
  "Jan-Apr (2021)", "May-Aug (2021)", "Sep-Dec (2021)",
  "Jan-Apr (2022)", "May-Aug (2022)", "Sep-Dec (2022)",
  "Jan-Apr (2023)", "May-Aug (2023)", "Sep-Dec (2023)",
  "Jan-Apr (2024)", "May-Aug (2024)", "Sep-Dec (2024)",
  "Jan-Apr (2025)", "May-Aug (2025)", "Sep-Dec (2025)"
)
  
  if (wave_num < 1 | wave_num > length(label_vec)){
    warning(paste0("Enter integer between 1 and ", length(label_vec)))
    return(NULL)
  }

  duration_label <- label_vec[[wave_num]]

  if(start_date == T){

    start_month <- sub("-.*", "", sub(" .*", "", duration_label))
    year <- sub(".*\\(([0-9]{4})\\)", "\\1", duration_label)

  as.Date(
    paste(start_month, "1", year),
    format = "%b %d %Y"
  )
  } else {
    return(duration_label)
  }

} 
