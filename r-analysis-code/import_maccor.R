import_maccor <- function() {
  
  # Checks if you have the proper packages to run the following code
  
  if (length(grep("tidyverse", .packages(), value = TRUE)) == 0) {
    library(tidyverse)
    print("tidyverse is now loaded")
  } else if (grep("tidyverse", .packages(), value = TRUE) == "tidyverse") {
    print("tidyverse was loaded")
  }
  
  # Grabs the files you want to import, they should be maccor data text files
  
  untidy_files <- choose.files()
  
  for (i in seq_along(untidy_files)) {
    temp_filename <- untidy_files[[i]]
    temp_data <- read_tsv(
      temp_filename,
      skip = 15,
      col_names = c("obs_number", "cycleP", "cycleC", "step", 
                    "testtime_s", "steptime_s", "cap_mAhg", 
                    "energy_Wh", "current_mA", "voltage_V",
                    "mode", "ES", "time", "fat")
    )
    
    #Section below is to cut the unnecessary columns
    
    temp_mass <- readline(prompt= paste0(temp_filename, "    What is the electrode active material mass (mg)? = ")) 
    temp_mass <- as.numeric(temp_mass) / 1000
    
    temp_data <- select(temp_data, obs_number, cycleC, mode, voltage_V, current_mA, cap_mAhg, time)
    temp_data <- mutate(temp_data, cap_mAhg = (cap_mAhg * 1000) / temp_mass)
    temp_data <- mutate(temp_data, current_mA = current_mA * 1000)
    
    temp_data <- mutate(temp_data, time = as.POSIXct(temp_data$time, format = "%m/%d/%Y %H:%M:%S %p"))
    temp_data <- mutate(temp_data, reltime = c(0,diff(temp_data$time)))
    temp_data <- mutate(temp_data, reltime = cumsum(temp_data$reltime))
    
    #Section below is to rename the new tibble with the related filename
    
    name_head <- str_locate_all(temp_filename, "\\\\")
    name_beg_value <- name_head[[1]][length(name_head[[1]])] + 1
    name_end_value <- str_locate_all(temp_filename, "TXT")[[1]][1] - 2
    if (is.na(name_end_value) == TRUE) {
      name_end_value <- str_locate_all(temp_filename, "txt")[[1]][1] - 2
    }
    sample_name <- str_sub(temp_filename,name_beg_value,name_end_value)
    sample_name <- gsub("#", "num", sample_name)
    assign(sample_name, temp_data, envir = .GlobalEnv)
  }
}