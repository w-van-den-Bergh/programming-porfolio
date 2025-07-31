summarise_set_capacity_CE <- function(...) {

  #This function takes a set of data files in the workspace and summarises them to capacity and CE values
  
  if (length(grep("tidyverse", .packages(), value = TRUE)) == 0) {
    library(tidyverse)
    print("tidyverse is now loaded")
  } else if (grep("tidyverse", .packages(), value = TRUE) == "tidyverse") {
    print("tidyverse was loaded")
  }  
    
  input_list <- list(...)
  temp_list <- list()
  
  for (i in seq_along(input_list)){
    temp_data <- filter(input_list[[i]], mode %in% c("D")) %>% 
      group_by(, cycleC, mode)
    temp_list[[i]] <- summarise(temp_data, capacity = max(cap_mAhg))
    CE <-  temp_list[[i]]$capacity / lag(temp_list[[i]]$capacity) # for some reason mutate is not behaving, so extra step
    temp_list[[i]] <- add_column(temp_list[[i]], CE)
  }
  
  combined_data <- bind_rows(temp_list[1:length(temp_list)])
  combinded_data <- group_by(combined_data,cycleC)
  summarized_data_set <- summarise(combined_data, avg_cap = mean(capacity), avg_CE = mean(CE), SE_cap = sd(capacity)/sqrt(length(temp_list)), SE_CE = sd(CE)/sqrt(length(temp_list)))
  
 return(summarized_data_set)
}
