# This runfile is meant to plot discharge capacity of summarized data

# Prelim Script -------------------------------------------------------------
rm(list = ls())
if (length(grep("tidyverse", .packages(), value = TRUE)) == 0) {
  library(tidyverse)
  print("tidyverse is now loaded")
} else if (grep("tidyverse", .packages(), value = TRUE) == "tidyverse") {
  print("tidyverse was loaded")
}
if (length(grep("scico", .packages(), value = TRUE)) == 0) {
  library(scico)
  print("scico is now loaded")
} else if (grep("scico", .packages(), value = TRUE) == "scico") {
  print("scico was loaded")
}  
# Data and Value Input ----------------------------------------------------
load('230124_testdata.RData') #put in saved data here

  # Input data sets here
input_list <- list()
legend_labels <- character()
  
input_list[[1]] <- wvb1_63
legend_labels[1] <- "680°C"

input_list[[2]] <- wvb1_64
legend_labels[2] <- "720°C"
  
input_list[[3]] <- wvb1_65
legend_labels[3] <- "780°C"  

comb_data <- bind_rows(input_list, .id = "id")
for (i in seq_along(legend_labels)){
  comb_data$id[comb_data$id == i] <- legend_labels[i]
}
  
  # Input values here
color_palette <- scico(length(input_list), begin = 0.15, end = 0.85, palette = 'batlow')
errorbar_width <- 0.3
errorbar_thickness <- 1.5
point_size <- 4

x_label <- "Cycle Number"
y1_label <- "Discharge Capacity (mAh/g)"
y2_label <- "Coulombic Efficiency (%)"
axis_y_value_font_size <-  15
axis_x_value_font_size <-  15
axis_x_label_font_size <- 15
axis_y_label_font_size <- 15
axis_tick_length <- -0.4
axis_tick_size <- 2
axis_border_size <- 2

y1max <- 300 # y-axis maximum
y1min <- 0 # y-axis minimum

x1max <- 100 # x-axis maximum
x1min <- 0 # x-axis minimum

legend_font_size <- 20


# Plotting ----------------------------------------------------------------

capCE_plot <- ggplot(data = comb_data) +
  geom_point(aes(x = cycleC, y = avg_cap, color = id), size = point_size) +
  geom_errorbar(aes(x = cycleC, ymax = avg_cap + SE_cap,  ymin = avg_cap - SE_cap, color = id), width = errorbar_width, size = errorbar_thickness) +
  scale_color_manual(values = color_palette, name = NULL) +
  ylim(y1min, y1max) +
  xlim(x1min, x1max)

capCE_plot <- capCE_plot + 
  labs(x = x_label, y = y1_label, color = "Legend") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_rect(size = axis_border_size),
    axis.ticks.length = unit(axis_tick_length, "cm"), axis.ticks = element_line(size = 2),
    axis.text.x = element_text(size = axis_x_value_font_size, face = "bold", color = "black"), axis.text.y = element_text(size = axis_y_value_font_size, face = "bold", color = "black"), 
    axis.title.x = element_text(size = axis_x_label_font_size, face = "bold"), axis.title.y = element_text(size = axis_y_label_font_size, face = "bold"),
    legend.text = element_text(size = legend_font_size, face = "bold"), legend.justification = c(0,0), legend.position = c(0.01,0.01), legend.key = element_blank(), legend.key.size = unit(1, "cm")) +
  guides(fill = guide_legend(byrow = TRUE))
print(capCE_plot)