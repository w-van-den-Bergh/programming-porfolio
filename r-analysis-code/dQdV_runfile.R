# Takes electrochemical data from ONE imported maccor file and plots the dQ/dV

# Preliminary Script ------------------------------------------------------
rm(list = ls())
if (length(grep("tidyverse", .packages(), value = TRUE)) == 0) {
  library(tidyverse)
  print("tidyverse is now loaded")
} else if (grep("tidyverse", .packages(), value = TRUE) == "tidyverse") {
  print("tidyverse was loaded")
}
if (length(grep("scico", .packages(), value = TRUE)) == 0) {
  library(tidyverse)
  print("scico is now loaded")
} else if (grep("scico", .packages(), value = TRUE) == "scico") {
  print("scico was loaded")
}  
# Input Data here ---------------------------------------------------------
load('maccor_test_data.RData') #put in saved data here

sample <- `MACCOR#2_N5307_1wvb63-1`
cycles_to_view <- c(1, 3, 5, 10)

sample <- filter(sample, mode %in% c("D", "C"))
sample <- filter(sample, cycleC %in% cycles_to_view)

sample <- mutate(sample, dqdv = (lead(cap_mAhg) - cap_mAhg) / (lead(voltage_V) - voltage_V))

# Input values here
color_palette <- scico(length(cycles_to_view), begin = 0.15, end = 0.85, palette = 'batlow')
line_size <- 4

x_label <- "Voltage (V, vs Li/Li+)"
y1_label <- "dQ/dV (mAh/gV, vs Li/Li+)"
axis_y_value_font_size <-  15
axis_x_value_font_size <-  15
axis_x_label_font_size <- 15
axis_y_label_font_size <- 15
axis_tick_length <- -0.4
axis_tick_size <- 2
axis_border_size <- 2

xmax <- 200
xmin <- 0
y1max <- 4.5
y1min <- 2.8

legend_font_size <- 20

# Plotting script ---------------------------------------------------------

dqdv_plot <- ggplot(data = sample) +
  geom_line(aes(x = cap_mAhg, y = voltage_V, color = cycleC, group = interaction(cycleC, mode)), size = line_size) +
  ylim(y1min, y1max) +
  xlim(xmin, xmax)

dqdv_plot <- dqdv_plot + 
  labs(x = x_label, y = y1_label, color = "Legend") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(size = axis_border_size),
        axis.ticks.length = unit(axis_tick_length, "cm"), axis.ticks = element_line(size = 2),
        axis.text.x = element_text(size = axis_x_value_font_size, face = "bold", color = "black"), axis.text.y = element_text(size = axis_y_value_font_size, face = "bold", color = "black"), 
        axis.title.x = element_text(size = axis_x_label_font_size, face = "bold"), axis.title.y = element_text(size = axis_y_label_font_size, face = "bold"),
        legend.text = element_text(size = legend_font_size, face = "bold"), legend.justification = c(0,0), legend.position = c(0.90,0.01), legend.key = element_blank(), legend.key.size = unit(1, "cm")) +
  guides(fill = guide_legend(byrow = TRUE))

print(dqdv_plot)