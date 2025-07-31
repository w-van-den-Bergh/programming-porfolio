# Plots charge discharge curve of ONE maccor data file

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

# Input data here ---------------------------------------------------------
load('230124_testdata.RData') #put in saved data here

sample <- `MACCORnum2_N5307_1wvb63-1`
cycles_to_view <- c(1, 3, 6, 9, 12, 15, 18)

sample <- filter(sample, mode %in% c("D", "C"))
sample <- filter(sample, cycleC %in% cycles_to_view)

# Input values here
color_palette <- scico(length(cycles_to_view), begin = 0.15, end = 0.85, palette = 'batlow')
line_size <- 4

x_label <- "Capacity (mAg/g)"
y1_label <- "Voltage (V, vs Li/Li+)"
axis_y_value_font_size <-  15
axis_x_value_font_size <-  15
axis_x_label_font_size <- 15
axis_y_label_font_size <- 15
axis_tick_length <- -0.4
axis_tick_size <- 2
axis_border_size <- 2

xmax <- 250
xmin <- 0
y1max <- 4.4
y1min <- 3.0

legend_font_size <- 20
legend_title_font_size <- 20

# Plotting Script ---------------------------------------------------------
chargedischarge_plot <- ggplot(data = sample) +
  geom_line(aes(x = cap_mAhg, y = voltage_V, color = cycleC, group = interaction(cycleC, mode)), size = line_size) +
  ylim(y1min, y1max) +
  xlim(xmin, xmax)

chargedischarge_plot <- chargedischarge_plot + 
  labs(x = x_label, y = y1_label, color = "Legend") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(size = axis_border_size),
        axis.ticks.length = unit(axis_tick_length, "cm"), axis.ticks = element_line(size = 2),
        axis.text.x = element_text(size = axis_x_value_font_size, face = "bold", color = "black"), axis.text.y = element_text(size = axis_y_value_font_size, face = "bold", color = "black"), 
        axis.title.x = element_text(size = axis_x_label_font_size, face = "bold"), axis.title.y = element_text(size = axis_y_label_font_size, face = "bold"),
        legend.text = element_text(size = legend_font_size, face = "bold"), legend.justification = c(0,0), legend.position = c(0.85,0.02), legend.key = element_blank(), legend.key.size = unit(1, "cm"), legend.title = element_text(size = legend_title_font_size, face = "bold")) + 
  labs(color = "Cycle") +
  guides(fill = guide_legend(reverse = TRUE))

print(chargedischarge_plot)