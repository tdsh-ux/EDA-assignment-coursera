download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
              destfile = paste0(getwd(), "/EPC.zip"))

hpc <- read.csv2(file = "household_power_consumption.txt", header = TRUE, sep = ";")
library(tidyverse); library(lubridate); library(cowplot)

hpc_february <- hpc %>%
  filter(Date %in% c("1/2/2007", "2/2/2007")) %>%   # dates in local format
  mutate(Global_active_power = as.numeric(Global_active_power),
         Time = hms(Time),
         Global_reactive_power = as.numeric(Global_reactive_power),
         Voltage = as.numeric(Voltage)) %>%
  mutate(Time = minute(Time)/60 + hour(Time) +
           ifelse(Date == "1/2/2007", 0, 24)) %>%
  mutate(Sub_metering_1 = as.numeric(Sub_metering_1),
         Sub_metering_2 = as.numeric(Sub_metering_2),
         Sub_metering_3 = as.numeric(Sub_metering_3))

hpc_february %>%
  select(Time, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
  pivot_longer(cols = c(Sub_metering_1, Sub_metering_2, Sub_metering_3)) %>%
  ggplot(aes(x = Time, y = value)) + 
  geom_line(aes(color = name)) +
  scale_x_continuous(name = " ",
                     breaks = c(0, 24, 48),
                     labels = c("Thu", "Fri", "Sat")) + 
  scale_y_continuous(name = "Energy sub metering") + 
  scale_color_manual(values = c("black", "red", "blue")) + 
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.background = element_rect(color = "black"),
        legend.title = element_blank(),
        legend.position = c(.85, 0.9)) -> plotIII

ggsave(plot = plotIII, filename = "plot3.png", device = "png", units = "cm",
       width = 12.7, height = 12.7)

