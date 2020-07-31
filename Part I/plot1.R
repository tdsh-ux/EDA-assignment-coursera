download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
              destfile = paste0(getwd(), "/EPC.zip"))

hpc <- read.csv2(file = "household_power_consumption.txt", header = TRUE, sep = )
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
  ggplot(aes(x = Global_active_power)) + 
  geom_histogram(fill = "red", color = "black", breaks = seq(0, 6, .5)) + 
  theme_classic() + 
  scale_y_continuous(name = "Frequency") + 
  scale_x_continuous(name = "Global Active Power (kilowatts)") -> plotI

ggsave(plot = plotI, filename = "plot1.png", device = "png", unit = "cm",
       width = 12.7, height = 12.7, dpi = 150)









