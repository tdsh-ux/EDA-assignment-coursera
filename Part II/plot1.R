df <- readRDS("C:/Users/tiago/Desktop/blogdown/data/summarySCC_PM25.rds")
scc <- readRDS("C:/Users/tiago/Desktop/blogdown/data/Source_Classification_Code.rds")
library(tidyverse)
df_year <- df %>%
  group_by(year) %>%
  summarise(emissions = sum(Emissions)) %>%
  ungroup()
plot(x = df_year$year, y = df_year$emissions, xaxt = "n", xlab = "Year", ylab = "Emissions", type = "line")
axis(1, at = df_year$year, labels = df_year$year); title("US PM2.5 emissions")

# clearly, we see a mitigation of the emissions in the decade considered.