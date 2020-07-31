# the data frames are in the plot1.R archive.
df_baltimore <- df %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(emissions = sum(Emissions)) %>%
  ungroup()

plot(x = df_baltimore$year, y = df_baltimore$emissions, xaxt = "n", xlab = "Year", ylab = "Emissions", type = "line")
axis(side = 1, at = df_baltimore$year, labels = df_baltimore$year); title("Baltimore City PM2.5 emissions")

# in general, we see an improve in the emissions.