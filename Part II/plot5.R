df_scc_vehicle <- df %>% 
  inner_join(scc) %>% 
  filter(str_detect(Short.Name, pattern = "[Vv]ehicle"),
         fips == "24510") %>% 
  group_by(year) %>% 
  summarise(emissions = sum(Emissions)) %>% 
  ungroup()

df_scc_vehicle %>%
  ggplot(aes(x = year, y = emissions)) + 
  geom_line() + 
  geom_point() + 
  theme_bw() + 
  ggtitle("Emissions from motor vehicles in Baltimore City")