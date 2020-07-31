library(ggsci)
df_types <- df %>%
  filter(fips == "24510") %>% 
  group_by(year, type) %>% 
  summarise(emissions = sum(Emissions)) %>% 
  ungroup()

df_types %>%
  ggplot(aes(x = year, y = emissions)) + 
  geom_line(aes(color = type)) + 
  geom_point(aes(color = type), size = 3) + 
  theme_classic() + 
  scale_color_simpsons() + 
  theme(panel.background = element_rect(fill = "#2D2D2D")) + 
  facet_grid(.~type) + 
  guides(color = FALSE)