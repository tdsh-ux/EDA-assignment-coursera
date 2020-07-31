df_cities <- df %>% 
  filter(fips %in% c("24510", "06037")) %>% 
  mutate(fips = gsub(x = fips, pattern = "24510", replacement = "Baltimore City")) %>%
  mutate(fips = gsub(x = fips, pattern = "06037", replacement = "Los Angeles")) %>%
  inner_join(scc) %>% 
  filter(str_detect(Short.Name, pattern = "[Vv]ehicle")) %>%
  group_by(year, fips) %>%
  summarise(emissions = sum(Emissions)) %>%
  ungroup()
df_cities %>%
  ggplot(aes(x = year, y = emissions, fill = fips)) + 
  geom_col() + 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
  theme_bw() + 
  facet_wrap(.~fips, scales = "free") + 
  guides(fill = FALSE) + 
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008))

