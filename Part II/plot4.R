df_scc_coal <- df %>%
  inner_join(scc) %>%
  filter(str_detect(Short.Name, pattern = "[Cc]oal")) %>%
  group_by(year) %>%
  summarise(emissions = sum(Emissions)) %>%
  ungroup()

df_scc_coal %>%
  ggplot(aes(x = year, y = emissions)) + 
  geom_line() + 
  geom_point() + 
  theme_bw() + 
  ggtitle("Coal combustion-related emissions in US")