diff_by_card = color_differences %>%
  select(Difference, Row, Column, Sheet) %>%
  group_by(Row, Column, Sheet) %>%
  summarise(mean_diff = mean(Difference)) %>%
  mutate(card_number = (Column-1) * 7 + Row)

anova_result <- aov(mean_diff ~ card_number, data = diff_by_card)
summary(anova_result)

diff_by_card %>% 
  group_by(Row, Column) %>%
  summarise(mean_diff = mean(mean_diff)) %>%
  ggplot() +
  geom_tile(aes(fill = mean_diff, x = Column, y = Row),
             width=1, height=1)
