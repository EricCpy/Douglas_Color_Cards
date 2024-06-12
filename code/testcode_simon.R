# color_dispersion_over_sheets <- color_differences %>% group_by(Sheet, Field) %>% 
#   summarise(median_dE = median(Difference)) %>% mutate(Aggregated_by = "Sheet") %>% 
#   ungroup()
# color_dispersion_over_targets <- color_differences %>% group_by(Row, Column, Field) %>% 
#   summarise(median_dE = median(Difference)) %>% mutate(Aggregated_by = "Target") %>% 
#   ungroup()
# 
# # zu erwarten (Mittelwertstreuung)
# color_dispersion_over_sheets %>% select(-Sheet) %>% bind_rows(
#   color_dispersion_over_targets %>% select(-Row, -Column)
# ) %>% ggplot() +
#   geom_boxplot(aes(x = Aggregated_by, y = median_dE)) +
#   facet_wrap(Field ~ .) +
#   coord_cartesian(ylim = c(0, 6))

color_dispersion_on_sheet1 <- color_differences %>% group_by(Sheet, Row, Column, Field) %>% 
  filter(Sheet == 1) %>% 
  # summarise(median_dE = median(Difference)) %>% 
  mutate(Aggregated_by = "Sheet") %>% 
  ungroup()

color_dispersion_on_target1_1 <- color_differences %>% group_by(Sheet, Row, Column, Field) %>% 
  filter(Row == 1, Column == 1) %>% 
  # summarise(median_dE = median(Difference)) %>% 
  mutate(Aggregated_by = "Target") %>% 
  ungroup()

color_dispersion_on_sheet1 %>% bind_rows(
  color_dispersion_on_target1_1
) %>% # filter(Field != 18) %>% 
  ggplot() +
  geom_boxplot(aes(x = Aggregated_by, y = Difference)) +
  facet_grid(Crow ~ Ccol, switch = "both", as.table=FALSE) +
  coord_cartesian(ylim = c(0, 6.5))

# color_differences %>% group_by(Row, Column, Sheet, Field) %>% 
#   summarise(median_dE = median(Difference)) %>% 
#   # arrange(desc(median_dE)) %>% 
#   ggplot() +
#   geom_boxplot(aes(group = Field, y = median_dE))
# 
# color_differences %>% group_by(Row, Column, Sheet, Field) %>% 
#   summarise(median_dE = median(Difference)) %>% 
#   # arrange(desc(median_dE)) %>% 
#   ggplot() +
#   geom_boxplot(aes(group = Sheet, y = median_dE)) +
#   facet_wrap(facets = Field ~ ., scales = "free")

# Questions:
# Is the color difference greater within a sheet (among positions) or across sheets?
# Are there colors that are very far off the master target? <- Field 18 (Why; see Lab values)
# Are findings on L, a or b also there in dE? (On the subjective meaning)?

#### Special regression ####
# How is the color special used (regression)?

color_regression_linear <- lm(data = master_colors, cbind(L, a, b) ~ C + M + Y + K + S)
color_regression_poly <- lm(data = master_colors, cbind(L, a, b) ~ poly(C, 2) + poly(M, 2) + poly(Y, 2) + poly(K, 2) + poly(S, 2))
# color_regression_interaction <- lm(data = master_colors, cbind(L, a, b) ~ C * M * Y * K * S)
color_regression_interaction_3terms <- lm(data = master_colors %>% select(C, M, Y, K, S, L, a, b), cbind(L, a, b) ~ .^3)
# mean-delta-E?
color_regression <- color_regression_interaction_3terms
color_differences_regression <- dE(predict(color_regression), master_colors %>% select("L", "a", "b")) %>% 
  data.frame(Difference = .)

color_differences %>% 
  filter(Sheet == 1, Row == 1, Column == 1) %>% select(-Difference, -L, -a, -b) %>% bind_cols(
    color_differences_regression, predict(color_regression)
  ) %>% mutate(Color = lab_to_rgb(L, a, b)) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
  geom_point(aes(x = Crow+0.25, y = Ccol, color = Color), size = 12) +
  scale_color_identity() +
  geom_point(
    data = master_colors %>% rowwise() %>% mutate(Color = lab_to_rgb(L, a, b)),
    aes(x = Crow-0.25, y = Ccol, color = Color), size = 12
  ) +
  coord_fixed()

skin_color <- c(
  F, F, F, F, F, F, F, F,
  F, T, T, T, T, T, T, F,
  F, T, T, T, T, T, T, F,
  F, T, F, F, F, F, T, F,
  F, T, T, F, F, T, T, F,
  F, T, T, T, T, T, T, F,
  F, F, T, T, T, T, F, F,
  F, F, F, F, F, F, F, F
  )
