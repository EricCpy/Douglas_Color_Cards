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

# color_dispersion_over_sheets <- color_differences %>% group_by(Sheet, Field) %>% 
#   summarise(median_dE = median(Difference)) %>% mutate(Aggregated_by = "Sheet") %>% 
#   ungroup()
 color_dispersion_over_targets <- color_differences %>% group_by(Row, Column, Field) %>% 
   summarise(median_dE = median(Difference)) %>% mutate(Aggregated_by = "Target") %>% 
   ungroup()
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

color_dispersion_over_sheets <- color_differences %>% group_by(Sheet, Field) %>% 
  summarise(median_dE = median(Difference), count = n(), mad_dE = mad(Difference)) %>% 
  mutate(Aggregated_by = "Sheet") %>% 
  ungroup()

color_dispersion_over_targets <- color_differences %>% group_by(Field, Row, Column) %>% 
  summarise(median_dE = median(Difference), count = n(), mad_dE = mad(Difference)) %>% 
  mutate(Aggregated_by = "Target") %>% 
  ungroup()

color_dispersion_compare <- color_dispersion_over_sheets %>% bind_rows(
  color_dispersion_over_targets
)

# color_dispersion_compare %>% left_join(
#   color_differences %>% select(Field, Crow, Ccol) %>% unique()
# ) %>% filter(Field != 18) %>% ggplot() +
#   geom_point(aes(x = Field, y = median_dE, color = Aggregated_by, size = mad_dE), alpha = 0.5)
# 
# color_dispersion_compare %>% left_join(
#   color_differences %>% select(Field, Crow, Ccol) %>% unique()
# ) %>% filter(Field != 18) %>% ggplot() +
#   geom_jitter(aes(x = Crow, y = Ccol, color = Aggregated_by, size = median_dE), alpha = 0.5)

color_dispersion_compare %>% left_join(
  color_differences %>% select(Field, Crow, Ccol) %>% unique()
) %>% #filter(Field != 18) %>% 
  ggplot() +
  geom_boxplot(aes(x = Aggregated_by, y = mad_dE)) +
  facet_grid(Crow ~ Ccol, as.table = FALSE)

color_dispersion_compare %>% left_join(
  color_differences %>% select(Field, Crow, Ccol) %>% unique()
) %>% filter(Field != 18) %>% 
  ggplot() +
  geom_boxplot(aes(x = Aggregated_by, y = median_dE)) +
  geom_jitter(aes(x = Aggregated_by, y = median_dE), alpha = 0.5) +
  facet_grid(Crow ~ Ccol, as.table = FALSE)

##### linear regression on color differences ####

lm1a <- lm(data = color_differences, Difference ~ Sheet)
summary(lm1a)
lm1b <- lm(data = color_differences, Difference ~ factor(Sheet))
summary(lm1b)

anova(lm1a, lm1b)

lm2a <- lm(data = color_differences, Difference ~ Row + Column)
summary(lm2a)
lm2b <- lm(data = color_differences, Difference ~ factor(Row) + factor(Column))
summary(lm2b)
lm2c <- lm(data = color_differences, Difference ~ factor(Row)*factor(Column))
summary(lm2c)

anova(lm1a, lm1b, lm2a, lm2b, lm2c)

# Questions:
# Is the color difference greater within a sheet (among positions) or across sheets?
# Are there colors that are very far off the master target? <- Field 18 (Why; see Lab values)
# Are findings on L, a or b also there in dE? (On the subjective meaning)?

#### Special regression ####
# How is the color special used (regression)?

color_regression_linear <- lm(data = master_colors, cbind(L, a, b) ~ C + M + Y + K + S)
color_regression_poly <- lm(data = master_colors, cbind(L, a, b) ~ poly(C, 3) + poly(M, 3) + poly(Y, 3) + poly(K, 3) + poly(S, 3))
# color_regression_interaction <- lm(data = master_colors, cbind(L, a, b) ~ C * M * Y * K * S)
color_regression_interaction_3terms <- lm(data = master_colors %>% select(C, M, Y, K, S, L, a, b), cbind(L, a, b) ~ .^3)
# mean-delta-E?
color_regression <- color_regression_linear
color_regression <- color_regression_interaction_3terms
color_differences_regression <- dE(predict(color_regression), master_colors %>% select("L", "a", "b")) %>% 
  data.frame(Difference = .)

color_differences %>% 
  filter(Sheet == 1, Row == 1, Column == 1) %>% select(-Difference, -L, -a, -b) %>% bind_cols(
    color_differences_regression, predict(color_regression)
  ) %>% mutate(Color = lab_to_rgb(L, a, b)) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
  geom_point(aes(x = Crow+0.175, y = Ccol, color = Color), size = 13) +
  scale_color_identity() +
  geom_point(
    data = master_colors %>% rowwise() %>% mutate(Color = lab_to_rgb(L, a, b)),
    aes(x = Crow-0.175, y = Ccol, color = Color), size = 13
  ) +
  coord_fixed()

# master cmyk vs cmyks
mastercolor_cmyk_difference <- master_colors %>% rowwise() %>% mutate(rgb_by_cmyk = cmyk_to_rgb(C,M,Y,K)) %>% 
  bind_cols(
    map_dfr(srgb_to_Lab, .x = .$rgb_by_cmyk) 
  ) %>% bind_cols(
    dE(.[c("L", "a", "b")], .[c("Lab.L", "Lab.a", "Lab.b")]) %>% as.data.frame() %>% setNames("Difference")
  )

mastercolor_cmyk_difference %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Ccol, y = Crow, color = Difference > 2),
            lwd = 1.5, linetype = 1, width=0.9, height=0.9) +
  ggnewscale::new_scale_colour() +
  geom_point(aes(x = Ccol+0.175, y = Crow, color = rgb_by_cmyk), size = 14) +
  scale_color_identity() +
  geom_point(
    aes(x = Ccol-0.175, y = Crow, color = Color), size = 14
  ) +
  coord_fixed() +
  labs(caption = "Left circle: master color; Right circle: sample color") +
  xlab("Column") + ylab("Row") +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    plot.caption = element_text(size=12, hjust = 0)
  )

# average colors target 1-1

average_color_card_target_1_1 <- lab_colors_master_shape %>% filter(Row == 1, Column == 1) %>% 
  group_by(Ccol, Crow, Field) %>% 
  summarise(L = mean(L), a = mean(a), b = mean(b)) %>% ungroup() %>% 
  arrange(Field) %>% 
  mutate(Difference = generate_color_difference_df(master_colors, .)$Difference) %>% 
  rowwise() %>% mutate(
    Color = lab_to_rgb(L, a, b)
    )

average_color_card_target_1_1 %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Ccol, y = Crow, color = Difference > 2),
            lwd = 1.5, linetype = 1, width=0.9, height=0.9) +
  scale_color_manual(values = c("#FFFFFF00", "#CC3333")) +
  ggnewscale::new_scale_colour() +
  geom_point(aes(x = Ccol+0.175, y = Crow, color = Color), size = 14) +
  scale_color_identity() +
  geom_point(
    aes(x = Ccol-0.175, y = Crow, color = master_colors$Color), size = 14
  ) +
  coord_fixed() +
  labs(caption = "Left circle: master color; Right circle: sample color") +
  xlab("Column") + ylab("Row") +
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"),
    plot.caption = element_text(size=12, hjust = 0)
  )

#### regression lap cmyks ####

color_regression_linear <- lm(data = master_colors, cbind(L, a, b) ~ C + M + Y + K + S)
summary(color_regression_linear)

unique_master_colors <- master_colors %>% select(C, M, Y, K, S, L, a, b) %>% unique()

color_regression_linear <- lm(data = unique_master_colors, cbind(L, a, b) ~ C + M + Y + K + S)
color_regression <- color_regression_linear
color_differences_regression <- dE(predict(color_regression), unique_master_colors %>% select("L", "a", "b")) %>% 
  data.frame(Difference = .)

sum(color_differences_regression > 2)

color_regression_interaction_3terms <- lm(data = master_colors %>% select(C, M, Y, K, S, L, a, b), cbind(L, a, b) ~ .^3)
color_regression <- color_regression_interaction_3terms
color_differences_regression <- dE(predict(color_regression), master_colors %>% select("L", "a", "b")) %>% 
  data.frame(Difference = .)



test_indizes <- sample(1:49, 16)
train_data <- unique_master_colors[-test_indizes,]
test_data <- unique_master_colors[test_indizes,]

color_regression_interaction_3terms <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), cbind(L, a, b) ~ .^3)
color_regression <- color_regression_interaction_3terms
color_differences_regression <- dE(predict(color_regression, test_data), test_data %>% select("L", "a", "b")) %>% 
  data.frame(Difference = .)

color_regression$coefficients

library(boot)

color_difference <- function(formula, data, indices)
{
  train_data <- data[indices,]
  fit <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), formula)
  return(
    dE(predict(fit, test_data), test_data %>% select("L", "a", "b")) %>% 
      median()
  )
}

results1 <- boot(data=train_data, statistic=color_difference,
                R=1000, formula=cbind(L, a, b) ~ C + M + Y + K + S)
median(results1$t)
boot.ci(results1, type="perc")

results1$t %>% as_tibble() %>% ggplot() +
  geom_violin(aes(x= 1, y = V1))

results <- boot(data=train_data, statistic=color_difference,
                R=1000, formula=cbind(L, a, b) ~ .^3)
boot.ci(results)

custom_boot <- function(formula) {
  results <- c()
  for (i in 1:1000) {
    test_indizes <- sample(1:49, 16)
    train_data <- unique_master_colors[-test_indizes,]
    test_data <- unique_master_colors[test_indizes,]
    
    fit <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), formula)
    results[i] <- dE(predict(fit, test_data), test_data %>% select("L", "a", "b")) %>% 
      median()
  }  
  
  return(results)
}

results <- custom_boot(cbind(L, a, b) ~ C + M + Y + K + S)
quantile(results, c(.05, .5, .95))
mad(results)

results <- custom_boot(cbind(L, a, b) ~ .^3)
quantile(results, c(.05, .5, .95))
mad(results)


set.seed(123)
set.seed(124)
library(boot)

test_indizes <- sample(1:49, 16)
train_data <- unique_master_colors[-test_indizes,]
test_data <- unique_master_colors[test_indizes,]

color_difference <- function(formula, data, indices)
{
  train_data <- data[indices,]
  fit <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), formula)
  return(
    dE(predict(fit, test_data), test_data %>% select("L", "a", "b")) %>% 
      median()
  )
}

color_difference_perm <- function(formula, data, indices)
{
  train_data <- data[indices[1:33],]
  test_data <- data[indices[34:49],]
  fit <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), formula)
  return(
    dE(predict(fit, test_data), test_data %>% select("L", "a", "b")) %>% 
      median()
  )
}

custom_boot <- function(formula, R = 10000) {
  results <- c()
  for (i in 1:R) {
    test_indizes <- sample(1:49, 16)
    train_data <- unique_master_colors[-test_indizes,]
    test_data <- unique_master_colors[test_indizes,]
    
    fit <- lm(data = train_data %>% select(C, M, Y, K, S, L, a, b), formula)
    results[i] <- dE(predict(fit, test_data), test_data %>% select("L", "a", "b")) %>% 
      median()
  }  
  
  return(results)
}


results_lm_bootstrap <- boot(data=train_data, statistic=color_difference,
                             R=10000, formula=cbind(L, a, b) ~ C + M + Y + K + S)
median(results_lm_bootstrap$t)
boot.ci(results_lm_bootstrap, type="perc")

results_i2_bootstrap <- boot(data=train_data, statistic=color_difference,
                             R=10000, formula=cbind(L, a, b) ~ .^2)
median(results_i2_bootstrap$t)
boot.ci(results_i2_bootstrap, type="perc")

results_i3_bootstrap <- boot(data=train_data, statistic=color_difference,
                             R=10000, formula=cbind(L, a, b) ~ .^3)
median(results_i3_bootstrap$t)
boot.ci(results_i3_bootstrap, type="perc")

results_lm_bootstrap_perm <- boot(data=unique_master_colors, statistic=color_difference_perm, sim = "permutation",
                                  R=10000, formula=cbind(L, a, b) ~ C + M + Y + K + S)
median(results_lm_bootstrap_perm$t)
boot.ci(results_lm_bootstrap_perm, type="perc")

results_i2_bootstrap_perm <- boot(data=unique_master_colors, statistic=color_difference_perm, sim = "permutation",
                                  R=10000, formula=cbind(L, a, b) ~ .^2)
median(results_i2_bootstrap_perm$t)
boot.ci(results_i2_bootstrap_perm, type="perc")

results_i3_bootstrap_perm <- boot(data=unique_master_colors, statistic=color_difference_perm, sim = "permutation",
                                  R=10000, formula=cbind(L, a, b) ~ .^3)
median(results_i3_bootstrap_perm$t)
boot.ci(results_i3_bootstrap_perm, type="perc")

results_lm_fullrepetition <- custom_boot(cbind(L, a, b) ~ C + M + Y + K + S)
quantile(results_lm_fullrepetition, c(.05, .5, .95))

results_i2_fullrepetition <- custom_boot(cbind(L, a, b) ~ .^2)
quantile(results_i2_fullrepetition, c(.05, .5, .95))

results_i3_fullrepetition <- custom_boot(cbind(L, a, b) ~ .^3)
quantile(results_i2_fullrepetition, c(.05, .5, .95))

bootstrap_results <- results_lm_bootstrap$t %>% as_tibble() %>% mutate(method = "ordinary", formula = "linear") %>% 
  bind_rows(
    results_i2_bootstrap$t %>% as_tibble() %>% mutate(method = "ordinary", formula = "interaction 2nd order")
  ) %>% 
  bind_rows(
    results_i3_bootstrap$t %>% as_tibble() %>% mutate(method = "ordinary", formula = "interaction 3nd order") 
  ) %>% 
  bind_rows(
    results_lm_bootstrap_perm$t %>% as_tibble() %>% mutate(method = "permutation", formula = "linear")
  ) %>% 
  bind_rows(
    results_i2_bootstrap_perm$t %>% as_tibble() %>% mutate(method = "permutation", formula = "interaction 2nd order") 
  ) %>% 
  bind_rows(
    results_i3_bootstrap_perm$t %>% as_tibble() %>% mutate(method = "permutation", formula = "interaction 3nd order") 
  ) %>% mutate(
    formula = factor(formula, levels=c("linear", "interaction 2nd order", "interaction 3nd order"))
    )

v1 <- bootstrap_results %>% ggplot() +
  geom_violin(aes(x = formula, y = V1, fill = method), draw_quantiles = c(0.5))

# ggpubr::ggviolin(bootstrap_results, x = "formula", y = "V1", fill = "method", add = "jitter")

v2 <- bootstrap_results %>% ggplot(aes(x = formula, y = V1, fill = method)) +
  geom_violin(draw_quantiles = c(0.5)) +
  coord_cartesian(ylim = c(0, 10))

b1 <- bootstrap_results %>% ggplot() +
  geom_boxplot(aes(x = formula, y = V1, fill = method)) # +
  # geom_jitter(aes(x = formula, y = V1), alpha = .1)

b2 <- bootstrap_results %>% ggplot() +
  geom_boxplot(aes(x = formula, y = V1, fill = method)) +
  coord_cartesian(ylim = c(0, 10))

grid.arrange(v1, v2, b1, b2,
  ncol = 2, nrow = 2
  )

ggpubr::ggarrange(
  v1, v2, b1, b2,
  ncol = 2, nrow = 2,
  common.legend = TRUE,
  legend="bottom"
)

(results_lm_bootstrap$t %>% sort()) - (results_i2_bootstrap$t %>% sort())
ggplot() +
  geom_density(aes(x = V1))

library(ggridges)

lm_vs_i2 <- ((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i2_bootstrap$t %>% sample(100000, replace = TRUE))) %>% as_tibble() %>% mutate(compare = "lm vs i2")

sum(lm_vs_i2$value > 0)/nrow(lm_vs_i2)

mean((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i2_bootstrap$t %>% sample(100000, replace = TRUE)) < 0)
((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i2_bootstrap$t %>% sample(100000, replace = TRUE))) %>% 
  as_tibble() %>% ggplot(aes((x = value))) +
  geom_density()

mean((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i3_bootstrap$t %>% sample(100000, replace = TRUE)) < 0)
((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i3_bootstrap$t %>% sample(100000, replace = TRUE))) %>% 
  as_tibble() %>% ggplot(aes((x = value))) +
  geom_density()

((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i2_bootstrap$t %>% sample(100000, replace = TRUE))) %>% 
  as_tibble() %>% mutate(compare = "lm vs i2") %>% bind_rows(
    ((results_lm_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i3_bootstrap$t %>% sample(100000, replace = TRUE))) %>% 
    as_tibble() %>% mutate(compare = "lm vs i3")
  ) %>% ggplot(aes(x = value, y = compare, fill = factor(after_stat(x) > 0))) +
  geom_density_ridges_gradient(scale = 0.9) +
  coord_cartesian(xlim=c(-20,10))

mean((results_i2_bootstrap$t %>% sample(100000, replace = TRUE)) - (results_i3_bootstrap$t %>% sample(100000, replace = TRUE)) < 0)
