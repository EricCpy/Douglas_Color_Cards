source("code/setup.R")

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)

display_color_sheet(lab_colors, color_sheet_idx = 1)
display_color_sheet(lab_colors, color_sheet_idx = 5)
display_color_sheet(lab_colors, color_sheet_idx = 13)

# mean color card evaluation
display_color_card(mean_lab_color_card, "Row", "Col", "Lab", 2)
plot_card_differences_to_master(mean_lab_color_differences %>% rename(Crow = Row, Ccol = Col), master_colors)
plot_card_vs_master(mean_lab_color_card, master_colors)
plots <- plot_density_vs_master(lab_colors_master_shape, master_colors)
grid.arrange(plots[["L"]], plots[["a"]], plots[["b"]], nrow = 3)
# master cmyk vs master lab 
master_colors_cmyk <- master_colors %>%
  rowwise() %>%
  mutate(L = cmyk_to_lab(C, M, Y, K)[1],
         a = cmyk_to_lab(C, M, Y, K)[2],
         b = cmyk_to_lab(C, M, Y, K)[3]) %>%
  as.data.frame()
master_colors_cmyk_differences <- generate_color_difference_df(master_colors, master_colors_cmyk)
plot_card_differences_to_master(master_colors_cmyk_differences, master_colors)

# 2D Scatterplots Chroma and single channels to S
plot_correlation_with_categories(master_colors, "L", "S")
plot_correlation_with_categories(master_colors, "a", "S")
plot_correlation_with_categories(master_colors, "b", "S")
plot_correlation_with_categories(master_colors, "Chroma", "S")

# 2D Scatterplots for correlation and channel
grid.arrange(plot_correlation(mean_lab_color_differences, "L", "Difference"),
             plot_correlation(mean_lab_color_differences, "a", "Difference"),
             plot_correlation(mean_lab_color_differences, "b", "Difference"),
             nrow = 3)
