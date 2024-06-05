source("code/setup.R")

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)

display_color_sheet(lab_colors, color_sheet_idx = 1)
display_color_sheet(lab_colors, color_sheet_idx = 5)
display_color_sheet(lab_colors, color_sheet_idx = 13)

mean_lab_color_card <- mean_lab_colors_for_sheets(lab_colors, 1:13)
plot_card_vs_master(mean_lab_color_card, master_colors)

#difference between mean of measurements and master
# plot_card_vs_master mit Standard Deviation
# how does the column S in master influence CMYK values
# correlation between S and deltaE bei CMYK  

# Kommen bestimmte L, a, b Werte häufiger im Master als im Mean vor
# distribution of L, a, b
ggplot(master_colors, aes(x = master_colors$L)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Values", x = "Values", y = "Density") +
  theme_minimal()
