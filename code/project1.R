source("code/setup.R")

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)

display_color_sheet(lab_colors, color_sheet_idx = 1)
display_color_sheet(lab_colors, color_sheet_idx = 5)
display_color_sheet(lab_colors, color_sheet_idx = 13)

mean_lab_color_card <- mean_lab_colors_for_sheets(lab_colors, 1:13)
plot_card_vs_master(mean_lab_color_card, master_colors)

# plot_card_vs_master mit Standard Deviation
# how does the column S in master influence CMYK values
# correlation between S and deltaE bei CMYK  

# add dispersion here also
# plot_card_vs_master mit Standard Deviation
plot_density_vs_master(mean_lab_color_card, master_colors)
