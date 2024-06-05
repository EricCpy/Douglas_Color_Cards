source("code/setup.R")

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)

display_color_sheet(lab_colors, color_sheet_idx = 1)
display_color_sheet(lab_colors, color_sheet_idx = 5)
display_color_sheet(lab_colors, color_sheet_idx = 13)

# how does the column S in master influence CMYK values
# compare cmyk lab to real lab
# regression doesnt really help R2 of 90%, but regression still doesnt make sense if they have a formula for CMYKS to Lab

mean_lab_color_card <- mean_lab_colors_for_sheets(lab_colors, 1:13)
plot_card_vs_master(mean_lab_color_card, master_colors)
