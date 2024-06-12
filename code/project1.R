source("code/setup.R")

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)

display_color_sheet(lab_colors, color_sheet_idx = 1)
display_color_sheet(lab_colors, color_sheet_idx = 5)
display_color_sheet(lab_colors, color_sheet_idx = 13)

# mean color card evaluation
display_color_card(mean_lab_color_card, "Row", "Col", "Lab", 2)
plot_card_differences_to_master(mean_lab_color_differences, master_colors)
plot_card_vs_master(mean_lab_color_card, master_colors)
plot_density_vs_master(mean_lab_color_card, master_colors)

# master cmyk vs master lab
master_colors_cmyk <- master_colors %>%
  rowwise() %>%
  mutate(L = cmyk_to_lab(C, M, Y, K)[1],
         a = cmyk_to_lab(C, M, Y, K)[2],
         b = cmyk_to_lab(C, M, Y, K)[3]) %>%
  ungroup()
master_colors_cmyk_differences <- generate_color_difference_df(master_colors, master_colors_cmyk)
plot_card_differences_to_master(master_colors_cmyk_differences %>% rename(Row = Crow, Col = Ccol), master_colors)

# add correlation 
# 2D Scatterplot Chroma und L einzeln zu S
# 3D Scatterplot L, Chroma zu S
# correlationen berechnen

