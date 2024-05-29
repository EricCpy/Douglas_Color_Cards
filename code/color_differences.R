source("code/setup.R")

#### DeltaE for single Card ####

lab_colors_master_shape_sheet_1_row_1_col_1 <- lab_colors_master_shape %>% 
  filter(Sheet == 1, Row == 1, Column == 1)

dE(
  master_colors[, c("L", "a", "b")], 
  lab_colors_master_shape_sheet_1_row_1_col_1 %>% select("L", "a", "b"),
  metric = 2000
  ) %>% 
  data.frame(Difference = .) %>% 
  bind_cols(lab_colors_master_shape_sheet_1_row_1_col_1) %>% rowwise() %>% 
  mutate(Color = lab_to_rgb(L, a, b)) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
  geom_point(aes(x = Crow+0.25, y = Ccol, color = Color), size = 12) +
  scale_color_identity() +
  geom_point(
    data = master_colors %>% rowwise() %>% mutate(Color = lab_to_rgb(L, a, b)),
    aes(x = Crow-0.25, y = Ccol, color = Color), size = 12
    ) +
  coord_fixed()

#### DeltaE for a Sheet ####

lab_colors_master_shape_sheet_1 <- lab_colors_master_shape %>% 
  filter(Sheet == 1)

DeltaE_map_for_sheet <- function(data) {
  dE(
    master_colors %>% attach_replicas_to_df_by_rows(cards_per_sheet) %>% select("L", "a", "b"), 
    data %>% select("L", "a", "b")
  ) %>% 
    data.frame(Difference = .) %>% 
    bind_cols(data) %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
    coord_fixed() +
    facet_grid(Row ~ Column)
}

DeltaE_map_for_sheet(lab_colors_master_shape_sheet_1)

#### DeltaE for all Sheets ####

DeltaE_by_sheet <- list()

for (i in 1:max(lab_colors$Sheet)) {
  DeltaE_by_sheet[[i]] <- lab_colors_master_shape %>% 
    filter(Sheet == i) %>% 
    DeltaE_map_for_sheet()
}

do.call(grid.arrange, c(DeltaE_by_sheet, ncol = 4))
