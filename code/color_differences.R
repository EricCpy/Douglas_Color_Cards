source("code/setup.R")

#### DeltaE for single Card ####

color_differences %>% 
  filter(Sheet == 1, Row == 1, Column == 1) %>% 
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

DeltaE_map_for_sheet <- function(data) {
  data %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
    coord_fixed() +
    facet_grid(Row ~ Column)
}

DeltaE_map_for_sheet(color_differences %>% filter(Sheet == 1))

#### DeltaE for all Sheets ####

DeltaE_by_sheet <- list()

for (i in 1:n_sheets) {
  DeltaE_by_sheet[[i]] <- color_differences %>% 
    filter(Sheet == i) %>% 
    DeltaE_map_for_sheet()
}

do.call(grid.arrange, c(DeltaE_by_sheet, ncol = 4))
