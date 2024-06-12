source("code/setup.R")

#### DeltaE for single Card ####

color_differences %>% 
  filter(Sheet == 1, Row == 1, Column == 1) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Ccol, y = Crow, color = Difference > 2),
            lwd = 1.5, linetype = 1, width=0.9, height=0.9) +
  ggnewscale::new_scale_colour() +
  geom_point(aes(x = Ccol+0.175, y = Crow, color = Color), size = 14) +
  scale_color_identity() +
  geom_point(
    data = master_colors %>% rowwise() %>% mutate(Color = lab_to_rgb(L, a, b)),
    aes(x = Ccol-0.175, y = Crow, color = Color), size = 14
    ) +
  coord_fixed()

#### DeltaE for a Sheet ####

DeltaE_map_for_sheet <- function(data) {
  data %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Ccol, y = Crow)) +
    coord_fixed() +
    facet_grid(Row ~ Column)
}

DeltaE_map_for_sheet(color_differences %>% filter(Sheet == 1))

#### DeltaE for all Sheets ####

DeltaE_by_sheet <- list()

for (i in 1:n_sheets) {
  DeltaE_by_sheet[[i]] <- color_differences %>% 
    filter(Sheet == i) %>% 
    DeltaE_map_for_sheet() + 
    guides(fill = "none") +
    theme_void()
}

do.call(grid.arrange, c(DeltaE_by_sheet, ncol = 5))
