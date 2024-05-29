master_colors[, c("L", "a", "b")]
DeltaE(as.matrix(master_colors[1, c("L", "a", "b")]), as.matrix(master_colors[2, c("L", "a", "b")]), metric = 2000)

# does the order/direction of the squares corresponds into those of the cards?
dE(
  master_colors[, c("L", "a", "b")], 
  lab_colors_master_shape %>% filter(Sheet == 1, Row == 1, Column == 1) %>% select("L", "a", "b")
  ) %>% 
  data.frame(Difference = .) %>% 
  bind_cols(master_colors) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) 

lab_colors_master_shape_sheet_1 <- lab_colors_master_shape %>% 
  filter(Sheet == 3)

#### not clean ####
add_day <- function(day, df) {
  df$day <- day
  df
}

day_df <- purrr::map_dfr(
  1:(max(lab_colors$Row)*max(lab_colors$Column)),
  add_day,
  df = master_colors
)

dE(
  day_df[, c("L", "a", "b")], 
  lab_colors_master_shape_sheet_1 %>% select("L", "a", "b")
) %>% 
  data.frame(Difference = .) %>% 
  bind_cols(lab_colors_master_shape_sheet_1) %>% 
  ggplot() +
  geom_tile(aes(fill = Difference, x = Crow, y = Ccol)) +
  facet_grid(Row ~ Column)
