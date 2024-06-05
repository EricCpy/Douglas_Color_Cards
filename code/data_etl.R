#### master color data ####

master_colors <- read.csv2("data/MasterColorCard.csv")
master_colors <- master_colors %>%
  rename(
    C = p1,
    M = p2,
    Y = p3,
    K = p4,
    S = p5
  )

#### sample color measurements ####

lab_colors <- read.csv2("data/LabMeasurements-Color-Card.csv")
# every observation is one color card
# 13 big color sheets that means ROW 1 COL 1 in idx 1 = first color card of first color sheet  
# ROW 2 COL 1 in idx 14 = second color card of first color sheet and so on
lab_colors$Sheet <- rep(1:13) # for better usage give color sheet idx

# get the data in L,a,b column format with field-number information
lab_colors_master_shape <- lab_colors %>%
  pivot_longer(cols = -c("Row", "Column", "Sheet")) %>% 
  separate_wider_regex(
    cols = name, c(Type = "[a-zA-Z]", Crow = "[0-9]", Ccol = "[0-9]")
  ) %>% 
  pivot_wider(names_from = Type, values_from = value) %>% 
  mutate(
    Crow = as.numeric(Crow),
    Ccol = as.numeric(Ccol),
    Field = (Crow-1)*8+Ccol
  )

cards_per_sheet <- max(lab_colors_master_shape$Row)*max(lab_colors_master_shape$Column)
n_sheets <- max(lab_colors$Sheet)

color_differences <- dE(
    master_colors %>% attach_replicas_to_df_by_rows(cards_per_sheet*n_sheets) %>% select("L", "a", "b"), 
    lab_colors_master_shape %>% select("L", "a", "b")
  ) %>% 
  data.frame(Difference = .) %>% 
  bind_cols(lab_colors_master_shape) %>% 
  rowwise() %>% 
  mutate(Color = lab_to_rgb(L, a, b))
