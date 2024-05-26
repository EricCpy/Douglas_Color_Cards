library(dplyr)
library(ggplot2)
library(colorspace)
library(grDevices)
library(tidyr)

lab_colors <- read.csv2("LabMeasurements-Color-Card.csv")
# every observation is one color card
# 13 big color sheets that means ROW 1 COL 1 in idx 1 = first color card of first color sheet  
# ROW 2 COL 1 in idx 14 = second color card of first color sheet and so on
lab_colors$Sheet <- rep(1:13) # for better usage give color sheet idx

master_colors <- read.csv2("MasterColorCard.csv")
master_colors <- master_colors %>%
  rename(
    C = p1,
    M = p2,
    Y = p3,
    K = p4,
    S = p5
  )

cmyk_to_rgb <- function(c, m, y, k) {
  c <- c / 100
  m <- m / 100
  y <- y / 100
  k <- k / 100
  
  r <- (1 - c) * (1 - k)
  g <- (1 - m) * (1 - k)
  b <- (1 - y) * (1 - k)
  
  rgb(r, g, b)
}

lab_to_rgb <- function(l, a, b) {
  # clips sRGB and uses D65 white point
  rgb(convertColor(c(l, a, b), from = "Lab", to = "sRGB"))
}

display_color_card <- function(df, row_name = "Row", col_name = "Col", color_space = NaN, space_between = 1) {
  if (!all(c(row_name, col_name) %in% colnames(df))) {
    stop("Dataframe must contain " + row_name + " and " + col_name + " columns")
  }
  
  if(color_space != "Lab" && all(c("C", "M", "Y", "K") %in% colnames(df))) {
    df$Color <- mapply(cmyk_to_rgb, df$C, df$M, df$Y, df$K)
  } else if (color_space != "CYMK" && all(c("L", "a", "b") %in% colnames(df))) {
    df$Color <- mapply(lab_to_rgb, df$L, df$a, df$b)
  } else {
    stop("Dataframe must contain either 'C', 'M', 'Y', 'K' columns or 'L', 'a', 'b' columns")
  }
  
  max_row <- max(df[[row_name]])
  max_col <- max(df[[col_name]])
  corners <- df %>%
    filter((.[[row_name]] == 1 & .[[col_name]] == 1) |
             (.[[row_name]] == 1 & .[[col_name]] == max_col) |
             (.[[row_name]] == max_row & .[[col_name]] == 1) |
             (.[[row_name]] == max_row & .[[col_name]] == max_col))
  df_no_corners <- anti_join(df, corners, by = c(row_name, col_name))
  
  ggplot() +
    geom_tile(data = df_no_corners, aes_string(x = col_name, y = row_name, fill = "Color"), color = "white", size = space_between) +
    geom_point(data = corners, aes_string(x = col_name, y = row_name, fill = "Color"), shape = 21, color = "white", size = 20) +
    scale_fill_identity() +
    theme_void() +
    coord_fixed()
}

display_color_card(master_colors, "Crow", "Ccol", "CMYK", 2)
display_color_card(master_colors, "Crow", "Ccol", "Lab", 2)
# color sheet display function 
# how does the column S in master influence CMYK values

