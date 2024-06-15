cmyk_to_srgb <- function(c, m, y, k) {
  c <- c / 100
  m <- m / 100
  y <- y / 100
  k <- k / 100
  
  r <- (1 - c) * (1 - k)
  g <- (1 - m) * (1 - k)
  b <- (1 - y) * (1 - k)
  c(r, g, b)
}

cmyk_to_rgb <- function(c, m, y, k) {
  rgb <- cmyk_to_srgb(c, m, y, k)
  rgb(rgb[1], rgb[2], rgb[3])
}

cmyk_to_lab <- function(c, m, y, k) {
  convertColor(cmyk_to_srgb(c, m, y, k), from = "sRGB", to = "Lab")
}

lab_to_rgb <- function(l, a, b) {
  # clips sRGB and uses D65 white point
  rgb(convertColor(c(l, a, b), from = "Lab", to = "sRGB"))
}

dE <- function(colors1, colors2, metric = 2000) {
  DeltaE(as.matrix(colors1), as.matrix(colors2), metric = metric)
}

attach_replicas_to_df_by_rows <- function(data, n_rep) {
  add_rep_id <- function(id, df) {
    df$reP_id <- id
    df
  }
  
  purrr::map_dfr(
    1:n_rep,
    add_rep_id,
    df = data
  )
}

generate_color_difference_df <- function(master_colors, lab_colors) {
  color_difference <- dE(
    master_colors %>% select("L", "a", "b"),
    lab_colors %>% select("L", "a", "b"),
  )  %>% 
    data.frame(Difference = .) %>% 
    bind_cols(lab_colors) %>% 
    rowwise() %>% 
    mutate(Color = lab_to_rgb(L, a, b))  %>% 
    as.data.frame()
  
  color_difference
}


mean_lab_colors_for_sheets <- function(lab_colors_df, sheets) {
  filtered_lab_colors <- lab_colors %>% filter(Sheet %in% sheets)
  mean_lab_colors <- colMeans(filtered_lab_colors)
  sd_lab_colors <- sapply(filtered_lab_colors, sd)
  mean_cols_vertical <- data.frame()
  for (r in 1:8) {
    for (c in 1:8) {
      lab_mean <- as.numeric(mean_lab_colors[paste0(c("L", "a", "b"), r, c)])
      lab_sd <- as.numeric(sd_lab_colors[paste0(c("L", "a", "b"), r, c)])
      mean_cols_vertical <- rbind(mean_cols_vertical, data.frame(Row = r,
                                                                 Col = c, 
                                                                 L = lab_mean[1], 
                                                                 a = lab_mean[2], 
                                                                 b = lab_mean[3],
                                                                 L_sd = lab_sd[1],
                                                                 a_sd = lab_sd[2],
                                                                 b_sd = lab_sd[3]))
    }
  }
  mean_cols_vertical
}
