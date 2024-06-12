display_color_card <- function(df, row_name = "Row", col_name = "Col", color_space = NaN, space_between = 1, circle_size=21) {
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
  adb <- 1
  corners <- df %>%
    filter((df[[row_name]] == 1 & df[[col_name]] == 1) |
             (df[[row_name]] == 1 & df[[col_name]] == max_col) |
             (df[[row_name]] == max_row & df[[col_name]] == 1) |
             (df[[row_name]] == max_row & df[[col_name]] == max_col))
  df_no_corners <- anti_join(df, corners, by = c(row_name, col_name))
  
  ggplot() +
    geom_tile(data = df_no_corners, aes_string(x = col_name, y = row_name, fill = "Color"), color = "white", size = space_between) +
    geom_point(data = corners, aes_string(x = col_name, y = row_name, fill = "Color"), shape = 21, color = "white", size= circle_size) +
    scale_fill_identity() +
    theme_void() +
    coord_fixed()
}

display_color_sheet <- function(df, color_sheet_idx = 1) {
  df_sheet <- lab_colors %>% filter(Sheet == color_sheet_idx)
  plots <- list()
  
  for (i in 1:nrow(df_sheet)) {
    row <- df_sheet[i, ]
    tiles <- data.frame()
    for (r in 1:8) {
      for (c in 1:8) {
        lab <- as.numeric(row[paste0(c("L", "a", "b"), r, c)])
        tiles <- rbind(tiles, data.frame(Row = r, Col = c, L = lab[1], a = lab[2], b = lab[3]))
      }
    }
    plots[[i]] <- display_color_card(tiles, row_name = "Row", col_name = "Col", color_space = "Lab", circle_size=3)
  }
  
  do.call(grid.arrange, c(plots, ncol = 6))
}

plot_card_vs_master <- function(card_colors, master_colors, channels = c("L", "a", "b")) {
  par(mfrow = c(length(channels), 1))
  for(channel in channels) {
    # Don't add dispersion since it doesn't significantly affect the visualization and can make the plot visually cluttered.
    plot(
      master_colors[, channel], type = "o", cex=1.5, lwd= 2, col = "red", pch=17, xlab="Colorspot", ylab = paste0(channel, "-value"), main = paste0(" Master vs. Card: Channel ", channel))
      lines(card_colors[, channel], type = "o", cex=1.5, lwd= 2, col = "blue", pch=19)
      legend("topleft", 
           legend = c("Master", "Measurement"), 
           col = c("red", "blue"),
           pch = c(17, 19),
           pt.cex = 1.5
    ) 
  }
}

plot_density_vs_master <- function(card_colors, master_colors, channels = c("L", "a", "b")) {
  for(channel in channels) {
    sd_val <- card_colors[, paste0(channel,"_sd")]
    bounds_data <- data.frame(
      upper_sd = card_colors[, channel] + sd_val,
      lower_sd = card_colors[, channel] - sd_val
    )
    
    p <- ggplot() +
      geom_density(data = master_colors, aes_string(x = channel, color = "'Master'"), size = 1) +
      geom_density(data = card_colors, aes_string(x = channel, color = "'Card'"), size = 1) +
      geom_density(data = bounds_data, aes_string(x = "upper_sd", color = "'Card Upper SD'"), linetype = "dashed") +
      geom_density(data = bounds_data, aes_string(x = "lower_sd", color = "'Card Lower SD'"), linetype = "dotted") +
      scale_color_manual(name = "",
                         values = c("Master" = "red", "Card" = "blue", "Card Upper SD" = "blue", "Card Lower SD" = "blue")) +
      labs(title = paste0("Density Plot of ", channel, "-value"), x = paste0(channel, "-value"), y = "Density") +
      theme_minimal() +
      guides(color = guide_legend(override.aes = list(size = 6)))
    print(p)
  }
}

plot_card_differences_to_master <- function(card_colors, master_colors) {
  card_colors %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Col, y = Row)) +
    geom_point(aes(x = Col+0.25, y = Row, color = Color), size = 12) +
    scale_color_identity() +
    geom_point(data = master_colors, aes(x = Ccol-0.25, y = Crow, color = Color), size = 12) +
    coord_fixed()
}

