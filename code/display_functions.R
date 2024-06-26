display_color_card <- function(df, row_name = "Row", col_name = "Col", color_space = NaN, space_between = 1, circle_size=19) {
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
      master_colors[, channel], type ="o", cex=1.5, lwd= 2, col = "red", pch=17, xlab="Colorspot", ylab = paste0(channel, "-value"), main = paste0(" Master vs. Card: Channel ", channel))
      lines(card_colors[, channel], type ="o", cex=1.5, lwd= 2, col = "blue", pch=19)
      legend("topleft", 
           legend = c("Master", "Measurement"), 
           col = c("red", "blue"),
           pch = c(17, 19),
           pt.cex = 1.5
    ) 
  }
}

plot_density_vs_master <- function(card_colors, master_colors, channels = c("L", "a", "b"), n_bootstrap = 1000) {
  output_plots <- list()
  for(channel in channels) {
    density_quantiles <- card_colors %>%
      group_by(Sheet, Row, Column) %>%
      do(tidy(density(.[[channel]], from = min(card_colors[[channel]]), to = max(card_colors[[channel]])))) %>%
      ungroup() %>%
      rename(value = x, density = y) %>%
      group_by(value) %>%
      summarise(q025 = quantile(density, 0.025),
                q5   = quantile(density, 0.5),
                q975 = quantile(density, 0.975))
    
    master_density_df <- tidy(density(master_colors[[channel]], from = min(master_colors[[channel]]), to = max(master_colors[[channel]])))
    
    p <- ggplot() +
      geom_ribbon(data = density_quantiles, aes(x = value, ymin = q025, ymax = q975), alpha = 0.5, fill = "grey50") +
      geom_line(data = master_density_df, aes(x = x, y = y, color = "Master"), size = 1.5) +
      geom_line(data = density_quantiles, aes(x = value, y = q5, color = "Card"), size = 1.5, alpha = 0.8) +
      scale_color_manual(name = "",
                         values = c("Master" = "red", "Card" = "blue")) +
      labs(title = paste0("Density Plot of ", channel, "-value"), x = paste0(channel, "-value"), y = "Density") +
      theme_minimal() +
      guides(color = guide_legend(override.aes = list(size = 6)))
    
    output_plots[[channel]] <- p
  }
  output_plots
}

plot_card_differences_to_master <- function(card_colors, master_colors, spotsize = 11, spotdistance = 0.25, tilesize = 1, tilehighlightcolor = "#000000", lwd = 1) {
  card_colors %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Ccol, y = Crow),
              lwd = lwd, width=tilesize, height=tilesize) +
    scale_color_manual(values = c("#FFFFFF00", tilehighlightcolor)) +
    ggnewscale::new_scale_colour() +
    geom_point(aes(x = Ccol+spotdistance, y = Crow, color = Color), size = spotsize) +
    scale_color_identity() +
    geom_point(
      data = master_colors,
      aes(x = Ccol-spotdistance, y = Crow, color = Color), size = spotsize
    ) +
    coord_fixed() +
    xlab("Column") + ylab("Row") +
    theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"),
      plot.caption = element_text(size=12, hjust = 0)
    )
}

plot_correlation <- function(data, x_var, y_var) {
  cor_spearman <- cor(data[[x_var]], data[[y_var]], method = "spearman")
  cor_pearson <- cor(data[[x_var]], data[[y_var]], method = "pearson")
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste0("Correlation between ", x_var, " and ", y_var, "\nPearson: ", round(cor_pearson, 2), " Spearman: ", round(cor_spearman, 2)),
         x = paste0(x_var, "-value"), y = paste0(y_var, "-value"))
}


plot_correlation_with_categories <- function(data, x_var, y_var, exclude_below=1) {
  data$category <- ifelse(data[[y_var]] > exclude_below, "Above", "Below")
  # Total correlations
  cor_spearman_total <- cor(data[[x_var]], data[[y_var]], method = "spearman")
  cor_pearson_total <- cor(data[[x_var]], data[[y_var]], method = "pearson")
  # Correlations without "Low"
  data_high <- data[data$category == "Above", ]
  cor_spearman_high <- cor(data_high[[x_var]], data_high[[y_var]], method = "spearman")
  cor_pearson_high <- cor(data_high[[x_var]], data_high[[y_var]], method = "pearson")

  ggplot(data, aes_string(x = x_var, y = y_var, color = "category")) +
    geom_point() +
    geom_smooth(aes(group = 1, linetype = "Total"), method = "lm", se = FALSE, color = "black") +
    geom_smooth(data = data_high, aes(linetype = "Excluded"), method = "lm", se = FALSE, color = "blue") + 
    labs(title = paste0("Correlation between ", x_var, " and ", y_var, "\n",
                        "Total: Pearson: ", round(cor_pearson_total, 2), " Spearman: ", round(cor_spearman_total, 2), "\n",
                        "Exclude ", y_var, " < ", exclude_below, ": Pearson: ", round(cor_pearson_high, 2), " Spearman: ", round(cor_spearman_high, 2)),
         x = paste0(x_var, "-value"), y = paste0(y_var, "-value")) +
    theme_minimal() +
    scale_color_manual(values = c("Above" = "blue", "Below" = "red"), name = y_var) +
    scale_linetype_manual(values = c("Total" = "dotted", "Excluded" = "solid"), name = "Regression Line") +
    guides(color = guide_legend(order = 1), linetype = guide_legend(order = 2))
}

color_compar_tiles <- function(spotsize, spotdistance, tilesize = 1, tilehighlightcolor = "#000000", lwd = 1) {
  color_differences %>% 
    filter(Sheet == 1, Row == 1, Column == 1) %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Ccol, y = Crow),
              lwd = lwd, width=tilesize, height=tilesize) +
    scale_color_manual(values = c("#FFFFFF00", tilehighlightcolor)) +
    ggnewscale::new_scale_colour() +
    geom_point(aes(x = Ccol+spotdistance, y = Crow, color = Color), size = spotsize) +
    scale_color_identity() +
    geom_point(
      data = master_colors %>% rowwise() %>% mutate(Color = lab_to_rgb(L, a, b)),
      aes(x = Ccol-spotdistance, y = Crow, color = Color), size = spotsize
    ) +
    coord_fixed() +
    xlab("Column") + ylab("Row") +
    theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"),
      plot.caption = element_text(size=12, hjust = 0)
    )
}

DeltaE_map_for_sheet <- function(data) {
  data %>% 
    ggplot() +
    geom_tile(aes(fill = Difference, x = Ccol, y = Crow)) +
    coord_fixed() +
    theme(
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"),
      plot.caption = element_text(size=12, hjust = 0)
    )
}