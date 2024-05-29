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
