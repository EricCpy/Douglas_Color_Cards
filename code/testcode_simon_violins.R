
#### from stackovefrflow ###

violincol <- function(x,quantiles=c(.25,.75),col='grey'){ 
  # browser()
  
  d <- density(x$x)
  right <- data.frame(x=d$y, y=d$x) #note flip of x and y, prevents coord_flip later
  from=c(-Inf,quantile(x$x, quantiles[1]),quantile(x$x, quantiles[2]))
  to=c(quantile(x$x, quantiles[1]),quantile(x$x, quantiles[2]),Inf)
  
  whichrange <- function(r,x){x <= r[2] & x > r[1]}
  ranges <- cbind(from,to)  
  
  right$col <- sapply(right$y,function(y){
    id <- apply(ranges,1,whichrange,y)
    if(all(id==FALSE)) NA else col[which(id)]
  })
  
  left <- right[nrow(right):1,]
  left$x <- 0 - left$x
  
  dat <- rbind(right,left)
  
  p <- ggplot(dat, aes(x=x,y=y)) +
    geom_polygon(data=dat,aes(fill=col),show.legend = F)+
    geom_path()+
    scale_fill_manual(values=col)
  return(p)
}


x <- bootstrap_results %>% rename(x = value)
violincol(x=x)
violincol(x=x,col=c('#00000000','red','#00000000'))
r <- seq(-5,5,0.5)
violincol(x=x,from=r,to=r+0.5,col=rainbow(length(r))) +
  coord_cartesian(ylim=c(0,10))

#### other stack overflox

p <- bootstrap_results %>%  ggplot() +
  geom_violin(aes(x = formula, fill = method, group=str_c(method, formula), y = value), alpha = 0) +
  geom_violin(aes(x = formula, fill = method, group=str_c(method, formula), y = value), draw_quantiles = c(.25, .5, .75)) +
  coord_cartesian(ylim=c(0,10))

mywidth <- .225 # bit of trial and error
diff_multiplier <- 5

p_build <- ggplot_build(p)
p_build$data[[1]] <- p_build$data[[1]] %>% rowwise() %>% mutate(
  alpha = if_else(y < 2, 1, 0)
)
p_gtable <- p_build %>% ggplot_gtable() 
grid::grid.newpage()
grid::grid.draw(p_gtable)

p_build$data[[1]] %>% summarise(mean = mean(y))

mean_range_value <- function (df, y_low, y_high, current_group = NULL) {
  df %>%
    mutate(y = value,
           group = as.integer(value),
           value = value) %>%
    select(y, group, value) %>%
    filter(y > y_low & y <= y_high & group == current_group) %>%
    pull(value) %>%
    mean(., na.rm = TRUE)
}

mean_range_value2 <- function(df, y_low, y_high, current_group = NULL) {
  if (length(y_low) != length(y_high)) stop("y_low and y_high don't have equal length")
  
  results <- list()
  for (i in seq_along(y_low)) {
    results[i] <- mean_range_value(df, y_low[i], y_high[i], current_group = current_group)
  }
  return(as.double(as.vector(results)))
}

df_input <- bootstrap_results 

vl_fill <- ggplot_build(p)$data[[1]] %>%
  mutate(xnew = x - mywidth * violinwidth, xend = x + mywidth * violinwidth) %>%
  group_by(group) %>%
  mutate(y_diff = c(diff(y, 1), last(diff(y, 1))) * diff_multiplier,
         y_low = y - y_diff/2,
         y_high = y + y_diff/2) %>%
  mutate(value = mean_range_value2(df_input, y_low, y_high, first(group))) %>%
  mutate(value = if_else(is.finite(value), value, NA))

breaks <- unique(as.integer(df_input$cut))
labels <- unique(df_input$cut)

ggplot() +
  geom_segment(data = vl_fill, aes(x = xnew, xend = xend, y = y, yend = y,
                                   color = value)) +
  geom_violin(data = df_input, aes(x = as.integer(formula), y = value, fill = method, group = str_c(method, formula)),
              alpha = 0, draw_quantiles = c(0.25, 0.5, 0.75),
              show.legend = FALSE) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  coord_cartesian(ylim=c(0,10))


####

library(tidyverse)
library(viridisLite)
theme_set(theme_light(base_size = 14))

df_input <- diamonds

p <- ggplot(df_input, aes(x=cut,y=carat)) + geom_violin()

mywidth <- .45 # bit of trial and error
diff_multiplier <- 5

mean_range_value <- function (df, y_low, y_high, current_group = NULL) {
  df %>%
    mutate(y = carat,
           group = as.integer(cut),
           value = price) %>%
    select(y, group, value) %>%
    filter(y > y_low & y <= y_high & group == current_group) %>%
    pull(value) %>%
    mean(., na.rm = TRUE)
}

mean_range_value2 <- function(df, y_low, y_high, current_group = NULL) {
  if (length(y_low) != length(y_high)) stop("y_low and y_high don't have equal length")
  
  results <- list()
  for (i in seq_along(y_low)) {
    results[i] <- mean_range_value(df, y_low[i], y_high[i], current_group = current_group)
  }
  return(as.double(as.vector(results)))
}

# all you need for the gradient fill
vl_fill <- data.frame(ggplot_build(p)$data) %>%
  mutate(xnew = x - mywidth * violinwidth, xend = x + mywidth * violinwidth) %>%
  group_by(group) %>%
  mutate(y_diff = c(diff(y, 1), last(diff(y, 1))) * diff_multiplier,
         y_low = y - y_diff/2,
         y_high = y + y_diff/2) %>%
  mutate(price = mean_range_value2(df_input, y_low, y_high, first(group))) %>%
  mutate(price = if_else(is.finite(price), price, NA))

breaks <- unique(as.integer(df_input$cut))
labels <- unique(df_input$cut)

ggplot() +
  geom_segment(data = vl_fill, aes(x = xnew, xend = xend, y = y, yend = y,
                                   color = price)) +
  geom_violin(data = df_input, aes(x = as.integer(cut), y = carat, fill = cut),
              #color = "grey", 
              alpha = 0, 
              #draw_quantiles = c(0.25, 0.5, 0.75),
              show.legend = FALSE) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  scale_color_viridis_c() +
  labs(x = "Cut", y = "Carat", color = "Price") +
  theme(legend.position = "bottom")

library(tidyverse)
library(viridisLite)

mywidth <- .225 # bit of trial and error

df_input <- bootstrap_results %>% mutate(group = as.integer(factor(str_c(formula, method))))
quantiles <- df_input %>% group_by(formula, method) %>% summarise(q25 = quantile(value, .25), q75 = quantile(value, .75))

p <- ggplot(df_input, aes(x=factor(formula),y=value, fill = method, group = group)) + stat_ydensity() + scale_y_continuous(limits = c(0, 10), 
                                                                                                                   oob = scales::oob_keep)

# all you need for the gradient fill
vl_fill <- ggplot_build(p)$data[[1]] %>% select(x, violinwidth, y, group, fill) %>% 
  mutate(
    xnew = x - mywidth * violinwidth, 
    xend = x + mywidth * violinwidth,
    xnewlast = lag(xnew, 1),
    xendlast = lag(xend, 1),
    ylast = lag(y, 1),
    
  ) %>% 
  rowwise() %>% 
  mutate(
    col = if_else(y >= quantiles$q25[group] && y <= quantiles$q75[group], "ja", "nein"),
    col = if_else(col == "ja", str_c(col, fill), col)
  )

ggplot() +
  geom_rect(data = vl_fill, aes(xmin = xnew, xmax = xend, ymin = ylast, ymax = y, fill = col), show.legend = FALSE) + 
  # Re-use geom_violin to plot the outline
  scale_fill_manual(values = c("#FF00FF", "#00FFFF", "#FFFFFF00")) +
  ggnewscale::new_scale_fill() +
  geom_violin(
    data = df_input, aes(
      x = as.integer(factor(formula)), y = value, fill = method, group = group), 
    draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0, show.legend = FALSE) + 
  labs(x = "model", y = "median dE") +
  coord_cartesian(ylim=c(0,10)) +
  scale_x_continuous(breaks = 1:3, labels = c("linear", "interaction 2nd order", "interaction 3nd order")) +
  ggnewscale::new_scale_fill()

ggplot() +
  geom_segment(data = vl_fill, aes(x = xnew, xend = xend, y = y, yend = y, color = col), show.legend = FALSE, linewidth = 1) +
  # Re-use geom_violin to plot the outline
  geom_violin(
    data = df_input, 
    aes(x = as.integer(factor(formula, levels = c("linear", "interaction 2nd order", "interaction 3nd order"))), y = value, fill = method, group = group), 
    draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0, show.legend = FALSE) + 
  scale_color_manual(values = c("#FF00FF", "#00FFFF", "#FFFFFF00")) +
  labs(x = "model", y = "median dE") +
  coord_cartesian(ylim=c(0,10)) +
  scale_x_continuous(breaks = 1:3, labels = c("linear", "interaction 2nd order", "interaction 3nd order"))

d <- density(bootstrap_results %>% filter(formula == "linear", method == "permutation") %>% pull(value), n = 1024)
approx(d$x, d$y, xout = c(0, 2, 4, 6))

##### https://stackoverflow.com/questions/36203195/fill-specific-regions-in-geom-violin-plot

library(ggplot2)
# library(plyr)

#Data setup

dat <- bootstrap_results %>% mutate(group = str_c(formula, method)) %>% dplyr::rename(y = "value")
quantiles <- dat %>% group_by(formula, method) %>% summarise(q25 = quantile(y, .25), q75 = quantile(y, .75))

p <- ggplot() + geom_violin(data = dat,aes(x = factor(formula),y = y, fill = method))
p_build <- ggplot2::ggplot_build(p)$data[[1]]

for (i in 1:6) {
  dx <- p_build %>% filter(group == i) %>% pull(y)
  dv <- p_build %>% filter(group == i) %>% pull(violinwidth)
  
  quartils <- c(quantiles$q25[i], quantiles$q75[i])
  
  violin_width_approx <- approx(dx, dv, xout = quartils)
  
  new_rows <- (p_build %>% filter(group == i))[1:2,]
  new_rows$violinwidth <- violin_width_approx$y
  
  dy <- p_build %>% filter(group == i) %>% pull(x)
  x_approx <- approx(dx, dy, xout = quartils)
  
  new_rows$x <- x_approx$y
  new_rows$y <- quartils
  
  p_build <- p_build %>% bind_rows(new_rows)
}

p_build <- p_build %>% 
  mutate(
    xminv = x - violinwidth * (x - xmin),
    xmaxv = x + violinwidth * (xmax - x)
    )

p_build <- bind_rows(
  arrange(p_build %>% mutate(x = xminv), y),
  arrange(p_build %>% mutate(x = xmaxv), -y)
  ) %>% 
  mutate(
    c25 = quantiles$q25[group],
    c75 = quantiles$q75[group],
    fill_group = if_else(y >= c25 & y <= c75,'ja','nein'),
    group1 = str_c(group, fill_group)
    )

#Note the use of the group aesthetic here with our computed version,
# group1
ggplot() + 
  scale_fill_manual(values = c("#FF00FF", "#00FFFF", "#FFFFFF00")) +
  # scale_fill_manual(values = c("#FF00FF", "#FFFFFF00")) +
  geom_polygon(data = p_build %>% filter(fill_group == "ja"),
               aes(x = x,y = y,group = group, fill = fill))+
  ggnewscale::new_scale_fill() +
  geom_violin(data = dat, aes(x = as.integer(factor(formula)), y = y, fill = group), show.legend = F, color="black", alpha=0, draw_quantiles = c(0.25, 0.5, 0.75)) +
  coord_cartesian(ylim = c(0,10))

#####

p_build <- ggplot2::ggplot_build(p)$data[[1]]

for (i in 1:6) {
  dx <- p_build %>% filter(group == i) %>% pull(y)
  dv <- p_build %>% filter(group == i) %>% pull(violinwidth)
  
  quartils <- c(quantiles$q25[i], quantiles$q75[i])
  
  violin_width_approx <- approx(dx, dv, xout = quartils)
  
  new_rows <- (p_build %>% filter(group == i))[1:2,]
  new_rows$violinwidth <- violin_width_approx$y
  
  dy <- p_build %>% filter(group == i) %>% pull(x)
  x_approx <- approx(dx, dy, xout = quartils)
  
  new_rows$x <- x_approx$y
  new_rows$y <- quartils
  
  p_build <- p_build %>% bind_rows(new_rows)
}

p_build %>% tail()

