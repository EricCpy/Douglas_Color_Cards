
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

df_input <- bootstrap_results %>% mutate(group = str_c(formula, method))
quantiles <- df_input %>% group_by(formula, method) %>% summarise(q25 = quantile(value, .25), q75 = quantile(value, .75))

p <- ggplot(df_input, aes(x=formula,y=value, fill = group)) + stat_ydensity() + scale_y_continuous(limits = c(0, 10), 
                                                                                                oob = scales::oob_keep)

# all you need for the gradient fill
vl_fill <- ggplot_build(p)$data[[1]] %>% select(x, violinwidth, y, group) %>% 
  mutate(
    xnew = x - mywidth * violinwidth, 
    xend = x + mywidth * violinwidth,
    xnewlast = lag(xnew, 1),
    xendlast = lag(xend, 1),
    ylast = lag(y, 1),
    
  ) %>% 
  rowwise() %>% 
  mutate(col = if_else(y >= quantiles$q25[group] && y <= quantiles$q75[group], "ja", "nein")) %>% 
  
  
  ggplot() +
  geom_segment(data = vl_fill, aes(x = xnew, xend = xend, y = y, yend = y, color = col), show.legend = FALSE, linewidth = 1) +
  # geom_rect(data = vl_fill, aes(xmin = xnew, xmax = xend, ymin = ylast, ymax = y, fill = col)) + 
  # Re-use geom_violin to plot the outline
  geom_violin(data = df_input, aes(x = as.integer(formula), y = value, group = group),
              color = "black", alpha = 0, draw_quantiles = c(0.25, 0.5, 0.75),
              show.legend = FALSE) +
  scale_color_manual(values = c("#FF00FF", "#FFFFFF00")) +
  scale_fill_manual(values = c("#FF00FF", "#FFFFFF00")) +
  labs(x = "Cut", y = "Carat") +
  coord_cartesian(ylim=c(0,10))

##### https://stackoverflow.com/questions/36203195/fill-specific-regions-in-geom-violin-plot

library(ggplot2)
# library(plyr)

#Data setup
set.seed(123)
dat <- bootstrap_results %>% mutate(group = str_c(formula, method)) %>% dplyr::rename(y = "value")

p <- ggplot() + 
  geom_violin(data = dat,aes(x = factor(formula),y = y, fill = group))
p_build <- ggplot2::ggplot_build(p)$data[[1]]

#This comes directly from the source of geom_violin
p_build <- p_build %>% mutate(
                     xminv = x - violinwidth * (x - xmin),
                     xmaxv = x + violinwidth * (xmax - x)
                     )

p_build <- rbind(plyr::arrange(transform(p_build, x = xminv), y),
                 plyr::arrange(transform(p_build, x = xmaxv), -y))

quantiles <- dat %>% group_by(formula, method) %>% summarise(q25 = quantile(y, .25), q75 = quantile(y, .75))
p_build <- p_build %>% mutate(
                     c25 = quantiles$q25[group],
                     c75 = quantiles$q75[group],
                     fill_group = if_else((lag(y,1) >= c25 | lead(y,1) >= c25) & y <= c75,'ja','nein'),
                     group1 = str_c(group, fill_group)
)

#Note the use of the group aesthetic here with our computed version,
# group1
p_fill <- ggplot() + 
  scale_fill_manual(values = c("#FF00FF", "#FFFFFF00")) +
  geom_polygon(data = p_build,
               aes(x = x,y = y,group = group1,fill = fill_group))+
  ggnewscale::new_scale_fill() +
  geom_violin(data = dat, aes(x = as.integer(formula), y = y, fill = group), show.legend = F, alpha=0, draw_quantiles = c(0.25, 0.5, 0.75)) +
  coord_cartesian(ylim = c(0,10))
p_fill

#####

set.seed(20160229)

my_data = data.frame(
  y=c(rnorm(1000), rnorm(1000, 0.5), rnorm(1000, 1), rnorm(1000, 1.5)),
  x=c(rep('a', 2000), rep('b', 2000)),
  m=c(rep('i', 1000), rep('j', 2000), rep('i', 1000))
)

pdat <- my_data %>%
  group_by(x, m) %>%
  do(data.frame(loc = density(.$y)$x,
                dens = density(.$y)$y))

pdat$dens <- ifelse(pdat$m == 'i', pdat$dens * -1, pdat$dens)
pdat$dens <- ifelse(pdat$x == 'b', pdat$dens + 1, pdat$dens)

ggplot(pdat, aes(dens, loc, fill = m, group = interaction(m, x))) + 
  geom_polygon() +
  scale_x_continuous(breaks = 0:1, labels = c('a', 'b')) +
  ylab('density') +
  theme_minimal() +
  theme(axis.title.x = element_blank())
