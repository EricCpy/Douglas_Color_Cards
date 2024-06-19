color_diff = color_differences %>%
  mutate(is_calibration = (Crow == 1 | Crow == 8 | Ccol == 1 | Ccol == 8)) %>%
  mutate(is_center = ((Crow == 4 | Crow == 5) & (Ccol == 4 | Ccol == 5))) %>%
  mutate(is_outlier = (Crow == 3 & Ccol == 2))

boxplot(total_diff ~ is_calibration, color_diff %>%
  filter(is_center == FALSE) %>%
  select(Difference, is_calibration, Row, Column, Sheet) %>% 
  group_by(is_calibration, Row, Column, Sheet) %>%
  summarise(total_diff = sum(Difference))
)

boxplot(total_diff ~ is_calibration, color_diff %>%
          filter(is_center == FALSE) %>%
          filter(is_outlier == FALSE) %>%
          select(Difference, is_calibration, Row, Column, Sheet) %>% 
          group_by(is_calibration, Row, Column, Sheet) %>%
          summarise(total_diff = sum(Difference))
)

boxplot(Difference ~ is_calibration, color_diff %>%
          filter(is_center == FALSE) %>%
          select(Difference, is_calibration, Row, Column, Sheet)
)

boxplot(Difference ~ is_calibration, color_diff %>%
          filter(is_center == FALSE) %>%
          filter(is_outlier == FALSE) %>%
          select(Difference, is_calibration, Row, Column, Sheet)
)
