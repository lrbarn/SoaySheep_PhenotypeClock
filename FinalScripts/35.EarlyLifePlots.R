#### 35. Combining early life plots####
## need to run the plots from 34a and b to have this
library(patchwork)

p3 <- PLOT_earlyAdult_birthWeight_f
p4 <- PLOT_earlyAdult_birthWeight_m
p1 <- PLOT_earlyAll_birthWeight_f
p2 <- PLOT_earlyAll_birthWeight_m

patch <- (p1 | p2) / (p3 | p4)
patch2 <- p1 + p2 +
  plot_layout(axis_titles = "collect", axes = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ")") &

  labs(x = NULL,
       y = "Delta Age")
patch3 <- p3 + p4 +
  plot_layout(axis_titles = "collect", axes = "collect") +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ")") &
  
  labs(x = "Birth Weight (Residuals)",
       y = "Delta Age")

patch +
  plot_layout(axis_titles = "collect", axes = "collect") &
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ")") &

  labs(x = "Birth Weight (Residuals)",
       y = " Delta Age")



patch2 / patch3 +
  plot_annotation(tag_levels = 'A',
                  tag_suffix = ")") &
  xlim(-1.3, 1.6) &
  ylim(-10, 5)



