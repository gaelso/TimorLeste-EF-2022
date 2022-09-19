
library(tidyverse)
library(ggrepel)


tt <- read_csv("data/NFI optimization/oNFI-results.csv")
tt

tt %>%
  filter(nest2_radius == 10) %>%
  filter(distance_multiplier == 2) %>%
  ggplot(aes(x = total_time, y = cv)) +
  geom_point(aes(
    color = nest1_radius, 
    shape = as.character(subplot_count),
    size = n_plot
    )) +
  #geom_label_repel(aes(label = nest1_radius)) +
  scale_color_viridis_c() +
  theme_bw()


tt %>%
  filter(
    nest1_radius == 20,
    nest2_radius == 10,
    distance_multiplier == 2,
    ) %>%
  ggplot(aes(x = total_time, y = cv)) +
  geom_point() +
  geom_label_repel(aes(label = subplot_count)) +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  theme_bw()
  
tt %>%
  filter(
    nest1_radius == 20,
    nest2_radius == 10,
    distance_multiplier == 2,
  ) %>%
  ggplot(aes(x = total_time, y = cv)) +
  geom_point() +
  geom_label_repel(aes(label = subplot_count)) +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  theme_bw()



tt2 <- tt %>%
  filter(
    nest1_radius == 20,
    nest2_radius == 10,
    distance_multiplier == 2
    )

## Dual axis
axis_coeff1 <- (max(tt2$cv) - min(tt2$cv)) / (max(tt2$total_time) - min(tt2$total_time))
axis_coeff2 <- max(tt2$cv) - axis_coeff1 * max(tt2$total_time)

ggplot(tt2, aes(x = subplot_count)) +
  geom_line(aes(y = cv)) +
  geom_line(aes(y = total_time * axis_coeff1 + axis_coeff2), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(tt2$subplot_count), max(tt2$subplot_count), 1)) +
  scale_y_continuous(
    name = "----- CV (%)",
    #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
    sec.axis = sec_axis(~ (. - axis_coeff2) / axis_coeff1, name = "- - - Time (months)")
  ) +
  labs(x = "Number of subplots")
  

axis_coeff <- max(tt2$cv) / max(tt2$total_time)

ggplot(tt2, aes(x = subplot_count)) +
  geom_line(aes(y = cv)) +
  geom_line(aes(y = total_time * axis_coeff), linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(tt2$subplot_count), max(tt2$subplot_count), 1)) +
  scale_y_continuous(
    name = "----- CV (%)",
    #limits = c(rv$results$cv_limit$min, rv$results$cv_limit$max),
    sec.axis = sec_axis(~ ./axis_coeff1, name = "- - - Time (months)")
  ) +
  labs(x = "Number of subplots")


ggplot(tt2, aes(x = total_time, y = cv)) +
  geom_point(aes(color = subplot_count)) 


ggplot(tt2, aes(x = total_time, y = cv/min(cv))) +
  geom_point(aes(color = subplot_count)) +
  scale_x_continuous(breaks = seq(min(tt2$subplot_count), max(tt2$subplot_count), 1))



##
## CV total toime model 1 ######################################################
##

lm1 <- lm(log(cv) ~ log(total_time), data = tt2)

coef(lm1)
predict(lm1)
tt3 <- tt2 %>%
  mutate(cv_pred = exp(coef(lm1)[1]) * total_time^coef(lm1)[2])

ggplot(tt3, aes(x = total_time)) +
  geom_point(aes(y = cv, color = subplot_count)) +
  geom_line(aes(y = cv_pred))


lm1 <- lm(cv ~ log(total_time), data = tt2)


coef(lm1)
predict(lm1)
tt3 <- tt2 %>%
  mutate(cv_pred = coef(lm1)[1] + log(total_time) * coef(lm1)[2])

ggplot(tt3, aes(x = total_time)) +
  geom_point(aes(y = cv, color = subplot_count)) +
  geom_line(aes(y = cv_pred))



##
## Select mid range point ######################################################
##

tt2 <- tt %>% filter(nest2_radius == 12, distance_multiplier == 2)

ggplot(tt2, aes(x = total_time, y = cv)) +
  geom_point(aes(
    color = n_plot, 
    fill = n_plot,
    shape = as.character(subplot_count),
    size = nest1_radius
  )) +
  #geom_label_repel(aes(label = nest1_radius)) +
  scale_color_viridis_c(alpha = 0.8) +
  scale_fill_viridis_c(alpha = 0.8) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25, 8, 9, 7)) +
  theme_bw()

mid_cv   <- median(tt2$cv)
mid_time <- median(tt2$total_time)

tt3 <- tt2 %>%
  mutate(
    dev_cv = abs((cv - mid_cv) / mid_cv),
    dev_time = abs((total_time - mid_time) / mid_time)
    )




