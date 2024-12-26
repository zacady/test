
library(tidyverse)
library(ggimage)

############################
### Sample distributions ###
############################

my_samples <- rep(0,10) %>%
  imap(
    \(x, idx) tibble(
      sample = rep(x, each = 100),
      x = rnorm(100, x, 1)
    ) %>%
      ggplot(aes(x = x)) +
      geom_histogram(fill = "white", color = "black", bins = 10, lwd = 2) +
      theme_void() +
      theme(
        plot.background = element_rect(color = "black", linewidth = 1.5)
      )
  )

if(!dir.exists(here::here("images", "distributions"))) {
  dir.create(here::here("images", "distributions"))
}

for (i in 1:10) {
  ggsave(plot = my_samples[[i]], filename = paste0("sample_dist", i, ".png"), path = here::here("images", "distributions"), dpi = 500, bg = "#1078951A", width = 7, height = 5)
}

#################
### Main plot ###
#################

dt_curve <- tibble(
  x = seq(-2,2,.005),
  y = dnorm(x, mean = 0, sd = .55)
)

dt_circle1 <- tibble(
  x = c(-4, -3.8, -3.25, -2.3, -.9, .9, 2.3, 3.25, 3.8, 4),
  y = ((-sqrt((4^2)-(x^2)))/4)+.25,
  image = paste0(here::here("images", "distributions"), "/", dir(here::here("images", "distributions"))),
  sample_id = paste0("Sample ", 1:10),
  Mean_number = 1:10
)

dt_circle2 <- tibble(
  x1 = dt_circle1$x*.8,
  y1 = ((-sqrt((3.2^2)-(x1^2)))/4)+.25,
  x2 = dt_circle1$x*.6,
  y2 = ((-sqrt((2.4^2)-(x2^2)))/4)+.25
)

dt_labels <- tibble(
  x = case_when(
    (dt_circle2$x1+dt_circle2$x2)/2 < 0 & (dt_circle2$x1+dt_circle2$x2)/2 > - 2.5 ~ ((dt_circle2$x1+dt_circle2$x2)/2)+.12,
    (dt_circle2$x1+dt_circle2$x2)/2 > 0 & (dt_circle2$x1+dt_circle2$x2)/2 < 2.5 ~ ((dt_circle2$x1+dt_circle2$x2)/2)-.12,
    .default = (dt_circle2$x1+dt_circle2$x2)/2
  ),
  y = ifelse(
    ((dt_circle2$y1+dt_circle2$y2)/2) > -2,
    ((dt_circle2$y1+dt_circle2$y2)/2)-.05,
    ((dt_circle2$y1+dt_circle2$y2)/2)
  ),
  sample= latex2exp::TeX(paste0("$\\bar{X}_{", 1:10, "}$"), italic = T)
)

dt_points <- tibble(
  x = rep(4, 3),
  y = max(dt_circle1$y) + c(.2,.25,.3)
)

samp_dist_p <- dt_curve %>% 
  ggplot() +
  geom_line(data = dt_curve, aes(x,y), color = "#107895", lwd = 1) +
  geom_image(data = dt_circle1, aes(x,y, image = image), size = .14) +
  geom_segment(
    data = dt_circle2,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.25, "cm"), type = "closed", angle = 20)
  ) +
  geom_text(data = dt_circle1, aes(x = x, y = y+.11, label = sample_id), size = 3.2) +
  geom_text(
    data = dt_labels,
    aes(x,y,label = sample), parse = T
  ) +
  geom_point(data = dt_points, aes(x,y), size = 1.7) +
  xlim(c(-4.5, 4.5)) + ylim(c(-0.75, .8)) +
  theme_void()

samp_dist_p

ggsave(
  filename = "Sampling_dist.png", path = here::here("images"), dpi = 500,
  width = 7, height = 5, bg = "white"
)


