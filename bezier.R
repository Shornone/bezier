library(tidyverse)
library(ggart)
library(ggforce)
library(tweenr)

set.seed(101)

n <- 3 * 100
v <- rep(seq(1, n / 3), 3)
v2 <- v + n + 1
eps <- 0.25

df1 <- data.frame(
  index = 1:n,
  x = (cos(1:n))^3 + runif(n, -eps, eps),
  y = (sin(1:n))^2 + runif(n, -eps, eps),
  type = factor(v[order(v)]),
  point = rep(c('end', 'control', 'end'), n / 3)
)

df2 <- data.frame(
  index = 1:n,
  x = (cos(1:n)) + runif(n, -eps, eps),
  y = (sin(1:n)) + runif(n, -eps, eps),
  type = factor(v[order(v2)]),
  point = rep(c('end', 'control', 'end'), n / 3)
)

df <- list(df1, df2)

tf <- tween_states(df, tweenlength = 1, statelength = 0,
                   ease = "linear",
                   nframes = 100) %>%
  mutate(type = paste(type, "_", .frame, sep = ""))

p <- ggplot() +
  geom_bezier(aes(x = x, y = y, group = type, frame = .frame), tf, alpha = 0.4, size = 0.05) +
  coord_equal() +
  theme_blankcanvas()

ggsave("plots/plot001.png", p, width = 25, height = 25, units = "cm")