library(tidyverse)
library(ggart)
library(ggforce)
library(tweenr)
library(viridis)

set.seed(101)

n <- 3 * 10
v <- rep(seq(1, n / 3), 3)
v2 <- v + n + 1
eps <- 225

df1 <- data.frame(
  index = 1:n,
  x = (sin(1:n))^1 + runif(n, -eps, eps),
  y = (sin(1:n))^1 + runif(n, -eps, eps),
  type = factor(v[order(v)]),
  point = rep(c('end', 'control', 'end'), n / 3)
)

df2 <- data.frame(
  index = 1:n,
  x = (sin(1:n)) + runif(n, -eps, eps),
  y = (sin(1:n)) + runif(n, -eps, eps),
  type = factor(v[order(v2)]),
  point = rep(c('end', 'control', 'end'), n / 3)
)

df <- list(df1, df2)

tf <- tween_states(df, tweenlength = 1, statelength = 0,
                   ease = "linear",
                   nframes = 10000) %>%
  mutate(type = paste(type, "_", .frame, sep = ""))

p <- ggplot() +
  geom_bezier(aes(x = x, y = y, group = type, frame = .frame),
              tf, alpha = 0.03, size = 0.03) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0)

ggsave("plots/plot004.png", p, width = 25, height = 25, units = "cm")