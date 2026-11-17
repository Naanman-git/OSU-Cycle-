 library(readxl); library(dplyr); library(tidyr)

d <- read_excel('Europe-Central-Asia_aggregated_data_up_to_week_of-2026-09-19.xlsx')
d$WEEK <- as.Date(d$WEEK)

ukr <- d |>
  filter(COUNTRY == 'Ukraine',
         EVENT_TYPE == 'Explosions/Remote violence',
         WEEK >= as.Date('2022-02-26'))

grid <- ukr |>
  group_by(ADMIN1, WEEK) |>
  summarise(n = sum(EVENTS), .groups = 'drop') |>
  complete(ADMIN1, WEEK = seq(as.Date('2022-02-26'), max(d$WEEK), by = 7),
           fill = list(n = 0))

grid |> group_by(ADMIN1) |> summarise(vmr = var(n) / mean(n))


# Sanity check on the grid
dim(grid); n_distinct(grid$ADMIN1); n_distinct(grid$WEEK)  # 6214 rows, 26 oblasts, 239 weeks
sum(grid$n)                                                 # 203,987
mean(grid$n == 0)                                           # about 0.40

# Check the time series
library(ggplot2)
ggplot(grid, aes(WEEK, n)) + geom_line() + facet_wrap(~ADMIN1, scales = 'free_y')


# Check against poisson alone
set.seed(1)
grid |> group_by(ADMIN1) |>
  mutate(sim_n = rpois(length(n), mean(n))) |>
  summarise(vmr_sim = var(sim_n) / mean(sim_n))

# limit time frame
grid |> filter(WEEK >= as.Date('2025-01-01'), WEEK <= as.Date('2025-12-31')) |>
  group_by(ADMIN1) |> summarise(vmr = var(n) / mean(n)) |>
  summarise(median(vmr, na.rm = TRUE))

# baseline fit and measure overdispersion
grid$year <- as.numeric(format(grid$WEEK, '%Y'))

fit_pois <- glm(n ~ ADMIN1 + factor(year), family = poisson, data = grid)
sum(residuals(fit_pois, type = 'pearson')^2) / df.residual(fit_pois)   # about 13

fit_nb <- MASS::glm.nb(n ~ ADMIN1 + factor(year), data = grid)
AIC(fit_pois, fit_nb)

# add last week's count
g2 <- grid |> arrange(ADMIN1, WEEK) |> group_by(ADMIN1) |>
  mutate(n_lag = lag(n)) |> ungroup() |> filter(!is.na(n_lag))

fit_a <- MASS::glm.nb(n ~ ADMIN1 + factor(year), data = g2)
fit_b <- MASS::glm.nb(n ~ ADMIN1 + factor(year) + log1p(n_lag), data = g2)
AIC(fit_a, fit_b)
coef(fit_b)['log1p(n_lag)']


# some more plots
# national weekly total
grid |> group_by(WEEK) |> summarise(n = sum(n)) |>
  ggplot(aes(WEEK, n)) + geom_line()

# the same panels on a log scale
ggplot(grid, aes(WEEK, log1p(n))) + geom_line() + facet_wrap(~ADMIN1)

# mean vs variance-to-mean ratio by oblast (log-log)
grid |> group_by(ADMIN1) |> summarise(m = mean(n), vmr = var(n) / mean(n)) |>
  ggplot(aes(m, vmr)) + geom_point() + scale_x_log10() + scale_y_log10()