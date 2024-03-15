# Dynamic Regression Models

# Regression with ARIMA errors using fable

ARIMA(y ~ x + pdq(1,1,0))

# US Personal Consumption & Income Example

us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption & personal income",
       y = "Quarterly % change")

fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))

report(fit)

bind_rows(
  `Regression residuals` = as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` = as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) %>%
  mutate(
    type = factor(type, levels = c(
      "Regression residuals", "ARIMA residuals"))
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit %>% gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)

# Forecasting

# US Personal Consumption & Income
us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) + labs(y = "Percentage change")

# Forecasting electricity demand

vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximun daily temperature")

vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + ylab("")

fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday")))

fit %>%
   gg_tsresiduals()

augment(fit) %>%
  features(.innov, ljung_box, dof = 9, lag = 14)

vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title = "Daily electricity demand: Victoria",
      y="GW")

# Stochastic & deterministic trends
# Air transport passengers Australia

aus_airpassengers %>%
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")

# deterministic trend:
fit_deterministic <- aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() +
                                pdq(d = 0)))

report(fit_deterministic)

# stochastic trend:
fit_stochastic <- aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))

report(fit_stochastic)

aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fit_stochastic %>% forecast(h = 20),
             colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20),
             colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecast from trend models")

# Dynamic Harmonig Regression
# Australian eating out expenditure

aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, retaurants & takeaway food services",
    year(Month) %in% 2004:2008
  ) %>%
  summarise(Turnover = sum(Turnover))


fit <- model(aus_cafe,
             `k = 1` = ARIMA(log(Turnover) ~ fourier(k=1) + PDQ(0,0,0)),
             `k = 2` = ARIMA(log(Turnover) ~ fourier(k=2) + PDQ(0,0,0)),
             `k = 3` = ARIMA(log(Turnover) ~ fourier(k=3) + PDQ(0,0,0)),
             `k = 4` = ARIMA(log(Turnover) ~ fourier(k=4) + PDQ(0,0,0)),
             `k = 5` = ARIMA(log(Turnover) ~ fourier(k=5) + PDQ(0,0,0)),
             `k = 6` = ARIMA(log(Turnover) ~ fourier(k=6) + PDQ(0,0,0))
             )

##########
library(fable)
library(dplyr)
library(tsibble)

# Assuming insurance is already a tsibble with Month as the index.
# If not, you'd convert it with something like:
# insurance <- as_tsibble(insurance, index = Month)

fit <- insurance %>%
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1) + lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1) + lag(TVadverts, 2) + lag(TVadverts, 3))
  )

##########

fit %>% 
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = FALSE, fill = FALSE, level = FALSE) +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc =" , format(AICc))),
    data = glance(fit)
  ) +
  labs(title = "Total monthly eating-out expenditure",
       y="$ billions")

# TV advertising & insurance

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising & quotations")

fit <- insurance %>%
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + 
                   TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                   TVadverts + lag(TVadverts)) + lag(TVadverts, 2))
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts) + lag(TVadverts, 2) + lag(TVadverts, 3))

######
library(fable)
library(dplyr)
library(tsibble)
    

insurance <- insurance %>%
  as_tsibble(index = Month)

fit <- insurance_tsibble %>%
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1) + lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts + lag(TVadverts, 1) + lag(TVadverts, 2) + lag(TVadverts, 3))
  )
    
#####

insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising & quotations")

glance(fit)

fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) +
                TVadverts + lagTVadverts))

report(fit_best)

insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)

fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )



