# Time Series Regression Models

# The Linear Model
# US consumption expenditure

us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
                 autoplot(value) +
                 labs(y= "% change")


us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)", 
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Estimating equation with TSLM() function:
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  report()

# Multiple Linear Regression
# US consumption expenditure

us_change %>%
  GGally::ggpairs(columns = 2:6)

# Least Squares Estimation
# US consumption expenditure

fit_consMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production +
                      Unemployment + Savings))

report(fit_consMR)

# Fitted Values
library(ggplot2)

augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
       title = "Percent change in US consumption expenditure") +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consMR) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US cosumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

# ACF plot of Residuals
# Histogram Residuals
fit_consMR %>% gg_tsresiduals()

augment(fit_consMR) %>%
  features(.innov, ljung_box, lag = 10, daf = 5)


# Residual Plots against Predictors
us_change %>%
  left_join(residuals(fit_consMR), by = "Quarter") %>%
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Residuals Plots against Fitted Values
augment(fit_consMR) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

#Spurious Regression
fit <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  left_join(guinea_rice, by = "Year") %>%
  model(TSLM(Passengers ~ Production))

report(fit)

fit %>% gg_tsresiduals()

# Seasonal Dummy Variables

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))

report(fit_beer)

augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

augment(fit_beer) %>%
  ggplot(aes(x = "Beer", y = .fitted,
         colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

# Fourier Series
fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K = 2)))

report(fourier_beer)

# Selecting Predictors

glance(fit_consMR) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

# Forecasting with Regression
# Ex-ante vs Ex-post Forecast: Australian quarterly beer production

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))

fc_beer <- forecast(fit_beer)
fc_beer %>%
  autoplot(recent_production) +
  labs(
    title = "Forecast of beer production using regression",
    y = "megalitres"
  )

# Scenario based forecasting

fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease=new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenarios")

fc <- forecast(fit_consBest, new_data = future_scenarios)
us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title="US cosumption", y = "% change")

# Building a Predictive Regression Model

# Prediction Intervals

fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))

new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) %>%
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) %>%
    mutate(Income = 12),
  names_to = "Scenario"
)

fcast <- forecast(fit_cons, new_cons)

us_change %>%
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

# Forecasting a Non-Linear Trend
# Boston Marathon Winning

boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

fit_trends <- boston_men %>%
  model(
    linear = TSML(Minutes ~ trend()),
    exponential = TSML(log(Minutes) ~ trend()),
    piecewise = TSML(Minutes ~ trend(knots = c(1950, 1980)))
  )

fc_trends <- fit_trends %>% forecast(h = 10)

boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

# Alternative

library(fable)
library(tsibble)
library(dplyr)
library(ggplot2)

boston_men <- boston_marathon %>%
  filter(Year >= 1924, Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time) / 60) %>%
  as_tsibble(index = Year)

# Creating indicator variables for piecewise regression
boston_men <- boston_men %>%
  mutate(after_1950 = pmax(Year - 1950, 0),
         after_1980 = pmax(Year - 1980, 0))

fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend() + after_1950 + after_1980)
  )

fc_trends <- fit_trends %>% forecast(h = 10)

# Plotting
boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5) +
  labs(y = "Minutes",
       title = "Boston Marathon Winning Times")























