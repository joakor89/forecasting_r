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



































