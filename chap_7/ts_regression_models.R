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

# Goodness-of-Fit































