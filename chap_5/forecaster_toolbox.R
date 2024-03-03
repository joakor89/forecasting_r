# Data preparation (tidy)

gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

# Data visualisation (specify)

gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for sweden")

# Defining a model

TSLM(GDP_per_capita ~ trend())

# Train the model (estimate)

fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
fit

# Check model performance (evaluate)
# Produce forecast (forecast)

fit %>% forecast(h = "3 years")

fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

# Some Simple Forecasting Methods

bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

# Average method

bricks %>% model(MEAN(Bricks))

# Naïve method

bricks %>% model(NAIVE(Bricks))

# Seasonal naïve method
bricks %>% model(SNAIVE(Bricks ~ lag("year")))

# Drift method

bricks %>% model(RW(Bricks ~ drift()))

# Australian quarterly beer production

# Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")

# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasona naïve` = SNAIVE(Beer)
  )

# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)

# Plot forecast against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts. for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

# Google's daily closing stock price

library(tsibble) # Load tsibble library for time series tibbles

google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(gay = row_number()) %>%
  as_tsibble(index = Date, regular = TRUE) # Use Date as the index for the tsibble


# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015 <- google_stock %>%
  filter(year(Date) == 2015) %>%
  fill_gaps() # Fill gaps, by default fills with NA

# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close), 
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )

# Produce forecast for the trading days in January 2016

# Ensure google_jan_2016 is a tsibble and fill any gaps
google_jan_2016_filled <- google_jan_2016 %>%
  fill_gaps()

# Now attempt the forecast using the filled data
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016_filled)

# Plotting the forecast 
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autoplayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(JAN 2015 - JAN 2016)") +
  guide(colour = guide_legend(title = "forecast"))

# 5.3 Fitted Values & Residuals

# Residuals

augment(beer_fit)

# Residuals Diagnostics

# Google daily closing stock prices

autoplot(google_2015, Close) +
  labs(y = "$US",
       TITLE = "Google daily closing stock prices in 2015")

aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from Naïve method")

aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the Naïve method")

google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

aug %>% features(.innov, box_pierce, alg = 10, dof = 0)

aug %>% features(.innov, ljung_box, alg = 10, dof = 0)

fit <- google_2015 %>% model(RW(Close ~ Drift))
tidy(fit)

augment(fit) %>% features(.innov, ljung_box, lag = 10, dof = 1)

# Distribution Forecast & Prediction Intervals
# # Benchmark Method

google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo()

google_2015 %>% 
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015) +
  labs(title = "Google daily closing stock price", y ="$US")

# Prediction intervals from bootstrapped residuals

fit <- google_2015 %>%
  model(NAIVE(Close))

sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, color = as.factor(.rep)),
            data = sim) +
  labs(title = "Google daily closing stock price", y = "$US") +
  guides(col = FALSE)


ggplot(google_2015, aes(x = Date)) + 
  geom_line(aes(y = Close), color = "blue") + 
  geom_line(data = sim, aes(y = .sim, group = .rep, color = as.factor(.rep)), alpha = 0.5) + 
  labs(title = "Google daily closing stock price", y = "$US") +
  guides(color = FALSE) 

# Forecasting using transformation

# Bias Adjustments

prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)),
           level = 80, point_forecast = lst(mean, median)
           ) +
  labs(title = "Annual egg prices",
       y = "$US (in cent adjusted for inflation)" )

# Forecasting with Decomposition
# Employment in th US retail sector

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)

dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))

fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment) +
  labs(y = "Number of people",
       title = "Monthly US retail employment")

fit_dcmp %>% gg_tsresiduals()

# Evaluating Point Forecast Accuracy

# Functions to subset a time series
aus_production %>% filter(year(Quarter) >= 1995)

aus_production %>%
  slice(n()-19:0)

aus_retail %>%
  group_by(State, Industry) %>%
  slice(1:12)

# Scaled Errors: Example

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

beer_train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecast for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  ) 

google_fc <- google_fit %>%
  forecast(google_jan_2016)

library(tsibble)

google_jan_2016_filled <- google_jan_2016 %>%
  fill_gaps() 

google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016_filled)


google_fc %>%
  autoplot(bind_rows(google_2015, google_jan_2016),
           level = NULL) +
  labs(y = "$US",
       title = "Google clsoing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

# Evaluating Distributional Forecast Accuracy

# Quantile Scores

google_fc %>%
  filter(.model == "Naïve") %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level = 80) +
  labs(y = "$US",
       title = "Google closing stock prices")

google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

# Winkler Score

google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock,
           list(winkler = winkler_score), level = 80)

# Continuoes ranked probability score

google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

# Scale-free comparisons using skill scores
google_fc %>%
  accuracy(google_stock, list(skill = skill_score(CRPS)))

# Time Series Cross-Validation

google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)

# TSCV accuracy
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)

# Training set accuracy 
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  accuracy()

# Example:  Forecasting horizon accuracy with cross-validation
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)

fc <- google_2015 %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()

fc <- google_2015 %>%
  model(RW = RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  mutate(forecast_horizon = row_number())  # Rename 'h' to 'forecast_horizon' for clarity

fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()



