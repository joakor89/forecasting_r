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