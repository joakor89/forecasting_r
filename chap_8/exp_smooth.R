# Exponential Smoothing

# Simple exponential smoothing

algeria_economy <- global_economy %>%
  filter(Country == "Algeria")

algeria_economy %>%
  autoplot(Exports) + labs(y = "% of GDP", title = "Exports: Algeria")

# Optimisation: Algerian Export

fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

fc <- fit %>%
  forecast(h = 5)

fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col ="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title = "Exports: Algeria") +
  guides(colour = "none")

# Method with Trend
# Holts linear trend method

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

fit <- aus_economy %>%
  model(
    ANN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit %>% forecast(h = 10)

# Damped trend methods
# Autralian population (continued)

aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") + 
                            trend("A") + season("N")),
    `Demand Holt's method` = ETS(Pop ~ error("A") +
                                   trend("Ad", phi = 0.9) + season("N")) 
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

# Example: Internet Usage

www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  labs(x="Minute", y="Number of users",
       title= "Internet usage per minute")

# Applying Timeseries cross-validation

www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") + season("N")),
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)

fit <- www_usage %>%
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                                 season("N"))
  )

tidy(fit)

fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

# Methods with Seasonality
# Domestic overnight trips in Australia

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") +
                     season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") +
                           season("M"))
  )

fc <- fit %>% forecast(h = "3 years")
fc %>% 
  autoplot(aus_holidays, level = NULL) +
  labs(title = "Australian domestic torusim",
       y = "Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

# Holts-Winters damped method
#Holts-Winters method with daily data

sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count =sum(Count)/1000)

sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Souther Cross",
       y="Pedestrians ('000)")

# Estimation & Model Slection
# Model Selection

aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)

fit <- aus_holidays %>%
  model(ETS(Trips))

report(fit)

# states over time
components(fit) %>%
  autoplot() +
  labs(title = "E.T.S(M,N,A) components")

# Forecasting with ETS Models
fit %>%
  forecast(h = 8) %>%
  autoplot(aus_holidays) +
  labs(title = "Australian domestic tourism",
       y = "Overnight trips (millions)")
































