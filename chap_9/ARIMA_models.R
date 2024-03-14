# ARIMA Models

# Differencing
 google_2015 %>%
   mutate(diff_close = difference(Close)) %>%
   features(diff_close, ljung_box, lag = 10)
 
 # Seasonal Differencing
 
 PBS %>%
   filter(ATC2 == "H02") %>%
   summarise(Cost = sum(Cost)/1e6) %>%
   transmute(
     `Sales ($millions)` = Cost,
     `Log sales` = log(Cost),
     `Annual change in log sales` = difference(log(Cost), 12),
     `Double differenced log sales` = difference(difference(log(Cost), 12), 1)
   ) %>%
   pivot_longer(-Month, names_to = "Type", values_to="Sales") %>%
   mutate(
     Type = factor(Type, levels = c(
       "Sles ($million)",
       "Log sales",
       "Annual change in log sales",
       "Doubly differenced log sales"))
   ) %>%
   ggplot(aes(x = Month, y = Sales)) +
   geom_line() +
   facet_grid(vars(Type), scales = "free_y") +
   labs(title = "Corticosteroid drug sales", y = NULL)
 
###############################################
 
PBS <- as_tsibble(PBS, index = Month)
 
PBS_filtered <- PBS %>%
   filter(ATC2 == "H02") %>%
   summarise(Cost = sum(Cost)/1e6) %>%
   transmute(
     Month = Month, # Preserving Month for plotting; adjust based on your actual data
     `Sales ($millions)` = Cost,
     `Log sales` = log(Cost),
     `Annual change in log sales` = difference(log(Cost), 1), # Adjusted; consider your time series frequency
     `Double differenced log sales` = difference(difference(log(Cost), 1), 1) # Adjusted as above
   ) %>%
   pivot_longer(-Month, names_to = "Type", values_to = "Sales") %>%
   mutate(
     Type = factor(Type, levels = c(
       "Sales ($millions)",
       "Log sales",
       "Annual change in log sales",
       "Double differenced log sales")) # Corrected level names
   )
 
 # Plotting
 PBS_filtered %>%
   ggplot(aes(x = Month, y = Sales)) +
   geom_line() +
   facet_grid(vars(Type), scales = "free_y") +
   labs(title = "Corticosteroid drug sales", y = NULL)
 
# Unit root test

google_2015 %>%
  features(Close, unitroot_kpss)
 
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)
 
google_2015 %>%
  features(Close, unitroot_ndiffs)
 
aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover))

aus_total_retail %>%
  mutate(log_turnover = log(Turnover)) %>%
  features(log_turnover, unitroot_ndiffs)
 
aus_total_retail %>%
  mutate(log_turnover = difference(log(Turnover), 12)) %>%
  features(log_turnover, unitroot_ndiffs)

# Non seasonal ARIMA
# Egyption exports

global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) + 
  labs(y = "% of GDP", title = "Egyptian Exports")
 
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))

report(fit)
 
fit %>% forecast (h = 10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# ACF PACF

global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()
 
global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot()
 
fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4, 0, 0)))

report(fit2)
 
# ARIMA Model in Fable
# Central African Republic Exports

global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')
 
cat_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))
 
cat_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")
 
 
cat_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")
 
glance(cat_fit) %>% arrange(AICc) %>% select(.model:BIC)
 
cat_fit %>%
  select(search) %>%
  gg_tsresiduals()
 
augment(cat_fit) %>%
  filter(.model =='search') %>%
  features(.innov, ljung_box, lag = 10, dof =3)
 
cat_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)
 
gg_arma(cat_fit)

# ACF/PACF
# Monthly US leisure & hospitality employment

leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure & hospitality",
       y="Number of people (millions)")

leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag = 36) +
  labs(title = "Double differenced", y="")

leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type='partial', lag = 36) +
  labs(title = "Double differenced", y="")
 
fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
 
fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Ordes")

glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

fit %>% select(auto) %>% gg_tsresiduals(lag=36)

augment(fit) %>% features(.innov, ljung_box, lag=24, dof=4)

forecast(fit, h=36) %>%
  filter(.model=='auto') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

# Corticosteroid drug sales in Australia

h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)

h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

h02 %>% gg_tsdisplay(difference(log(Cost), 12),
                     plot_type='partial', lag_max = 24)

fit <- h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
 
fit %>% gg_tsresiduals(lag_max=36)

# Test set evaluation:
h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) %>%
  forecast() %>%
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")

# Comapring ARIMA() & ETS() on non-seasonal data

aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, RMSE:MAPE)

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")

# Comapring ARIMA() & ETS() on seasonal data

cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)
train <- cement %>% filter_index(. ~ "2007 Q4")

fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)

fit_arima %>% gg_tsresiduals(lag_max = 16)

augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

fit_ets <- train %>% model(ETS(Cement))
report(fit_ets)

fit_ets %>%
  gg_tsresiduals(lag_max = 16)

augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

# Generate forecasts & compare accuracy over the test set
bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = 10) %>% accuracy(cement),
  fit_ets %>% forecast(h = 10) %>% accuracy(cement)
) %>%
  select(-ME, -MPE, -ACF1)

cement %>%
  model(ARIMA(Cement)) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title="Cement production in Australia",
       y = "Tonnes ('000)")

