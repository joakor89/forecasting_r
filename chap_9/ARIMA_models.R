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
 
 
 
 
 
 
 
 
 