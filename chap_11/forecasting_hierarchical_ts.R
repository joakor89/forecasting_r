# Forecasting Hierarchical & Grouped Time Series

# Hierarchical Time Series
# Australian tourism hierarchy

tourism <- tsibble::tourism %>%
  mutate(State = recode(State,
                        `New South Wales` = "NSW",
                        `Northern Territory` = "NT",
                        `Queensland` = "QLD",
                        `South Australia` = "SA",
                        `Tasmania` = "TAS",
                        `Victoria` = "VIC",
                        `Western Australia` = "WA",
                        ))

tourism_hts <- tourism %>%
  aggregate_key(State / Region, Trips = sum(Trips))

tourism_hts

tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(y = "Trips ('000)",
       title = "Australian tourism: national and states") +
  facet_wrap(vars(State), scales= "free_y", ncol = 3) +
  theme(legend.position = "none")

# Grouped time series
# Australian prison population

library(readr)

prison <- readr::read_csv("https://Otexts.com/fpp3/extrafiles/prison_population.csv") %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(Gender, Legal, State, Indigenous),
             index = Quarter) %>%
  relocate(Quarter)

#############

prison_gts <- prison %>%
  aggregate_key(Gender * Legal * State, Count = sum(Count)/1e3)

prison_gts %>%
  filter(!is_aggregated(Gender), is_aggregated(Legal),
         is_aggregated(State)) %>%
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")

# Mixed hierarchical & group structure

tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

# Single Level Approaches
# The botton-up approach
tourism_states <- tourism %>%
  aggregate_key(State, Trips = sum(Trips))

fcast_state <- tourism_states %>%
  filter(!is_aggregated(State)) %>%
  model(ets = ETS(Trips)) %>%
  forecast()

fcast_national <- fcast_state %>%
  summarise(value = sum(Trips), mean = mean(value))

tourism_states %>%
  model(ets = ETS(Trips)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  forecast()

#####
data %>% aggregate_key() %>% model() %>%
  reconcile() %>% forecast()
#####

# Forecasting Australian Domestic Tourism

tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full %>%
  filter(year(Quarter) <= 2015) %>%
  model(base = ETS(Trips)) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method ="ols"),
    min = min_trace(base, method = "mint_shrink"),
  )

fc <- fit %>% forecast(h = "2 years")

fc %>%
  filter(is_aggregated(Region), is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")

fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scale = "free_y")

fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase))

# Reconciled Distributional Forecast
# forecasting Australian pison population

fit <- prison_gts %>% 
  filter(year(Quarter) <= 2014) %>%
  model(base = ETS(Count)) %>%
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )

fc <- fit %>% forecast(h = 8)

fc %>% 
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) %>%
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(y = "Number of prisoners ('000)",
       title = "Australian prison population (total)")

fc %>%
  filter(
    .model %in% c("base","MinT"),
    !is_aggregated(State), is_aggregated(Legal),
    is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by state)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust =))

fc %>%
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) %>%
  accuracy(data = prison_gts,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)
                           )
           ) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100)





