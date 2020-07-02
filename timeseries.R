library(feasts)
library(tsibble)
library(tsibbledata) # Timeseries data examples
library(dplyr)
library(ggplot2)
library(lubridate)
library(fable)
library(fabletools)


# Data exploratory analysis -----------------------------------------------

# Datasets examples
aus_production %>% autoplot(Beer) # 1Q
vic_elec %>% autoplot(Demand) # 30-minutes

# Seasons
aus_production %>% gg_season(Beer) # Year decomposition

# Subseries
aus_production %>% gg_subseries(Beer)


# Decomposition -----------------------------------------------------------

# Is there seasonality?
## Lag plot
aus_production %>% gg_lag(Beer, geom = "point")

## Autocorrelation Factor
aus_production %>% ACF(Beer) %>% autoplot()

# Decomposition: classical
classical_dcmp <- aus_production %>%
  model(classical_decomposition(Beer, type = "additive")) %>%
  components()

classical_dcmp %>% autoplot()

# Decomposition: X11
x11_dcmp <- aus_production %>%
  model(x11 = feasts:::X11(Beer, type = "additive")) %>%
  components()

x11_dcmp %>% autoplot()

# SEATS
seats_dcmp <- aus_production %>%
  model(seats = feasts:::SEATS(Beer)) %>%
  components()

seats_dcmp %>% autoplot()

# Decomposition: STL
stl_dcmp <- aus_production %>%
  model(STL(Beer ~ trend(window = 7) + season(window = 'periodic'), robust = TRUE)) %>%
  components()

stl_dcmp %>% autoplot()


# Features extraction -----------------------------------------------------
tourism

tourism %>%
  features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x=trend_strength, y=seasonal_strength_year, col=Purpose)) +
  geom_point() + facet_wrap(vars(State))




# Forecast ----------------------------------------------------------------

stl_dcmp %>% autoplot()

# Filter
recent_production <- aus_production %>% filter(year(Quarter) >= 1990) # 1990 - 2010

# Simple models ---------------------------------------------------------------------

beer_train <- recent_production %>% filter(year(Quarter) <= 2007) # Train: 1990 - 2007

# Comparison between models
beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 12)

beer_fc %>%
  autoplot(filter(aus_production, year(Quarter) >= 1992), level = NULL) +
  labs(x = "Year", y = "Megalitres", color = "Forecast")


accuracy(beer_fc, recent_production)


# Seasonal Naive Model

beer_fit <- beer_train %>%
  model(
    `Seasonal naïve` = SNAIVE(Beer)
  )

beer_fit %>% gg_tsresiduals()



# Better models: ARIMA ----------------------------------------------------

# Decomposition
recent_production_dcmp <- recent_production %>%
  model(STL(Beer ~ trend(window = 7) + season(window = 'periodic'), robust = TRUE)) %>%
  components() %>%
  select(-.model) %>%
  as_tsibble()

recent_production_dcmp %>%
  autoplot(season_adjust)


fit <- recent_production_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(3,1,1) + PDQ(0,0,0))
  )


fit %>% gg_tsresiduals()

fit %>% forecast() %>% autoplot(recent_production_dcmp)






