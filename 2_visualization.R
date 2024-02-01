library(ggplot2)
library(dygraphs)


# Number of sessions per day ----------------------------------------------
sessions_summary

sessions_summary %>%
  ggplot(aes(x = date, y = n, color = Profile)) +
  geom_line(linewidth = 1) +
  # geom_point(aes(y = max_kWh)) +
  labs(x = "", y = "Number of sessions", title = "Test with {ggplot2}") +
  scale_x_datetime(date_labels = "%d %b", date_breaks = "3 days") +
  # facet_wrap(vars(Profile))
  theme_dark()


# Each profile a column
sessions_summary_spread <- sessions_summary %>%
  select(date, Profile, n) %>%
  spread(Profile, n)

sessions_summary_spread

# Fill the gaps
sessions_summary_spread %>%
  replace_na(list(Dinner = 0, Home = 0, Pillow = 200, Worktime = 1000))
  # replace(is.na(.), 0)
  # mutate_all(replace_na, 0)
  # mutate_if(is.numeric, replace_na, 0)
  # mutate_all(`*`, 100)
  # mutate_all(`+`, 100)


# Statistics --------------------------------------------------------------

# Histogram
sessions_models %>%
  ggplot(aes(x = Energy)) +
  geom_histogram(binwidth = 0.5)

# Density 1 variable
sessions_models %>%
  ggplot(aes(x = Energy)) +
  geom_histogram(aes(y = stat(density)), binwidth = 1) +
  geom_density(color = "blue", size = 1) +
  facet_wrap(~ Profile)

# Density 2 variables
sessions_models %>%
  ggplot(aes(x = ConnectionStartDateTime, y = ChargingHours)) +
  stat_density2d(geom = "polygon", aes(fill = stat(nlevel)), bins = 30) +
  xlab("\nSession start time") + ylab("Number of connection hours\n") +
  theme_light()

# Regression
random_data <- tibble(
  x = 1:10,
  y = c(0, 6, 9, 14, 21, 26, 30, 34, 38, 45)
)

random_plot <-  random_data %>%
  ggplot(aes(x, y)) +
  geom_line(size = 1)

random_plot

random_plot +
  stat_smooth(method = "lm", formula = y ~ poly(x, 1), fullrange = T) +
  scale_x_continuous(limits = c(0, 20))

# Regression model
random_model <- lm(
  formula= y ~ x,
  data = random_data
)
random_model

predict(
  random_model,
  tibble(x = c(20, 50, 100))
)



# Interactive plots -------------------------------------------------------

# Plotly
library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig



# Dygraphs
df_to_ts <- function(df) {
  xts::xts(df[-1], order.by = df[[1]]) # Last item of a function is returned
}

sessions_summary_spread %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  df_to_ts() %>%
  dygraph(main = "Test with {dygraphs}", ylab = "Number of session or kWh") %>%
  dyOptions(fillGraph = T, stepPlot = T, drawPoints = T, useDataTimezone = T)





