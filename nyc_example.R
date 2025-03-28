library(tidyverse)
library(broom)

dat <- read_csv("ny.csv")
dat

## Make a plot of daily mortality
dat |>
    filter(!is.na(l1pm10tmean)) |>
    ggplot(aes(date, death)) +
    geom_point() +
    labs(x = "Date",
         y = "Daily mortality",
         title = "Daily Mortality for New York City, 1987--2005")

## Make a plot of daily PM10
dat |>
    ggplot(aes(date, l1pm10tmean)) +
    geom_point() +
    labs(x = "Date",
         y = "Daily PM10",
         title = "Daily PM10 for New York City, 1987--2005")

## Plot mortality vs. PM10
dat |>
    ggplot(aes(l1pm10tmean, death)) +
    geom_point() +
    labs(x = "Lag 1 PM10 Pollution (mean-centered)",
         y = "Daily mortality (all non-accidental causes)",
         title = "Daily mortality and PM10 for 1987--2005") +
    geom_smooth(method = "lm")


## Fit a linear regression model
fit <- lm(death ~ l1pm10tmean,
          data = dat,
          na.action = na.exclude)

## Summarize the fit
summary(fit)

## Use tidy() function from the 'broom' package
tidy(fit, conf.int = TRUE)

## Look at augmented dataset (na.exclude is important here)
fit |>
    augment(dat)

## Density plot of raw residuals
fit |>
    augment(dat) |>
    ggplot(aes(x = .resid)) +
    geom_density() +
    geom_rug() +
    labs(x = "Residuals")

## Density plot of standardized residuals
fit |>
    augment(dat) |>
    ggplot(aes(x = .std.resid)) +
    geom_density() +
    geom_rug() +
    labs(x = "Standardized Residuals")

## Plot mortality vs. PM10
dat |>
    ggplot(aes(l1pm10tmean, death)) +
    geom_point() +
    labs(x = "Lag 1 PM10 Pollution (mean-centered)",
         y = "Daily mortality (all non-accidental causes)",
         title = "Daily mortality and PM10 for 1987--2005") +
    geom_smooth(method = "lm")

## Take a look at some outliers
dat |>
    filter(l1pm10tmean > 60)

## Take a look at some outliers
dat |>
    ggplot(aes(date, l1pm10tmean)) +
    geom_point() +
    labs(x = "Date",
         y = "Lag 1 PM10 Pollution (mean-centered)")

## Residuals vs. Predicted plot (any patterns?)
fit |>
    augment(dat) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, lty = 2) +
    geom_smooth(method = "gam") +
    labs(x = "Predicted mortality",
         y = "Residual mortality")

## Residuals vs. Predicted colored by season
fit |>
    augment(dat) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point(aes(color = season), size = 2) +
    # geom_point(size = 2) +
    # facet_wrap(vars(season)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_smooth(method = "gam") +
    labs(x = "Predicted mortality",
         y = "Residual mortality") +
    scale_color_brewer(type = "qual", palette = "Set1")

## Residuals vs. Predicted faceted by season
fit |>
    augment(dat) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point(size = 2) +
    facet_wrap(vars(season)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_smooth(method = "gam") +
    labs(x = "Predicted mortality",
         y = "Residual mortality") +
    scale_color_brewer(type = "qual", palette = "Set1")

## Make some boxplots of residuals by season
fit |>
    augment(dat) |>
    ggplot(aes(season, .resid)) +
    geom_boxplot() +
    labs(x = NULL,
         y = "Residual mortality")

## Look at residuals over time
fit |>
    augment(dat) |>
    ggplot(aes(date, .resid)) +
    geom_point() +
    labs(x = NULL,
         y = "Residual mortality")

## Fit a new model with season and date
fit <- lm(death ~ l1pm10tmean + season + date, data = dat,
          na.action = na.exclude)
summary(fit)
tidy(fit, conf.int = TRUE)

## Residuals vs predicted for new model
fit |>
    augment(dat) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth(method = "gam") +
    labs(x = "Predicted mortality",
         y = "Residual mortality")

## Boxplots of residuals by season
fit |>
    augment(dat) |>
    ggplot(aes(season, .resid)) +
    geom_boxplot()

## Desnity plot of standardized residuals
fit |>
    augment(dat) |>
    ggplot(aes(x = .std.resid)) +
    geom_density() +
    geom_rug() +
    labs(x = "Standardized Residual")

## Take a look at large standardized residuals
fit |>
    augment(dat) |>
    filter(.std.resid > 3)

## Look at residuals and temperature
fit |>
    augment(dat) |>
    ggplot(aes(temp, .resid)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Temperature (degrees F)",
         y = "Residual mortality")

## Fit a larger model including temperature
fit <- lm(death ~ l1pm10tmean + season + date + temp,
          data = dat,
          na.action = na.exclude)

summary(fit)
tidy(fit, conf.int = TRUE)

## Residuals vs. predicted
fit |>
    augment(dat) |>
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth(method = "gam") +
    labs(x = "Predicted mortality",
         y = "Residual mortality")

## Density of standardized residuals
fit |>
    augment(dat) |>
    ggplot(aes(x = .std.resid)) +
    geom_density() +
    geom_rug() +
    labs(x = "Standardized Residual")


## Look at residuals and temperature
fit |>
    augment(dat) |>
    ggplot(aes(temp, .resid)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Temperature (degrees F)",
         y = "Residual mortality")
