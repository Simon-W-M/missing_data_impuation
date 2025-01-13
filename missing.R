library(tidyverse)
library(zoo)
library(forecast)

dates <- c('2021-01-01',
           '2021-02-01',
           '2021-03-01',
           '2021-04-01',
           '2021-07-01',
           '2021-10-01',
           '2021-11-01',
           '2021-12-01',
           '2022-01-01',
           '2022-02-01')

dates <- as.Date(dates)

values <- round(rnorm(10, 100, 20))

# make us a starting dataframe
data <- data.frame(dates, values)

# lets do a simple plot
data |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# lets expand out dataframe to have all the missing dates in it
# so lets make a sequence of dates
dates <- seq(from = min(data$dates), 
                    to = as.Date('2022-05-01'), 
                    by = '1 month')

# and then join our sequence to the dataframe
data <- data.frame(dates) |>
  left_join(data,
            by =  'dates')

# we can see we now have NAs in our gaps
view(data)
 
# which messes up our plot
data |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# let replace with 0s
data_na0 <-  data |>
  mutate(values = replace_na(values, 0))

# it works but nor sure it is right
data_na0 |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# let replace with mean
data_na_mean <-  data |>
  mutate(values = replace_na(values, 
                             mean(data$values, na.rm = T)))

# better?
data_na_mean |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# let replace with median
data_na_median <-  data |>
  mutate(values = replace_na(values, 
                             median(data$values, na.rm = T)))

# better?
data_na_median |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# use the last data point  (last object carried forward)
data_last  <-  data |>
  mutate(values = na.locf(values, fromLast = FALSE))

data_last |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# linear interpolation 
# The algorithm uses linear interpolation for non-seasonal series and a 
# periodic STL-decomposition with seasonal series to replace missing values.
data_lin  <-  data |>
  mutate(values = na.interp(values))

data_lin |> ggplot(aes(x= dates, y=values)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '1 month')

# more advanced methods when you have further data items

library(mice)
# MICE stands for Multivariate Imputation via Chained Equations

# This method is cool if you really need a better inference of missing 
# data around expected values

data <- mtcars

dates <- seq(from = as.Date('2021-01-01'),
                  length.out = 32, 
                  by = "1 month")

data$dates <- dates

view(data)

data |> ggplot(aes(x= dates, y=mpg)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '3 month')

# add in some nulls
data$mpg[4] <- NA
data$mpg[17] <- NA
data$mpg[22] <- NA

# look at our chart
data |> ggplot(aes(x= dates, y=mpg)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = '3 month')

# lets use mice to impute values from the other data
mice_imputed <- data.frame(
  original = data$mpg,
  imputed_pmm = complete(mice(data, method = "pmm"))$mpg,
  imputed_cart = complete(mice(data, method = "cart"))$mpg,
  imputed_lasso = complete(mice(data, method = "lasso.norm"))$mpg
)

# lets add the actuals back in to compare
mice_imputed$actual <- mtcars$mpg

view(mice_imputed)

library(missForest)
# another simple function was the mice that uses a random forest method
# of determining missing values

missForest_imputed <- data |>
  select (-dates) 

missForest_imputed <- missForest_imputed |>
  data.frame(
  original = missForest_imputed$mpg,
  imputed_missForest = missForest(missForest_imputed)$ximp$mpg
)

missForest_imputed$actual <- mtcars$mpg

view(missForest_imputed)

# both mice and missForest have loads of extra parameters to mess about with
# just wanted to show what was possible straight out of the box



