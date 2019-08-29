library(tidyquant)
library(timetk)
library(broom)

#### data Bike sharing dataset

# Read data
bikes <- read_csv(file.choose())   # bikes.csv dataset

# Select date and count
bikes <- bikes %>%
    select(dteday, cnt) %>%
    rename(date = dteday)


# Visualize data and training/testing regions
bikes %>%
    ggplot(aes(x = date, y = cnt)) +
    geom_rect(xmin = as.numeric(ymd("2012-07-01")),
              xmax = as.numeric(ymd("2013-01-01")),
              ymin = 0, ymax = 10000,
              fill = palette_light()[[4]], alpha = 0.01) +
    annotate("text", x = ymd("2011-10-01"), y = 7800,
             color = palette_light()[[1]], label = "Train Region") +
    annotate("text", x = ymd("2012-10-01"), y = 1550,
             color = palette_light()[[1]], label = "Test Region") +
    geom_point(alpha = 0.5, color = palette_light()[[1]]) +
    labs(title = "Bikes Sharing Dataset: Daily Scale", x = "") +
    theme_tq()


# Split into training and test sets
train <- bikes %>%
    filter(date < ymd("2012-07-01"))

test <- bikes %>%
    filter(date >= ymd("2012-07-01"))


############ modelling

train

# Add time series signature
train_augmented <- train %>%
    tk_augment_timeseries_signature()
train_augmented

# Model using the augmented features
fit_lm <- lm(cnt ~ ., data = train_augmented)


# Visualize the residuals of training set
fit_lm %>%
    augment() %>%
    ggplot(aes(x = date, y = .resid)) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    theme_tq() +
    labs(title = "Training Set: lm() Model Residuals", x = "") +
    scale_y_continuous(limits = c(-5000, 5000))


# RMSE
sqrt(mean(fit_lm$residuals^2))

############## validation

test_augmented <- test %>%
    tk_augment_timeseries_signature()

test_augmented

yhat_test <- predict(fit_lm, newdata = test_augmented)

pred_test <- test %>%
    add_column(yhat = yhat_test) %>%
    mutate(.resid = cnt - yhat)
pred_test

ggplot(aes(x = date), data = bikes) +
    geom_rect(xmin = as.numeric(ymd("2012-07-01")),
              xmax = as.numeric(ymd("2013-01-01")),
              ymin = 0, ymax = 10000,
              fill = palette_light()[[4]], alpha = 0.01) +
    annotate("text", x = ymd("2011-10-01"), y = 7800,
             color = palette_light()[[1]], label = "Train Region") +
    annotate("text", x = ymd("2012-10-01"), y = 1550,
             color = palette_light()[[1]], label = "Test Region") + 
    geom_point(aes(x = date, y = cnt), data = train, alpha = 0.5, color = palette_light()[[1]]) +
    geom_point(aes(x = date, y = cnt), data = pred_test, alpha = 0.5, color = palette_light()[[1]]) +
    geom_point(aes(x = date, y = yhat), data = pred_test, alpha = 0.5, color = palette_light()[[2]]) +
    theme_tq() 

########### test accuracy

# Calculating forecast error
error_tbl <- pred_test %>%
  mutate(pct_err = .resid/cnt * 100) %>%
  summarize(
    me = mean(.resid, na.rm = TRUE),
    rmse = mean(.resid^2, na.rm = TRUE)^0.5,
    mae = mean(abs(.resid), na.rm = TRUE),
    mape = mean(abs(pct_err), na.rm = TRUE),
    mpe = mean(pct_err, na.rm = TRUE)
  )

error_tbl

### visualize residuals

ggplot(aes(x = date, y = .resid), data = pred_test) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_smooth() +
    theme_tq() +
    labs(title = "Test Set: lm() Model Residuals", x = "") +
    scale_y_continuous(limits = c(-5000, 5000))

### forcasting

# Extract bikes index
idx <- bikes %>%
    tk_index()

# Get time series summary from index
bikes_summary <- idx %>%
    tk_get_timeseries_summary()

bikes_summary[1:6]

bikes_summary[7:12]

idx_future <- idx %>%
    tk_make_future_timeseries(n_future = 180)

data_future <- idx_future %>%
    tk_get_timeseries_signature() %>%
    rename(date = index)


pred_future <- predict(fit_lm, newdata = data_future)

bikes_future <- data_future %>%
    select(date) %>%
    add_column(cnt = pred_future)


bikes %>%
    ggplot(aes(x = date, y = cnt)) +
    geom_rect(xmin = as.numeric(ymd("2012-07-01")),
              xmax = as.numeric(ymd("2013-01-01")),
              ymin = 0, ymax = 10000,
              fill = palette_light()[[4]], alpha = 0.01) +
    geom_rect(xmin = as.numeric(ymd("2013-01-01")),
              xmax = as.numeric(ymd("2013-07-01")),
              ymin = 0, ymax = 10000,
              fill = palette_light()[[3]], alpha = 0.01) +
    annotate("text", x = ymd("2011-10-01"), y = 7800,
             color = palette_light()[[1]], label = "Train Region") +
    annotate("text", x = ymd("2012-10-01"), y = 1550,
             color = palette_light()[[1]], label = "Test Region") +
    annotate("text", x = ymd("2013-4-01"), y = 1550,
             color = palette_light()[[1]], label = "Forecast Region") +
    geom_point(alpha = 0.5, color = palette_light()[[1]]) +
    geom_point(aes(x = date, y = cnt), data = bikes_future,
               alpha = 0.5, color = palette_light()[[2]]) +
    geom_smooth(aes(x = date, y = cnt), data = bikes_future,
                method = 'loess') + 
    labs(title = "Bikes Sharing Dataset: 6-Month Forecast", x = "") +
    theme_tq()

####### forecast error

# Calculate standard deviation of residuals
test_resid_sd <- sd(pred_test$.resid)

bikes_future <- bikes_future %>%
    mutate(
        lo.95 = cnt - 1.96 * test_resid_sd,
        lo.80 = cnt - 1.28 * test_resid_sd,
        hi.80 = cnt + 1.28 * test_resid_sd,
        hi.95 = cnt + 1.96 * test_resid_sd
        )

bikes %>%
    ggplot(aes(x = date, y = cnt)) +
    geom_point(alpha = 0.5, color = palette_light()[[1]]) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), data = bikes_future, 
                fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), data = bikes_future,
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_point(aes(x = date, y = cnt), data = bikes_future,
               alpha = 0.5, color = palette_light()[[2]]) +
    geom_smooth(aes(x = date, y = cnt), data = bikes_future,
                method = 'loess', color = "white") + 
    labs(title = "Bikes Sharing Dataset: 6-Month Forecast with Prediction Intervals", x = "") +
    theme_tq()


######################## Forecasting prophet package ############################

fin="C://Users//user//Documents//R//win-library//3.6//finance"

.libPaths(c(.libPaths(), fin))
.libPaths()


library(quantmod)
library(prophet)
library(dplyr)

stk="DIS"
tdy=Sys.Date()
ytd9=tdy - (9*365)

y=getSymbols(stk, source='yahoo', from=ytd9, to=tdy, adjust=T, auto.assign=F)
spyt <- y[,4]
d1=index(spyt)
d2=coredata(spyt)
d1=as.Date(d1)
d2=as.numeric(d2)
df=data.frame(ds=d1, y=d2)

colnames(df) <- c("ds", "y")   # should rename the columns
summary(df)
plot(y~ds, df, type="l")


m <- prophet(df,yearly.seasonality =TRUE , 
             weekly.seasonality = TRUE, 
             daily.seasonality = FALSE)


# We need to construct a dataframe for prediction. The make_future_dataframe function # takes the model object and a number of periods to forecast:

future <- make_future_dataframe(m, periods = 365)
tail(future)

# make prediction 
forecast <- predict(m, future)
head(forecast)

tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])

plot(m, forecast)

# plot the components of the forecast.
prophet_plot_components(m, forecast)


# Customizing holidays and events

playoff_brackets <- data_frame(
 holiday = "playoffs",
 ds = as.Date(c("2019-04-16" ,"2019-04-17", "2019-04-19", "2019-04-19")),
 lower_window = 0,
 upper_window = 45
)
playoff_finals <- data_frame(
 holiday = "playoff_finals",
 ds = as.Date(c("2019-06-02", "2019-06-04", "2019-06-05")),
 lower_window = 0,
 upper_window = 20
)

holidays <- bind_rows(playoff_brackets, playoff_finals)
m <- prophet(df, holidays = playoff_brackets)
forecast <- predict(m, future)
plot(m, forecast)



# Anomaly detection

combined_data <- cbind(head(forecast, nrow(df)), df[order(df$ds),])
combined_data$diff_values <- (combined_data$y - combined_data$yhat)
summary(combined_data$diff_values)

# normalized diff values representing the percent difference from actual values
combined_data$diff_values_normalized <-(combined_data$y - combined_data$yhat) / combined_data$y

# visualize the normalized diff values over time
plot(diff_values_normalized ~ ds, combined_data, type = "l")


nrow(combined_data[abs(combined_data$diff_values_normalized) > 0.1
& !is.na(combined_data$y),]) / nrow(combined_data)

# 2% of data are anomalies based on the given threshold.
