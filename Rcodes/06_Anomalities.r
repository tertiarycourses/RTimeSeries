library(tidyverse)
library(anomalize)

############### getting strated ##########

#We can use the general workflow for anomaly detection, which involves #three main functions:

#time_decompose(): Separates the time series into seasonal, trend, and #remainder components
#anomalize(): Applies anomaly detection methods to the remainder #component.
#time_recompose(): Calculates limits that separate the “normal” data from #the anomalies!

tidyverse_cran_downloads_anomalized <- tidyverse_cran_downloads %>%
  time_decompose(count, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()

#We can then visualize the anomalies using the plot_anomalies() function.

tidyverse_cran_downloads_anomalized %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)


#### Adjusting Decomposition Trend and Seasonality

lubridate_daily_downloads <- tidyverse_cran_downloads %>%
  filter(package == "lubridate") %>%
  ungroup()

lubridate_daily_downloads

lubridate_daily_downloads_anomalized <- lubridate_daily_downloads %>% 
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose()

p1 <- lubridate_daily_downloads_anomalized %>%
  plot_anomaly_decomposition() +
  ggtitle("Freq/Trend = 'auto'")

p1

get_time_scale_template()


#### Local Parameter Adjustment

# Local adjustment via time_decompose
p2 <- lubridate_daily_downloads %>%
  time_decompose(count,
                 frequency = "auto",
                 trend     = "2 weeks") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Local)")
#> frequency = 7 days
#> trend = 14 days

# Show plots
p1
p2

#### Global Parameter Adjustement

# Globally change time scale template options
time_scale_template() %>%
  mutate(trend = ifelse(time_scale == "day", "2 weeks", trend)) %>%
  set_time_scale_template()

get_time_scale_template()

p3 <- lubridate_daily_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Trend = 2 Weeks (Global)")
#> frequency = 7 days
#> trend = 14 days

p3


#### Let’s reset the time scale template defaults back to the original # defaults.

# Set time scale template to the original defaults
time_scale_template() %>%
  set_time_scale_template()

# Verify the change
get_time_scale_template()

p1

#### Adjusting Anomaly Detection Alpha and Max Anoms
# The alpha and max_anoms are the two parameters that control the # anomalize() function. Here’s how they work.

# Alpha

p4 <- lubridate_daily_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")
#> frequency = 7 days
#> trend = 91 days

p4


p5 <- lubridate_daily_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder, alpha = 0.025, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("alpha = 0.025")
#> frequency = 7 days
#> trend = 91 days

p4 
p5


## Max Anoms
#The max_anoms parameter is used to control the maximum percentage of 
# data that can be an anomaly

p6 <- lubridate_daily_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.2) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("20% Anomalies")
#> frequency = 7 days
#> trend = 91 days

p7 <- lubridate_daily_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder, alpha = 0.3, max_anoms = 0.05) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("5% Anomalies")
#> frequency = 7 days
#> trend = 91 days

p6
p7


##### Comparison of STL and Twitter Decomposition Methods

# Data on `lubridate` package daily downloads
lubridate_download_history <- tidyverse_cran_downloads %>%
  filter(package == "lubridate") %>%
  ungroup()

# Output first 10 observations
lubridate_download_history %>%
  head(10) %>%
  knitr::kable()

# STL Decomposition Method
p1 <- lubridate_download_history %>%
  time_decompose(count, 
                 method    = "stl",
                 frequency = "1 week",
                 trend     = "3 months") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("STL Decomposition")
#> frequency = 7 days
#> trend = 91 days

# Twitter Decomposition Method
p2 <- lubridate_download_history %>%
  time_decompose(count, 
                 method    = "twitter",
                 frequency = "1 week",
                 trend     = "3 months") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Twitter Decomposition")
#> frequency = 7 days
#> median_span = 85 days

# Show plots
p1
p2

############# detecting anomalities in remainders

# Comparison of IQR and GESD Methods
# We can generate anomalous data to illustrate how each method work # compares to each other.

# Generate anomalies
set.seed(100)
x <- rnorm(100)
idx_outliers    <- sample(100, size = 5)
x[idx_outliers] <- x[idx_outliers] + 10

# Visualize simulated anomalies
qplot(1:length(x), x, 
      main = "Simulated Anomalies",
      xlab = "Index") 


# Analyze outliers: Outlier Report is available with verbose = TRUE
iqr_outliers <- iqr(x, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report

gesd_outliers <- gesd(x, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report

# ploting function for anomaly plots
ggsetup <- function(data) {
  data %>%
    ggplot(aes(rank, value, color = outlier)) +
    geom_point() +
    geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
    geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
    geom_text(aes(label = index), vjust = -1.25) +
    theme_bw() +
    scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
    expand_limits(y = 13) +
    theme(legend.position = "bottom")
}


# Visualize
p3 <- iqr_outliers %>% 
  ggsetup() +
  ggtitle("IQR: Top outlers sorted by rank") 

p4 <- gesd_outliers %>% 
  ggsetup() +
  ggtitle("GESD: Top outlers sorted by rank") 

# Show plots
p3
p4



############## Visualizing financial ###################

library(tibbletime)
library(TSstudio)


AMZNTTab <- AMZN %>%
  time_decompose(close, merge = TRUE) %>%
  anomalize(remainder) %>%
  time_recompose()

#We can then visualize the anomalies using the plot_anomalies() function.

AMZNTTab %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.5,time_recomposed = TRUE) +
  ggtitle("alpha = 0.05")


