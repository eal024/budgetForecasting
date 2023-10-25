
# Packages
library(forecast)
library(xts)
library(tidyverse)

# Test data
df

# Graphical look
plot( x = df$date, y = df$value, "ln")

# Tesing for stationarity

## Arima auto
auto.arima( y = df$value, seasonal = T)

model_diff <- auto.arima( y = diff(df$value) )

acf(model_diff$residuals)
