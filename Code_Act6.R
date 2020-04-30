library(tidyverse)
library(lubridate)
library(forecast)
#library(fpp2)

hawai <- read_csv("data/hawai.csv")

date1 <- date_decimal(1958.75)
class(date1)

hawai <- hawai %>%
  mutate(time = date(date_decimal(time)))

hawai %>% pull(time) %>% class()
ymd_temp <- ymd(hawai$time)

hawai %>%
  ggplot(aes(x = time, y = CO2)) +
  geom_line()

hawai_ts <- ts(hawai %>% dplyr::select(-time),
               start = c(1958, 3),
               frequency = 12)

autoplot(hawai_ts)

hawai_ts_train <- window(hawai_ts, start = c(1958, 3), end = 1988.999)
hawai_ts_test <- window(hawai_ts, start = 1989)
co2_model <- ets(hawai_ts_train)

autoplot(co2_model)

co2_ets1 <- hawai_ts_train %>% ets()
co2_fc1 <- co2_ets1 %>% forecast(h = length(hawai_ts_test))
plot_fc1 <- co2_fc1 %>% autoplot() +
  autolayer(hawai_ts_test, color = rgb(0, 0, 0, 0.6))
plot_fc1

BoxCox.lambda(hawai_ts_train)

co2_ets2 <- hawai_ts_train %>% ets(lambda = 0.1127346)
co2_fc2 <- co2_ets2 %>% forecast(h = length(hawai_ts_test))
plot_fc2 <- co2_fc2 %>% autoplot() +
  autolayer(hawai_ts_test, color = rgb(0, 0, 0, 0.6))
plot_fc2

co2_ets3 <- hawai_ts_train %>% ets(lambda = 0)
co2_fc3 <- co2_ets3 %>% forecast(h = length(hawai_ts_test))
plot_fc3 <- co2_fc3 %>% autoplot() +
  autolayer(hawai_ts_test, color = rgb(0, 0, 0, 0.6))
plot_fc3

autoplot(co2_ets1)
autoplot(co2_ets2)
autoplot(co2_ets3)

plot(plot_fc1)
plot(plot_fc2)
plot(plot_fc3)

checkresiduals(co2_ets1)
checkresiduals(co2_ets2)
checkresiduals(co2_ets3)

stn <- shapiro.test(residuals(co2_ets1))$p.value
class(stn)

accuracy(co2_fc1, hawai_ts)
accuracy(co2_fc2, hawai_ts)
accuracy(co2_fc3, hawai_ts)


hawai_ts_train2 <- window(hawai_ts, start = c(1959), end = c(1988, 12))
hawai_ts_test2 <- window(hawai_ts, start = 1989)