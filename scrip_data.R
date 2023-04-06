# Data preparation =====
# Pre-Settings =====
rm(list=ls())
setwd("~/Documents/Egyetem/BCE - GTK/#Egyéb/TDK2023/data")

# Libraries =====
library(readxl)
library(tidyverse)

# Data for Long-term analysis =====
#data_monthly <- read_excel("data_monthly.xlsx")
#date_index <- which.max(data_monthly$Period > "2008-02-27") # set date.... 
#

# Data for Short-term analysis =====
# import
data_daily_BLOOMBERG <- read_excel("BLOOMBERG_eurhuf.xlsx", sheet = "daily (2022)")
data_daily_BLOOMBERG <- data_daily_BLOOMBERG %>% arrange(Date)

data_days_MNB <- read_excel("MNB_alapkamat.xlsx")
colnames(data_days_MNB) <- c("Date", "IR")
data_days_MNB <- data_days_MNB %>% arrange(Date) %>% dplyr::select("Date") %>% filter(Date > "2022-01-01")

# daily data
daily_data_nominal <- data_daily_BLOOMBERG %>% dplyr::select(-c(1, 2, 3, 4)) %>% rename("EUR" = "Close")
daily_data_logdiff <- daily_data_nominal %>% as.matrix() %>% log() %>% diff() %>% data.frame()

# event data
event_window <- 4
event_data_nominal <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = nrow(data_days_MNB)))
colnames(event_data_nominal) <- as.character(data_days_MNB$Date, "%m-%d")
daily_data_nominal_excluded <- daily_data_nominal

for (i in 1:nrow(data_days_MNB)) {
  indice <- which(data_daily_BLOOMBERG$Date == data_days_MNB$Date[i])
  event_data_nominal[, i] <- data_daily_BLOOMBERG$Close[(indice - event_window):(indice + event_window)]
  daily_data_nominal_excluded <- data.frame(daily_data_nominal_excluded[-((indice - event_window - ((i - 1) * (2 * event_window + 1))):(indice + event_window - ((i - 1) * (2 * event_window + 1)))),])
}

colnames(daily_data_nominal_excluded) <- "EUR"
event_data_logdiff <- event_data_nominal %>% as.matrix() %>% log() %>% diff() %>% data.frame()
colnames(event_data_logdiff) <- as.character(data_days_MNB$Date, "%m-%d")
daily_data_logdiff_excluded <- daily_data_nominal_excluded %>% as.matrix() %>% log() %>% diff() %>% data.frame()


# export
write.csv(daily_data_nominal, "tables/daily_data_nominal.csv", row.names = FALSE)
write.csv(daily_data_logdiff, "tables/daily_data_logdiff.csv", row.names = FALSE)

write.csv(daily_data_nominal_excluded, "tables/daily_data_nominal_excluded.csv", row.names = FALSE)
write.csv(daily_data_logdiff_excluded, "tables/daily_data_logdiff_excluded.csv", row.names = FALSE)

write.csv(event_data_nominal, "tables/event_data_nominal.csv", row.names = FALSE)
write.csv(event_data_logdiff, "tables/event_data_logdiff.csv", row.names = FALSE)



