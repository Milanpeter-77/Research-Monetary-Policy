# Pre-Settings =====
rm(list=ls())
setwd("~/Documents/Egyetem/BCE - GTK/#Egyéb/TDK2023/data")

# Libraries =====
library(tidyverse)
library(stats)

# Data =====
daily_data <- read.csv("tables/daily_data_logdiff_excluded.csv")
event_data <- read.csv("tables/event_data_logdiff.csv")

# T-Tests =====
# H0: sample mean = 0
ttest_onesample <- data.frame(matrix(0, nrow = ncol(event_data), ncol = 2))
colnames(ttest_onesample) <- c("date", "p-value")
for (i in 1:nrow(ttest_onesample)) {
  ttest_onesample[i, 1] <- gsub("X", "'", gsub("\\.", "-", colnames(event_data[i])))
  ttest_onesample[i, 2] <- t.test(event_data[, i])$p.value
}
write.csv(ttest_onesample, "tables/ttest_onesample.csv", row.names = FALSE)

# H0: sample mean = population mean
ttest_twosample <- data.frame(matrix(0, nrow = ncol(event_data), ncol = 2))
colnames(ttest_twosample) <- c("date", "p-value")
for (i in 1:nrow(ttest_twosample)) {
  ttest_twosample[i, 1] <- gsub("X", "'", gsub("\\.", "-", colnames(event_data[i])))
  ttest_twosample[i, 2] <- t.test(event_data[, i], daily_data$EUR)$p.value
}
write.csv(ttest_twosample, "tables/ttest_twosample.csv", row.names = FALSE)



t.test(event_data[4, ])

# Robustness Tests ===== 
robust_onesample <- data.frame(matrix(0, nrow = ncol(event_data), ncol = nrow(event_data)/2 + 1))
colnames(robust_onesample) <- c("date", seq(1, (nrow(event_data)/2), 1))
for (r in ncol(robust_onesample):2) for (i in 1:nrow(robust_onesample)) {
  robust_onesample[i, 1] <- gsub("X", "'", gsub("\\.", "-", colnames(event_data[i])))
  robust_onesample[i, r] <- t.test(event_data[(nrow(event_data)/2 - r + 1):(nrow(event_data)/2 + r), i])$p.value
}
write.csv(robust_onesample, "tables/robust_onesample.csv", row.names = FALSE)

robust_twosample <- data.frame(matrix(0, nrow = ncol(event_data), ncol = nrow(event_data)/2 + 1))
colnames(robust_twosample) <- c("date", seq(1, (nrow(event_data)/2), 1))
for (r in ncol(robust_twosample):2) for (i in 1:nrow(robust_twosample)) {
  robust_twosample[i, 1] <- gsub("X", "'", gsub("\\.", "-", colnames(event_data[i])))
  robust_twosample[i, r] <- t.test(event_data[(nrow(event_data)/2 - r + 1):(nrow(event_data)/2 + r), i], daily_data$EUR)$p.value
}
write.csv(robust_twosample, "tables/robust_twosample.csv", row.names = FALSE)


