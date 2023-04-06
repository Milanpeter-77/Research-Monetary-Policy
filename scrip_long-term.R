# III.1. Long-Term Effects =====
# Pre-Settings =====
rm(list=ls())
setwd("~/Documents/Egyetem/BCE - GTK/#Egyéb/TDK2023/data")

# Libraries =====
library(readxl)
library(tidyverse)
library(tseries)
library(vars)
library(TSstudio)
library(stats)

# Data =====
# Reading and filtering monthly data (prepared in script_data.R)
data_monthly <- read_excel("data_monthly.xlsx")
date_index <- which.max(data_monthly$Period > "2008-02-27") # set date.... 

# Time series -----
ts_IR <- ts(data_monthly$IR, start = c(1987, 1, 31), frequency = 12)
ts_CPI <- ts(na.remove(data_monthly$CPI), start = c(1993, 1, 31), frequency = 12)
ts_EUR <- ts(na.remove(data_monthly$EUR), start = c(1999, 1, 31), frequency = 12)

# Binding vectors -----
vector_IRCPI <- na.omit(cbind(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$CPI[date_index:length(data_monthly$CPI)]))
colnames(vector_IRCPI) <- cbind("IR", "CPI")

vector_IREUR <- na.omit(cbind(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$EUR[date_index:length(data_monthly$EUR)]))
colnames(vector_IREUR) <- cbind("IR", "EUR")

vector_CPIEUR <- na.omit(cbind(data_monthly$CPI[date_index:length(data_monthly$CPI)], data_monthly$EUR[date_index:length(data_monthly$EUR)]))
colnames(vector_CPIEUR) <- cbind("CPI", "EUR")

vector_ALL <- na.omit(cbind(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$CPI[date_index:length(data_monthly$CPI)], data_monthly$EUR[date_index:length(data_monthly$EUR)]))
colnames(vector_ALL) <- cbind("IR", "CPI", "EUR")

# Descriptive statistics =====
# Plots -----
ts_plot(ts_IR)
ts_plot(ts_CPI)
ts_plot(ts_EUR)

# Stationarity
adf.test(data_monthly$IR[date_index:length(data_monthly$IR)], alternative = "s")
adf.test(data_monthly$CPI[date_index:length(data_monthly$CPI)], alternative = "s")
adf.test(data_monthly$EUR[date_index:length(data_monthly$EUR)], alternative = "s")

# Autocorrelation -----
ts_cor(ts_IR, lag.max = 24, seasonal = FALSE)
ts_cor(ts_CPI, lag.max = 24, seasonal = FALSE)
ts_cor(ts_EUR, lag.max = 24, seasonal = FALSE)

autocorrelation_table <- cbind(ts_cor(ts_IR, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[2]][["y"]], ts_cor(ts_IR, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[5]][["y"]],
                               ts_cor(ts_CPI, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[2]][["y"]], ts_cor(ts_CPI, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[5]][["y"]],
                               ts_cor(ts_EUR, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[2]][["y"]], ts_cor(ts_EUR, lag.max = 24, seasonal = FALSE)[["x"]][["data"]][[5]][["y"]])
colnames(autocorrelation_table) <- cbind("ACF_IR", "PACF_IR", "ACF_CPI", "PACF_CPI", "ACF_EUR", "PACF_EUR")
write.csv(autocorrelation_table, "tables/autocorrelation_table.csv")


# Correlation matrix -----
cor(data_monthly[2:4], use = "pairwise.complete.obs") # entire time series
cor(data_monthly[1:date_index, 2:4], use = "pairwise.complete.obs") # before 2008
cor(data_monthly[date_index:length(data_monthly$IR), 2:4], use = "pairwise.complete.obs") # after 2008

# Cross-Correlation -----
ccf(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$CPI[date_index:length(data_monthly$CPI)], lag.max = 12, na.action = na.pass)
ccf(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$EUR[date_index:length(data_monthly$EUR)], lag.max = 12, na.action = na.pass)
ccf(data_monthly$CPI[date_index:length(data_monthly$CPI)], data_monthly$EUR[date_index:length(data_monthly$EUR)], lag.max = 12, na.action = na.pass)

crosscorrelation_table <- cbind(ccf(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$CPI[date_index:length(data_monthly$CPI)], lag.max = 12, na.action = na.pass)[["acf"]],
                                ccf(data_monthly$IR[date_index:length(data_monthly$IR)], data_monthly$EUR[date_index:length(data_monthly$EUR)], lag.max = 12, na.action = na.pass)[["acf"]],
                                ccf(data_monthly$CPI[date_index:length(data_monthly$CPI)], data_monthly$EUR[date_index:length(data_monthly$EUR)], lag.max = 12, na.action = na.pass)[["acf"]])
colnames(crosscorrelation_table) <- cbind("IR_CPI", "IR_EUR", "CPI_EUR")
write.csv(crosscorrelation_table, "tables/crosscorrelation_table.csv")


# Estimating VARs =====
# IR and CPI -----
VARselect(vector_IRCPI, lag.max = 24, type = "const")$selection
lag_number_IRCPI <- 15 # FPE
model_IRCPI <- VAR(vector_IRCPI, p = lag_number_IRCPI, type = "const", season = NULL, exog = NULL) 
summary(model_IRCPI)

# IR and EUR -----
VARselect(vector_IREUR, lag.max = 12, type = "const")$selection
lag_number_IREUR <- 11 # AIC and FPE
model_IREUR <- VAR(vector_IREUR, p = lag_number_IREUR, type = "const", season = NULL, exog = NULL) 
summary(model_IREUR)

# CPI and EUR -----
VARselect(vector_CPIEUR, lag.max = 12, type = "const")$selection
lag_number_CPIEUR <- 5 # AIC and FPE
model_CPIEUR <- VAR(vector_CPIEUR, p = lag_number_CPIEUR, type = "const", season = NULL, exog = NULL) 
summary(model_CPIEUR)

# IR and CPI and EUR -----
VARselect(vector_ALL, lag.max = 24, type = "const")$selection
lag_number_ALL <- 6 # HQ
model_ALL <- VAR(vector_ALL, p = lag_number_ALL, type = "const", season = NULL, exog = NULL) 
summary(model_ALL)

# Predictions =====
# Causality tests -----
causality(model_IREUR, cause = "IR")
causality(model_IREUR, cause = "EUR")

# IRFs -----
set.seed(7) # my favourite number
IRtoCPI <- irf(model_IRCPI, impulse = "IR", response = "CPI", n.ahead = 24)
plot(IRtoCPI, ylab = "CPI", main = "IR's shock to CPI")
set.seed(7)
CPItoIR <- irf(model_IRCPI, impulse = "CPI", response = "IR", n.ahead = 24)
plot(CPItoIR, ylab = "IR", main = "CPI's shock to IR")

set.seed(7)
IRtoEUR <- irf(model_IREUR, impulse = "IR", response = "EUR", n.ahead = 12)
plot(IRtoEUR, ylab = "EUR", main = "IR's shock to EUR") # makes sense - include
set.seed(7)
EURtoIR <- irf(model_IREUR, impulse = "EUR", response = "IR", n.ahead = 12)
plot(EURtoIR, ylab = "IR", main = "EUR's shock to IR")

set.seed(7)
CPItoEUR <- irf(model_CPIEUR, impulse = "CPI", response = "EUR", n.ahead = 24)
plot(CPItoEUR, ylab = "EUR", main = "CPI's shock to EUR")
set.seed(7)
EURtoCPI <- irf(model_CPIEUR, impulse = "EUR", response = "CPI", n.ahead = 24)
plot(EURtoCPI, ylab = "CPI", main = "EUR's shock to CPI")

set.seed(7)
IRtoEUR <- irf(model_ALL, impulse = "IR", response = "EUR", n.ahead = 12)
plot(IRtoEUR, ylab = "EUR", main = "IR's shock to EUR") # makes sense - include

IRF_table_IRtoEUR <- cbind(irf(model_IREUR, impulse = "IR", response = "EUR", n.ahead = 12)[["irf"]][["IR"]],
                           irf(model_IREUR, impulse = "IR", response = "EUR", n.ahead = 12)[["Lower"]][["IR"]],
                           irf(model_IREUR, impulse = "IR", response = "EUR", n.ahead = 12)[["Upper"]][["IR"]],
                           irf(model_ALL, impulse = "IR", response = "EUR", n.ahead = 12)[["irf"]][["IR"]],
                           irf(model_ALL, impulse = "IR", response = "EUR", n.ahead = 12)[["Lower"]][["IR"]],
                           irf(model_ALL, impulse = "IR", response = "EUR", n.ahead = 12)[["Upper"]][["IR"]])
colnames(IRF_table_IRtoEUR) <- cbind("IRtoEUR_v1", "Lower_v1", "Upper_v1", "IRtoEUR_v2", "Lower_v2", "Upper_v2")
write.csv(IRF_table_IRtoEUR, "tables/IRF_table_IRtoEUR.csv")
