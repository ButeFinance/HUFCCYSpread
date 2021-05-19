rm(list = ls())

library(tidyr)
library(dplyr) 
source("FitARMAGarch.R")
source("RegressResiduals.R")
source("TestUnitroot.R")
source("SaveHistACF.R")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# https://www.mnb.hu/monetaris-politika/a-monetaris-politikai-eszkoztar/aukciok-tenderek-gyorstenderek/forint-tenderek-gyorstenderek


# ==============
# DATA

# Set data
pathData <- "Data/USDHUF.csv"

# Read test data
ccy <- substr(unlist(strsplit(pathData,"/"))[-1], 0, 6)
dailyClose <- read.table(file = pathData, header=TRUE, row.names = 1 ,sep = ',', na.strings = "#NA", fill=TRUE, check.names=TRUE)
# only regression variables marked! other variables could have been also marked as non Index
notSpreadColumns <- c("HUN.5Y.USD.CDS", "EMBI", "Swap.tender", "FX.tender")
notSpreadColumns <- c(notSpreadColumns, "OIS 3M spread", "EUR 3M REPO")
notSpreadColumns <- c(notSpreadColumns, "ON deposit rate")
notSpreadColumns <- c(notSpreadColumns, "CHFHUF", "CHF.ON", "HUF.ON", "CHFHUF.3M", "CHF.3M.Libor", "CHF.ON.Libor", "CHF.OIS.3M")
# non spread columns of data
notSpreadColumns <- colnames(dailyClose)[colnames(dailyClose) %in% notSpreadColumns]
dummyColumns <- c("FX.tender", "Swap.tender", "Swap.3M.spread", "ECB DEPOSIT")
# not daily data
notDaily <- c("HUF.3Y.HUF.5Y.overquote", "HUF.5Y.overquote", "HUF.3Y.overquote")
for(c in notDaily){dailyClose[dailyClose[, c] < 0.000001, c] <- NaN}

# Fit ARIMA-GARCH(1,1)
fitNSpread <- fitARMAGarch(dailyClose[,notSpreadColumns], 1)
fitSpread <- fitARMAGarch(dailyClose[,!(colnames(dailyClose) %in% notSpreadColumns)], 0)

# Prepare TS residuals
Residuals <- merge(fitSpread$`GARCH residuals`, fitNSpread$`GARCH residuals`, by = 0)
row.names(Residuals) <- Residuals$Row.names
Residuals$Row.names <- NULL
# Overwrite dummy
Residuals[,dummyColumns] <- dailyClose[,dummyColumns]
# Overwrite not daily
for(c in notDaily){Residuals[is.na(Residuals[c]), c] <- 0}
# Save Residuals
write.csv(Residuals, file = paste("Residuals", paste(paste(ccy, "Residuals", sep = " "), "csv", sep = "."), sep = "/"))


# ==============
# ANALAYZE FIT

# Stationarity tests
ur <- TestUnitroot(Residuals)
write.csv(ur, paste("UnitRoot", paste(paste(ccy, "UnitRoot", sep = " "), "csv", sep = "."), sep = "/"))

# PLot Hist, P/ACF
SaveHist(Residuals, paste("Hist", ccy, sep="/"))
SaveACF(Residuals, paste("ACF", ccy, sep="/"))


# ==============
# OLS

# Initialize OLS
dependent <- "Swap.3M.5Y.spread"
independent <- c(ccy, paste("d",ccy,sep=""), "TED.spread", "EMBI", "HUN.5Y.USD.CDS", "Swap.3M.spread", "FX.tender", "HUF.3Y.HUF.5Y.overquote")
# filter independet variables of Residuals
independent <- colnames(Residuals)[colnames(Residuals) %in% independent]

# Run OLS
model <- lmResiduals(Residuals, dependent, independent)

# Save summary
sink(paste("OLS", paste(paste(ccy, "OLS", sep = " "), "txt", sep = "."), sep = "/"))
print(summary(model))
sink()