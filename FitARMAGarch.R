library(tseries)
library(forecast)

fitARMAGarch <- function(data, max.d = 1)
{
  # Initialize returns
  models = list()
  resARMA <- data
  resGARCH <- data
  
  # Loop through the dataframe
  for(cn in colnames(data))
  {
    # Fit ARIMA
    fitARMA <- auto.arima(data[!is.na(data[,cn]),cn], max.p = 7, max.q = 7, max.d = max.d, lambda=NULL, stepwise = FALSE)
    # Replace NAN  wit NA
    fitARMA$residuals[sapply(fitARMA$residuals, is.nan)] <- NA
    
    # Fit GARCH(1,1)
    invisible(capture.output(fitGARCH <- garch(fitARMA$residuals, order = c(1,1))))
    # Replace NAN  wit NA
    fitGARCH$residuals[sapply(fitGARCH$residuals, is.nan)] <- NA
    
    # Save models and residuals
    models[[cn]] = list("ARMA" = fitARMA, "GARCH" = fitGARCH)
    resGARCH[!is.na(data[,cn]),cn] <- fitGARCH$residuals
  }
  return(list("models" = models, "ARMA residuals" = resARMA, "GARCH residuals" = resGARCH))
}