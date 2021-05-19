# Plot histogram
SaveHist <- function(data, path)
{
  for(c in colnames(data))
  {
    cData <- na.omit(data[,c])
    pdf(paste(paste(path, c, sep = ""), "pdf", sep="."))
    par(mfrow=c(1,1))
    H <- hist(as.numeric(cData), 50, plot = FALSE)
    plot(H, main = paste("Histogram of residuals of ", c))
    dev.off()
  }
}


# Plot ACF, PACF of residuals
SaveACF <- function(data, path)
{
  for(c in colnames(data))
  {
    # clean data
    cData <- na.omit(data[,c])
    pdf(paste(paste(path, c, sep = ""), "pdf", sep="."))
    # calc acf, pacf of residuals and squared residuals
    par(mfrow = c(2, 2), mar=c(3,3,3,3))
    ACF <- acf(cData,  plot = FALSE)
    plot(ACF, main = paste("ACF of residuals of ", c) )
    ACF2 <- acf(cData^2, plot = FALSE)
    plot(ACF2, main = paste("ACF of squared residuals of ", c))
    PACF <- pacf(cData, plot = FALSE)
    plot(PACF, main = paste("PACF of residuals of ", c) )
    PACF2 <- pacf(cData^2, plot = FALSE)
    plot(PACF2, main = paste("PACF of squared residuals of ", c) )
    dev.off()
  }
}