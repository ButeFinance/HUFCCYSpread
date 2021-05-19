TestUnitroot <- function(Residuals)
{
  pUnitroot <- data.frame(ADF = double(), KPSS = double() )
  for(c in colnames(Residuals))
  {
      res <- Residuals[,c]
      pUnitroot[c, 1] <- adf.test(as.numeric(unlist(res[!is.na(res)])))
      pUnitroot[c, 2] <- kpss.test(as.numeric(unlist(res[!is.na(res)])))
  }
  return(pUnitroot)
}
