lmResiduals <- function(data, dependent, independent)
{
  # Prepare data
  data <- data[c(dependent, independent)]
  data <- na.omit(data)
  
  # Calculate OLS
  frm <- formula(paste(dependent,"~."))
  return(lm(frm,data=data))
}