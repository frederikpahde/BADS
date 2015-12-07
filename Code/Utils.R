getNumericVariables <- function(df){
  return(df[sapply(df, is.numeric)])
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}
