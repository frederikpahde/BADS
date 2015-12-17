getNumericVariables <- function(df){
  return(df[sapply(df, is.numeric)])
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

getMissingValueRate <- function(col){
  amount = sum(as.numeric(sapply(col, is.na)))
  return(amount/length(col))
}

setNA <- function(v, dv){
  inds <- v == dv
  v[inds] = NA
  return(v)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}