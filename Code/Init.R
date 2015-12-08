
requiredPackages = c("data.table", "cvTools", "ggplot2", "corrplot", "Hmisc", "randomForest", "gmodels")
requirePackage <- function(x)
{
  if (!require(x,character.only = TRUE, warn.conflicts = FALSE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

for (package in requiredPackages) {
  requirePackage(package)
}
