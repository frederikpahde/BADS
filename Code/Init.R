requiredPackages = c("data.table","mvoutlier", "cvTools","CORElearn", "ggplot2", "lattice", "corrplot", "DMwR", "caret",  "Hmisc", "randomForest", "gmodels", "outliers", "glmnet")
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
