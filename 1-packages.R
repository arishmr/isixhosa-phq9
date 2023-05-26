# Define vector of package names
packages <- c("ggplot2", "RColorBrewer", "tidyverse", "corrr", "psych", "summarytools", "htmlTable", "BSDA", "ggsignif", "boot", "dplyr", "stringr", "gtsummary", "flextable", "officer", "openxlsx", "reshape2", "pROC", "ppcor")

# Load packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

rm(packages, package)


## Outline of main statistical analyses

## 3 - RELIABILITY
## Cronbach's α as a measure of internal consistency for the translated version
## Inter-item and item-total score correlations

## 4 - CONVERGENT VALIDITY
## Pearson's correlation coefficients for total PHQ-9 and YSR T scores on components

## 5 - CRITERION VALIDITY
## diagnostic sensitivity and specificity
## PPV, NPV
## PLR, NLR
## Youden's index
## ROC curve with AUC calculation

## 6 - Individual item analyses to determine whether mean scores on each item of the PHQ-9 differed between depressed, borderline, and non-depressed participants as shown on YSR