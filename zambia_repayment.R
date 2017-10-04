## Zambia repayment analysis
## Last edited: 10/4 (BK)

#### Set-up ####
## Directories
wd <- "C:/Users/OAFUser/Google Drive/Zambia Repayment"
dd <- paste (wd, "data", sep = "/")
od <- paste (wd, "output", sep = "/")

## Libs
libs <- c("dplyr", "tibble", "reshape2", "ggplot2")

for(i in 1:length(libs)) {
 if(!require(libs[i])) {
  install.packages(libs[i], dependencies = T)
  require(libs[i])
 }
 require(libs[i])
}

#### Functions ####



#### Data prep ####



#### Summary stats ####



#### Time series graphs ####



