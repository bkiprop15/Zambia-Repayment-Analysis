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
  #require(libs[i])
 }
 #require(libs[i])
}
lapply(libs, "require", character.only = T)

#### Functions ####



#### Data prep ####
## Seasons clients data
sc16r <- read.csv(paste(dd, "Season Clients Detailed_2016.csv", sep = "/"),
 stringsAsFactors = F)
sc17r <- read.csv(paste(dd, "Season Clients Detailed_2017.csv", sep = "/"),
 stringsAsFactors = F)

#Select variables of interest from each df:
sc16 <- sc16r %>% mutate(
 
)

## Vertical repayment data
vr16r <- read.csv(paste(dd, "vertical_2016.csv", sep = "/"), stringsAsFactors = F)
vr17r <- read.csv(paste(dd, "vertical_2017.csv", sep = "/"), stringsAsFactors = F)



#### Summary stats ####



#### Time series graphs ####





























