## Zambia repayment analysis
## Last edited: 10/4 (BK)

#### Set-up ####
## Directories
wd <- "C:/Users/OAFUser/Google Drive/Zambia Repayment"
dd <- paste (wd, "data", sep = "/")
od <- paste (wd, "output", sep = "/")

## Libs
#Note: some libs may not load if dependencies don't exist
#colorspace is a dependency for ggplot2, and stringi for reshape2
libs <- c("dplyr", "tibble", "reshape2", "ggplot2", "colorspace", "stringi")

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

#Create unique group names (in case we'll need for group analysis)
sc16r$groupName <- paste0(sc16r$GroupName, "_", sc16r$SiteName)
sc17r$groupName <- paste0(sc17r$GroupName, "_", sc17r$SiteName)

#Select variables of interest from each df:
sc16 <- sc16r %>% tbl_df() %>%
 transmute(
  clientID = GlobalClientID,
  groupName = groupName,
  site = SiteName,
  district = DistrictName,
  tenure = TotalEnrolledSeasons,
  gLeader = ifelse(Facilitator == "True", 1, 0),
  #first_repaid = FirstRepayment %>% as.Date(),
  #last_repaid = LastRepayment %>% as.Date(),
  txn_size = TotalCredit,
  num_payments = NbOfRepayments,
  pct_repaid = X..Repaid,
  overpaid = TotalRepaid_IncludingOverpayments - TotalRepaid,
  maize = Maize.qty
 ) %>% as.data.frame()

sc17 <- sc17r %>% tbl_df() %>% 
 transmute(
  clientID = GlobalClientID,
  groupName = groupName,
  site = SiteName,
  district = DistrictName,
  tenure = TotalEnrolledSeasons,
  gLeader = ifelse(Facilitator == "True", 1, 0),
  #first_repaid = FirstRepayment %>% as.Date(),
  #last_repaid = LastRepayment %>% as.Date(),
  txn_size = TotalCredit,
  num_payments = NbOfRepayments,
  pct_repaid = X..Repaid,
  overpaid = TotalRepaid_IncludingOverpayments - TotalRepaid,
  maize = Maize.Bundle.qty,
  solar = SKPro2.qty
 ) %>% as.data.frame()

## Vertical repayment data
vr16r <- read.csv(paste(dd, "vertical_2016.csv", sep = "/"), stringsAsFactors = F)
vr17r <- read.csv(paste(dd, "vertical_2017.csv", sep = "/"), stringsAsFactors = F)
#Select variables of interest from each df:
vr16 <- vr16r %>% tbl_df() %>% 
 transmute(
  clientID = GlobalClientID,
  groupName = paste0(Group, "_", Site),
  site = Site,
  district = District,
  status = Dropped,
  amount = Amount,
  type = Type,
  dateRepaid = RepaymentDate %>% as.Date()
 ) %>% as.data.frame()

vr17 <- vr17r %>% tbl_df() %>% 
 transmute(
  clientID = GlobalClientID,
  groupName = paste0(Group, "_", Site),
  site = Site,
  district = District,
  status = Dropped,
  amount = Amount,
  type = Type,
  dateRepaid = RepaymentDate %>% as.Date()
 ) %>% as.data.frame()

## Filter for dropped clients and receipt type
vr16 <- vr16 %>% dplyr::filter(type == "Receipt" & status == "False")
vr17 <- vr17 %>% dplyr::filter(type == "Receipt" & status == "False")

## Merge with sc data
vr16 <- left_join(vr16, sc16 %>% dplyr::select(-site, -district, -groupName),
 by = c("clientID" = "clientID"))
vr17 <- left_join(vr17, sc17 %>% dplyr::select(-site, -district, -groupName),
 by = c("clientID" = "clientID"))

## Updates:
#Converting dates for sc datasets not working well -- revisit?

#### Summary stats ####



#### Time series graphs ####





























