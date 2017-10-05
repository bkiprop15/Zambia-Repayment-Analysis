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

# for(i in 1:length(libs)) {
#  if(!require(libs[i])) {
#   install.packages(libs[i], dependencies = T)
#   #require(libs[i])
#  }
#  #require(libs[i])
# } #Ignore this for now; keeps re-installing packages
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
  maize = Maize.qty,
  completed = ifelse(pct_repaid >= 100, 1, 0)#Check for completion
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
  solar = SKPro2.qty,
  completed = ifelse(pct_repaid >= 100, 1, 0)#Check for completion
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
#We'll get the summary stats simply using 'View(df)'
### 2017 tables:
## Breakdown by maize acreage
#Note: there are people who didn't take any maize (24, but all are staff)
mzt17 <- sc17 %>% tbl_df() %>% group_by(maize) %>% 
 summarize(
  txn_size = mean(txn_size),
  num_payments = mean(num_payments),
  pct_repaid = mean(pct_repaid),
  overpaid = mean(overpaid),
  solar = mean(solar)
 ) %>% ungroup() %>% as.data.frame()

## Breakdown by completion rate
cmt17 <- sc17 %>% tbl_df() %>% group_by(completed) %>% 
 summarize(
  numClients = n(),
  txn_size = mean(txn_size),
  pct_repaid = mean(pct_repaid),
  num_payments = mean(num_payments),
  solar = mean(solar),
  maize = mean(maize),
  overpaid = mean(overpaid)
 ) %>% ungroup() %>% as.data.frame()

### 2016 tables
## No maize acreage breakdown (all clients offered 1 ha)

## Breakdown by completion rate

cmt16 <- sc16 %>% tbl_df() %>% group_by(completed) %>% 
 summarize(
  numClients = n(),
  txn_size = mean(txn_size),
  pct_repaid = mean(pct_repaid),
  num_payments = mean(num_payments),
  overpaid = mean(overpaid)
 ) %>% ungroup %>% as.data.frame()

#### Time series graphs ####
### Objective: create weekly time series graphs

### 2017
## First let's create a week number indicator
mnDate <- min(vr17$dateRepaid)
#Formula below from: http://r.789695.n4.nabble.com/Week-number-from-a-date-td4410223.html
vr17$weekNo <- floor((as.numeric(vr17$dateRepaid - mnDate))/7)

## For % repaid, we'll also group by clientID
vr17Clients <- vr17 %>% tbl_df() %>% group_by(weekNo, clientID) %>%
 summarize(
  amount = sum(amount),
  #txn_size = mean(txn_size), #same size for each client, so will come out same
  maize = mean(maize), #same size for each client, so will come out same
  completed = mean(completed)
 ) %>% ungroup() %>% as.data.frame()

## Comparing across the 4 package sizes – 2017
#Create each package size separately by filtering clients for pckg size then
#group clients by week no

#half ha
#First get total txn size for clients who took this maize package
totPhalf <- sc17 %>% filter(maize == 0.5) %>% select(txn_size) %>% sum()
#Then calculate cumm. weekly repayment for this maize package
phalf <- vr17Clients %>% tbl_df() %>% filter(maize == 0.5) %>% group_by(weekNo) %>% 
 summarize(
#  txn_size = sum(txn_size),
  amount = sum(amount),
  pct_repaid = (amount / totPhalf) * 100
 ) %>% ungroup() %>% as.data.frame()
#For each packg group, we'll have to calculate the cummulatives
#We could use for loops
for(i in 1:nrow(phalf)) {
 if(i == 1) {
  phalf$pct_repaid_cumm[i] <- phalf$pct_repaid[i]
 }
 else{
  phalf$pct_repaid_cumm[i] <- phalf$pct_repaid[i] + phalf$pct_repaid_cumm[i-1]
 }
}
phalf <- phalf %>% select(weekNo, pct_repaid_cumm)
#We could also use mutate -- will do from now to shorten code
#Mutate suggestion from: https://stackoverflow.com/questions/33038240/add-row-value-to-previous-row-value-in-r
#Ignore mutate; adds row n-1 to row n, but not the new cumm total
# phalf <- phalf %>% mutate(amount_cumm = amount + lag(amount, default = 0)) %>% 
#  select(weekNo, pct_repaid_cumm, amount_cumm)

#One ha
totPone <- sc17 %>% filter(maize == 1) %>% select(txn_size) %>% sum()
pone <- vr17Clients %>% tbl_df() %>% filter(maize == 1) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPone) * 100
 ) %>% ungroup() %>% as.data.frame()
for(i in 1:nrow(pone)) {
 if(i == 1) {
  pone$pct_repaid_cumm[i] <- pone$pct_repaid[i]
 }
 else{
  pone$pct_repaid_cumm[i] <- pone$pct_repaid[i] + pone$pct_repaid_cumm[i-1]
 }
}
pone <- pone %>% select(weekNo, pct_repaid_cumm)


#One half ha
totPonehalf <- sc17 %>% filter(maize == 1.5) %>% select(txn_size) %>% sum()
ponehalf <- vr17Clients %>% tbl_df() %>% filter(maize == 1.5) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPonehalf) * 100
 ) %>% ungroup() %>% as.data.frame()
for(i in 1:nrow(ponehalf)) {
 if(i == 1) {
  ponehalf$pct_repaid_cumm[i] <- ponehalf$pct_repaid[i]
 }
 else{
  ponehalf$pct_repaid_cumm[i] <- ponehalf$pct_repaid[i] + ponehalf$pct_repaid_cumm[i-1]
 }
}
ponehalf <- ponehalf %>% select(weekNo, pct_repaid_cumm)


#Two ha
totPtwo <- sc17 %>% filter(maize == 2) %>% select(txn_size) %>% sum()
ptwo <- vr17Clients %>% tbl_df() %>% filter(maize == 2) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPtwo) * 100
 ) %>% ungroup() %>% as.data.frame()
for(i in 1:nrow(ptwo)) {
 if(i == 1) {
  ptwo$pct_repaid_cumm[i] <- ptwo$pct_repaid[i]
 }
 else{
  ptwo$pct_repaid_cumm[i] <- ptwo$pct_repaid[i] + ptwo$pct_repaid_cumm[i-1]
 }
}
ptwo <- ptwo %>% select(weekNo, pct_repaid_cumm)


#Three ha
totPthree <- sc17 %>% filter(maize == 3) %>% select(txn_size) %>% sum()
pthree <- vr17Clients %>% tbl_df() %>% filter(maize == 3) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPthree) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pthree)) {
 if(i == 1) {
  pthree$pct_repaid_cumm[i] <- pthree$pct_repaid[i]
 }
 else{
  pthree$pct_repaid_cumm[i] <- pthree$pct_repaid[i] + pthree$pct_repaid_cumm[i-1]
 }
}
pthree <- pthree %>% select(weekNo, pct_repaid_cumm)

#Plots for pct repaid
(p1 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = phalf$pct_repaid_cumm), data = phalf, color = "red") +
 geom_line(aes(y = pone$pct_repaid_cumm), data = pone, color = "green") +
 geom_line(aes(y = ponehalf$pct_repaid_cumm), data = ponehalf, color = "yellow") +
 geom_line(aes(y = ptwo$pct_repaid_cumm), data = ptwo, color = "blue") +
 geom_line(aes(y = pthree$pct_repaid_cumm), data = pthree, color = "purple") +
 theme_bw()+
 scale_color_hue(labels = c("Half", "One", "One Half", "Two", "Three")) +
 theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))
 

## Comparing finisher vs. non-finisher clients – 2017
#Finishers
totPcomplete <- sc17 %>% filter(completed == 1) %>% select(txn_size) %>% sum()
pcomplete <- vr17Clients %>% tbl_df() %>% filter(completed == 1) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPcomplete) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pcomplete)) {
 if(i == 1) {
  pcomplete$pct_repaid_cumm[i] <- pcomplete$pct_repaid[i]
 }
 else{
  pcomplete$pct_repaid_cumm[i] <- pcomplete$pct_repaid[i] + pcomplete$pct_repaid_cumm[i-1]
 }
}
pcomplete <- pcomplete %>% select(weekNo, pct_repaid_cumm)

#Non-finishers
totPnoncomplete <- sc17 %>% filter(completed == 0) %>% select(txn_size) %>% sum() 
pnoncomplete <- vr17Clients %>% tbl_df() %>% filter(completed == 0) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPnoncomplete) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pnoncomplete)) {
 if(i == 1) {
  pnoncomplete$pct_repaid_cumm[i] <- pnoncomplete$pct_repaid[i]
 }
 else{
  pnoncomplete$pct_repaid_cumm[i] <- pnoncomplete$pct_repaid[i] + pnoncomplete$pct_repaid_cumm[i-1]
 }
}
pnoncomplete <- pnoncomplete %>% select(weekNo, pct_repaid_cumm)

#Plot for finisher vs. non-finisher:
(p2 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = pcomplete, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pnoncomplete, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))

#----------------------

## Comparing finisher vs. non-finisher clients – 2016

#First let's create a week number indicator
mnDate <- min(vr16$dateRepaid)
vr16$weekNo <- floor((as.numeric(vr16$dateRepaid - mnDate))/7)

## For % repaid, we'll also group by clientID
vr16Clients <- vr16 %>% tbl_df() %>% group_by(weekNo, clientID) %>%
 summarize(
  amount = sum(amount),
  #txn_size = mean(txn_size), #same size for each client, so will come out same
  maize = mean(maize), #same size for each client, so will come out same
  completed = mean(completed)
 ) %>% ungroup() %>% as.data.frame()

totPcomplete16 <- sc16 %>% filter(completed == 1) %>% select(txn_size) %>% sum()
pcomplete16 <- vr16Clients %>% tbl_df() %>% filter(completed == 1) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPcomplete16) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pcomplete16)) {
 if(i == 1) {
  pcomplete16$pct_repaid_cumm[i] <- pcomplete16$pct_repaid[i]
 }
 else{
  pcomplete16$pct_repaid_cumm[i] <- pcomplete16$pct_repaid[i] + pcomplete16$pct_repaid_cumm[i-1]
 }
}
pcomplete16 <- pcomplete16 %>% select(weekNo, pct_repaid_cumm)

#Non-finishers
totPnoncomplete16 <- sc16 %>% filter(completed == 0) %>% select(txn_size) %>% sum() 
pnoncomplete16 <- vr16Clients %>% tbl_df() %>% filter(completed == 0) %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPnoncomplete16) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pnoncomplete16)) {
 if(i == 1) {
  pnoncomplete16$pct_repaid_cumm[i] <- pnoncomplete16$pct_repaid[i]
 }
 else{
  pnoncomplete16$pct_repaid_cumm[i] <- pnoncomplete16$pct_repaid[i] + pnoncomplete16$pct_repaid_cumm[i-1]
 }
}
pnoncomplete16 <- pnoncomplete16 %>% select(weekNo, pct_repaid_cumm)

#Plot for finisher vs. non-finisher:
(p3 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = pcomplete16, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pnoncomplete16, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))



## Comparing finisher clients – 2016 vs. 2017
(p4 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = pcomplete, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pcomplete16, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))


## Comparing  non-finisher clients – 2016 vs. 2017
(p4 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = pnoncomplete, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pnoncomplete16, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))

## Comparing clients 2016 clients to those who took 1 ha in 2017
#Note: everyone in 2016 took 1ha

totPone16 <- sc16 %>% select(txn_size) %>% sum()
pone16 <- vr16Clients %>% tbl_df() %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totPone16) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(pone16)) {
 if(i == 1) {
  pone16$pct_repaid_cumm[i] <- pone16$pct_repaid[i]
 }
 else{
  pone16$pct_repaid_cumm[i] <- pone16$pct_repaid[i] + pone16$pct_repaid_cumm[i-1]
 }
}
pone16 <- pone16 %>% select(weekNo, pct_repaid_cumm)

(p5 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = pone, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pone16, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))


## Overall comparison (all clients 2017 vs. all clients 2017)

totP17 <- sc17 %>% select(txn_size) %>% sum()
tot17 <- vr17Clients %>% tbl_df() %>% group_by(weekNo) %>% 
 summarize(
  amount = sum(amount),
  pct_repaid = (amount / totP17) * 100
 ) %>% ungroup() %>% as.data.frame()

for(i in 1:nrow(tot17)) {
 if(i == 1) {
  tot17$pct_repaid_cumm[i] <- tot17$pct_repaid[i]
 }
 else{
  tot17$pct_repaid_cumm[i] <- tot17$pct_repaid[i] + tot17$pct_repaid_cumm[i-1]
 }
}
tot17 <- tot17 %>% select(weekNo, pct_repaid_cumm)

(p6 <- ggplot(NULL, aes(x = weekNo)) +
 geom_line(aes(y = pct_repaid_cumm, color = "Finishers"),
  data = tot17, color = "green") +
 geom_line(aes(y = pct_repaid_cumm, color = "Non-finishers"),
  data = pone16, color = "red") +
 theme_bw()+
 scale_color_manual(name = "Legend:", values = cols) +
 #theme(legend.position = "bottom") +
 #theme(legend.text = c("Half", "One", "One half", "Two", "Three")) +
 labs(title = "",
  x = "Week Number", y = "Cumm % repaid"))























