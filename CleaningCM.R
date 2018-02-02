##########################################################
# name: claire mcnellan
# purpose: Cleaning first look at 2017 data from 6-mo OJ 
# date: Jan 30, 2018
# last updated: Jan 31, 2018
##########################################################
##########################################################

rm(list = ls())
ls()
getwd()
setwd("P:/")
library("ggplot2")
library("dplyr")
library("foreign")

#########################################################
#########################################################

## bring in data ##
df <- read.csv(file = "Planning, Research & Analysis/2017 ALPS/final data from six month.csv", 
               header = TRUE,
               sep = ",",
               na.strings = c("", " ", "  "),
               colClasses = "character"
)

#########################################################
#########################################################

## A bit of tidying ##

# Correctly identify state organizations
# got these program IDs by looking at data
notstates <- c("10155", "10153", "10147", "10168", "11498", 
               "10203", "10390", "10405", "10464", "10931", 
               "11538", "11525", "11000", "10998", "10053")

# for this var: 2 = not state, 1 = state
# q2a1 = prog_ID 
df$qv3[df$q2a1 %in% notstates] <- "2"

# Make state org dummy variable
df$state_org <- as.numeric(df$qv3 == "1")

# Correct wrong program IDs and names
df$q2a2[df$q2a1 == "10155"] <- "CASA of Kendall County" 
df$q2a1[df$q2a1 == "4392256"] <- "11439" 
df$q2a1[df$q2a1 == "1051278"] <-  "11442"
df$q2a1[df$q2a3 == "Mt Vernon"] <-  "11141"
df$q2a1[df$q2a1 == "9231129"] <-  "11293"
df$q2a1[df$q2a1 == "4987"] <-  "10310"
df$q2a1[df$q2a1 == "6203"] <-  "10408"
df$q2a1[df$q2a1 == "1051278"] <-  "10394"
df$q2a1[df$q2a3 == "Shawnee"] <-  "11088"
df$q2a1[df$q2a1 == "61"] <-  "10496"
df$q2a1[df$q2a1 == "826"] <-  "10505"
df$q2a1[df$q2a1 == "676"] <-  "10606"

# Correct State names
df$state <- df$q2a4

# Alabama
AL <- c("AL", "Alabama")
df$state[df$q2a4 %in% AL] <- "AL"

# Alaska	AK
AK <- c("AK", "Alaska")
df$state[df$q2a4 %in% AK] <- "AK"

# Arizona	AZ
AZ <- c("AZ", "Arizona", "Arizona ")
df$state[df$q2a4 %in% AZ] <- "AZ"

# Arkansas	AR
AR <- c("AR", "Arkansas", "Arkansas ")
df$state[df$q2a4 %in% AR] <- "AR"

# California	CA
CA <- c("CA", "California", "CA
")
df$state[df$q2a4 %in% CA] <- "CA"

# Colorado	CO
CO <- c("CO", "Colorado", "Colordao")
df$state[df$q2a4 %in% CO] <- "CO"

# Connecticut	CT (okay)

# Delaware	DE (okay)

# Florida	FL (okay)

# Georgia	GA
GA <- c("GA", "Georgia")
df$state[df$q2a4 %in% GA] <- "GA"

# Hawaii	HI
HI <- c("HI", "Hawaii", "HAWAII")
df$state[df$q2a4 %in% HI] <- "HI"

# Idaho	ID
ID <- c("ID", "Idaho")
df$state[df$q2a4 %in% ID] <- "ID"

# Illinois	IL
IL <- c("IL", "Illinois")
df$state[df$q2a4 %in% IL] <- "IL"

# Indiana	IN
IN <- c("IN", "Indiana")
df$state[df$q2a4 %in% IN] <- "IN"

# Iowa	IA
IA <- c("IA", "Iowa")
df$state[df$q2a4 %in% IA] <- "IA"

# Kansas	KS
KS <- c("KS", "Kansas")
df$state[df$q2a4 %in% KS] <- "KS"

# Kentucky	KY
KY <- c("KY", "Kentucky")
df$state[df$q2a4 %in% KY] <- "KY"

# Louisiana	LA
LA <- c("LA", "Louisiana")
df$state[df$q2a4 %in% LA] <- "LA"

# Maine	ME
ME <- c("ME", "Maine")
df$state[df$q2a4 %in% ME] <- "ME"

# Maryland	MD
MD <- c("MD", "Maryland")
df$state[df$q2a4 %in% MD] <- "MD"

# Massachusetts	MA
MA <- c("MA", "Massachusetts")
df$state[df$q2a4 %in% MA] <- "MA"

# Michigan	MI
MI <- c("MI", "Michigan")
df$state[df$q2a4 %in% MI] <- "MI"

# Minnesota	MN
MN <- c("MN", "Minnesota")
df$state[df$q2a4 %in% MN] <- "MN"

# Mississippi	MS
MS <- c("MS", "Mississippi")
df$state[df$q2a4 %in% MS] <- "MS"

# Missouri	MO
MO <- c("MO", "Missouri", "MISSOURI")
df$state[df$q2a4 %in% MO] <- "MO"

# Montana	MT
MT <- c("MT", "Montana")
df$state[df$q2a4 %in% MT] <- "MT"

# Nebraska	NE
NE <- c("NE", "Nebraska")
df$state[df$q2a4 %in% NE] <- "NE"

# Nevada	NV
NV <- c("NV", "Nevada")
df$state[df$q2a4 %in% NV] <- "NV"

# New Hampshire	NH
NH <- c("NH", "New Hampshire")
df$state[df$q2a4 %in% NH] <- "NH"

# New Jersey	NJ
NJ <- c("NJ", "New Jersey")
df$state[df$q2a4 %in% NJ] <- "NJ"

# New Mexico	NM
NM <- c("NM", "New Mexico")
df$state[df$q2a4 %in% NM] <- "NM"

# New York	NY
NY <- c("NY", "New York")
df$state[df$q2a4 %in% NY] <- "NY"

# North Carolina	NC
NC <- c("North Carolina")
df$state[df$q2a4 %in% NC] <- "NC"

# North Dakota	ND
ND <- c("ND", "North Dakota")
df$state[df$q2a4 %in% ND] <- "ND"

# Ohio	OH
OH <- c("OH", "ohio", "Ohio")
df$state[df$q2a4 %in% OH] <- "OH"

# Oklahoma	OK
OK <- c("OK", "Oklahoma", "State") # yes, one person wrote "state" in OK
df$state[df$q2a4 %in% OK] <- "OK"

# Oregon	OR
OR <- c("OR", "Oregon")
df$state[df$q2a4 %in% OR] <- "OR"

# Pennsylvania	PA
PA <- c("PA", "Pennsylvania")
df$state[df$q2a4 %in% PA] <- "PA"

# Rhode Island	RI
RI <- c("RI", "Rhode Island")
df$state[df$q2a4 %in% RI] <- "RI"

# South Carolina	SC
SC <- c("SC", "South Carolina", "South Carilina")
df$state[df$q2a4 %in% SC] <- "SC"

# South Dakota	SD
SD <- c("SD", "South Dakota")
df$state[df$q2a4 %in% SD] <- "SD"

# Tennessee	TN
TN <- c("TN", "Tennessee")
df$state[df$q2a4 %in% TN] <- "TN"

# Texas	TX
TX <- c("TX", "Texas")
df$state[df$q2a4 %in% TX] <- "TX"

# Utah	UT
UT <- c("UT", "Utah")
df$state[df$q2a4 %in% UT] <- "UT"

# Vermont	VT
VT <- c("VT", "Vermont")
df$state[df$q2a4 %in% VT] <- "VT"

# Virginia	VA
VA <- c("VA", "Virginia")
df$state[df$q2a4 %in% VA] <- "VA"

# Washington	WA
WA <- c("WA", "Washington", "Washington ")
df$state[df$q2a4 %in% WA] <- "WA"

# West Virginia	WV
WV <- c("WV", "West Virginia")
df$state[df$q2a4 %in% WV] <- "WV"

# Wisconsin	WI
WI <- c("WI", "Wisconsin")
df$state[df$q2a4 %in% WI] <- "WI"

# Wyoming	WY
WY <- c("WY", "Wyoming")
df$state[df$q2a4 %in% WY] <- "WY"

## Save a version with grantees ##
write.csv(df, paste0("Planning, Research & Analysis/2017 ALPS/DataCleaned", ".csv"))

# Drop any grantees that reported
# Don't want to double count for these purposes
grantees_in_aggregate <- c("10749",
              "10536",
              "10541",
              "10532",
              "10116",
              "10086",
              "10084",
              "10025",
              "10749",
              "10515",
              "10749",
              "11438",
              "11228",
              "10514",
              "10491",
              "10112",
              "10025",
              "10018")

df2 <- filter(df, !q2a1 %in% grantees_in_aggregate)

# Again, to avoid double count, look for orgs that did
# not need to respond because their state did so

aggregate_states <- c("GA", "LA", "NE", "NM", "TX", "AR", "AZ", "NH", "CT",
                      "DE", "FL", "IA", "ME", "NC", "RI", "SC", "UT", "VT")

df3 <- filter(df2, df2$state %in% aggregate_states)
dups <- df3 %>% count(state)
dups

# it only happened in NE, so we drop those that aren't state org
df2 <- filter(df2, df2$q2a1 != "11354")



#########################################################
#########################################################

## Save a version with grantees ##
write.csv(df2, paste0("Planning, Research & Analysis/2017 ALPS/DataCleanedMinusGrantees", ".csv"))

