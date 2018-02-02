##########################################################
# name: claire mcnellan
# purpose: Cleaning first look at 2017 data from 6-mo OJ 
# date: Jan 30, 2018
# last updated: Feb. 1, 2018
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
df2 <- read.csv(file = "Planning, Research & Analysis/2017 ALPS/DataCleanedMinusGrantees.csv", 
               header = TRUE,
               sep = ",",
               na.strings = c("", " ", "  "),
               colClasses = "character"
)

#########################################################
#########################################################

## Quick look at network-wide response rate ##
# response rate = (Sum of # reporting for 
#                 / Total number of programs)

# rename var that says # of programs reporting for
df2$reporting_for <- as.numeric(df2$qo3)

# Correct programs that said they were reporting for more than one county
# instead of more than one program

# 1: Prgs that are not state are reporting for 1
df2$reporting_for[df2$state_org!=1] <- 1

# 2: Prgs that didn't answer are reporting for 1
df2$reporting_for[is.na(df2$reporting_for)] <- 1

# 3: Prgs that included the state office as a program need -1
df2$reporting_for[df2$state == "SC"] <- 37
df2$reporting_for[df2$state == "TX"] <- 71
df2$reporting_for[df2$state == "NM"] <- 14
df2$reporting_for[df2$state == "GA"] <- 45
df2$reporting_for[df2$state == "AZ"] <- 14
df2$reporting_for[df2$state == "NE"] <- 18

# Look at the total
prog.reporting <- sum(df2$reporting_for)

# the following totals come from Erica 1/30/18 email
# add state plus local, subtract not direct service state
prog.total <- (42+891)-(33)
report.rate <- prog.reporting/prog.total

#########################################################
#########################################################

## Determine item level response rate
# Total N kids served q5a1
df2$q5a1 <- as.numeric(df2$q5a1)

rrtotn <- df2$q5a1 %>% is.na %>% mean
rrtotn <- 1-rrtotn

tot.kid <- (1/rrtotn) * sum(df2$q5a1, na.rm = TRUE)
tot.kid

# New kids served q5a2
df2$q5a2 <- as.numeric(df2$q5a2)

rrnewkid <- df2$q5a2 %>% is.na %>% mean
rrnewkid <- 1-rrnewkid

new.kid <- (1/rrnewkid) * sum(df2$q5a2, na.rm = TRUE)
new.kid


# Cases closed q5a3

df2$q5a3 <- as.numeric(df2$q5a3)
rrcaseclose <- df2$q5a3 %>% is.na %>% mean
rrcaseclose <- 1-rrcaseclose

case.closed <- (1/rrcaseclose) * sum(df2$q5a3, na.rm = TRUE)
case.closed


# Active vols q6a1

# New vols q6a2

df2$q6a2 <- as.numeric(df2$q6a2)
rrnewvol <- df2$q6a2 %>% is.na %>% mean
rrnewvol <- 1-rrnewvol

tot.newvol <- (1/rrnewvol) * sum(df2$q6a2, na.rm = TRUE)
tot.newvol

# Resigned/terminated q6a3

# Total Vol Hrs q6a4



# Imputation:
# Multiplier is 1 / decimal response rate 



#########################################################
#########################################################

## First Look Annual Variables ## <-- REDO THIS WITH IMPUTED NUMBERS

# volunteer info
df2 <- mutate(df2, vol_tot_17 = as.numeric(q6a1), vol_new_17 = as.numeric(q6a2), 
              vol_exit_17 = as.numeric(q6a3), vol_hrs_17 = as.numeric(q6a4))
df2 <- mutate(df2, vol_avg = vol_hrs_17/as.numeric(vol_tot_17))

# children served info
df2 <- mutate(df2, child_tot_17 = as.numeric(q5a1), child_new_17 = as.numeric(q5a2), 
              case_closed_17 = as.numeric(q5a3))

# check for outliers
p1 <- ggplot(df2, aes(group = state_org, x = df2$state_org, y = df2$vol_avg)) + 
  geom_boxplot()

p1.5 <- ggplot(df2, aes(x = df2$q2a1, y = df2$vol_hrs)) +
  geom_point() +
  geom_text(aes(label= q2a1), hjust=0, vjust=0)
p2 <- ggplot(df2, aes(x = df2$vol_hrs, y = df2$vol_tot, label = q2a2)) +
  geom_point() +
  geom_text(aes(label= q2a1), hjust=0, vjust=0)
p3 <- hist(df2$vol_avg)
p4 <- ggplot(df2, aes(x = df2$child_tot_17, y = df2$vol_avg, label = q2a2)) +
  geom_point() +
  geom_text(aes(label= q2a1), hjust=0, vjust=0)
