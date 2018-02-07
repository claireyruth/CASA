##########################################################
# name: claire mcnellan
# purpose: Cleaning first look at 2017 data from 6-mo OJ 
# date: Jan 30, 2018
# last updated: Feb. 7, 2018
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

df2 <- "Planning, Research & Analysis/2017 ALPS/DataCleanedMinusGrantees.csv" %>%
  read.csv(stringsAsFactors = FALSE,
           header = TRUE,
           sep = ",",
           na.strings = c("", " ", "  ", "NA", "n/a", "N/A"),
           colClasses = "character") %>% 
  mutate_at(c("q5a1", "q5a2", "q5a3", "q2a1", "q6a1", "q6a2", "q6a3", "q6a4"), 
            function(x) gsub(",|", "", x)) %>%
              mutate_at(c("q6a4"), 
                        function(x) gsub("~|", "", x))

#########################################################
#########################################################

## Quick look at network-wide response rate ##
# response rate = (Sum of # reporting for 
#                 / Total number of programs)

# rename var that says # of programs reporting for
# anyone that put text ws confirmed as 1, so we will 
# treat the NAs appropriately later on
df2 <- mutate(df2, reporting_for = as.numeric(df2$qo3))

# Correct programs that said they were reporting for more than one county
# instead of more than one program

# 1: Prgs that are not state are reporting for 1
# 1 is a state, 2 is not state
df2$reporting_for[df2$state_org!=1] <- 1

# 2: Prgs that didn't answer are reporting for 1
df2$reporting_for[is.na(df2$reporting_for)] <- 1
# df2 %>% filter(reporting_for == 1) %>% select(qo3, reporting_for) %>% View
df2 %>% filter(reporting_for == 1 & !is.na(qo3) & state_org!=1) %>% select(state_org, qo3, reporting_for) %>% View

# 3: Prgs that included the state office as a program need -1
df2$reporting_for[df2$state == "SC"] <- 37
df2$reporting_for[df2$state == "TX"] <- 71
df2$reporting_for[df2$state == "NM"] <- 14
df2$reporting_for[df2$state == "GA"] <- 45
df2$reporting_for[df2$state == "AZ"] <- 14
df2$reporting_for[df2$state == "NE"] <- 21 # check that this number is the same as Dec 31

# Look at the total
prog.reporting <- summarise(df2, prog.reporting = sum(df2$reporting_for))

# add state (42) plus local (891), subtract not direct service state
# original based on Eric's 1/30/18 email: prog.total <- (42+891) -27
# this is from Erica's spreadsheet
prog.total <- (731+195)

# programs in GA 45
# LA 17
# NE 18
# NM 14
# TX 71
# AR 22
# AZ 14
# NH
# CT 1
# DE
# FL 20
# IA 12
# ME 
# NC 43
# RI
# SC 37
# UT 9
# VT 

# local programs in aggregate states
# locinagg <- 17 + 18 +14 + 71 + 22 + 14 + 1 + 20 + 12 + 43 + 37 + 9
# prog.total <- (42 +891) - locinagg

report.rate <- prog.reporting/prog.total

#########################################################
#########################################################

## Clean children info ##

df2 <- mutate(df2, 
              child_tot_17 = as.numeric(q5a1), 
              child_new_17 = as.numeric(q5a2), 
              case_closed_17 = as.numeric(q5a3))

## Children served
# some respondants used text, so se have to edit those
  # df2 %>% filter(is.na(child_tot_17)) %>%
  # filter(!is.na(q5a1)) %>%
  # select(q2a1, child_tot_17, q5a1) %>%
  # View
df2$child_tot_17[df2$q2a1 == "10626"] <- 49
df2$child_tot_17[df2$q2a1 == "10063"] <- 405 # this one should be flagged as needing to check w program. They served 365 in '16

## New kids
  # df2 %>% filter(is.na(child_new_17)) %>%
  # filter(!is.na(q5a2)) %>%
  # select(q2a1, child_new_17, q5a2) %>%
  # View

## Cases closed
  # df2 %>% filter(is.na(case_closed_17)) %>%
  # filter(!is.na(q5a3)) %>%
  # select(q2a1, case_closed_17, q5a3) %>%
  # View

df2$case_closed_17[df2$q2a1 == "11212"] <- 15

#########################################################
#########################################################


## Determine item level response rate ##

## Imputation:
## Multiplier is 1 / decimal response rate 


# Total N kids served q5a1
rrtotn <- df2$child_tot_17 %>% is.na %>% mean
rrtotn <- 1-rrtotn

tot.kid <- round((1/rrtotn) * sum(df2$child_tot_17, na.rm = TRUE), 
                 digits = 0)
tot.kid

# New kids served q5a2
rrnewkid <- df2$child_new_17 %>% is.na %>% mean
rrnewkid <- 1-rrnewkid

new.kid <- round((1/rrnewkid) * sum(df2$child_new_17, na.rm = TRUE),
                 digits = 0)
new.kid

# Cases closed q5a3
rrcaseclose <- df2$case_closed_17 %>% is.na %>% mean
rrcaseclose <- 1-rrcaseclose

case.closed <- round((1/rrcaseclose) * sum(df2$case_closed_17, na.rm = TRUE),
                     digits = 0)
case.closed

#########################################################
#########################################################

## Clean volunteer info ##

df2 <- mutate(df2, vol_tot_17 = as.numeric(q6a1), vol_new_17 = as.numeric(q6a2), 
              vol_exit_17 = as.numeric(q6a3), vol_hrs_17 = as.numeric(q6a4))

# Vol tot
  # df2 %>% filter(is.na(vol_tot_17)) %>%
  # filter(!is.na(q6a1)) %>%
  # select(q2a1, vol_tot_17, q6a1) %>%
  # View

# New vols
  # df2 %>% filter(is.na(vol_new_17)) %>%
  # filter(!is.na(q6a2)) %>%
  # select(q2a1, vol_new_17, q6a2) %>%
  # View

# Exiting vols
  # df2 %>% filter(is.na(vol_exit_17)) %>%
  # filter(!is.na(q6a3)) %>%
  # select(q2a1, vol_exit_17, q6a3) %>%
  # View

#  Vol hours
#   df2 %>% filter(is.na(vol_hrs_17)) %>%
#   filter(!is.na(q6a4)) %>%
#   select(q2a1, vol_hrs_17, q6a4) %>%
#   View

df2$vol_hrs_17[df2$q2a1 == "10067"] <- 2724
df2$vol_hrs_17[df2$q2a1 == "10905"] <- 7436 # they say only 55% of vols report


# Active vols q6a1

rrvols <- df2$vol_tot_17 %>% is.na %>% mean
rrvols <- 1-rrvols

tot.vol <- round((1/rrvols) * sum(df2$vol_tot_17, na.rm = TRUE), 
                 digits = 0)
tot.vol

# New vols q6a2

rrnewvol <- df2$vol_new_17 %>% is.na %>% mean
rrnewvol <- 1-rrnewvol

tot.newvol <- round((1/rrnewvol) * sum(df2$vol_new_17, na.rm = TRUE),
                    digits = 0)
tot.newvol

# Resigned/terminated q6a3

rrterm <- df2$vol_exit_17 %>% is.na %>% mean
rrterm <- 1-rrterm

vol.term <- round((1/rrterm) * sum(df2$vol_exit_17, na.rm = TRUE), 
                  digits = 0)
vol.term

# Total Vol Hrs q6a4

df2$vol_hrs_17[df2$vol_hrs_17 == 0] <- NA
rrvolhr <- df2$vol_hrs_17 %>% is.na %>% mean
rrvolhr <- 1-rrvolhr

tot.volhrs <- round((1/rrvolhr) * sum(df2$vol_hrs_17, na.rm = TRUE),
                    digits = 0)
tot.volhrs

# write to csv
blank <- ""
KPI <- cbind(prog.reporting, prog.total, report.rate, 
             tot.vol, tot.volhrs, tot.newvol, vol.term,
             tot.kid, new.kid, case.closed, blank, blank, blank,
             blank, blank, blank, blank)

write.csv(KPI, file = "Planning, Research & Analysis/2017 ALPS/FirstLookKeyIndicators4.csv")

#########################################################
#########################################################

## First Look Annual Variables ## <-- REDO THIS WITH IMPUTED NUMBERS

df2 <- mutate(df2, vol_avg = vol_hrs_17/as.numeric(vol_tot_17))

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
