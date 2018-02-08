##########################################################
# name: claire mcnellan
# purpose: Identify programs with revic rates much higher
# or lower than their state avg
# date: Feb 7, 2018
# last updated: 
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

## Bring in state-level revictimization data ##
statedf <- "/Planning, Research & Analysis/Gap/Copy of Revictimized within 6 months of placement (003).csv" %>%
  read.csv(stringsAsFactors = FALSE,
           header = TRUE,
           sep = ",",
           colClasses = "character") %>% 
  mutate_at(c("rev.6mo", "rev.2016", "victims.2010", "prop.rev.6mo", "prop.rev.2016"), 
            function(x) gsub(",|", "", x)
            )

## Bring in program-level revictimization data ##
prgdf <- read.csv(file = "Planning, Research & Analysis/2016 ALPS/Local 2016 data Sept.csv", 
               header = TRUE,
               sep = ",",
               colClasses = "character",
               na.strings =c( " ", "", "  ")
)
head(prgdf)
colnames(prgdf)[1] <- "prog_id"


## keep the variables we are interested in ##

prgdf <- prgdf %>% select(prog_id, prog_name, city, state,
                     recidivism, unserved, average_LOS, 
                     cost_child_16, prog_age, cost_vol_16, 
                     Vol_Status, ql8, ql7) 

## Merge the two datasets ##

df2 <- left_join(prgdf, statedf, by = "state")

#########################################################
#########################################################

# create difference var 

# difference = program recid rate - state 6 mo recid rate
# therefore, larger = programs with recid rates much higher than their state avg
# negative numbers are programs with recid rates lower than their state avg
# we ask about reopened cases in the last year, so we will use the 2016 rate for 
# comparison
df2 <- mutate(df2, diff = as.numeric(recidivism) - as.numeric(prop.rev.2016))
df2$diff
mboxplot <- boxplot(df2$diff)

# df2$outliers <- (mboxplot$out)

# 13 programs are sig lower than their state avg
# 11 programs are sig higher than their state avg


