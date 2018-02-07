##########################################################
# name: claire mcnellan
# purpose: Map state level data on permanency
# date: DECEMBER 22, 2017
# last updated: JAN 05, 2018
##########################################################
##########################################################

rm(list = ls())
ls()
getwd()
setwd("P:/")
library("dplyr")
library("leaflet")
library("maps")
library("maptools")
library("geojsonio")

#########################################################
#########################################################

# bring in data
fips <- read.csv(file = "Planning, Research & Analysis/GAP/US_FIPS_Codes for Programs and Unserved v2.csv", 
                 header = TRUE, 
                 sep = ",", 
                 colClasses = "character")

perm.los <- read.csv(file = "_Staff Folders/Claire McNellan/Gap/Data/PermanencyAndPlacementNum.csv",
                       header = TRUE, sep = "," )

perm.los[!complete.cases(perm.los), ]

# create permanency rate var
perm.los <- transform(perm.los, perm.rate = 
                        (as.numeric(as.character(perm.los$exits_permanency))/
                                      as.numeric(as.character(perm.los$fc_entries_2010))
                        )
)
        
  
#########################################################
#########################################################

# map the states
map.states <- map("state", fill = TRUE, plot = FALSE)

class(map.states)
m <- leaflet(data = map.states) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), 
              stroke = FALSE)

# create a continuous pallette function
pal <- colorNumeric(
  palette = "Blues", 
  domain = as.numeric(perm.los$perm.rate)
)

# create a quantile pallete function
qpal <- colorQuantile("Blues", as.numeric(perm.los$perm.rate), n = 5)

# add continuous colors to map
m %>% 
  addPolygons(stroke = FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = 1, 
              color = ~pal(as.numeric(perm.los$perm.rate))
  )
# add quantiles to map
m %>% addPolygons(stroke = FALSE,
                  smoothFactor = 0.2, 
                  fillOpacity = 1,
                  color = ~qpal(as.numeric(perm.los$perm.rate))
                  )

# 
# states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
# class(states)
# 
# m <- leaflet(states) %>%
#   setView(-96, 37, 8, 4) %>%
#   addTiles()
# 
# m %>% addPolygons

