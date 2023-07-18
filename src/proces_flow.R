############# FEO indicators / Biodiversity.fi indicators / Rivers without barriers #########################################################################
### This is a proof-of-concept version of automated computation of the Rivers without barriers indicator 
### of the biodiversity.fi website (https://www.biodiversity.fi/en/habitats/inland-waters/iw11-rivers-without-barriers).
### The aim is to automate the production of the indicator using only open source software and publicly available data.
### The computation of the original indicator  was described by Reima Hyytiäinen in his masters thesis work
### "SUOMEN JOKIEN PADOT JA KALATIET 1900-2017 - AIKASARJA KALOILLE VAELLUSKELPOISTEN JOKIUOMIEN MUUTOKSISTA" 
###(https://erepo.uef.fi/handle/123456789/21543, http://urn.fi/urn:nbn:fi:uef-20190863)
### The method used here is different from the original publication, but is designed to produce as similar results as possible (although not identical).
### This code aims to be a technical proof-of-concept and the results have not been double-checked for errors and
### thus should not be taken as true description of the situation. 
### Known errors and shortcomings of the code are listed in the end of this document
### This work was inspired by the development of the Finish Ecosystem Observatory (FEO, www.syke.fi/hankkeet/feo.fi)
### The code takes several hours to run (roughly 8 hours with a i5-8265U CPU @ 1.60GHz, 16 GB RAM). 
### Author: peter.kullberg@syke.fi 
############################################################################################################################################


# Fetch the river and dam data
## Outputs three files to /data folder, on containing the rivers  
## For full description and source of the data see src/data.R file
source("data.R")

## processes data and computes the indicator values
# Outputs two csv-tables containing the  indicator values and a GeoPackage containing spatial information about the rivers to /outputs folder
# /pub_outputs/river_lengths_b.csv contains information about the amount (km and proportion) of rivers in different accessibility classes
# /pub_outputs/barrier_types.csv contains information on proportion of different types of migration barriers
# /outputs/rivers_30km_accessibility.gpkg contains geographic information on rivers and their accessibility
source("indicator.R")

## Runs shiny app locally that visualizes the results
source("app.R")