 ## set gobal options
 options(stringsAsFactors=TRUE, shiny.error=traceback)

## Load dependencies
library(zoo)
library(shiny)
library(shinyBS)
library(shinyTable)
library(data.table)
library(devtools)
library(plyr)
library(dplyr)
library(testthat)
library(ggplot2)
library(ggfortify)

## Set global parameters
# dir params
main_DIR="data"
project_names_VCHR=c(crabs="Crabs", spiders="Spiders", `mangrove herbivory`="Mangroves_Herbivory", `mangrove community`="Mangroves_Community", `vegetation assessment`="Vegetation_Assessment")
group_colors_VCHR=c(blue="blue",green="green", red="red", yellow="yellow")
week_numbers_VCHR=paste("Week", seq_len(2))
names(week_numbers_VCHR)=tolower(week_numbers_VCHR)

## Source code
source("classes.R")
source("functions.R")


