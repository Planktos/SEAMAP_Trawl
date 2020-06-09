#Code to list species functional groups from Name Translator Table in order to pull biomass data from SEAMAP
#Stacy Calhoun; created 5 Aug 2019


remove(list=ls())
assign("last.warning", NULL, envir = baseenv())



# Libraries ---------------------------------------------------------------

library(plyr)
library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)


# Data --------------------------------------------------------------------

NameTrans = read_xlsx("NameTranslator_table06012020.xlsx")

NameTrans = subset(NameTrans, NameTrans$AggGrp_20130430 != 'ignore')



# Subsetting by Functional Group ------------------------------------------

FG_list = split(NameTrans, f = NameTrans$AggGrp_20130430)



FuncList = function(x)
{
  Sp = as.list((unique(x$TAXONOMIC)))
  #names(Sp) = paste("FuncGrp", x, sep = "_")
   return(Sp)
}


Species_List = lapply(FG_list, FuncList)



