## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read 2024 ACS 5-year sample
ddi_acs = read_ipums_ddi("data/input/usa_00035.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
  rename_with(tolower) %>%
  select(year, multyear, perwt, hhwt, 
    bpl, bpld, citizen, yrnatur, yrimmig,
    empstat, empstatd, labforce, occ, occsoc, ind, indnaics)
