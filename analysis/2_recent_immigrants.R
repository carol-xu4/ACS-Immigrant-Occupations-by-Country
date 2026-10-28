## Preliminaries---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS ONE YEAR SAMPLES
acs = read_rds("data/output/acs_recent.RDS")

occ_labels = read_csv("data/output/occ2018_labels.csv")

# recent immigrants: [YEAR] - [YRIMMIG] = 1