# check refugee data pull, and recode country names
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

refugees = read_csv("data/output/refugee_arrivals_by_nationality_fy2021_fy2026.csv")

# total refugees by year and unique nationalities
refugees %>%
    group_by(fy) %>%
    summarise(n_countries = n_distinct(nationality),
        total_refugees = sum(arrivals),
        .groups = "drop")

unique(refugees$nationality)

