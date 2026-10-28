## OCCUPATION TABLES FOR INDIVIDUAL COUNTRIES

## Preliminaries -------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS -----------------------------------------------------------------------------
acs = read_rds("data/output/acs.RDS")

# tables
dir.create("results/country_tables", recursive = TRUE, showWarnings = FALSE)

# Immigrants only (naturalized + noncitizen)
immig = acs %>%
  filter(country != "United States", citizen %in% c(2, 3))

# Country totals
country_totals = immig %>%
  group_by(country) %>%
  summarise(n               = n(),                                              # unweighted, all ages
            pop             = sum(perwt),                                       # weighted, all ages
            n_workers_18_64 = sum(empstat == 1 & age >= 18 & age <= 64),        # unweighted
            workers_18_64   = sum(perwt[empstat == 1 & age >= 18 & age <= 64]), # weighted
            .groups = "drop")

# All occupations for employed 18-64, sorted largest to smallest within each country
occ_by_country = immig %>%
  filter(empstat == 1, age >= 18, age <= 64) %>%
  mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
  group_by(country, occ) %>%
  summarise(occ_n       = n(),            # unweighted workers in that occupation
            occ_workers = sum(perwt),     # weighted workers in that occupation
            .groups = "drop_last") %>%
  mutate(occ_share = occ_workers / sum(occ_workers)) %>%
  arrange(desc(occ_workers), .by_group = TRUE) %>%
  ungroup()

# adding ipums occ labels
occ_labels = read_html("https://usa.ipums.org/usa/volii/occ2018.shtml") %>%
  html_elements("table") %>% .[[2]] %>%
  html_table(header = FALSE) %>%
  select(occ = X1, occ_title = X2) %>%
  filter(str_detect(occ, "^\\d{3,4}$")) %>%          # drop section headings
  mutate(occ = sprintf("%04d", as.integer(occ))) %>%
  distinct(occ, .keep_all = TRUE)

write_csv(occ_labels, "data/output/occ2018_labels.csv") 

occ_by_country = occ_by_country %>%
  left_join(occ_labels, by = "occ") %>%
  relocate(occ_title, .after = occ)

country_tables = country_totals %>%
  left_join(occ_by_country, by = "country")

# One CSV per country
for (c in unique(country_tables$country)) {
  out   = country_tables %>% filter(country == c)
  fname = str_replace_all(c, "[^A-Za-z0-9]+", "_")
  write_csv(out, file.path("results/country_tables", paste0(fname, ".csv")))}
