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

# All occupations for employed, sorted largest to smallest within each country
occ_by_country = immig %>%
  filter(empstat == 1) %>%
  mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
  group_by(country, occ) %>%
  summarise(n_workers = n(),            # unweighted workers in that occupation
            workers   = sum(perwt),     # weighted workers in that occupation
            .groups = "drop_last") %>%
  mutate(occ_share = workers / sum(workers)) %>%
  arrange(desc(workers), .by_group = TRUE) %>%
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

country_tables = occ_by_country %>%
  left_join(occ_labels, by = "occ") %>%
  relocate(occ_title, .after = occ) %>%
  select(country, n_workers, workers, occ, occ_title, occ_share)

# One CSV per country
for (c in unique(country_tables$country)) {
  out   = country_tables %>% filter(country == c)
  fname = str_replace_all(c, "[^A-Za-z0-9]+", "_")
  write_csv(out, file.path("results/country_tables", paste0(fname, ".csv")))}
