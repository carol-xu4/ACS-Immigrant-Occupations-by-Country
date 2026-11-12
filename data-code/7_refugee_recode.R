## Preliminaries ----------------------------------------------------------------------------------
pacman::p_load(tidyverse, readxl)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

source("data-code/5_IV_NIV_recoding.R")   # for recode_visa_country() and acs_bpl

## Read in four data sources: ACS, IV+NIV combined data, refugees, asylees ------------------------
acs = read_rds("data/output/acs.RDS")

visas = read_csv("data/output/visa_country_monthly.csv")

## Refugees, by country and calendar month, FY2021-FY2026 -----------------------------------------
refugee_path = "C:/Users/CarolXu/Downloads/Refugee Admissions Report as of October 31, 2026 (1).xlsx"

read_refugee_sheet = function(fy) {
    raw = read_excel(refugee_path, sheet = as.character(fy), col_names = FALSE)
    raw %>%
        transmute(
            country = str_trim(.[[2]]),
            OCT = as.numeric(.[[5]]), NOV = as.numeric(.[[6]]), DEC = as.numeric(.[[7]]),
            JAN = as.numeric(.[[8]]), FEB = as.numeric(.[[9]]), MAR = as.numeric(.[[10]]),
            APR = as.numeric(.[[11]]), MAY = as.numeric(.[[12]]), JUN = as.numeric(.[[13]]),
            JUL = as.numeric(.[[14]]), AUG = as.numeric(.[[15]]), SEP = as.numeric(.[[16]])) %>%
        filter(!is.na(country), !str_starts(country, "Total "), country != "Grand Totals") %>%
        pivot_longer(-country, names_to = "fiscal_month", values_to = "arrivals") %>%
        mutate(
            month = c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9)[
                match(fiscal_month, c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"))],
            year = if_else(fiscal_month %in% c("OCT", "NOV", "DEC"), fy - 1L, fy),
            fy = fy) %>%
        select(fy, year, month, country, arrivals)
}

refugees_monthly = map_dfr(2021:2026, read_refugee_sheet)

# sanity check: should match the known FY totals (11,411 / 25,465 / 60,014 / 100,034 / 38,102 / 16,292)
refugees_monthly %>%
    group_by(fy) %>%
    summarise(total = sum(arrivals), .groups = "drop")

refugees_recoded = refugees_monthly %>%
    mutate(country = recode_visa_country(country))

# confirm clean recoding -- should return 0 rows
refugees_recoded %>%
    group_by(country) %>%
    summarise(arrivals = sum(arrivals), .groups = "drop") %>%
    anti_join(acs_bpl, by = "country")

write_csv(refugees_recoded, "data/output/refugee_arrivals_by_country_fy2021_fy2026_recoded.csv")
