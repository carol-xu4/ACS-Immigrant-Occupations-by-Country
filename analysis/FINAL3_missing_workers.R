## Preliminaries ----------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS
acs = read_rds("data/output/acs.RDS")

# read in monthly IV and NIV issuances
# country-level, IV + selected NIV, recoded country names
visa_country_monthly = read_csv("data/output/visa_country_monthly.csv")       

## TOTAL IV AND NIV ISSUED BY FISCAL YEAR -----------------------------------------------------------
# NIV restricted to selected visa types
fy_totals = visa_country_monthly %>%
    group_by(visa_type, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    arrange(visa_type, fy)

print(fy_totals, n = Inf)

# unrestricted NIV total
niv_totals = read_csv("data/output/niv_issuances_monthly_totals.csv")

niv_fy_totals_unrestricted = niv_totals %>%
    filter(country == "ALL COUNTRIES") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year)) %>%
    group_by(fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop")

print(niv_fy_totals_unrestricted, n = Inf)

# january and february 2026 IV and NIV totals
visa_monthly = visa_country_monthly %>%
    group_by(visa_type, date, year, month, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(month_date = as.Date(paste0(date, "-01"))) %>%
    arrange(visa_type, month_date)

print(visa_monthly, n = Inf)

visas_2024_by_month = visa_monthly %>%
    filter(year == 2024) %>%
    select(visa_type, month, visas_2024 = issuances)

visa_2026 = visa_monthly %>%
    filter(year == 2026) %>%
    select(visa_type, month, month_date, visas_2026 = issuances) %>%
    left_join(visas_2024_by_month, by = c("visa_type", "month")) %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

print(visa_2026, n = Inf)

## march-september 2026 projections --------------------------------------------------------------
# projection 1: flat 49%(IV) and 31%(NIV) reduction across all countries
# (THIS ONE IS WRONG)
visa_monthly = visa_country_monthly %>%
    group_by(visa_type, date, year, month, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop")

visas_2024_by_month = visa_monthly %>%
    filter(fy == 2024) %>%
    select(visa_type, month, visas_2024 = issuances)

visa_2026_actual = visa_monthly %>%
    filter(fy == 2026) %>%
    select(visa_type, month, visas_2026 = issuances) %>%
    left_join(visas_2024_by_month, by = c("visa_type", "month")) %>%
    mutate(pct_change = (visas_2026 - visas_2024) / visas_2024)

feb_pct_change = visa_2026_actual %>%
    filter(month == 2) %>%
    select(visa_type, feb_pct_change = pct_change)

print(feb_pct_change) 

visa_projection_blanket = visa_monthly %>%
    filter(fy == 2024, month >= 3, month <= 9) %>%
    select(visa_type, month, visas_2024 = issuances) %>%
    left_join(feb_pct_change, by = "visa_type") %>%
    mutate(visas_2026_projected = round(visas_2024 * (1 + feb_pct_change)))

blanket_total = bind_rows(
        visa_2026_actual %>% select(visa_type, month, visas_2024, visas_2026),
        visa_projection_blanket %>% select(visa_type, month, visas_2024, visas_2026 = visas_2026_projected)) %>%
    group_by(visa_type) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        .groups = "drop") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

print(blanket_total)

# projection 2: by individual country's feb2024-feb2026 change
acs_bpl = acs %>%
    filter(country != "United States") %>%
    distinct(country)

visa_country_2024 = visa_country_monthly %>%
    filter(fy == 2024) %>%
    select(visa_type, country, month, visas_2024 = issuances)

visa_country_2026 = visa_country_monthly %>%
    filter(fy == 2026) %>%
    select(visa_type, country, month, visas_2026 = issuances) %>%
    left_join(visa_country_2024, by = c("visa_type", "country", "month")) %>%
    mutate(change = visas_2026 - visas_2024)

# february change by country, for each visa type -- joined against every ACS country so countries with zero visas in either year still show up
country_feb_change = expand_grid(visa_type = c("IV", "NIV"), acs_bpl) %>%
    left_join(
        visa_country_2026 %>%
            filter(month == 2) %>%
            select(visa_type, country, visas_2024, visas_2026),
        by = c("visa_type", "country")) %>%
    filter(!(is.na(visas_2024) & !is.na(visas_2026))) %>%   # drop countries that went from zero to >=1 in february
    mutate(visas_2024 = coalesce(visas_2024, 0),
        visas_2026 = coalesce(visas_2026, 0),  # BUG!!!!!!!! countries that went from zero to zero btwn feb 2024 and 2026 = -100% change in final calculation
        pct_change = if_else(visas_2024 == 0, 0, (visas_2026 - visas_2024) / visas_2024)) # changed pct_change to 0 for these cases, add in january fallback

print(country_feb_change, n = Inf)

write_csv(country_feb_change, "results/country_feb_change_2024_2026.csv")

country_feb_change %>%
    filter(visas_2024 == 0, visas_2026 == 0) %>% print(n = Inf)

# countries with zero visas in feb24 and feb26, what do their other months look like
zero_feb_countries = country_feb_change %>%
    filter(visas_2024 == 0, visas_2026 == 0) %>%
    select(visa_type, country)

zero_feb_countries = zero_feb_countries %>%
    expand_grid(month = 1:12) %>%
    left_join(
        visa_country_monthly %>% filter(fy == 2024) %>% select(visa_type, country, month, issuances),
        by = c("visa_type", "country", "month")) %>%
    mutate(issuances = coalesce(issuances, 0)) %>%
    pivot_wider(names_from = month, values_from = issuances, names_prefix = "month_") %>%
    arrange(visa_type, country)

view(zero_feb_countries)

# for countries with zero visas in february of both years, fall back to january's change, rather than defaulting to 0% for all of them
jan_fallback = zero_feb_countries %>%
    left_join(
        visa_country_monthly %>%
            filter(month == 1, fy %in% c(2024, 2026)) %>%
            select(visa_type, country, fy, issuances) %>%
            pivot_wider(names_from = fy, values_from = issuances, values_fill = 0, names_prefix = "jan_"),
        by = c("visa_type", "country")) %>%
    mutate(jan_2024 = coalesce(jan_2024, 0),
        jan_2026 = coalesce(jan_2026, 0),
        pct_change = if_else(jan_2024 == 0, 0, (jan_2026 - jan_2024) / jan_2024),
        jan_fallback_used = TRUE) %>%
    select(visa_type, country, pct_change, jan_fallback_used)

country_feb_change = country_feb_change %>%
    mutate(jan_fallback_used = FALSE) %>%
    rows_update(jan_fallback, by = c("visa_type", "country"))

print(country_feb_change %>% filter(jan_fallback_used), n = Inf)
