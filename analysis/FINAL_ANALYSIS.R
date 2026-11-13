# read in final data/output files -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

acs = read_rds("data/output/acs.RDS")

visas = visa_country_monthly = read_csv("data/output/visa_country_monthly.csv")

refugees = read_csv("data/output/refugee_arrivals_by_country_fy2021_fy2026_recoded.csv")

asylees = read_csv("data/output/asylee_encounters_by_country_fy2021_fy2026_recoded.csv")

cato_plot_theme = theme_minimal() +
    theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 20),
        legend.key.width = unit(1.5, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
        panel.grid.minor.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 25, color = "gray40"),
        axis.text.y = element_text(size = 25, color = "gray40"),
        plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

# Total entries ----------------------------------------------------------------------------

# TOTAL VISAS (appendix table 1)
visa_fy_totals = visas %>%
    group_by(visa_type, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = visa_type, values_from = issuances)

print(visa_fy_totals, n = Inf)

# unrestricted NIV total
niv_totals = read_csv("data/output/niv_issuances_monthly_totals.csv")

niv_fy_totals_unrestricted = niv_totals %>%
    filter(country == "ALL COUNTRIES") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year)) %>%
    group_by(fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop")

print(niv_fy_totals_unrestricted, n = Inf)

# TOTAL REFUGEES 
refugee_fy_totals = refugees %>%
    group_by(fy) %>%
    summarise(arrivals = sum(arrivals), .groups = "drop")

print(refugee_fy_totals, n = Inf)

# TOTAL ASYLEES
asylee_fy_totals = asylees %>%
    group_by(fy) %>%
    summarise(total = sum(encounters, na.rm = TRUE), .groups = "drop")

print(asylee_fy_totals, n = Inf)

# appendix table 2, "other" country encounters in asylee data 
asylee_other_by_fy = asylees_monthly %>%
    filter(citizenship == "OTHER") %>%
    group_by(fy) %>%
    summarise(other_encounters = sum(encounters), .groups = "drop") %>%
    left_join(
        asylees_monthly %>% group_by(fy) %>% summarise(total_encounters = sum(encounters), .groups = "drop"),
        by = "fy") %>%
    mutate(pct_other = other_encounters / total_encounters)

print(asylee_other_by_fy, n = Inf)

write_csv(asylee_other_by_fy, "results/asylee_unattributed_by_fy.csv")

# Table 1: total legal entries by fiscal year
master_fy_totals = visa_fy_totals %>%
    left_join(refugee_fy_totals %>% rename(Refugees = arrivals), by = "fy") %>%
    left_join(asylee_fy_totals %>% rename(Asylees = total), by = "fy")

print(master_fy_totals, n = Inf)

write_csv(master_fy_totals, "results/master_fy_totals.csv")

# Table 1 + projected visas
master_fy_totals = visa_fy_totals_updated %>%
    select(fy, IV, NIV) %>%
    left_join(refugee_fy_totals %>% rename(Refugees = arrivals), by = "fy") %>%
    left_join(asylee_fy_totals %>% rename(Asylees = total), by = "fy")

print(master_fy_totals, n = Inf)

write_csv(master_fy_totals, "results/master_fy_totals.csv")

# MASTERFILE: by month_year and country, all entries
visa_wide = visas %>%
    select(visa_type, country, year, month, fy, issuances) %>%
    pivot_wider(names_from = visa_type, values_from = issuances)

master_monthly = visa_wide %>%
    full_join(refugees %>% rename(Refugees = arrivals), by = c("country", "year", "month", "fy")) %>%
    full_join(asylees %>% rename(Asylees = encounters), by = c("country", "year", "month", "fy"))

print(master_monthly, n = 100)

write_csv(master_monthly, "results/master_monthly_by_country.csv")

# VISA PROJECTIONS ----------------------------------------------------------------------------

# WRONG METHOD: blanket 49% and 31% drop
visa_national_monthly = visas %>%
    group_by(visa_type, year, month, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop")

national_feb_change = visa_national_monthly %>%
    filter(month == 2, fy %in% c(2024, 2026)) %>%
    select(visa_type, fy, issuances) %>%
    pivot_wider(names_from = fy, values_from = issuances, names_prefix = "visas_") %>%
    mutate(feb_pct_change = (visas_2026 - visas_2024) / visas_2024)

print(national_feb_change)

blanket_marsep_projected = visa_national_monthly %>%
    filter(fy == 2024, month %in% 3:9) %>%
    select(visa_type, month, visas_2024 = issuances) %>%
    left_join(national_feb_change %>% select(visa_type, feb_pct_change), by = "visa_type") %>%
    mutate(visas_2026_projected = round(visas_2024 * (1 + feb_pct_change)))

blanket_fy2026_total = bind_rows(
        visa_national_monthly %>% filter(fy == 2026) %>% select(visa_type, visas_2026 = issuances),
        blanket_marsep_projected %>% select(visa_type, visas_2026 = visas_2026_projected)) %>%
    group_by(visa_type) %>%
    summarise(visas_2026 = sum(visas_2026), .groups = "drop")

print(blanket_fy2026_total)

## CORRECT METHOD: by each individual country's own feb24-feb26 change, then sum to national total
acs_bpl = acs %>%
    filter(country != "United States") %>%
    distinct(country)

visa_feb_change = expand_grid(visa_type = c("IV", "NIV"), acs_bpl) %>%
    left_join(
        visas %>%
            filter(month == 2, fy %in% c(2024, 2026)) %>%
            select(visa_type, country, fy, issuances) %>%
            pivot_wider(names_from = fy, values_from = issuances, names_prefix = "visas_"),
        by = c("visa_type", "country")) %>%
    filter(!(is.na(visas_2024) & !is.na(visas_2026))) %>%   # drop countries that went from zero to >=1 in february
    mutate(visas_2024 = coalesce(visas_2024, 0),
        visas_2026 = coalesce(visas_2026, 0),
        pct_change = if_else(visas_2024 == 0, -1, (visas_2026 - visas_2024) / visas_2024))   # zero visas in feb 2026 = full decline

print(visa_feb_change, n = Inf)

write_csv(visa_feb_change, "results/country_feb_change_2024_2026.csv")

# avg february change
visa_feb_change %>%
    group_by(visa_type) %>%
    summarise(mean_pct_change = mean(pct_change, na.rm = TRUE), .groups = "drop")

# march-september 2026 projection, by country and visa type
visa_marsep_projected = visas %>%
    filter(fy == 2024, month %in% 3:9) %>%
    select(visa_type, country, month, visas_2024 = issuances) %>%
    inner_join(visa_feb_change %>% select(visa_type, country, pct_change), by = c("visa_type", "country")) %>%
    mutate(visas_2026_projected = round(visas_2024 * (1 + pct_change)))

# oct-feb actual totals by country, visa type, fiscal year
octfeb_actual = visas %>%
    filter(fy %in% c(2024, 2026), month %in% c(10, 11, 12, 1, 2)) %>%
    semi_join(visa_feb_change, by = c("visa_type", "country")) %>%
    group_by(visa_type, country, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, values_fill = 0, names_prefix = "visas_")

marsep_total = visa_marsep_projected %>%
    group_by(visa_type, country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026_projected),
        .groups = "drop")

# full FY2026 table
visa_issuances_updated = bind_rows(octfeb_actual, marsep_total) %>%
    group_by(visa_type, country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        .groups = "drop") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024) %>%
    arrange(visa_type, change)

print(visa_issuances_updated, n = Inf)

write_csv(visa_issuances_updated, "results/visa_issuances_fy2024_fy2026_updated.csv")

# table 2: total visa issuances, including projected fy2026
visa_fy_totals_updated = visas %>%
    group_by(visa_type, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    filter(fy != 2026) %>%
    bind_rows(
        visa_issuances_updated %>%
            group_by(visa_type) %>%
            summarise(issuances = sum(visas_2026), .groups = "drop") %>%
            mutate(fy = 2026)) %>%
    pivot_wider(names_from = visa_type, values_from = issuances) %>%
    mutate(Total = IV + NIV) %>%
    arrange(fy)

print(visa_fy_totals_updated, n = Inf)

write_csv(visa_fy_totals_updated, "results/visa_fy_totals_updated.csv")

# plot visas monthly
visa_monthly_plot = visas %>%
    group_by(visa_type, year, month, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month)))

visa_national_projection = visa_marsep_projected %>%
    group_by(visa_type, month) %>%
    summarise(issuances = sum(visas_2026_projected), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("2026-%02d-01", month)))

plot_data = bind_rows(
    visa_monthly_plot %>%
        transmute(visa_type, month_date, issuances, series = "Actual"),
    visa_monthly_plot %>%
        group_by(visa_type) %>%
        filter(month_date == max(month_date)) %>%
        ungroup() %>%
        transmute(visa_type, month_date, issuances, series = "Projected"),
    visa_national_projection %>%
        transmute(visa_type, month_date, issuances, series = "Projected"))

ggplot(plot_data, aes(month_date, issuances, color = visa_type, linetype = series)) +
    geom_line(linewidth = 1.8) +
    scale_color_manual(values = c("IV" = "#3043B4", "NIV" = "#C97703")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    guides(linetype = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, 300000), breaks = seq(0, 300000, by = 50000)) +
    labs(
        title = "Immigrant and Nonimmigrant Visas Issued per Month, 2021-2026",
        subtitle = "Mar-Sep 2026 projected using each country's own change versus fiscal year 2024",
        x = NULL, y = NULL, color = NULL, linetype = NULL,
        caption = "Source: U.S. Department of State, Monthly Immigrant and Nonimmigrant Visa Issuance Statistics.") +
    cato_plot_theme

ggsave("results/visa_monthly_iv_niv_fy2021_2026.png", width = 15, height = 10)

# REFUGEES & ASYLEES ------------------------------------------------------------------
refugees_monthly_total = refugees %>%
    group_by(year, month) %>%
    summarise(value = sum(arrivals, na.rm = TRUE), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month)), series = "Refugees")

asylees_monthly_total = asylees %>%
    group_by(year, month) %>%
    summarise(value = sum(encounters, na.rm = TRUE), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month)), series = "Asylees")

refugee_asylee_monthly = bind_rows(refugees_monthly_total, asylees_monthly_total)

ggplot(refugee_asylee_monthly, aes(month_date, value, color = series)) +
    geom_line(linewidth = 1.8) +
    scale_color_manual(values = c("Refugees" = "#0D0E51", "Asylees" = "#7C756D")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
    labs(
        title = "Refugee Arrivals and Asylee Encounters per Month, FY2021-2026",
        x = NULL, y = NULL, color = NULL,
        caption = "Source: U.S. Department of State, Bureau of Population, Refugees, and Migration, Summary of Refugee Admissions; U.S. Customs and Border Protection, Nationwide Encounters.") +
    cato_plot_theme

ggsave("results/refugees_asylees_monthly.png", width = 15, height = 10)

# how many countries
unique(refugees$country)

unique(asylees$country)

# total countries in refugee/asylee data before recoding
refugee_path = "data/input/Refugee Admissions Report as of October 31, 2026.xlsx"

refugee_raw_countries = map(2021:2026, function(fy) {
    read_excel(refugee_path, sheet = as.character(fy), col_names = FALSE) %>%
        transmute(country = str_trim(.[[2]])) %>%
        filter(!is.na(country), country != "", !str_starts(country, "Total "), country != "Grand Totals")
}) %>%
    bind_rows() %>%
    distinct(country)

nrow(refugee_raw_countries)

asylee_raw_countries = bind_rows(
        read_csv("data/input/nationwide-encounters-fy20-fy23-aor.csv", show_col_types = FALSE) %>%
            filter(`Fiscal Year` %in% c(2021, 2022)),
        read_csv("data/input/nationwide-encounters-fy23-fy26-sep-aor (3).csv", show_col_types = FALSE) %>%
            filter(`Fiscal Year` %in% c(2023, 2024, 2025, 2026))) %>%
    filter(Component == "Office of Field Operations",
        `Land Border Region` == "Southwest Land Border",
        `Encounter Type` == "Inadmissibles") %>%
    distinct(Citizenship) %>%
    filter(Citizenship != "OTHER")

nrow(asylee_raw_countries)

# unknown asylee countries
asylee_unknown_pct_by_fy = asylees %>%
    group_by(fy) %>%
    summarise(
        total = sum(encounters, na.rm = TRUE),
        unknown = sum(encounters[is.na(country)], na.rm = TRUE),
        .groups = "drop") %>%
    mutate(pct_unknown = unknown / total)

print(asylee_unknown_pct_by_fy, n = Inf)

write_csv(asylee_unknown_pct_by_fy, "results/asylee_unknown_pct_by_fy.csv")

## MISSING WORKERS --------------------------------------------------------------
## ACS recent immigrants' occupation shares by country
RECENT_YEARS = 2020:2024

occ_group_fn = function(occ) {
    case_when(
        occ >= 10   & occ <= 440  ~ "Management",
        occ >= 500  & occ <= 960  ~ "Business & financial operations",
        occ >= 1005 & occ <= 1240 ~ "Computer & mathematical",
        occ >= 1305 & occ <= 1560 ~ "Architecture & engineering",
        occ >= 1600 & occ <= 1980 ~ "Life, physical & social science",
        occ >= 2001 & occ <= 2060 ~ "Community & social service",
        occ >= 2100 & occ <= 2180 ~ "Legal",
        occ >= 2205 & occ <= 2555 ~ "Education, training & library",
        occ >= 2600 & occ <= 2920 ~ "Arts, design, entertainment, sports & media",
        occ >= 3000 & occ <= 3550 ~ "Healthcare practitioners & technical",
        occ >= 3601 & occ <= 3655 ~ "Healthcare support",
        occ >= 3700 & occ <= 3960 ~ "Protective service",
        occ >= 4000 & occ <= 4160 ~ "Food preparation & serving",
        occ >= 4200 & occ <= 4255 ~ "Building & grounds cleaning",
        occ >= 4330 & occ <= 4655 ~ "Personal care & service",
        occ >= 4700 & occ <= 4965 ~ "Sales",
        occ >= 5000 & occ <= 5940 ~ "Office & administrative support",
        occ >= 6005 & occ <= 6130 ~ "Farming, fishing & forestry",
        occ >= 6200 & occ <= 6950 ~ "Construction & extraction",
        occ >= 7000 & occ <= 7640 ~ "Installation, maintenance & repair",
        occ >= 7700 & occ <= 8990 ~ "Production",
        occ >= 9005 & occ <= 9760 ~ "Transportation & material moving",
        occ >= 9800 & occ <= 9830 ~ "Military",
        TRUE ~ NA_character_)}

bpl_occ_group = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(country, pop, occ_group) %>%
    summarise(occ_workers = sum(perwt), .groups = "drop") %>%
    mutate(occ_share = occ_workers / pop)

## FY2026 changes in total entries first
visa_change_by_country = visa_issuances_updated %>%
    group_by(country) %>%
    summarise(value_2024 = sum(visas_2024), value_2026 = sum(visas_2026), .groups = "drop")

refugee_change_by_country = refugees %>%
    filter(fy %in% c(2024, 2026), !is.na(country)) %>%
    group_by(country, fy) %>%
    summarise(value = sum(arrivals), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = value, names_prefix = "value_", values_fill = 0)

asylee_change_by_country = asylees %>%
    filter(fy %in% c(2024, 2026), !is.na(country)) %>%
    group_by(country, fy) %>%
    summarise(value = sum(encounters), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = value, names_prefix = "value_", values_fill = 0)

missing_total_by_country = bind_rows(visa_change_by_country, refugee_change_by_country, asylee_change_by_country) %>%
    group_by(country) %>%
    summarise(value_2024 = sum(value_2024, na.rm = TRUE),
        value_2026 = sum(value_2026, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(change = value_2026 - value_2024)

print(missing_total_by_country %>% arrange(change), n = Inf)

write_csv(missing_total_by_country, "results/missing_total_by_country_fy2024_fy2026.csv")

missing_workers_by_occ = missing_total_by_country %>%
    select(country, change) %>%
    inner_join(bpl_occ_group %>% select(country, occ_group, occ_share), by = "country") %>%
    mutate(missing = change * occ_share) %>%
    group_by(occ_group) %>%
    summarise(missing_workers = sum(missing, na.rm = TRUE), .groups = "drop")

print(missing_workers_by_occ %>% arrange(missing_workers), n = Inf)

missing_workers_by_occ %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

write_csv(missing_workers_by_occ, "results/missing_workers_by_occ_fy2024_fy2026.csv")

## missing workers, broken down by source category 
missing_workers_by_category = bind_rows(
    visa_change_by_country %>% mutate(category = NA_character_),  # placeholder, filled below
    ) # see note below -- build per-category, not just combined

visa_missing_workers = visa_issuances_updated %>%
    select(visa_type, country, change) %>%
    inner_join(bpl_occ_group %>% group_by(country) %>% summarise(occ_share = sum(occ_share), .groups = "drop"),
        by = "country") %>%
    mutate(missing_workers = change * occ_share) %>%
    group_by(visa_type) %>%
    summarise(missing_workers = sum(missing_workers, na.rm = TRUE), .groups = "drop") %>%
    rename(category = visa_type)

refugee_missing_workers = refugee_change_by_country %>%
    mutate(change = value_2026 - value_2024) %>%
    inner_join(bpl_occ_group %>% group_by(country) %>% summarise(occ_share = sum(occ_share), .groups = "drop"),
        by = "country") %>%
    summarise(missing_workers = sum(change * occ_share, na.rm = TRUE)) %>%
    mutate(category = "Refugees")

asylee_missing_workers = asylee_change_by_country %>%
    mutate(change = value_2026 - value_2024) %>%
    inner_join(bpl_occ_group %>% group_by(country) %>% summarise(occ_share = sum(occ_share), .groups = "drop"),
        by = "country") %>%
    summarise(missing_workers = sum(change * occ_share, na.rm = TRUE)) %>%
    mutate(category = "Asylees")

missing_workers_by_category = bind_rows(visa_missing_workers, refugee_missing_workers, asylee_missing_workers)

print(missing_workers_by_category, n = Inf)

## add Jan-sept2025 on top
# each month compares to same month in 2024
visa_baseline_fy2024 = visas %>%
    filter(fy == 2024) %>%
    transmute(source = visa_type, country, month, baseline = issuances)

refugee_baseline_fy2024 = refugees %>%
    filter(fy == 2024) %>%
    transmute(source = "Refugees", country, month, baseline = arrivals)

asylee_baseline_fy2024 = asylees %>%
    filter(fy == 2024) %>%
    transmute(source = "Asylees", country, month, baseline = encounters)

baseline_fy2024 = bind_rows(visa_baseline_fy2024, refugee_baseline_fy2024, asylee_baseline_fy2024)

# actual/projected values for every month Jan 2025 - Sep 2026
visa_actual_window = visas %>%
    filter((year == 2025) | (year == 2026 & month <= 2)) %>%
    transmute(source = visa_type, country, year, month, value = issuances)

visa_projected_window = visa_marsep_projected %>%
    transmute(source = visa_type, country, year = 2026L, month, value = visas_2026_projected)

refugee_window = refugees %>%
    filter(year %in% c(2025, 2026)) %>%
    transmute(source = "Refugees", country, year, month, value = arrivals)

asylee_window = asylees %>%
    filter(year %in% c(2025, 2026)) %>%
    transmute(source = "Asylees", country, year, month, value = encounters)

window_values = bind_rows(visa_actual_window, visa_projected_window, refugee_window, asylee_window)

cumulative_missing = window_values %>%
    left_join(baseline_fy2024, by = c("source", "country", "month")) %>%
    mutate(baseline = coalesce(baseline, 0),
        missing = value - baseline)

cumulative_missing %>%
    group_by(source) %>%
    summarise(total_missing = sum(missing, na.rm = TRUE), .groups = "drop")

cumulative_missing %>%
    summarise(total_missing = sum(missing, na.rm = TRUE))

cumulative_missing_by_country = cumulative_missing %>%
    filter(!is.na(country)) %>%
    group_by(country) %>%
    summarise(missing = sum(missing, na.rm = TRUE), .groups = "drop")

print(cumulative_missing_by_country %>% arrange(missing), n = Inf)

write_csv(cumulative_missing_by_country, "results/cumulative_missing_by_country_jan2025_sep2026.csv")

# cumulative missing workers, Jan 2025 - Sep 2026, by occupation group 
cumulative_missing_workers_by_occ = cumulative_missing_by_country %>%
    inner_join(bpl_occ_group %>% select(country, occ_group, occ_share), by = "country") %>%
    mutate(missing_workers = missing * occ_share) %>%
    group_by(occ_group) %>%
    summarise(missing_workers = sum(missing_workers, na.rm = TRUE), .groups = "drop")

print(cumulative_missing_workers_by_occ %>% arrange(missing_workers), n = Inf)

cumulative_missing_workers_by_occ %>%
    summarise(total_missing_workers = sum(missing_workers, na.rm = TRUE))

write_csv(cumulative_missing_workers_by_occ, "results/cumulative_missing_workers_by_occ_jan2025_sep2026.csv")

# cumulative missing workers, by source category
cumulative_missing_workers_by_source = cumulative_missing %>%
    filter(!is.na(country)) %>%
    inner_join(bpl_occ_group %>% group_by(country) %>% summarise(occ_share = sum(occ_share), .groups = "drop"),
        by = "country") %>%
    mutate(missing_workers = missing * occ_share) %>%
    group_by(source) %>%
    summarise(missing_workers = sum(missing_workers, na.rm = TRUE), .groups = "drop")

print(cumulative_missing_workers_by_source, n = Inf)

write_csv(cumulative_missing_workers_by_source, "results/cumulative_missing_workers_by_source_jan2025_sep2026.csv")

# Table 3: workers blocked, cumulative, by occupation group
occ_group_totals = acs %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ))),
        immigrant = citizen %in% c(2, 3)) %>%
    group_by(occ_group) %>%
    summarise(total_workers = sum(perwt),
        immigrant_workers = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = (immigrant_workers / total_workers) * 100)

cumulative_missing_workers_by_occ_table = cumulative_missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    left_join(occ_group_totals %>% select(occ_group, pct_immigrant), by = "occ_group") %>%
    select(occ_group, pct_immigrant, missing_workers) %>%
    arrange(missing_workers)

print(cumulative_missing_workers_by_occ_table, n = Inf)

cumulative_missing_workers_by_occ_table = cumulative_missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    left_join(occ_group_totals %>% select(occ_group, total_workers, pct_immigrant), by = "occ_group") %>%
    mutate(pct_missing = (missing_workers / total_workers) * 100) %>%
    select(occ_group, pct_immigrant, missing_workers, pct_missing) %>%
    arrange(missing_workers)

cumulative_missing_workers_by_occ_table = cumulative_missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    left_join(occ_group_totals %>% select(occ_group, total_workers, pct_immigrant), by = "occ_group") %>%
    mutate(pct_missing = (missing_workers / total_workers) * 100) %>%
    select(occ_group, total_workers, pct_immigrant, missing_workers, pct_missing) %>%
    arrange(missing_workers)

print(cumulative_missing_workers_by_occ_table, n = Inf)

write_csv(cumulative_missing_workers_by_occ_table, "results/cumulative_missing_workers_by_occ_table.csv")

sum(cumulative_missing_workers_by_occ_table$missing_workers)

# HEALTHCARE -------------------------------------------------------------------------------
# current immigrant share of healthcare workforce
healthcare_share = acs %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ))),
        immigrant = citizen %in% c(2, 3)) %>%
    filter(occ_group %in% c("Healthcare practitioners & technical", "Healthcare support")) %>%
    summarise(total_workers = sum(perwt),
        immigrant_workers = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = immigrant_workers / total_workers * 100)

print(healthcare_share)

labor_force_share = acs %>%
    filter(empstat %in% c(1, 2)) %>%   # employed + unemployed
    mutate(immigrant = citizen %in% c(2, 3)) %>%
    summarise(labor_force = sum(perwt),
        immigrant_labor_force = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = immigrant_labor_force / labor_force * 100)

print(labor_force_share)

population_share = acs %>%
    mutate(immigrant = citizen %in% c(2, 3)) %>%
    summarise(pop = sum(perwt),
        immigrant_pop = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = immigrant_pop / pop * 100)

print(population_share)

# missing healthcare workers, 21 month period
missing_healthcare_workers = cumulative_missing_by_country %>%
    select(country, missing) %>%
    left_join(
        bpl_occ_group %>%
            filter(occ_group %in% c("Healthcare practitioners & technical", "Healthcare support")) %>%
            select(country, occ_group, occ_share),
        by = "country") %>%
    mutate(missing_workers = missing * occ_share)

missing_healthcare_workers %>%
    group_by(occ_group) %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

missing_healthcare_workers %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

# nurses
nurse_codes = c("3255", "3256", "3258", "3500", "3603")

bpl_nurse_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ))),
        is_nurse = occ %in% nurse_codes) %>%
    group_by(country, pop) %>%
    summarise(nurse_workers = sum(perwt[is_nurse]), .groups = "drop") %>%
    mutate(nurse_share = nurse_workers / pop)

missing_nurses = cumulative_missing_by_country %>%
    select(country, missing) %>%
    left_join(bpl_nurse_share %>% select(country, nurse_share), by = "country") %>%
    mutate(missing_workers = missing * nurse_share)

missing_nurses %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

bpl_nurse_occ_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
    filter(occ %in% nurse_codes) %>%
    group_by(country, pop, occ) %>%
    summarise(nurse_workers = sum(perwt), .groups = "drop") %>%
    mutate(nurse_share = nurse_workers / pop)

missing_nurses_by_occ = cumulative_missing_by_country %>%
    select(country, missing) %>%
    left_join(bpl_nurse_occ_share %>% select(country, occ, nurse_share), by = "country") %>%
    mutate(missing_workers = missing * nurse_share)

missing_nurses_by_occ %>%
    mutate(occ_category = if_else(occ == "3255", "Registered nurses", "Other nursing occupations")) %>%
    group_by(occ_category) %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE), .groups = "drop")


## TOTAL PERCENT DECLINE OF ENTRIES, 21 MONTH PERIOD ------------------------------------------------------
cumulative_pct_change_by_source = cumulative_missing %>%
    group_by(source) %>%
    summarise(total_value = sum(value, na.rm = TRUE),
        total_baseline = sum(baseline, na.rm = TRUE),
        total_missing = sum(missing, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(pct_change = total_missing / total_baseline)

print(cumulative_pct_change_by_source, n = Inf)

cumulative_pct_change_total = cumulative_missing %>%
    summarise(total_value = sum(value, na.rm = TRUE),
        total_baseline = sum(baseline, na.rm = TRUE),
        total_missing = sum(missing, na.rm = TRUE)) %>%
    mutate(pct_change = total_missing / total_baseline)

print(cumulative_pct_change_total)

combined_fy_totals = visa_fy_totals_updated %>%
    select(fy, Total) %>%
    rename(visas = Total) %>%
    left_join(refugee_fy_totals %>% rename(refugees = arrivals), by = "fy") %>%
    left_join(asylee_fy_totals %>% rename(asylees = total), by = "fy") %>%
    mutate(combined_total = visas + refugees + asylees)

print(combined_fy_totals, n = Inf)

fy2024_baseline = combined_fy_totals %>% filter(fy == 2024) %>% pull(combined_total)

combined_pct_change = combined_fy_totals %>%
    mutate(pct_change_vs_2024 = (combined_total - fy2024_baseline) / fy2024_baseline * 100)

print(combined_pct_change, n = Inf)

# EDUCATIONAL ATTAINMENT OF MISSING WORKERS --------------------------------------------------------------
# bachelor's or higher share, recent immigrants (2020-2024)
bpl_bachelors_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS, age >= 25) %>%
    group_by(country) %>%
    summarise(pop = sum(perwt),
        bachelors_plus = sum(perwt[educd >= 101]),
        .groups = "drop") %>%
    mutate(bachelors_share = bachelors_plus / pop)

# missing workers with bachelors degree or higher
missing_bachelors = cumulative_missing_by_country %>%
    select(country, missing) %>%
    left_join(bpl_bachelors_share %>% select(country, bachelors_share), by = "country") %>%
    mutate(missing_bachelors = missing * bachelors_share)

missing_bachelors %>%
    summarise(total_missing_bachelors = sum(missing_bachelors, na.rm = TRUE))

acs %>%
    filter(age >= 25) %>%
    summarise(pop = sum(perwt),
        bachelors_plus_n = sum(perwt[educd >= 101]),
        .groups = "drop") %>%
    mutate(pct_bachelors_plus = bachelors_plus_n / pop * 100)

acs %>%
    filter(age >= 25, citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    summarise(pop = sum(perwt),
        bachelors_plus = sum(perwt[educd >= 101]),
        .groups = "drop") %>%
    mutate(pct_bachelors_plus = bachelors_plus / pop * 100)

# CUMULATIVE MISSING: overall decline in legal entries
plot_data_decline = cumulative_pct_change_by_source %>%
    select(source, total_baseline, total_value) %>%
    pivot_longer(c(total_baseline, total_value), names_to = "period", values_to = "count") %>%
    mutate(period = recode(period,
            total_baseline = "FY2024 baseline",
            total_value = "Jan 2025 - Sep 2026"),
        source = factor(source, levels = c("IV", "NIV", "Refugees", "Asylees")))

ggplot(plot_data_decline, aes(period, count, fill = period)) +
    geom_col(width = 0.6) +
    facet_wrap(~ source, scales = "free_y", nrow = 1) +
    scale_fill_manual(values = c(
        "FY2024 baseline" = "#7C756D",
        "Jan 2025 - Sep 2026" = "#3043B4")) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0)) +
    guides(fill = "none") +
    labs(
        title = "Legal Entries: FY2024 Baseline vs. Jan 2025 - Sep 2026",
        subtitle = "FY2024 baseline reflects the same 21 calendar months' worth of activity, using FY2024 as the comparison year",
        x = NULL, y = NULL,
        caption = "Source: U.S. Department of State; U.S. Customs and Border Protection; Bureau of Population, Refugees, and Migration.") +
    cato_plot_theme +
    theme(axis.text.x = element_text(size = 16, angle = 0))

ggsave("results/legal_entries_fy2024_vs_cumulative.png", width = 14, height = 7)

visa_monthly_total = visas %>%
    group_by(year, month) %>%
    summarise(value = sum(issuances), .groups = "drop")

visa_projected_monthly_total = visa_marsep_projected %>%
    group_by(month) %>%
    summarise(value = sum(visas_2026_projected), .groups = "drop") %>%
    mutate(year = 2026L)

visa_full_monthly_total = bind_rows(
    visa_monthly_total %>% filter(!(year == 2026 & month %in% 3:9)),
    visa_projected_monthly_total)

refugee_monthly_total = refugees %>%
    group_by(year, month) %>%
    summarise(value = sum(arrivals), .groups = "drop")

asylee_monthly_total = asylees %>%
    group_by(year, month) %>%
    summarise(value = sum(encounters, na.rm = TRUE), .groups = "drop")

total_monthly = bind_rows(visa_full_monthly_total, refugee_monthly_total, asylee_monthly_total) %>%
    group_by(year, month) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month)),
        series = if_else(year == 2026 & month %in% 3:9, "Projected", "Actual"))

# duplicate the last actual month as the first projected point, so the dotted line connects with no gap
plot_data_total = bind_rows(
    total_monthly %>% filter(series == "Actual"),
    total_monthly %>% filter(series == "Actual") %>% filter(month_date == max(month_date)) %>% mutate(series = "Projected"),
    total_monthly %>% filter(series == "Projected"))

ggplot(plot_data_total, aes(month_date, value, linetype = series)) +
    geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_line(linewidth = 1.8, color = "#3043B4") +
    geom_line(linewidth = 1.8, color = "#3043B4") +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    guides(linetype = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, 400000), breaks = seq(0, 400000, by = 100000)) +
    labs(
        title = "Total Monthly Legal Entries, FY2021-2026",
        subtitle = "Immigrant visas, nonimmigrant visas, refugee arrivals, and asylee encounters combined; Mar-Sep 2026 visas projected",
        x = NULL, y = NULL,
        caption = "Source: U.S. Department of State; U.S. Customs and Border Protection; Bureau of Population, Refugees, and Migration.") +
    cato_plot_theme

ggsave("results/total_monthly_legal_entries.png", width = 15, height = 8)

# zoomed version: fy2024-2026
ggplot(plot_data_total %>% filter(month_date >= as.Date("2023-10-01")),
        aes(month_date, value, linetype = series)) +
    geom_vline(xintercept = as.Date("2025-01-01"), linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_line(linewidth = 1.8, color = "#3043B4") +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    guides(linetype = "none") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, 400000), breaks = seq(0, 400000, by = 100000)) +
    labs(
        title = "Total Monthly Legal Entries, FY2024-2026",
        subtitle = "Immigrant visas, nonimmigrant visas, refugee arrivals, and asylee encounters combined; Mar-Sep 2026 visas projected",
        x = NULL, y = NULL,
        caption = "Source: U.S. Department of State; U.S. Customs and Border Protection; Bureau of Population, Refugees, and Migration.") +
    cato_plot_theme

ggsave("results/total_monthly_legal_entries_fy2024_2026.png", width = 15, height = 8)
