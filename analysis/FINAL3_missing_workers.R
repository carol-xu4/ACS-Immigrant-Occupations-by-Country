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
        visas_2026 = coalesce(visas_2026, 0),
        pct_change = if_else(visas_2024 == 0, -1, (visas_2026 - visas_2024) / visas_2024))   # any country with zero visas in february 2026 counts as a full decline

print(country_feb_change, n = Inf)

write_csv(country_feb_change, "results/country_feb_change_2024_2026.csv")

# march-september projection
visa_country_projection = visa_country_monthly %>%
    filter(fy == 2024, month >= 3, month <= 9) %>%
    select(visa_type, country, month, visas_2024 = issuances) %>%
    inner_join(country_feb_change %>% select(visa_type, country, pct_change), by = c("visa_type", "country")) %>%   # drops the zero-to-positive "Slovakia" style cases excluded from country_feb_change
    mutate(visas_2026_projected = round(visas_2024 * (1 + pct_change)),
        change = visas_2026_projected - visas_2024)

# oct-feb actual totals by country, visa type, and fiscal year 
octfeb_actual = visa_country_monthly %>%
    filter(fy %in% c(2024, 2026), month %in% c(10, 11, 12, 1, 2)) %>%
    semi_join(country_feb_change, by = c("visa_type", "country")) %>%
    group_by(visa_type, country, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, values_fill = 0, names_prefix = "visas_")

marsep_total = visa_country_projection %>%
    group_by(visa_type, country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026_projected),
        .groups = "drop")

# appendix table: by country and visa type, change in visas issued
missing_visas_by_country = bind_rows(octfeb_actual, marsep_total) %>%
    group_by(visa_type, country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        .groups = "drop") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024) %>%
    arrange(visa_type, change)

print(missing_visas_by_country, n = Inf)

write_csv(missing_visas_by_country, "results/missing_visas_by_country.csv")

# avg pct change for countries
missing_visas_by_country %>%
    group_by(visa_type) %>%
    summarise(mean_pct_change = mean(pct_change, na.rm = TRUE))

missing_visas_by_country_wide = missing_visas_by_country %>%
    select(visa_type, country, visas_2024, visas_2026, change, pct_change) %>%
    pivot_wider(names_from = visa_type, values_from = c(visas_2024, visas_2026, change, pct_change),
        names_glue = "{visa_type}_{.value}")

print(missing_visas_by_country_wide, n = Inf)

write_csv(missing_visas_by_country_wide, "results/missing_visas_by_country_wide.csv")

# totals, summed separately for IV and NIV
missing_visas_by_country %>%
    group_by(visa_type) %>%
    summarise(total_change = sum(change), .groups = "drop")

# combined IV + NIV change, by country 
missing_visas_total_by_country = missing_visas_by_country %>%
    group_by(country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        .groups = "drop") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

# totals for each fiscal year
fy_totals_with_projection = fy_totals %>%
    filter(fy != 2026) %>%
    bind_rows(
        missing_visas_by_country %>%
            group_by(visa_type) %>%
            summarise(issuances = sum(visas_2026), .groups = "drop") %>%
            mutate(fy = 2026)) %>%
    pivot_wider(names_from = visa_type, values_from = issuances) %>%
    mutate(Total = IV + NIV) %>%
    arrange(fy)

print(fy_totals_with_projection, n = Inf)

write_csv(fy_totals_with_projection, "results/visa_fy_totals_with_projection.csv")

visa_national_projection = visa_country_projection %>%
    group_by(visa_type, month) %>%
    summarise(issuances = sum(visas_2026_projected), .groups = "drop") %>%
    mutate(month_date = as.Date(paste0("2026-", sprintf("%02d", month), "-01")))

visa_monthly_plot = visa_country_monthly %>%
    group_by(visa_type, date, year, month, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(month_date = as.Date(paste0(date, "-01")))

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

# full range: FY2021-2026
ggplot(plot_data, aes(month_date, issuances, color = visa_type, linetype = series)) +
    geom_line(linewidth = 1.8) +
    scale_color_manual(values = c("IV" = "#3043B4", "NIV" = "#C97703")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    guides(linetype = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, 300000),breaks = seq(0, 300000, by = 50000)) +
    labs(
        title = "Immigrant and Nonimmigrant Visas Issued per Month, 2021-2026",
        subtitle = "Mar-Sep 2026 projected using each country's own change versus fiscal year 2024",
        x = NULL, y = NULL, color = NULL, linetype = NULL,
        caption = "Source: U.S. Department of State, Monthly Immigrant and Nonimmigrant Visa Issuance Statistics.") +
    cato_plot_theme

ggsave("results/visa_monthly_iv_niv_fy2021_2026.png", width = 15, height = 10)

# zoomed: FY2024-2026 only
ggplot(plot_data %>% filter(month_date >= as.Date("2023-10-01")),
        aes(month_date, issuances, color = visa_type, linetype = series)) +
    geom_line(linewidth = 1.8) +
    scale_color_manual(values = c("IV" = "#3043B4", "NIV" = "#C97703")) +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    guides(linetype = "none") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
    labs(
        title = "Immigrant and Nonimmigrant Visas Issued per Month, FY2024-2026",
        subtitle = "Mar-Sep 2026 projected using each country's own change versus fiscal year 2024",
        x = NULL, y = NULL, color = NULL, linetype = NULL,
        caption = "Source: U.S. Department of State, Monthly Immigrant and Nonimmigrant Visa Issuance Statistics.") +
    cato_plot_theme

ggsave("results/visa_monthly_iv_niv_fy2024_2026.png", width = 15, height = 10)

# map?
pacman::p_load(rnaturalearth, sf, countrycode)

world = ne_countries(scale = "medium", returnclass = "sf")

map_data = missing_visas_total_by_country %>%
    mutate(iso3 = countrycode(country, origin = "country.name", destination = "iso3c",
        custom_match = c("Kosovo" = "XKX", "Micronesia" = "FSM"))) %>%
    filter(!is.na(iso3))   # drops the "Other X" catch-alls, Israel/Palestine, and any unmatched names

world_joined = world %>%
    left_join(map_data, by = c("adm0_a3" = "iso3"))

ggplot(world_joined) +
    geom_sf(aes(fill = pct_change), color = "white", linewidth = 0.1) +
    scale_fill_gradient2(low = "#C97703", mid = "white", high = "#3043B4", midpoint = 0,
        labels = scales::percent, na.value = "gray90",
        name = "% change") +
    labs(
        title = "Change in Visas Issued by Country, FY2024-FY2026",
        subtitle = "Countries in gray had no comparable data or could not be matched to a single country",
        caption = "Source: U.S. Department of State, Monthly Immigrant and Nonimmigrant Visa Issuance Statistics.") +
    theme_void() +
    theme(
        plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
        legend.position = "right",
        plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA))

ggsave("results/visa_change_map.png", width = 15, height = 9)

## ACS: immigrants by country and their employment shares --------------------------------------------
bpl_table_workers = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    mutate(employed = empstat == 1) %>%
    group_by(country) %>%
    summarise(n           = n(),
        pop         = sum(perwt),
        n_workers   = sum(employed),
        workers     = sum(perwt[employed]),
        .groups = "drop") %>%
    mutate(pct_workers = workers / pop) %>%
    arrange(desc(pop))

print(bpl_table_workers, n = Inf)

write_csv(bpl_table_workers, "results/birthplace_workers_immigrants.csv")

## TOTAL MISSING WORKERS -----------------------------------------------------------------------------
missing_workers_total = missing_visas_total_by_country %>%
    select(country, change) %>%
    left_join(bpl_table_workers %>% select(country, pct_workers), by = "country") %>%
    mutate(missing_workers = change * pct_workers)

print(missing_workers_total, n = Inf)

missing_workers_total %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

missing_visas_by_country %>%
    group_by(visa_type) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        pct_change = sum(change) / sum(visas_2024),
        .groups = "drop")

# dropped countries
expand_grid(visa_type = c("IV", "NIV"), acs_bpl) %>%
    left_join(
        visa_country_2026 %>%
            filter(month == 2) %>%
            select(visa_type, country, visas_2024, visas_2026),
        by = c("visa_type", "country")) %>%
    filter(is.na(visas_2024) & !is.na(visas_2026)) %>%
    count(visa_type)

# how many immigrants in ACS
immigrants = acs %>%
    filter(citizen %in% c(2, 3)) %>%
    summarise(n = n(),
        pop = sum(perwt),
        .groups = "drop")

print(immigrants)

acs %>%
    filter(country == "United States", citizen %in% c(2, 3)) %>%
    count(bpld, sort = TRUE)

# missing workers by occupation group ------------------------------------------------
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

# share of ALL immigrants from each country working in each occupation
bpl_occ_group = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(country, pop, occ_group) %>%
    summarise(occ_workers = sum(perwt), .groups = "drop") %>%
    mutate(occ_share = occ_workers / pop)

# missing workers by occupation group, using the combined IV + NIV change by country
missing_workers_by_occ = missing_visas_total_by_country %>%
    select(country, change) %>%
    inner_join(bpl_occ_group %>% select(country, occ_group, occ_share), by = "country") %>%
    mutate(missing = change * occ_share) %>%
    group_by(occ_group) %>%
    summarise(missing_workers = sum(missing, na.rm = TRUE), .groups = "drop")

# current occupation totals (% of today's workforce that's immigrant)
occ_group_totals = acs %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ))),
        immigrant = citizen %in% c(2, 3)) %>%
    group_by(occ_group) %>%
    summarise(total_workers = sum(perwt),
        immigrant_workers = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = (immigrant_workers / total_workers) * 100)

# one table: occupation, % immigrant, missing workers
missing_workers_by_occ_table = missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    left_join(occ_group_totals %>% select(occ_group, pct_immigrant), by = "occ_group") %>%
    select(occ_group, pct_immigrant, missing_workers) %>%
    arrange(missing_workers)

print(missing_workers_by_occ_table, n = Inf)

write_csv(missing_workers_by_occ_table, "results/missing_workers_by_occ.csv")

sum(missing_workers_by_occ_table$missing_workers)   # close enough to original sum lol

## MISSING HEALTHCARE WORKERS (combined IV + NIV) ---------------------------------------------------
# immigrants as share of healthcare workforce
healthcare_workforce = acs %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ))),
        immigrant = citizen %in% c(2, 3)) %>%
    filter(occ_group %in% c("Healthcare practitioners & technical", "Healthcare support")) %>%
    group_by(occ_group) %>%
    summarise(total_workers = sum(perwt),
        immigrant_workers = sum(perwt[immigrant]),
        .groups = "drop") %>%
    mutate(pct_immigrant = immigrant_workers / total_workers)

healthcare_workforce_total = healthcare_workforce %>%
    summarise(occ_group = "All healthcare",
        total_workers = sum(total_workers),
        immigrant_workers = sum(immigrant_workers)) %>%
    mutate(pct_immigrant = immigrant_workers / total_workers)

bind_rows(healthcare_workforce, healthcare_workforce_total) %>%
    print(n = Inf)

missing_healthcare_workers = missing_visas_total_by_country %>%
    select(country, change) %>%
    left_join(
        bpl_occ_group %>%
            filter(occ_group %in% c("Healthcare practitioners & technical", "Healthcare support")) %>%
            select(country, occ_group, occ_share),
        by = "country") %>%
    mutate(missing_workers = change * occ_share)

missing_healthcare_workers %>%
    group_by(occ_group) %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

missing_healthcare_workers %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

## MISSING NURSES 
nurse_codes = c("3255", "3256", "3258", "3500", "3603")

bpl_nurse_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ))),
        is_nurse = occ %in% nurse_codes) %>%
    group_by(country, pop) %>%
    summarise(nurse_workers = sum(perwt[is_nurse]), .groups = "drop") %>%
    mutate(nurse_share = nurse_workers / pop)

missing_nurses = missing_visas_total_by_country %>%
    select(country, change) %>%
    left_join(bpl_nurse_share %>% select(country, nurse_share), by = "country") %>%
    mutate(missing_workers = change * nurse_share)

missing_nurses %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

# individual nursing occs
bpl_nurse_occ_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
    filter(occ %in% nurse_codes) %>%
    group_by(country, pop, occ) %>%
    summarise(nurse_workers = sum(perwt), .groups = "drop") %>%
    mutate(nurse_share = nurse_workers / pop)

missing_nurses_by_occ = missing_visas_total_by_country %>%
    select(country, change) %>%
    left_join(bpl_nurse_occ_share %>% select(country, occ, nurse_share), by = "country") %>%
    mutate(missing_workers = change * nurse_share)

missing_nurses_by_occ %>%
    mutate(occ_category = if_else(occ == "3255", "Registered nurses", "Other nursing occupations")) %>%
    group_by(occ_category) %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE), .groups = "drop")

## CORRECTING OVERESTIMATION: EST. MISSING WORKERS USING OCCUPATION SHARES OF RECENT IMMIGRANTS ONLY -----------
# arrived 2020-2024
RECENT_YEARS = 2020:2024

bpl_table_workers = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    mutate(employed = empstat == 1) %>%
    group_by(country) %>%
    summarise(n = n(), pop = sum(perwt), n_workers = sum(employed), workers = sum(perwt[employed]), .groups = "drop") %>%
    mutate(pct_workers = workers / pop) %>%
    arrange(desc(pop))

bpl_occ_group = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(country, pop, occ_group) %>%
    summarise(occ_workers = sum(perwt), .groups = "drop") %>%
    mutate(occ_share = occ_workers / pop)

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

occ_labels = read_csv("data/output/occ2018_labels.csv")

bpl_nurse_occ_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
    filter(occ %in% nurse_codes) %>%
    group_by(country, pop, occ) %>%
    summarise(nurse_workers = sum(perwt), .groups = "drop") %>%
    mutate(nurse_share = nurse_workers / pop) %>%
    left_join(occ_labels, by = "occ")

#recent arrival samples? compare workforce shares of recent arrivals vs all immigrants
bpl_table_workers %>% arrange(n) %>% print(n = 20)

worker_rates_all = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    mutate(employed = empstat == 1) %>%
    group_by(country) %>%
    summarise(n = n(), pop = sum(perwt), workers = sum(perwt[employed]), .groups = "drop") %>%
    mutate(pct_workers_all = workers / pop)

worker_rates_recent = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
    mutate(employed = empstat == 1) %>%
    group_by(country) %>%
    summarise(n_recent = n(), pop_recent = sum(perwt), workers_recent = sum(perwt[employed]), .groups = "drop") %>%
    mutate(pct_workers_recent = workers_recent / pop_recent)

worker_rates_compare = worker_rates_all %>%
    select(country, n, pop, pct_workers_all) %>%
    full_join(worker_rates_recent %>% select(country, n_recent, pop_recent, pct_workers_recent), by = "country") %>%
    mutate(diff = pct_workers_recent - pct_workers_all) %>%
    arrange(diff)

print(worker_rates_compare, n = Inf)

view(worker_rates_compare)

write_csv(worker_rates_compare, "results/worker_rates_all_vs_recent.csv")

bind_rows(
    acs %>%
        filter(country != "United States", citizen %in% c(2, 3)) %>%
        summarise(group = "All immigrants", pop = sum(perwt), workers = sum(perwt[empstat == 1])),
    acs %>%
        filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS) %>%
        summarise(group = "Recent arrivals (2020-2024)", pop = sum(perwt), workers = sum(perwt[empstat == 1]))) %>%
    mutate(pct_workers = workers / pop)
