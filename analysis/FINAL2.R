## Preliminaries ----------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS
acs = read_rds("data/output/acs.RDS")

## Read visa issuances
iv_totals = read_csv("data/output/iv_issuances_monthly_totals.csv")

## RECODING -----------------------------------------------------------------------------------------
# recode State Dept country names to match ACS country names
recode_visa_country = function(x) {
    case_when(
        # same country, different spelling
        x == "Bahamas, The"                                   ~ "Bahamas",
        x %in% c("Bosnia-Herzegovina", "Bosnia and Herzegovina") ~ "Bosnia and Herzegovina",
        x == "Cabo Verde"                                     ~ "Cape Verde",
        str_detect(x, "^China") & !str_detect(x, "Taiwan")    ~ "China",
        x %in% c("China - Taiwan born", "Taiwan")             ~ "Taiwan",
        x == "Congo, Democratic Republic of the"              ~ "DR Congo",
        x == "Congo, Republic of the"                         ~ "Republic of the Congo",
        x %in% c("Gambia, The", "The Gambia")                 ~ "Gambia",
        x %in% c("Great Britain and Northern Ireland", "United Kingdom",
            "Northern Ireland", "Northern Ireland (DV only)") ~ "United Kingdom",
        str_detect(x, "^Hong Kong")                           ~ "Hong Kong",      # incl. Hong Kong-BNO
        str_detect(x, "^Korea")                               ~ "Korea",          # ACS Korea incl. North Korea
        x %in% c("Macedonia", "North Macedonia")              ~ "North Macedonia",
        str_detect(x, "^Micronesia")                          ~ "Micronesia",
        x %in% c("Saint Kitts and Nevis", "St. Kitts and Nevis") ~ "St. Kitts and Nevis",
        x == "Saint Lucia"                                    ~ "St. Lucia",
        x == "Saint Vincent and the Grenadines"               ~ "St. Vincent and the Grenadines",
        x %in% c("Israel", "Jerusalem",
            "Palestinian Authority Travel Document")         ~ "Israel/Palestine",

        # no own ACS code: assign to the ACS catch-all they fall into
        x == "Samoa"                                          ~ "Other, n.e.c.",  # independent Samoa
        x %in% c("Anguilla", "Aruba", "British Virgin Islands", "Cayman Islands",
            "Curacao", "Guadeloupe", "Montserrat", "Saint Maarten",
            "Saint Martin", "Sint Maarten", "Turks and Caicos Islands") ~ "Other Caribbean",
        x %in% c("Suriname", "Falkland Islands")              ~ "Other South America",
        x %in% c("Greenland", "St. Pierre and Miquelon")      ~ "Other Americas",
        x %in% c("Andorra", "Estonia", "Faroe Islands", "Gibraltar", "Luxembourg",
            "Malta", "Monaco", "San Marino", "Slovenia", "Vatican City") ~ "Other Europe",
        x %in% c("Bahrain", "Brunei", "Cyprus", "Macau", "Macau S.A.R.", "Maldives",
            "Oman", "Qatar", "Timor-Leste", "Tajikistan", "Turkmenistan") ~ "Other Asia",
        x %in% c("Tunisia", "Western Sahara")                 ~ "Other North Africa",
        x %in% c("Benin", "Burkina Faso", "Guinea-Bissau", "Mali",
            "Mauritania", "Niger")                           ~ "Other West Africa",
        x %in% c("Burundi", "Comoros", "Djibouti", "Madagascar", "Malawi", "Mauritius",
            "Mozambique", "Seychelles", "South Sudan",
            "British Indian Ocean Territory")                ~ "Other East Africa",
        x %in% c("Angola", "Central African Republic", "Chad", "Equatorial Guinea",
            "Gabon", "Sao Tome and Principe", "Botswana", "Eswatini", "Lesotho",
            "Namibia", "St. Helena")                         ~ "Other Africa",
        x %in% c("Christmas Island", "Cocos Islands", "Cocos (Keeling) Islands",
            "Cook Islands", "French Polynesia", "Kiribati", "Nauru", "New Caledonia",
            "Niue", "Palau", "Papua New Guinea", "Pitcairn", "Solomon Islands",
            "Tuvalu", "Vanuatu", "Wallis and Futuna")        ~ "Other, n.e.c.",
        x %in% c("No Nationality", "Other")                   ~ NA_character_,
        TRUE ~ x)}

# appendix table: every raw visa country/territory mapped to its matched ACS category
country_crosswalk = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    distinct(country) %>%
    mutate(acs_category = recode_visa_country(country),
        acs_category = if_else(is.na(acs_category), "Not matched (excluded)", acs_category)) %>%
    rename(visa_country = country) %>%
    arrange(acs_category, visa_country)

write_csv(country_crosswalk, "results/country_crosswalk.csv")

## CHECKS -------------------------------------------------------------------------------------------
# check visa countries match ACS countries
acs_bpl = acs %>%
    filter(country != "United States") %>%
    distinct(country)

visa_bpl = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(country = recode_visa_country(country)) %>%
    group_by(country) %>%
    summarise(visas = sum(issuances), .groups = "drop")

# in visa data but not ACS
visa_bpl %>% anti_join(acs_bpl, by = "country") %>% arrange(desc(visas)) %>% print(n = Inf)

# in ACS but not visa data (legacy ACS categories: Czechoslovakia, USSR n.s., Yugoslavia)
acs_bpl %>% anti_join(visa_bpl, by = "country") %>% print(n = Inf)

iv_totals = iv_totals %>%
    mutate(country = if_else(country == "ALL COUNTRIES", country, recode_visa_country(country)))

## PROJECTIONS BASED ON FEBRUARY ---------------------------------------------------------------------

# monthly totals, all countries
iv_monthly = iv_totals %>%
    filter(country == "ALL COUNTRIES") %>%
    mutate(month_date = as.Date(paste0(date, "-01")),
        fy = if_else(month >= 10, year + 1L, year)) %>%   # Oct-Sep fiscal year
    select(month_date, year, month, fy, issuances) %>%
    arrange(month_date)

print(iv_monthly, n = Inf)

write_csv(iv_monthly, "results/iv_monthly_totals.csv")

# monthly totals by country
iv_country_monthly = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    group_by(country, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year)) %>%   # Oct-Sep fiscal year
    arrange(country, date)

write_csv(iv_country_monthly, "results/iv_monthly_by_country.csv")

# plot: total immigrant visas issued per month
event_dates = as.Date(c("2025-01-20", "2025-06-09", "2026-01-01"))
y_top = max(iv_monthly$issuances)

ggplot(iv_monthly, aes(month_date, issuances)) +
    annotate("segment", x = event_dates, xend = event_dates, y = 0, yend = 0.82 * y_top,
        linetype = "dashed", color = "gray60") +
    annotate("text", x = event_dates, y = c(1.08, 0.95, 1.08) * y_top, vjust = 1, hjust = 1.05,
        lineheight = 0.9, size = 6, color = "gray40",
        label = c("Inauguration", "19-country\nban", "39-country ban\n+ 75-country pause")) +
    geom_line(color = "#3043B4", linewidth = 1.8) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
    labs(
        title = "Immigrant Visas Issued per Month, 2021-2026",
        subtitle = "All countries",
        x = NULL,
        y = NULL,
        caption = "Source: U.S. Department of State, Monthly Immigrant Visa Issuance Statistics, Feb 2021-Feb 2026.") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
        plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
        legend.position = "none",
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

ggsave("results/iv_monthly_totals.png", width = 15, height = 10)

## 2026 visa projections based on february vs. fiscal year 2024 (national) --------------------------
# compare each actual month of fiscal year 2026 (oct 2025-feb 2026) to the same month in fy2024
visas_2024_by_month = iv_monthly %>%
    filter(fy == 2024) %>%
    select(month, visas_2024 = issuances)

iv_2026 = iv_monthly %>%
    filter(fy == 2026) %>%
    select(month, month_date, visas_2026 = issuances) %>%
    left_join(visas_2024_by_month, by = "month") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

print(iv_2026)

write_csv(iv_2026, "results/iv_decline_fy2026_vs_fy2024.csv")

# march-september projection, based on february's change vs. fiscal year 2024
feb_pct_change = iv_2026$pct_change[iv_2026$month == 2]

iv_projection = iv_monthly %>%
    filter(fy == 2024, month >= 3, month <= 9) %>%
    transmute(month,
        visas_2024 = issuances,
        visas_2026_projected = round(issuances * (1 + feb_pct_change)),
        change = visas_2026_projected - visas_2024,
        pct_change = change / visas_2024,
        month_date = as.Date(paste0("2026-", sprintf("%02d", month), "-01")))

print(iv_projection)

write_csv(iv_projection, "results/iv_projection_mar_sep_2026.csv")

projection_total = bind_rows(
        iv_2026 %>% select(month, visas_2024, visas_2026),
        iv_projection %>% select(month, visas_2024, visas_2026 = visas_2026_projected)) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026)) %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

print(projection_total)

write_csv(projection_total, "results/iv_projection_total_fy2026.csv")

# plot: actual visas with march-september 2026 projected (dotted) off fiscal year 2024 levels
plot_data = bind_rows(
    iv_monthly %>%
        transmute(month_date, issuances, series = "Actual"),
    iv_monthly %>%
        filter(month_date == max(month_date)) %>%
        transmute(month_date, issuances, series = "Projected"),   # connects the two lines
    iv_projection %>%
        transmute(month_date, issuances = visas_2026_projected, series = "Projected"))

ggplot(plot_data, aes(month_date, issuances, linetype = series)) +
    geom_line(color = "#3043B4", linewidth = 1.8) +
    scale_linetype_manual(values = c("Actual" = "solid", "Projected" = "dotted")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0)) +
    scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, NA)) +
    labs(
        title = "Immigrant Visas Issued per Month, 2021-2026",
        subtitle = "Mar-Sep 2026 projected using February's change versus fiscal year 2024",
        x = NULL,
        y = NULL,
        linetype = NULL,
        caption = "Source: U.S. Department of State, Monthly Immigrant Visa Issuance Statistics, Feb 2021-Feb 2026.") +
    theme_minimal() +
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

ggsave("results/iv_monthly_projection.png", width = 15, height = 10)

## 2026 visa projections based on february vs. fiscal year 2024 (by country) ------------------------

# fy2026 actual (oct 2025-feb 2026) and fy2024, by country
iv_country_2024 = iv_country_monthly %>%
    filter(fy == 2024) %>%
    select(country, month, visas_2024 = issuances)

iv_country_2026 = iv_country_monthly %>%
    filter(fy == 2026) %>%
    select(country, month, visas_2026 = issuances) %>%
    left_join(iv_country_2024, by = c("country", "month")) %>%
    mutate(change = visas_2026 - visas_2024)

# march-september projection rate by country, using each country's own february change vs. fy2024
country_pct_change = acs_bpl %>%
    left_join(
        iv_country_2026 %>%
            filter(month == 2) %>%
            select(country, visas_2024, visas_2026),
        by = "country") %>%
    filter(!(is.na(visas_2024) & !is.na(visas_2026))) %>%   # drop countries that went from zero to >=1 in february
    mutate(visas_2024 = coalesce(visas_2024, 0),
        visas_2026 = coalesce(visas_2026, 0),
        pct_change = if_else(visas_2024 == 0, -1, (visas_2026 - visas_2024) / visas_2024))

iv_country_projection = iv_country_monthly %>%
    filter(fy == 2024, month >= 3, month <= 9) %>%
    select(country, month, visas_2024 = issuances) %>%
    inner_join(country_pct_change %>% select(country, pct_change), by = "country") %>%   # drops countries excluded from country_pct_change (e.g. slovakia)
    mutate(visas_2026_projected = round(visas_2024 * (1 + pct_change)),
        change = visas_2026_projected - visas_2024)

write_csv(iv_country_projection, "results/iv_country_projection_mar_sep_2026.csv")

# oct-feb actual totals by country and fiscal year -- aggregated independently so a country missing
# a row in one fiscal year (e.g., zero february 2026 visas) doesn't silently drop its total in the other
octfeb_actual = iv_country_monthly %>%
    filter(fy %in% c(2024, 2026), month %in% c(10, 11, 12, 1, 2)) %>%
    semi_join(country_pct_change, by = "country") %>%   # drop countries excluded from country_pct_change (e.g. slovakia)
    group_by(country, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, values_fill = 0, names_prefix = "visas_")

marsep_total = iv_country_projection %>%
    group_by(country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026_projected),
        .groups = "drop")

# appendix table: by country, change in visas issued and estimated total missing workers
missing_visas_by_country = bind_rows(octfeb_actual, marsep_total) %>%
    group_by(country) %>%
    summarise(visas_2024 = sum(visas_2024),
        visas_2026 = sum(visas_2026),
        .groups = "drop") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024) %>%
    arrange(change)

# smell check: add feb 2024 and feb 2026 actuals alongside the fiscal-year totals
feb_check = iv_country_monthly %>%
    filter(month == 2, year %in% c(2024, 2026)) %>%
    select(country, year, issuances) %>%
    pivot_wider(names_from = year, values_from = issuances, names_prefix = "feb_") %>%
    mutate(feb_pct_change = (feb_2026 - feb_2024) / feb_2024)

missing_visas_by_country = missing_visas_by_country %>%
    left_join(feb_check, by = "country")

print(missing_visas_by_country, n = Inf)

write_csv(missing_visas_by_country, "results/missing_visas_by_country.csv")

# fy2026 total, summing each country's own projection (bottom-up, vs. the single national-rate projection)
# excludes slovakia entirely (zero feb 2024, >=1 feb 2026 -- no usable rate)
missing_visas_total_by_country = missing_visas_by_country %>%
    summarise(total_2024 = sum(visas_2024),
        total_2026 = sum(visas_2026),
        net_change = sum(change),
        pct_change = net_change / total_2024)

print(missing_visas_total_by_country)

## ACS: immigrants by country and their occupation shares ------------------------------------------

# workers by country, all ages
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

# occupations by US Standard Occupational Classification (SOC) system -- 23 major groups
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

# share of ALL immigrants from each country (not just the employed) working in each occupation --
# the IV visa data can't separate workers from family members, so the counterfactual needs the
# share of all immigrants, not the share conditional on being employed
bpl_occ_group = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(country, pop, occ_group) %>%
    summarise(occ_workers = sum(perwt), .groups = "drop") %>%
    mutate(occ_share = occ_workers / pop)

# one row per country, one column per occupation (share of all immigrants from that country)
bpl_jobs = bpl_occ_group %>%
    select(country, occ_group, occ_share) %>%
    pivot_wider(names_from = occ_group, values_from = occ_share, values_fill = 0) %>%
    left_join(bpl_table_workers %>% select(country, n, pop, n_workers, workers, pct_workers), by = "country") %>%
    relocate(n, pop, n_workers, workers, pct_workers, .after = country) %>%
    arrange(desc(pop))

print(bpl_jobs, n = Inf, width = Inf)

write_csv(bpl_jobs, "results/bpl_jobs_occ_shares.csv")

write_csv(missing_visas_by_country, "results/missing_visas_by_country.csv")

# total missing healthcare workers
missing_healthcare_workers = missing_visas_by_country %>%
    select(country, change) %>%
    left_join(
        bpl_occ_group %>%
            filter(occ_group %in% c("Healthcare practitioners & technical", "Healthcare support")) %>%
            select(country, occ_group, occ_share),
        by = "country") %>%
    mutate(missing_workers = change * occ_share)

print(missing_healthcare_workers, n = Inf)

missing_healthcare_workers %>%
    group_by(occ_group) %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

missing_healthcare_workers %>%
    summarise(total_missing = sum(missing_workers, na.rm = TRUE))

missing_healthcare_workers %>%
    group_by(country) %>%
    summarise(missing_workers = sum(missing_workers, na.rm = TRUE), .groups = "drop") %>%
    arrange(missing_workers) %>%
    print(n = Inf)
