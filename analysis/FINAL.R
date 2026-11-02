## Preliminaries ----------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS
acs = read_rds("data/output/acs.RDS")

# IPUMS occupation labels
occ_labels = read_csv("data/output/occ2018_labels.csv")

## Read visa issuances
iv_totals = read_csv("data/output/iv_issuances_monthly_totals.csv")

## VISA ISSUANCES -----------------------------------------------------------------------------------
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

# monthly totals, all countries
iv_monthly = iv_totals %>%
    filter(country == "ALL COUNTRIES") %>%
    mutate(month_date = as.Date(paste0(date, "-01"))) %>%
    select(month_date, year, month, issuances) %>%
    arrange(month_date)

print(iv_monthly, n = Inf)

write_csv(iv_monthly, "results/iv_monthly_totals.csv")

# monthly totals by country
iv_country_monthly = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    group_by(country, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
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

## 2026 visa projections based on 2024 
# compare jan/feb 2026 month to the same month in 2024
visas_2024_by_month = iv_monthly %>%
    filter(year == 2024) %>%
    select(month, visas_2024 = issuances)

iv_2026 = iv_monthly %>%
    filter(year == 2026) %>%
    select(month, month_date, visas_2026 = issuances) %>%
    left_join(visas_2024_by_month, by = "month") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024)

print(iv_2026)

write_csv(iv_2026, "results/iv_decline_2026_vs_2024.csv")

# march-september projection, based on february's change vs. 2024
feb_pct_change = iv_2026$pct_change[iv_2026$month == 2]

iv_projection = iv_monthly %>%
    filter(year == 2024, month >= 3, month <= 9) %>%
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

write_csv(projection_total, "results/iv_projection_total_jan_sep_2026.csv")

# plot: actual visas with march-september 2026 projected (dotted) off 2024 levels
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
        subtitle = "Mar-Sep 2026 projected using February's change versus 2024",
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

## ACS: immigrant occupations by country (2024 5-year sample) 

occ_group_fn = function(occ) {
    case_when(
        occ >= 10   & occ <= 960  ~ "Management, business & financial",
        occ >= 1005 & occ <= 1980 ~ "Computer, engineering & science",
        occ >= 2001 & occ <= 2920 ~ "Education, legal, community service, arts & media",
        occ >= 3000 & occ <= 3550 ~ "Healthcare practitioners & technical",  # doctors, nurses, technicians
        occ >= 3601 & occ <= 3655 ~ "Healthcare support",                    # home health & personal care aides, nursing assistants
        occ >= 3700 & occ <= 3960 ~ "Protective service",                   # police, correctional officers, firefighters,security guards, animal control
        occ >= 4000 & occ <= 4160 ~ "Food preparation & serving",
        occ >= 4200 & occ <= 4255 ~ "Building & grounds cleaning",           # janitors, maids, landscapers
        occ >= 4330 & occ <= 4655 ~ "Personal care & service",              # childcare, hairdressers, manicurists
        occ >= 4700 & occ <= 4965 ~ "Sales",
        occ >= 5000 & occ <= 5940 ~ "Office & administrative support",
        occ >= 6005 & occ <= 6130 ~ "Farming, fishing & forestry",
        occ >= 6200 & occ <= 6950 ~ "Construction & extraction",        # construction, electricians, painters, highway maintenance, mining
        occ >= 7000 & occ <= 7640 ~ "Installation, maintenance & repair", # mechanics, fire alarm systems installers, HVAC mechanics, locksmiths
        occ >= 7800 & occ <= 7855 ~ "Food processing",                       # bakers, butchers, meat/poultry processing, batchmakers
        occ >= 7700 & occ <= 8990 ~ "Production",                       # electronics assemblers, machinsts, metal furnace operators, laundry and dry-cleaning workers,
        occ >= 9005 & occ <= 9760 ~ "Transportation & material moving",   # pilots, flight attendants, bus drivers, taxi drivers
        occ >= 9800 & occ <= 9830 ~ "Military",
        TRUE ~ NA_character_)}

# country totals: immigrants and how many work
country_totals = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    summarise(n = n(),
        pop = sum(perwt),
        n_workers = sum(empstat == 1),
        workers = sum(perwt[empstat == 1]),
        .groups = "drop") %>%
    mutate(workers_per_immig = workers / pop) %>%
    arrange(desc(pop))

print(country_totals, n = Inf)

write_csv(country_totals, "results/birthplace_workers_immigrants.csv")

# occupation groups by country (employed immigrants)
country_occ_group = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(country, occ_group) %>%
    summarise(occ_n = n(),
        occ_workers = sum(perwt),
        .groups = "drop_last") %>%
    mutate(occ_share = occ_workers / sum(occ_workers)) %>%
    arrange(desc(occ_workers), .by_group = TRUE) %>%
    ungroup()

write_csv(country_occ_group, "results/country_occ_groups.csv")

# wide: one row per country, one column per occupation group (shares)
# apply to IV visa counts to estimate missing workers by occupation
country_occ_group_wide = country_occ_group %>%
    select(country, occ_group, occ_share) %>%
    pivot_wider(names_from = occ_group, values_from = occ_share, values_fill = 0) %>%
    left_join(country_totals %>% select(country, n_workers, workers), by = "country") %>%
    relocate(n_workers, workers, .after = country) %>%
    arrange(desc(workers))

write_csv(country_occ_group_wide, "results/country_occ_groups_wide.csv")

# detailed occupation codes + IPUMS titles by country (employed immigrants)
country_occ_detail = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
    group_by(country, occ) %>%
    summarise(occ_n       = n(),
        occ_workers = sum(perwt),
        .groups = "drop_last") %>%
    mutate(occ_share = occ_workers / sum(occ_workers)) %>%
    arrange(desc(occ_workers), .by_group = TRUE) %>%
    ungroup() %>%
    left_join(occ_labels, by = "occ") %>%
    relocate(occ_title, .after = occ)

write_csv(country_occ_detail, "results/country_occ_detail.csv")
