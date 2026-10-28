## Preliminaries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS-
acs = read_rds("data/output/acs.RDS")

# adding ipums occ labels
occ_labels = read_html("https://usa.ipums.org/usa/volii/occ2018.shtml") %>%
  html_elements("table") %>% .[[2]] %>%
  html_table(header = FALSE) %>%
  select(occ = X1, occ_title = X2) %>%
  filter(str_detect(occ, "^\\d{3,4}$")) %>%          # drop section headings
  mutate(occ = sprintf("%04d", as.integer(occ))) %>%
  distinct(occ, .keep_all = TRUE)

write_csv(occ_labels, "data/output/occ2018_labels.csv") 

occ_labels = read_csv("data/output/occ2018_labels.csv")

# counts by country, immigrants only (naturalized citizens and noncitizens)
bpl_table = acs %>%
    filter(bpld > 15000, citizen %in% c(2, 3)) %>%
    group_by(country) %>%
    summarise(n = n(), 
        pop = sum(perwt)) %>%
    arrange(desc(pop))

print(bpl_table, n = Inf)

write_csv(bpl_table, "results/birthplace_table_immigrants.csv")

# how many foreign-born with citizen == 0 (N/A)
acs %>%
  filter(country != "United States") %>%
  group_by(citizen = as_factor(citizen)) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop")

# caribbean? 
acs %>%
  filter(country != "United States", zap_labels(citizen) == 0) %>%
  group_by(bpld, bpld_lbl, country) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop") %>%
  arrange(desc(n))

# workers by country
# empstat universe: persons age 16+
bpl_work_table = acs %>%
    filter(bpld > 15000, citizen %in% c(2, 3)) %>%
    mutate(employed = empstat == 1) %>%
    group_by(country) %>%
    summarise(n = n(),
        pop = sum(perwt), 
        workers = sum(perwt[employed]),
        .groups = "drop") %>%
    mutate(pct_workers = workers / pop) %>%
    arrange(desc(pop))

print(bpl_work_table, n = Inf)

write_csv(bpl_work_table, "results/birthplace_workers_immigrants.csv")

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

View(bpl_table_workers)

# occupations by US Standard Occupational Classification (SOC) system -- 23 major groups
top_n_occ = 3

top_occ = acs %>%
  filter(country != "United States", citizen %in% c(2, 3), empstat == 1) %>%
  mutate(occ = as.integer(zap_labels(occ))) %>%
  group_by(country, occ) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop_last") %>%
  mutate(share = pop / sum(pop)) %>%
  slice_max(pop, n = top_n_occ, with_ties = FALSE) %>%
  mutate(rank  = row_number(),
         label = paste0(sprintf("%04d", occ), " (", round(100 * share, 1), "%)")) %>%
  ungroup()

top_occ_wide = top_occ %>%
  select(country, rank, occ = label, workers = pop) %>%
  pivot_wider(names_from = rank, values_from = c(occ, workers),
              names_glue = "{.value}_{rank}") %>%
  select(country, all_of(as.vector(rbind(paste0("occ_", 1:top_n_occ),
                                         paste0("workers_", 1:top_n_occ)))))

bpl_jobs = bpl_table_workers %>%
  left_join(top_occ_wide, by = "country") %>%
  arrange(desc(pop))

print(bpl_jobs, n = Inf, width = Inf)

write_csv(bpl_jobs, "results/top_3_jobs.csv")

# check missing countries
visas = read_csv("data/input/MonthlyImmigrantVisasFullData2024(FullData).csv",
    col_names = FALSE, col_types = cols(.default = "c"), locale = locale(encoding = "latin1"))

visas = visas %>%
    select(fy = X1, visa_country = X2, class = X3, issuances = X4, month = X5, year = X6) %>%
    mutate(issuances    = suppressWarnings(parse_number(issuances)),  # page headers -> NA
         visa_country = str_squish(visa_country)) %>%
    filter(str_detect(fy, "^FY"),
         !is.na(issuances),
         !is.na(class),
         str_to_lower(visa_country) != "grand total")

visas %>% group_by(fy) %>% summarise(total = sum(issuances))

acs_bpl = acs %>%
    filter(country != "United States") %>% distinct(country)

visa_bpl = visas %>%
    group_by(visa_country) %>%
    summarise(visas = sum(issuances), .groups = "drop")

# In visa data but not ACS
visa_bpl %>% anti_join(acs_bpl, by = c("visa_country" = "country")) %>% arrange(desc(visas)) %>% print(n = Inf)

# In ACS but not visa data
acs_bpl %>% anti_join(visa_bpl, by = c("country" = "visa_country")) %>% print(n = Inf)

# recode visa countries to match acs
visas = visas %>%
  mutate(country = case_when(
    # Same country, different spelling 
    visa_country == "Bahamas, The"                         ~ "Bahamas",
    visa_country %in% c("Bosnia-Herzegovina",
                        "Bosnia and Herzegovina")          ~ "Bosnia and Herzegovina",
    visa_country == "Cabo Verde"                           ~ "Cape Verde",
    str_detect(visa_country, "^China") &
      !str_detect(visa_country, "Taiwan")                  ~ "China",
    visa_country %in% c("China - Taiwan born", "Taiwan")   ~ "Taiwan",
    visa_country == "Congo, Democratic Republic of the"    ~ "DR Congo",
    visa_country == "Congo, Republic of the"               ~ "Republic of the Congo",
    visa_country %in% c("Gambia, The", "The Gambia")       ~ "Gambia",
    visa_country %in% c("Great Britain and Northern Ireland", "United Kingdom",
                        "Northern Ireland",
                        "Northern Ireland (DV only)")      ~ "United Kingdom",
    str_detect(visa_country, "^Hong Kong")                 ~ "Hong Kong",
    str_detect(visa_country, "^Korea")                     ~ "Korea",      # ACS Korea incl. North Korea
    visa_country %in% c("Macedonia", "North Macedonia")    ~ "North Macedonia",
    str_detect(visa_country, "^Micronesia")                ~ "Micronesia",
    visa_country %in% c("Saint Kitts and Nevis",
                        "St. Kitts and Nevis")             ~ "St. Kitts and Nevis",
    visa_country == "Saint Lucia"                          ~ "St. Lucia",
    visa_country == "Saint Vincent and the Grenadines"     ~ "St. Vincent and the Grenadines",
    visa_country %in% c("Israel", "Jerusalem",
                        "Palestinian Authority Travel Document") ~ "Israel/Palestine",

    # No own ACS code: assign to the ACS catch-all they fall into 
    visa_country %in% c("Anguilla", "Aruba", "British Virgin Islands", "Cayman Islands",
                        "Curacao", "Guadeloupe", "Montserrat", "Saint Maarten",
                        "Saint Martin", "Sint Maarten",
                        "Turks and Caicos Islands")        ~ "Other Caribbean",
    visa_country %in% c("Suriname", "Falkland Islands")    ~ "Other South America",
    visa_country %in% c("Greenland",
                        "St. Pierre and Miquelon")         ~ "Other Americas",
    visa_country %in% c("Andorra", "Estonia", "Faroe Islands", "Gibraltar",
                        "Luxembourg", "Malta", "Monaco", "San Marino",
                        "Slovenia", "Vatican City")        ~ "Other Europe",
    visa_country %in% c("Bahrain", "Brunei", "Cyprus", "Macau", "Macau S.A.R.",
                        "Maldives", "Oman", "Qatar", "Timor-Leste",
                        "Tajikistan", "Turkmenistan")      ~ "Other Asia",
    visa_country %in% c("Tunisia", "Western Sahara")       ~ "Other North Africa",
    visa_country %in% c("Benin", "Burkina Faso", "Guinea-Bissau", "Mali",
                        "Mauritania", "Niger")             ~ "Other West Africa",
    visa_country %in% c("Burundi", "Comoros", "Djibouti", "Madagascar", "Malawi",
                        "Mauritius", "Mozambique", "Seychelles", "South Sudan",
                        "British Indian Ocean Territory")  ~ "Other East Africa",
    visa_country %in% c("Angola", "Central African Republic", "Chad",
                        "Equatorial Guinea", "Gabon", "Sao Tome and Principe",
                        "Botswana", "Eswatini", "Lesotho", "Namibia",
                        "St. Helena")                      ~ "Other Africa",
    visa_country %in% c("Christmas Island", "Cocos Islands", "Cocos (Keeling) Islands",
                        "Cook Islands", "French Polynesia", "Kiribati", "Nauru",
                        "New Caledonia", "Niue", "Palau", "Papua New Guinea",
                        "Pitcairn", "Solomon Islands", "Tuvalu", "Vanuatu",
                        "Wallis and Futuna")               ~ "Other, n.e.c.",
    visa_country %in% c("No Nationality", "Other")         ~ NA_character_,
    TRUE ~ visa_country))

visa_bpl = visas %>%
  group_by(country) %>%
  summarise(visas = sum(issuances), .groups = "drop")

visa_bpl %>% anti_join(acs_bpl, by = "country")   
acs_bpl %>% anti_join(visa_bpl, by = "country")  

# ACS groupings by region
acs = acs %>%
  mutate(bpl_region = case_when(
    bpld < 10000                 ~ "United States",
    bpld < 15000 | bpld == 71047 ~ "US Territories",                # Puerto Rico, Guam, USVI, American Samoa, Northern Mariana Islands
    bpld < 20000                 ~ "Other North America",           # Canada, Bermuda, Cape Verde
    bpld < 30000                 ~ "Central America and Caribbean", # Mexico, Belize, Costa Rica, El Salvador, Guatemala, Honduras, Nicaragua, Panama, Cuba, Dominican Republic, Haiti, Jamaica, Bahamas, Barbados, etc.
    bpld < 40000                 ~ "South America",                 # Argentina, Bolivia, Brazil, Chile, Colombia, Ecuador, Guyana, Paraguay, Peru, Uruguay, Venezuela

    # Europe
    bpld < 42000                 ~ "Northern Europe",               # Denmark, Finland, Iceland, Norway, Sweden, England, Scotland, Northern Ireland, Ireland
    bpld < 43000                 ~ "Western Europe",                # Belgium, France, Netherlands, Switzerland
    bpld < 45000                 ~ "Southern Europe",               # Albania, Greece, North Macedonia, Italy, Portugal (incl. Azores), Spain
    bpld < 47000                 ~ "Central/Eastern Europe",        # Austria, Germany, Czech Republic, Slovakia, Hungary, Poland, Romania, Bulgaria, former Yugoslavia, Russia, Ukraine, Belarus, Moldova, Baltics, Caucasus, Kazakhstan, Kyrgyzstan, Uzbekistan
    bpld < 50000                 ~ "Other Europe",                  # 49900 Europe, n.s.

    # Asia
    bpld < 51000                 ~ "East Asia",                     # China, Hong Kong, Taiwan, Mongolia, Japan, Korea
    bpld < 52000                 ~ "Southeast Asia",                # Cambodia, Indonesia, Laos, Malaysia, Philippines, Singapore, Thailand, Vietnam
    bpld < 53000                 ~ "India/Southwest Asia",          # Afghanistan, India, Bangladesh, Bhutan, Burma, Pakistan, Sri Lanka, Iran, Nepal
    bpld < 55000                 ~ "Middle East/Asia Minor",        # Iraq, Israel/Palestine, Jordan, Kuwait, Lebanon, Saudi Arabia, Syria, Turkey, UAE, Yemen
    bpld < 60000                 ~ "Other Asia",                    # 59900 Asia, n.e.c./n.s.

    bpld < 70000                 ~ "Africa",                        # all African countries and n.s. codes
    bpld < 80000                 ~ "Oceania",                       # Australia, New Zealand, Fiji, Tonga, Marshall Islands, Micronesia
    TRUE                         ~ "Other/Unknown"                  # 95000 Other n.e.c.
  ))

acs %>%
  distinct(bpl_region, country) %>%
  arrange(bpl_region, country) %>%
  print(n = Inf)

# occupations by region
region_table = acs %>%
    filter(country != "United States", citizen %in% c(2, 3)) %>%
    group_by(bpl_region) %>%
    summarise(n = n(),
        pop = sum(perwt),
        n_workers = sum(empstat == 1),
        workers = sum(perwt[empstat == 1]),
        .groups = "drop")

region_occ = acs %>%
    filter(country != "United States", citizen %in% c(2, 3),
        empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ)))) %>%
    group_by(bpl_region, occ) %>%
    summarise(occ_n = n(),
        occ_workers = sum(perwt),
        .groups = "drop_last") %>%
    mutate(occ_share = occ_workers / sum(occ_workers)) %>%
    arrange(desc(occ_workers), .by_group = TRUE) %>%
    ungroup() %>%
    left_join(occ_labels, by = "occ") %>%
    relocate(occ_title, .after = occ)

region_table = region_table %>%
    left_join(region_occ, by = "bpl_region") %>%
    arrange(desc(pop), desc(occ_workers))

view(region_table)

write_csv(region_table, "results/region_occupations.csv")

dir.create("results/region_tables", recursive = TRUE, showWarnings = FALSE)

# individual csvs for each region
for (r in unique(region_table$bpl_region)) {
    out = region_table %>%
        filter(bpl_region == r)
    fname = str_replace_all(r, "[^A-Za-z0-9]+", "_")
    write_csv(out, file.path("results/region_tables", paste0(fname, ".csv")))}

# total immigrants in each job
occ_totals = acs %>%
    filter(empstat == 1) %>%
    mutate(occ = sprintf("%04d", as.integer(zap_labels(occ))),
        immig = country != "United States" & citizen %in% c(2, 3)) %>%
    group_by(occ) %>%
    summarise(n_immig = sum(immig),
        immig_workers = sum(perwt[immig]),
        all_workers = sum(perwt),
        .groups = "drop") %>%
    mutate(immig_share_of_occ = immig_workers / all_workers,
        share_of_immig_workers = immig_workers / sum(immig_workers)) %>%
    left_join(occ_labels, by = "occ") %>%
    relocate(occ_title, .after = occ) %>%
    arrange(desc(immig_workers))

print(occ_totals, n = 20, width = Inf)

write_csv(occ_totals, "results/occ_immigrant_totals.csv")

occ_by_immig_share = occ_totals %>% arrange(desc(immig_share_of_occ)) 

write_csv(occ_by_immig_share, "results/occ_immigrant_totals_by_share.csv")

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

occ_totals = occ_totals %>%
    mutate(occ_group = occ_group_fn(as.integer(occ))) %>%
    relocate(occ_group, .after = occ_title)

# Totals by group
occ_group_totals = occ_totals %>%
    group_by(occ_group) %>%
    summarise(n_immig = sum(n_immig),
        immig_workers = sum(immig_workers),
        all_workers = sum(all_workers),
        .groups = "drop") %>%
    mutate(immig_share_of_occ = immig_workers / all_workers,
        share_of_immig_workers = immig_workers / sum(immig_workers)) %>%
    arrange(desc(immig_workers))

print(occ_group_totals, n = Inf)

write_csv(occ_group_totals, "results/occupation_groups_all_immigrants.csv")

### VISA ISUANCES ###
iv_totals = read_csv("data/output/iv_issuances_monthly_totals.csv")

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
        x %in% c("Anguilla", "Aruba", "British Virgin Islands", "Cayman Islands",
            "Curacao", "Guadeloupe", "Montserrat", "Saint Maarten",
            "Saint Martin", "Sint Maarten", "Turks and Caicos Islands") ~ "Other Caribbean",
        x %in% c("Suriname", "Falkland Islands")              ~ "Other South America",
        x %in% c("Greenland", "St. Pierre and Miquelon")      ~ "Other Americas",
        x %in% c("Andorra", "Estonia", "Faroe Islands", "Gibraltar", "Luxembourg",
            "Malta", "Monaco", "San Marino", "Slovenia", "Vatican City") ~ "Other Europe",
        x %in% c("Bahrain", "Brunei", "Cyprus", "Macau", "Macau S.A.R.", "Maldives",
            "Oman", "Qatar", "Timor-Leste", "Tajikistan", "Turkmenistan") ~ "Other Asia",
        x == "Western Sahara"                                 ~ "Other North Africa",
        x %in% c("Benin", "Burkina Faso", "Guinea-Bissau", "Mali",
            "Mauritania", "Niger")                           ~ "Other West Africa",
        x %in% c("Burundi", "Comoros", "Djibouti", "Madagascar", "Malawi", "Mauritius",
            "Mozambique", "Seychelles", "British Indian Ocean Territory") ~ "Other East Africa",
        x %in% c("Angola", "Central African Republic", "Chad", "Equatorial Guinea",
            "Gabon", "Sao Tome and Principe", "Botswana", "Eswatini", "Lesotho",
            "Namibia", "St. Helena")                         ~ "Other Africa",
        x %in% c("Christmas Island", "Cocos Islands", "Cocos (Keeling) Islands",
            "Cook Islands", "French Polynesia", "Kiribati", "Nauru", "New Caledonia",
            "Niue", "Palau", "Papua New Guinea", "Pitcairn", "Solomon Islands",
            "Tuvalu", "Vanuatu", "Wallis and Futuna")        ~ "Other, n.e.c.",
        x %in% c("No Nationality", "Other")                   ~ NA_character_,
        TRUE ~ x)}

# monthly totals, all countries
iv_monthly = iv_totals %>%
    filter(country == "ALL COUNTRIES") %>%
    select(date, year, month, issuances) %>%
    arrange(date) %>%
    mutate(issuances_last_year = lag(issuances, 12),
        pct_change_yoy = issuances / issuances_last_year - 1)

print(iv_monthly, n = Inf)

write_csv(iv_monthly, "results/iv_monthly_totals.csv")

# totals by country, 12-month periods ending in February (Mar 2021-Feb 2022 ... Mar 2025-Feb 2026)
iv_country = iv_totals %>%
    filter(country != "ALL COUNTRIES", date >= "2021-03") %>%
    mutate(acs_country = recode_visa_country(country),
        period_end = if_else(month >= 3, year + 1, year),
        period = paste0("mar", period_end - 1, "_feb", period_end)) %>%
    group_by(acs_country, period) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = period, values_from = issuances, values_fill = 0) %>%
    mutate(total = rowSums(across(starts_with("mar"))),
        change_last_year = mar2025_feb2026 - mar2024_feb2025,
        pct_change_last_year = change_last_year / mar2024_feb2025) %>%
    arrange(desc(total))

print(iv_country, n = 20, width = Inf)

write_csv(iv_country, "results/iv_totals_by_country.csv")

# monthly totals by country 
iv_country_monthly = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(acs_country = recode_visa_country(country)) %>%
    group_by(acs_country, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    arrange(acs_country, date)

write_csv(iv_country_monthly, "results/iv_monthly_by_country.csv")

iv_country_monthly_wide = iv_country_monthly %>%
    select(acs_country, date, issuances) %>%
    pivot_wider(names_from = date, values_from = issuances, values_fill = 0)

write_csv(iv_country_monthly_wide, "results/iv_monthly_by_country_wide.csv")


## Plot: monthly immigrant visas by policy group -----------------------------------------------

# policy groups, using State Dept country names
ban_countries = c(
    # full (19 + Palestinian Authority documents)
    "Afghanistan", "Burma", "Burkina Faso", "Chad", "Congo, Republic of the",
    "Equatorial Guinea", "Eritrea", "Haiti", "Iran", "Laos", "Libya", "Mali", "Niger",
    "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Syria", "Yemen",
    "Palestinian Authority Travel Document",
    # partial (20)
    "Angola", "Antigua and Barbuda", "Benin", "Burundi", "Cote d'Ivoire", "Cuba",
    "Dominica", "Gabon", "Gambia, The", "Malawi", "Mauritania", "Nigeria", "Senegal",
    "Tanzania", "Togo", "Tonga", "Turkmenistan", "Venezuela", "Zambia", "Zimbabwe")

pause_only_countries = c(
    "Albania", "Algeria", "Armenia", "Azerbaijan", "Bahamas, The", "Bangladesh",
    "Barbados", "Belarus", "Belize", "Bhutan", "Bosnia-Herzegovina", "Brazil",
    "Cambodia", "Cameroon", "Cabo Verde", "Colombia", "Congo, Democratic Republic of the",
    "Egypt", "Ethiopia", "Fiji", "Georgia", "Ghana", "Grenada", "Guatemala", "Guinea",
    "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Kosovo", "Kuwait", "Kyrgyzstan",
    "Lebanon", "Liberia", "North Macedonia", "Moldova", "Mongolia", "Montenegro",
    "Morocco", "Nepal", "Nicaragua", "Pakistan", "Russia", "Rwanda",
    "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
    "Thailand", "Tunisia", "Uganda", "Uruguay", "Uzbekistan")

group_colors = c("Travel ban countries (39)" = "#3043B4",
    "75-country pause only" = "#C97703",
    "All other countries" = "#7C756D")

iv_groups = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(policy_group = case_when(
            country %in% ban_countries        ~ "Travel ban countries (39)",
            country %in% pause_only_countries ~ "75-country pause only",
            TRUE                              ~ "All other countries"),
        policy_group = factor(policy_group, levels = names(group_colors)),
        month_date = as.Date(paste0(date, "-01"))) %>%
    group_by(policy_group, month_date) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    group_by(policy_group) %>%
    mutate(index_2024 = 100 * issuances / mean(issuances[format(month_date, "%Y") == "2024"])) %>%
    ungroup()

write_csv(iv_groups, "results/iv_monthly_by_policy_group.csv")

event_dates = as.Date(c("2025-01-20", "2025-06-09", "2026-01-01"))
y_top = max(iv_groups$issuances)

ggplot(iv_groups, aes(month_date, issuances, color = policy_group)) +
    annotate("segment", x = event_dates, xend = event_dates, y = 0, yend = 0.82 * y_top,
        linetype = "dashed", color = "grey60") +
    annotate("text", x = event_dates, y = c(1.08, 0.95, 1.08) * y_top, vjust = 1, hjust = 1.05,
        lineheight = 0.9, size = 3, color = "grey35",
        label = c("Inauguration", "19-country\nban", "39-country ban\n+ 75-country pause")) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = group_colors) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    labs(x = NULL, y = "Immigrant visas issued per month", color = NULL,
        title = "Immigrant visas issued, by policy group",
        caption = "Source: U.S. Department of State, Monthly Immigrant Visa Issuance Statistics, Feb 2021-Feb 2026.") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot")

ggsave("results/iv_policy_group_monthly.png", width = 9, height = 5, dpi = 300, bg = "white")

# visas by region (matching acs groupings)
acs %>%
    filter(!is.na(country)) %>%
    distinct(country, bpl_region) %>%
    arrange(bpl_region, country) %>%
    write_csv("data/output/country_region_lookup.csv")

country_region = read_csv("data/output/country_region_lookup.csv")

iv_regions = iv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(acs_country = recode_visa_country(country),
        month_date = as.Date(paste0(date, "-01"))) %>%
    left_join(country_region, by = c("acs_country" = "country")) %>%
    mutate(bpl_region = if_else(acs_country == "Samoa", "Oceania", bpl_region))   # independent Samoa

iv_regions = iv_regions %>%
    filter(!is.na(bpl_region)) %>%
    group_by(bpl_region, month_date) %>%
    summarise(issuances = sum(issuances), .groups = "drop")

# order panels by 2024 volume, largest first
region_order = iv_regions %>%
    filter(format(month_date, "%Y") == "2024") %>%
    group_by(bpl_region) %>%
    summarise(avg_2024 = mean(issuances)) %>%
    arrange(desc(avg_2024))

iv_regions = iv_regions %>%
    mutate(bpl_region = factor(bpl_region, levels = region_order$bpl_region))

write_csv(iv_regions, "results/iv_monthly_by_region.csv")

event_dates = as.Date(c("2025-01-20", "2025-06-09", "2026-01-01"))

ggplot(iv_regions, aes(month_date, issuances)) +
    geom_vline(xintercept = event_dates, linetype = "dashed", color = "grey70", linewidth = 0.3) +
    geom_line(color = "#3043B4", linewidth = 0.6) +
    facet_wrap(~ bpl_region, scales = "free_y", ncol = 4) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    labs(x = NULL, y = "Immigrant visas issued per month",
        title = "Immigrant visas issued by region of birth",
        subtitle = "Dashed lines: inauguration (Jan 2025), 19-country ban (Jun 2025), 39-country ban + 75-country pause (Jan 2026). Y-axes differ by panel.",
        caption = "Source: U.S. Department of State, Monthly Immigrant Visa Issuance Statistics, Feb 2021-Feb 2026.") +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0),
        plot.title.position = "plot")

ggsave("results/iv_region_monthly.png", width = 11, height = 8, dpi = 300, bg = "white")
