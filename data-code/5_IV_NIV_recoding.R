## Preliminaries ----------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

iv_totals = read_csv("data/output/iv_issuances_monthly_totals.csv")

niv_totals = read_csv("data/output/niv_issuances_monthly_totals.csv")

acs = read_rds("data/output/acs.RDS")

## -------------------------------------------------------------------------------------------------
# country names
unique(iv_totals$country) %>% sort()

unique(niv_totals$country) %>% sort()

# recoding state dept data to match ACS countries
acs_bpl = acs %>%
    filter(country != "United States") %>%
    distinct(country)

recode_visa_country = function(x) {
    case_when(
        # same country, different spelling
        x == "Bahamas, The"                                   ~ "Bahamas",
        x %in% c("Bosnia-Herzegovina", "Bosnia and Herzegovina") ~ "Bosnia and Herzegovina",
        x == "Cabo Verde"                                     ~ "Cape Verde",
        str_detect(x, "^China") & !str_detect(x, "Taiwan")    ~ "China",
        x %in% c("China - Taiwan born", "Taiwan")             ~ "Taiwan",
        x %in% c("Congo, Democratic Republic of the", "Democratic Republic of the Congo",
            "Dem. Rep. Congo")                                ~ "DR Congo",
        x %in% c("Congo, Republic of the", "Congo")           ~ "Republic of the Congo",
        x == "Ivory Coast"                                    ~ "Cote d'Ivoire",
        x %in% c("Gambia, The", "The Gambia")                 ~ "Gambia",
        x %in% c("Great Britain and Northern Ireland", "United Kingdom",
            "Northern Ireland", "Northern Ireland (DV only)") ~ "United Kingdom",
        str_detect(x, "^Hong Kong") |
            x == "British National Overseas (Hong Kong) Passport" ~ "Hong Kong",  # Hong Kong-BNO (IV) / BNO passport (NIV)
        str_detect(x, "^Korea") | x == "South Korea"          ~ "Korea",          # ACS Korea incl. North Korea
        x %in% c("Macedonia", "North Macedonia")              ~ "North Macedonia",
        str_detect(x, "^Micronesia")                          ~ "Micronesia",
        x %in% c("Saint Kitts and Nevis", "St. Kitts and Nevis") ~ "St. Kitts and Nevis",
        x == "Saint Lucia"                                    ~ "St. Lucia",
        x == "Saint Vincent and the Grenadines"               ~ "St. Vincent and the Grenadines",
        x %in% c("Israel", "Jerusalem", "Palestine",
            "Palestinian Authority Travel Document")         ~ "Israel/Palestine",
        x == "Sri Lanka (Ceylon)"                             ~ "Sri Lanka",
        x == "Myanmar (Burma)"                                ~ "Burma",

        # no own ACS code: assign to the ACS catch-all they fall into
        x == "Samoa"                                          ~ "Other, n.e.c.",  # independent Samoa
        x %in% c("Anguilla", "Aruba", "British Virgin Islands", "Cayman Islands",
            "Curacao", "Guadeloupe", "Montserrat", "Saint Maarten",
            "Saint Martin", "Sint Maarten", "Turks and Caicos Islands") ~ "Other Caribbean",
        x %in% c("Suriname", "Falkland Islands")              ~ "Other South America",
        x %in% c("Greenland", "St. Pierre and Miquelon")      ~ "Other Americas",
        x %in% c("Andorra", "Estonia", "Faroe Islands", "Gibraltar", "Liechtenstein",
            "Luxembourg", "Malta", "Monaco", "San Marino", "Slovenia",
            "Vatican City")                                   ~ "Other Europe",
        x %in% c("Bahrain", "Brunei", "Cyprus", "Macau", "Macau S.A.R.", "Maldives",
            "Oman", "Qatar", "Timor-Leste", "Tajikistan", "Turkmenistan") ~ "Other Asia",
        x %in% c("Tunisia", "Western Sahara")                 ~ "Other North Africa",
        x %in% c("Benin", "Burkina Faso", "Burkina Faso (UVolta)", "Guinea-Bissau",
            "Guinea - Bissau", "Mali", "Mauritania", "Niger")  ~ "Other West Africa",
        x %in% c("Burundi", "Comoros", "Djibouti", "Madagascar", "Malawi", "Mauritius",
            "Mozambique", "Seychelles", "South Sudan", "Republic of South Sudan",
            "British Indian Ocean Territory")                ~ "Other East Africa",
        x %in% c("Angola", "Central African Republic", "Chad", "Equatorial Guinea",
            "Gabon", "Sao Tome and Principe", "Botswana", "Eswatini", "Lesotho",
            "Namibia", "St. Helena")                         ~ "Other Africa",
        x %in% c("Christmas Island", "Cocos Islands", "Cocos (Keeling) Islands",
            "Cook Islands", "French Polynesia", "Kiribati", "Nauru", "New Caledonia",
            "Niue", "Palau", "Papua New Guinea", "Pitcairn", "Solomon Islands",
            "Tuvalu", "Vanuatu", "Wallis and Futuna")        ~ "Other, n.e.c.",
        x %in% c("No Nationality", "Other",
            "Non-Nationality Based Issuances", "Unknown")    ~ NA_character_,
        TRUE ~ x)}

## appendix table: every raw country/territory from both datasets mapped to its ACS category 
country_crosswalk = bind_rows(
        iv_totals %>% filter(country != "ALL COUNTRIES") %>% distinct(country) %>% mutate(source = "IV"),
        niv_totals %>% filter(country != "ALL COUNTRIES") %>% distinct(country) %>% mutate(source = "NIV")) %>%
    distinct(country, source) %>%
    mutate(acs_category = recode_visa_country(country),
        acs_category = if_else(is.na(acs_category), "Not matched (excluded)", acs_category)) %>%
    rename(visa_country = country) %>%
    arrange(acs_category, visa_country, source)

write_csv(country_crosswalk, "results/country_crosswalk.csv")

## checks: visa countries vs. ACS countries, for each dataset separately 
check_recode = function(totals, label) {
    recoded = totals %>%
        filter(country != "ALL COUNTRIES") %>%
        mutate(country = recode_visa_country(country)) %>%
        group_by(country) %>%
        summarise(issuances = sum(issuances), .groups = "drop")

    cat("\n==", label, ": in visa data but not ACS ==\n")
    print(recoded %>% anti_join(acs_bpl, by = "country") %>% arrange(desc(issuances)), n = Inf)

    cat("\n==", label, ": in ACS but not visa data (legacy ACS categories expected) ==\n")
    print(acs_bpl %>% anti_join(recoded, by = "country"), n = Inf)
}

check_recode(iv_totals, "IV")
check_recode(niv_totals, "NIV")

## apply recoding to national-level totals (kept separate — still used for the blanket/national
## projection method, since "ALL COUNTRIES" isn't a meaningful concept once NIV is class-filtered)
iv_totals = iv_totals %>%
    mutate(country = if_else(country == "ALL COUNTRIES", country, recode_visa_country(country)))

write_csv(iv_totals, "data/output/iv_issuances_monthly_totals_recoded.csv")

## combine all IV and selected NIV (workers, students, exchange visitors who would appear in ACS as US residents)
niv_classes_included = c(
  # diplomats, int'l orgs, NATO, and their personal employees
  "A1", "A2", "A3",
  "G1", "G2", "G3", "G4", "G5",
  "NATO1", "NATO2", "NATO3", "NATO4", "NATO5", "NATO6", "NATO7",
  # treaty traders/investors, specialty workers
  "E1", "E2", "E3", "E3D", "E3R",
  "H1B", "H1B1", "H2A", "H2B", "H3", "H4",
  "L1", "L2",
  "O1", "O2", "O3", "P1", "P2", "P3", "P4",
  "R1", "R2",
  "TN", "TD",
  "Q1",           # international cultural exchange
  "I",            # foreign media
  # students and exchange visitors
  "F1", "F2", "M1", "M2",
  "J1", "J2",
  # fiancé(e)s and spouses of USCs
  "K1", "K2", "K3", "K4",
  # crime and trafficking victims' derivatives
  "U1", "U2", "U3", "U4", "U5",
  "T2", "T3", "T4", "T5", "T6",
  # parents/children of G-4 special immigrants
  "N8", "N9")

niv_by_class = read_csv("data/output/niv_issuances_by_country_visa_class.csv")

# country-level, recoded, same shape for both IV and NIV, tagged by visa_type
iv_country_monthly = iv_totals %>%
    filter(is.na(country) | country != "ALL COUNTRIES") %>%
    group_by(country, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year)) %>%
    arrange(country, date)

niv_country_monthly = niv_by_class %>%
    filter(country != "ALL COUNTRIES", visa_class %in% niv_classes_included) %>%
    mutate(country = recode_visa_country(country)) %>%
    group_by(country, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year)) %>%
    arrange(country, date)

visa_country_monthly = bind_rows(
    iv_country_monthly %>% mutate(visa_type = "IV"),
    niv_country_monthly %>% mutate(visa_type = "NIV"))

write_csv(visa_country_monthly, "data/output/visa_country_monthly.csv")

# unmatched countries, restricted NIV
niv_by_class %>%
    filter(country != "ALL COUNTRIES", visa_class %in% niv_classes_included) %>%
    mutate(country = recode_visa_country(country)) %>%
    filter(is.na(country)) %>%
    summarise(issuances = sum(issuances))

# unmatched, not restricted (full niv)
niv_totals %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(country = recode_visa_country(country)) %>%
    filter(is.na(country)) %>%
    summarise(issuances = sum(issuances))

# NIV unsrestricted full breakdown, feb 2024 - feb 2026 only  --------------------------
niv_by_class_national = niv_by_class %>%
    filter(country != "ALL COUNTRIES") %>%
    group_by(visa_class, date, year, month) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    mutate(fy = if_else(month >= 10, year + 1L, year))

niv_class_feb = niv_by_class_national %>%
    filter(fy %in% c(2024, 2026), month == 2) %>%
    group_by(visa_class, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, values_fill = 0, names_prefix = "visas_") %>%
    mutate(change = visas_2026 - visas_2024,
        pct_change = change / visas_2024) %>%
    arrange(desc(visas_2024))

print(niv_class_feb, n = Inf)

write_csv(niv_class_feb, "results/niv_class_breakdown_feb_2024_2026.csv")
