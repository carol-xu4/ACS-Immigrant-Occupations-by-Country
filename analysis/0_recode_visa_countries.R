## Recode IV and NIV country names to match ACS birthplace categories ------------------------------
## Run this once, before any downstream analysis (FINAL2.R etc.)

pacman::p_load(tidyverse)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read ACS and raw visa data ----------------------------------------------------------------------
acs = read_rds("data/output/acs.RDS")

iv_totals = read_csv("data/output/iv_issuances_monthly_totals.csv")
niv_totals = read_csv("data/output/niv_issuances_monthly_totals.csv")

acs_bpl = acs %>%
    filter(country != "United States") %>%
    distinct(country)

## Shared recode function: handles both IV and NIV raw country strings -----------------------------
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
        str_detect(x, "^Hong Kong") |
            x == "British National Overseas (Hong Kong) Passport" ~ "Hong Kong",  # Hong Kong-BNO (IV) / BNO passport (NIV)
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
        x %in% c("Andorra", "Estonia", "Faroe Islands", "Gibraltar", "Liechtenstein",
            "Luxembourg", "Malta", "Monaco", "San Marino", "Slovenia",
            "Vatican City")                                   ~ "Other Europe",
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
        x %in% c("No Nationality", "Other",
            "Non-Nationality Based Issuances", "Unknown")    ~ NA_character_,
        TRUE ~ x)}

## appendix table: every raw country/territory from both datasets mapped to its ACS category -------
country_crosswalk = bind_rows(
        iv_totals %>% filter(country != "ALL COUNTRIES") %>% distinct(country) %>% mutate(source = "IV"),
        niv_totals %>% filter(country != "ALL COUNTRIES") %>% distinct(country) %>% mutate(source = "NIV")) %>%
    distinct(country, source) %>%
    mutate(acs_category = recode_visa_country(country),
        acs_category = if_else(is.na(acs_category), "Not matched (excluded)", acs_category)) %>%
    rename(visa_country = country) %>%
    arrange(acs_category, visa_country, source)

write_csv(country_crosswalk, "results/country_crosswalk.csv")

## checks: visa countries vs. ACS countries, for each dataset separately ---------------------------
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

## apply recoding and write out --------------------------------------------------------------------
iv_totals = iv_totals %>%
    mutate(country = if_else(country == "ALL COUNTRIES", country, recode_visa_country(country)))

niv_totals = niv_totals %>%
    mutate(country = if_else(country == "ALL COUNTRIES", country, recode_visa_country(country)))

write_csv(iv_totals, "data/output/iv_issuances_monthly_totals_recoded.csv")
write_csv(niv_totals, "data/output/niv_issuances_monthly_totals_recoded.csv")
