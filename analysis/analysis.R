## Preliminaries -------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS -----------------------------------------------------------------------------
acs = read_rds("data/output/acs.RDS")

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

# restricted to ages 18-64
bpl_table_18_64 = acs %>%
  filter(country != "United States", citizen %in% c(2, 3),
         age >= 18, age <= 64) %>%
  mutate(employed = empstat == 1) %>%
  group_by(country) %>%
  summarise(n           = n(),
            pop         = sum(perwt),
            n_workers   = sum(employed),
            workers     = sum(perwt[employed]),
            .groups = "drop") %>%
  mutate(pct_workers = workers / pop) %>%
  arrange(desc(pop))

View(bpl_table_18_64)

# occupations by US Standard Occupational Classification (SOC) system -- 23 major groups
top_n_occ = 3

top_occ = acs %>%
  filter(country != "United States", citizen %in% c(2, 3),
         age >= 18, age <= 64, empstat == 1) %>%
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

bpl_jobs = bpl_table_18_64 %>%
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
















# census country groupings