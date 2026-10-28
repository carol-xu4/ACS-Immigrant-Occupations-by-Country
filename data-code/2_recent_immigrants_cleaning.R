## Preliminaries -------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read 2021-2024 ONE-YEAR SAMPLES
ddi_acs = read_ipums_ddi("data/input/usa_00045.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
  rename_with(tolower) %>%
  select(year, perwt, hhwt, 
    bpl, bpld, citizen, yrnatur, yrimmig, age, sex,
    empstat, empstatd, labforce, occ, occsoc, ind, indnaics,
    educ, educd, inctot, incwage, hhincome)

# country recodes --------------------------------------------------------------------------------
acs = acs %>%
  mutate(
    bpld_lbl = as.character(as_factor(bpld)),
    bpld = as.integer(zap_labels(bpld)),
    country = case_when(
      # United States: states, DC, territories 
      bpld < 15000 | bpld == 71047 ~ "United States",   # incl. Northern Mariana Islands

      # North America / Atlantic
      bpld == 15000 ~ "Canada",
      bpld == 16010 ~ "Bermuda",
      bpld == 16020 ~ "Cape Verde",

      # Mexico & Central America
      bpld == 20000 ~ "Mexico",
      bpld == 21010 ~ "Belize",
      bpld == 21020 ~ "Costa Rica",
      bpld == 21030 ~ "El Salvador",
      bpld == 21040 ~ "Guatemala",
      bpld == 21050 ~ "Honduras",
      bpld == 21060 ~ "Nicaragua",
      bpld == 21070 ~ "Panama",

      # Caribbean
      bpld == 25000 ~ "Cuba",
      bpld == 26010 ~ "Dominican Republic",
      bpld == 26020 ~ "Haiti",
      bpld == 26030 ~ "Jamaica",
      bpld == 26042 ~ "Antigua and Barbuda",
      bpld == 26043 ~ "Bahamas",
      bpld == 26044 ~ "Barbados",
      bpld == 26054 ~ "Dominica",
      bpld == 26055 ~ "Grenada",
      bpld == 26057 ~ "St. Kitts and Nevis",
      bpld == 26058 ~ "St. Lucia",
      bpld == 26059 ~ "St. Vincent and the Grenadines",
      bpld == 26060 ~ "Trinidad and Tobago",
      bpld %in% c(26091, 26094) ~ "Other Caribbean",
      bpld == 29900 ~ "Other Americas",

      # South America
      bpld == 30005 ~ "Argentina",
      bpld == 30010 ~ "Bolivia",
      bpld == 30015 ~ "Brazil",
      bpld == 30020 ~ "Chile",
      bpld == 30025 ~ "Colombia",
      bpld == 30030 ~ "Ecuador",
      bpld == 30040 ~ "Guyana",
      bpld == 30045 ~ "Paraguay",
      bpld == 30050 ~ "Peru",
      bpld == 30060 ~ "Uruguay",
      bpld == 30065 ~ "Venezuela",
      bpld == 30090 ~ "Other South America",

      # Northern & Western Europe
      bpld == 40000 ~ "Denmark",
      bpld == 40100 ~ "Finland",
      bpld == 40200 ~ "Iceland",
      bpld == 40400 ~ "Norway",
      bpld == 40500 ~ "Sweden",
      bpld %in% c(41000, 41100, 41300, 41410) ~ "United Kingdom",
      bpld == 41400 ~ "Ireland",
      bpld == 42000 ~ "Belgium",
      bpld == 42100 ~ "France",
      bpld == 42500 ~ "Netherlands",
      bpld == 42600 ~ "Switzerland",

      # Southern Europe
      bpld == 43000 ~ "Albania",
      bpld == 43300 ~ "Greece",
      bpld == 43330 ~ "North Macedonia",
      bpld == 43400 ~ "Italy",
      bpld %in% c(43600, 43610) ~ "Portugal",
      bpld == 43800 ~ "Spain",

      # Central & Eastern Europe
      bpld == 45000 ~ "Austria",
      bpld == 45100 ~ "Bulgaria",
      bpld == 45200 ~ "Czechoslovakia",
      bpld == 45212 ~ "Slovakia",
      bpld == 45213 ~ "Czech Republic",
      bpld == 45300 ~ "Germany",
      bpld == 45400 ~ "Hungary",
      bpld == 45500 ~ "Poland",
      bpld == 45600 ~ "Romania",
      bpld == 45700 ~ "Yugoslavia",
      bpld == 45710 ~ "Croatia",
      bpld == 45720 ~ "Montenegro",
      bpld == 45730 ~ "Serbia",
      bpld == 45740 ~ "Bosnia and Herzegovina",
      bpld == 45790 ~ "Kosovo",

      # Former USSR
      bpld == 46100 ~ "Latvia",
      bpld == 46200 ~ "Lithuania",
      bpld == 46500 ~ "Russia",            # IPUMS: "Other USSR/'Russia'"
      bpld == 46510 ~ "Belarus",
      bpld == 46520 ~ "Moldova",
      bpld == 46530 ~ "Ukraine",
      bpld == 46540 ~ "Armenia",
      bpld == 46541 ~ "Azerbaijan",
      bpld == 46542 ~ "Georgia",
      bpld == 46543 ~ "Kazakhstan",
      bpld == 46544 ~ "Kyrgyzstan",
      bpld == 46547 ~ "Uzbekistan",
      bpld == 46590 ~ "USSR, n.s.",
      bpld == 49900 ~ "Other Europe",

      # East Asia
      bpld == 50000 ~ "China",
      bpld == 50010 ~ "Hong Kong",
      bpld == 50030 ~ "Mongolia",
      bpld == 50040 ~ "Taiwan",
      bpld == 50100 ~ "Japan",
      bpld == 50200 ~ "Korea",

      # Southeast Asia
      bpld == 51100 ~ "Cambodia",
      bpld == 51200 ~ "Indonesia",
      bpld == 51300 ~ "Laos",
      bpld == 51400 ~ "Malaysia",
      bpld == 51500 ~ "Philippines",
      bpld == 51600 ~ "Singapore",
      bpld == 51700 ~ "Thailand",
      bpld == 51800 ~ "Vietnam",

      # South & Southwest Asia
      bpld == 52000 ~ "Afghanistan",
      bpld == 52100 ~ "India",
      bpld == 52110 ~ "Bangladesh",
      bpld == 52120 ~ "Bhutan",
      bpld == 52130 ~ "Burma",
      bpld == 52140 ~ "Pakistan",
      bpld == 52150 ~ "Sri Lanka",
      bpld == 52200 ~ "Iran",
      bpld == 52400 ~ "Nepal",

      # Middle East
      bpld == 53200 ~ "Iraq",
      bpld == 53400 ~ "Israel/Palestine",
      bpld == 53500 ~ "Jordan",
      bpld == 53600 ~ "Kuwait",
      bpld == 53700 ~ "Lebanon",
      bpld == 54000 ~ "Saudi Arabia",
      bpld == 54100 ~ "Syria",
      bpld == 54200 ~ "Turkey",
      bpld == 54300 ~ "United Arab Emirates",
      bpld == 54400 ~ "Yemen",
      bpld == 59900 ~ "Other Asia",

      # North Africa
      bpld == 60011 ~ "Algeria",
      bpld == 60012 ~ "Egypt",
      bpld == 60013 ~ "Libya",
      bpld == 60014 ~ "Morocco",
      bpld == 60015 ~ "Sudan",
      bpld == 60016 ~ "Tunisia",
      bpld == 60019 ~ "Other North Africa",   # incl. Tunisia

      # West Africa
      bpld == 60022 ~ "Gambia",
      bpld == 60023 ~ "Ghana",
      bpld == 60024 ~ "Guinea",
      bpld == 60026 ~ "Cote d'Ivoire",
      bpld == 60027 ~ "Liberia",
      bpld == 60031 ~ "Nigeria",
      bpld == 60032 ~ "Senegal",
      bpld == 60033 ~ "Sierra Leone",
      bpld == 60034 ~ "Togo",
      bpld == 60038 ~ "Other West Africa",    # incl. Benin, Burkina Faso, Mali, Mauritania, Niger

      # East Africa
      bpld == 60044 ~ "Ethiopia",
      bpld == 60045 ~ "Kenya",
      bpld == 60051 ~ "Rwanda",
      bpld == 60053 ~ "Somalia",
      bpld == 60054 ~ "Tanzania",
      bpld == 60055 ~ "Uganda",
      bpld == 60056 ~ "Zambia",
      bpld == 60057 ~ "Zimbabwe",
      bpld == 60065 ~ "Eritrea",
      bpld == 60066 ~ "South Sudan",
      bpld == 60064 ~ "Other East Africa",    # incl. Burundi, Malawi, South Sudan(?)

      # Central & Southern Africa
      bpld == 60072 ~ "Cameroon",
      bpld == 60075 ~ "Republic of the Congo",
      bpld == 60079 ~ "DR Congo",             # IPUMS label: "Zaire"
      bpld == 60094 ~ "South Africa",
      bpld == 60099 ~ "Other Africa",         # incl. Chad, Eq. Guinea, Gabon, Angola

      # Oceania
      bpld == 70010 ~ "Australia",
      bpld == 70020 ~ "New Zealand",
      bpld == 71015 ~ "Fiji",
      bpld == 71023 ~ "Tonga",
      bpld == 71041 ~ "Marshall Islands",
      bpld == 71042 ~ "Micronesia",

      # Other
      bpld == 95000 ~ "Other, n.e.c.",
      TRUE ~ NA_character_))

acs %>% filter(is.na(country)) %>% dplyr::count(bpld, bpld_lbl)

country_check = acs %>%
  filter(bpld >= 15000) %>%
  group_by(bpld, bpld_lbl, country) %>%
  summarise(n   = n(),            # unweighted
            pop = sum(perwt),     # weighted
            .groups = "drop") %>%
  arrange(bpld)

print(country_check, n = Inf)

country_check %>% filter(str_detect(country, "^Other") | is.na(country))

# write final dataset
write_rds(acs, "data/output/acs_2021_2024.RDS")
