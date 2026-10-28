## Preliminaries---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read cleaned ACS ONE YEAR SAMPLES
acs = read_rds("data/output/acs_2021_2024.RDS")

occ_labels = read_csv("data/output/occ2018_labels.csv")

# recent immigrants: [YEAR] - [YRIMMIG] = 1
n_years = n_distinct(acs$year)   # stacked 1-year samples: divide weights to get an average year

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


recent = acs %>%
    mutate(year = as.integer(zap_labels(year)),
        yrimmig = as.integer(zap_labels(yrimmig)),
        wt = perwt / n_years) %>%
    filter(country != "United States", citizen %in% c(2, 3),
        year - yrimmig == 1)

# region totals: recent arrivals and how many work
recent_region = recent %>%
    group_by(bpl_region) %>%
    summarise(n = n(),
        pop = sum(wt),
        n_workers = sum(empstat == 1),
        workers = sum(wt[empstat == 1]),
        .groups = "drop") %>%
    mutate(workers_per_immig = workers / pop) %>%
    arrange(desc(pop))

# occupation groups by region (employed recent arrivals)
recent_region_occ = recent %>%
    filter(empstat == 1) %>%
    mutate(occ_group = occ_group_fn(as.integer(zap_labels(occ)))) %>%
    group_by(bpl_region, occ_group) %>%
    summarise(occ_n = n(),
        occ_workers = sum(wt),
        .groups = "drop_last") %>%
    mutate(occ_share = occ_workers / sum(occ_workers)) %>%
    arrange(desc(occ_workers), .by_group = TRUE) %>%
    ungroup()

# wide: one row per region, one column per occupation group (shares)
recent_region_occ_wide = recent_region_occ %>%
    select(bpl_region, occ_group, occ_share) %>%
    pivot_wider(names_from = occ_group, values_from = occ_share, values_fill = 0) %>%
    left_join(recent_region %>% select(bpl_region, n_workers, workers), by = "bpl_region") %>%
    relocate(n_workers, workers, .after = bpl_region) %>%
    arrange(desc(workers))

write_csv(recent_region, "results/recent_immigrants_region_totals.csv")
write_csv(recent_region_occ, "results/recent_immigrants_region_occ_groups.csv")
write_csv(recent_region_occ_wide, "results/recent_immigrants_region_occ_groups_wide.csv")
