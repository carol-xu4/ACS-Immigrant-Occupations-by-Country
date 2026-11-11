# check refugee data pull, and recode country names
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

refugees = read_csv("data/output/refugee_arrivals_by_nationality_fy2021_fy2026.csv")

# total refugees by year and unique nationalities
refugees %>%
    group_by(fy) %>%
    summarise(n_countries = n_distinct(nationality),
        total_refugees = sum(arrivals),
        .groups = "drop")

unique(refugees$nationality)

# recoding
# pre-clean known duplicate/variant spellings before recoding
refugees = refugees %>%
    mutate(nationality = str_trim(nationality)) %>%
    mutate(nationality = case_when(
        nationality %in% c("Dem. Rep. Congo", "Democratic Republic of the Congo") ~ "Congo, Democratic Republic of the",
        nationality == "Congo" ~ "Congo, Republic of the",
        nationality == "Republic of South Sudan" ~ "South Sudan",
        nationality == "South Korea" ~ "Korea",
        nationality == "Korea, North" ~ "Korea",
        nationality == "Burkina Faso (UVolta)" ~ "Burkina Faso",
        nationality == "Palestine" ~ "Palestinian Authority Travel Document",
        nationality == "Sri Lanka (Ceylon)" ~ "Sri Lanka",
        nationality == "Ivory Coast" ~ "Cote d'Ivoire",
        TRUE ~ nationality)) %>%
    group_by(fy, nationality) %>%
    summarise(arrivals = sum(arrivals), .groups = "drop")

# apply the existing recode_visa_country()
refugees_recoded = refugees %>%
    mutate(country = recode_visa_country(nationality))

refugees_recoded %>%
    filter(nationality == "Ivory Coast") %>%
    distinct(nationality) %>%
    mutate(nchar = nchar(nationality)) 

# diagnostic: anything recode_visa_country explicitly doesn't recognize (NA)?
refugees_recoded %>%
    filter(is.na(country)) %>%
    distinct(nationality)

# diagnostic: anything that passed through UNCHANGED -- confirm these are real ACS country
# names, not just strings recode_visa_country didn't recognize and let fall through as-is
refugees_recoded %>%
    filter(nationality == country) %>%
    distinct(nationality) %>%
    anti_join(acs_bpl, by = c("nationality" = "country"))

refugees_recoded %>%
    filter(str_detect(country, "^Other")) %>%
    distinct(nationality, country) %>%
    arrange(country)

# checks
refugees_recoded %>% filter(is.na(country)) %>% distinct(nationality)

acs_bpl %>% filter(str_detect(country, regex("central african|south sudan|burundi", ignore_case = TRUE)))


# write final csv
write_csv(refugees_recoded, "data/output/refugee_arrivals_by_country_fy2021_fy2026_recoded.csv")