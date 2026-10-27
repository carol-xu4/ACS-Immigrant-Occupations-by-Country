## Preliminaries -------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read 2024 ACS 5-year sample
ddi_acs = read_ipums_ddi("data/input/usa_00044.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
  rename_with(tolower) %>%
  select(year, hhwt, statefip, gq, pernum, perwt, serial,
  relate, related, sex, age, marst, race, raced, hispan, hispand, bpl, bpld,
  citizen, yrimmig, language, languaged, speakeng, classwkr, classwkrd,
  hinscaid, hinscare, incss, incwelfr, incsupp, vetstat, occ2010,
  momloc, momloc2, poploc, poploc2)

# Residual Method ----------------------------------------------------------
acs = acs %>%
  mutate(immigrant = ifelse(citizen == 2 |
                            citizen == 3, 1, 0)) %>%
  mutate(immig_status = ifelse(bpl < 150 | immigrant == 0 | citizen == 1, 1, NA)) %>%
  mutate(foreign_born = ifelse(bpl >= 150, 1, 0)) %>%
  mutate(immig_status = case_when(
    immigrant == 1 & citizen == 2 ~ 2,
    immigrant == 1 & yrimmig < 1982 ~ 2,
    immigrant == 1 & incss > 0 & incss < 99999 ~ 2,
    immigrant == 1 & incsupp > 0 & incsupp < 99999 ~ 2,
    immigrant == 1 & incwelfr > 0 & incwelfr < 99999 ~ 2,
    immigrant == 1 & hinscare == 2 ~ 2,
    immigrant == 1 & hinscaid == 2 &
      # california — phased Medi-Cal expansion
      !(statefip == 6 & ((year >= 2016 & age <= 18) | (year >= 2020 & age <= 25) | (year >= 2022 & age >= 50) | (year >= 2024 & age >= 26 & age <= 49))) &
      # illinois — All Kids children 2006, HBIS seniors 2020, HBIA adults 42-64 2022
      !(statefip == 17 & ((year >= 2006 & age <= 18) | (year >= 2020 & age >= 65) | (year >= 2022 & age >= 42 & age <= 64))) &
      # washington — Apple Health for Kids children 2007 only (adult expansion is marketplace)
      !(statefip == 53 & year >= 2007 & age <= 18) &
      # new york — Child Health Plus children 2014, adults 65+ 2024
      !(statefip == 36 & ((year >= 2014 & age <= 18) | (year >= 2024 & age >= 65))) &
      # oregon — Cover All Kids children 2018, phase 1 ages 19-25/55+ 2022, full expansion 2023
      !(statefip == 41 & ((year >= 2018 & age <= 18) | (year == 2022 & (age <= 25 | age >= 55)) | (year >= 2023))) &
      # new jersey — children 2018
      !(statefip == 34 & year >= 2018 & age <= 18) &
      # connecticut — children under 15, 2010
      !(statefip == 9  & year >= 2010 & age <= 14) &
      # rhode island — children 2022
      !(statefip == 44 & year >= 2022 & age <= 18) &
      # maine — children 2022
      !(statefip == 23 & year >= 2022 & age <= 18) &
      # vermont — children 2022
      !(statefip == 50 & year >= 2022 & age <= 18) ~ 2,
    immigrant == 1 & vetstat == 2 ~ 2,
    immigrant == 1 & classwkrd %in% c(25, 26, 27, 28) ~ 2,
    # occupations requiring licensing/lawful status, per Pew (2018) methodology -----
    immigrant == 1 & occ2010 %in% c(
      2100, 3850, 3060, 3255,                                        # lawyers, police, physicians, RNs
      3000, 3010, 3030, 3040, 3050, 3110, 3120, 3140, 3150,
      3160, 3200, 3210, 3220, 3230, 3245, 3250, 3256, 3258, 3260,
      3310, 3500,                                                     # health care professionals
      2040, 2050, 2060,                                               # religious workers
      2600, 2630, 2700, 2710, 2720, 2740, 2750, 2760,                 # athletes/artists/entertainers
      9800, 9810, 9820, 9830                                          # current miligary
    ) ~ 2,
    # inferred from Pew's named visa categories (visiting scholars, high-tech workers) --
    immigrant == 1 & occ2010 %in% c(
      2200,                                                           # visiting scholars -> postsecondary teachers
      1005, 1006, 1007, 1010, 1020, 1030, 1050, 1060, 1105, 1106, 1107,  # high-tech: computer occupations
      1320, 1340, 1350, 1360, 1400, 1410, 1420, 1430, 1440, 1450, 1460, 1520, 1530  # high-tech: engineers
    ) ~ 2,
    immigrant == 1 & bpld == 25000 & yrimmig < 2017 ~ 2,
    TRUE ~ immig_status
  )) %>%
  mutate(legal = ifelse(
    immig_status == 1 | immig_status == 2, 1, 0
  )) %>%
  mutate(good = if_else(relate == 2 & immigrant == 1, 1, NA_real_)) %>%
  mutate(legal = ifelse(is.na(legal), 0, legal)) %>%
  group_by(year, serial) %>%
  mutate(slegal = mean(good * legal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(immig_status = ifelse((slegal > 0 | is.na(slegal)) & immigrant == 1 & relate == 1 & marst == 1,
                               2,
                               immig_status)) %>%
  mutate(good1 = ifelse(relate == 1 & immigrant == 1, 1, NA_real_)) %>%
  group_by(year, serial) %>%
  mutate(hlegal = mean(good1 * legal, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(immig_status =
           ifelse((hlegal > 0 | is.na(hlegal)) &
                    immigrant == 1 & ((relate == 2 & marst == 1) | relate == 3 | relate == 9),
                  2,
                  immig_status
           )) %>%
  mutate(immig_status = ifelse(is.na(immig_status), 3, immig_status)) %>%
  mutate(undercount =
           ifelse(immig_status == 3,
                  1 + (0.13) * (0.925) ^ (year - yrimmig), 0)) %>%
  mutate(perwt = ifelse(undercount > 0, perwt * undercount, perwt)) %>%
  mutate(immig_status = case_when(
    immig_status == 1 ~ "Native-born citizens",
    immig_status == 2 ~ "Legal immigrants",
    immig_status == 3 ~ "Illegal immigrants",
  )) %>%
  mutate(immig_status = factor(immig_status,
                               levels = c("Native-born citizens",
                                          "Legal immigrants",
                                          "Illegal immigrants")))

# Age distribution of speakeng == 0 (NA)
acs %>%
  filter(speakeng == 0) %>%
  summarise(n       = n(),
            min_age = min(age),
            max_age = max(age),
            n_age5plus = sum(age >= 5))

# english speaking ability by immig status
eng = acs %>%
  mutate(english = case_when(
    speakeng == 3 ~ "Only English",
    speakeng == 4 ~ "Very well",
    speakeng == 5 ~ "Well",
    speakeng == 6 ~ "Not well",
    speakeng == 1 ~ "Not at all",
    speakeng == 0 ~ "N/A (under 5)",
    TRUE          ~ "Missing"
  ),
  english = factor(english, levels = c("Only English", "Very well", "Well",
                                       "Not well", "Not at all",
                                       "N/A (under 5)", "Missing")))

eng_tab = eng %>%
  group_by(immig_status, english) %>%
  summarise(n = n(),
            pop   = sum(perwt),
            .groups = "drop") %>%
  group_by(immig_status) %>%
  mutate(share = pop / sum(pop)) %>%
  ungroup()

eng_tab

eng_wide = eng_tab %>%
  select(english, immig_status, share) %>%
  pivot_wider(names_from = immig_status, values_from = share, values_fill = 0)

eng_wide

# children of legal immigrants: age 5-17 only since speakeng universe excludes <5
eng = eng %>%
  mutate(across(c(pernum, momloc, momloc2, poploc, poploc2),
                ~ as.integer(haven::zap_labels(.x))))

# Lookup table: every person's status, keyed by household + person number
parents = eng %>%
  transmute(year, serial, ppernum = pernum, par_status = immig_status)

kids = eng %>%
  filter(age >= 5, age <= 17) %>%
  mutate(kid_nativity = ifelse(bpl < 150, "US-born child", "Foreign-born child"))

# link children to parents
kid_par = kids %>%
  select(year, serial, pernum, momloc, momloc2, poploc, poploc2) %>%
  pivot_longer(c(momloc, momloc2, poploc, poploc2),
               names_to = "parent", values_to = "ppernum") %>%
  filter(ppernum > 0) %>%
  left_join(parents, by = c("year", "serial", "ppernum")) %>%
  group_by(year, serial, pernum) %>%
  summarise(n_parents   = n(),
            any_legal   = any(par_status == "Legal immigrants"),
            any_illegal = any(par_status == "Illegal immigrants"),
            .groups = "drop")

kids = kids %>%
  left_join(kid_par, by = c("year", "serial", "pernum")) %>%
  mutate(parent_type = case_when(
    is.na(n_parents)           ~ "No parent in household",
    any_illegal                ~ "At least one illegal immigrant parent",
    any_legal                  ~ "Legal immigrant parent(s)",
    TRUE                       ~ "Native-born parent(s) only"))

kids %>% count(parent_type, kid_nativity)

# english ability by parent type
kids_eng = kids %>%
  group_by(parent_type) %>%
  summarise(
    pop          = sum(perwt),
    NA           = sum(perwt[speakeng == 0]) / pop,   # 0 = NA (under age 5)
    not_at_all   = sum(perwt[speakeng == 1]) / pop,   # 1 = Does not speak English
    only_english = sum(perwt[speakeng == 3]) / pop,   # 3 = Yes, speaks only English
    very_well    = sum(perwt[speakeng == 4]) / pop,   # 4 = Yes, speaks very well
    well         = sum(perwt[speakeng == 5]) / pop,   # 5 = Yes, speaks well
    not_well     = sum(perwt[speakeng == 6]) / pop,   # 6 = Yes, but not well
    n            = n()) %>%
  mutate(check = not_at_all + only_english + very_well + well + not_well)

print(kids_eng, n = Inf, width = Inf)

# kids of legal immigrants only, by their own birthplace
legal_kids_eng = kids %>%
  filter(any_legal %in% TRUE) %>%
  group_by(kid_nativity, english) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop") %>%
  group_by(kid_nativity) %>%
  mutate(share = pop / sum(pop)) %>%
  ungroup()

print(legal_kids_eng)
