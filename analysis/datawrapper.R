# converting FINAL_ANALYSIS outputs to datawrapper format

# fig 1 -- total legal entries by fiscal year
fig1 = master_fy_totals %>%
    rename(
        `Fiscal Year` = fy,
        `Immigrant Visas` = IV,
        `Nonimmigrant Visas (long-term)` = NIV,
        `Refugees` = Refugees,
        `Asylees` = Asylees)

write_csv(fig1, "results/datawrapper/fig1.csv")

# Fig. 2 -- Monthly legal entries, FY2021-FY2026 
monthly_base = total_monthly %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
    select(month_date, value, series)

last_actual = monthly_base %>%
    filter(series == "Actual") %>%
    filter(month_date == max(month_date)) %>%
    mutate(series = "Projected")

fig2 = bind_rows(monthly_base, last_actual) %>%
    pivot_wider(names_from = series, values_from = value) %>%
    arrange(month_date) %>%
    rename(Month = month_date)

write_csv(fig2, "results/datawrapper/fig2.csv")

# table 1 -- total IV and NIV issuances
table1  = visa_fy_totals_updated %>%
    rename(
        `Fiscal Year` = fy,
        `Immigrant Visas` = IV,
        `Nonimmigrant Visas (long-term)` = NIV,
        `Total` = Total)

write_csv(table1, "results/datawrapper/table1.csv")

# fig.3 -- IV and NIV issuances, including projected
visa_monthly_wide = visas %>%
    group_by(visa_type, year, month) %>%
    summarise(value = sum(issuances), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("%d-%02d-01", year, month))) %>%
    select(visa_type, month_date, value) %>%
    pivot_wider(names_from = visa_type, values_from = value)

visa_projected_wide = visa_marsep_projected %>%
    group_by(visa_type, month) %>%
    summarise(value = sum(visas_2026_projected), .groups = "drop") %>%
    mutate(month_date = as.Date(sprintf("2026-%02d-01", month))) %>%
    select(visa_type, month_date, value) %>%
    pivot_wider(names_from = visa_type, values_from = value, names_prefix = "Projected ")

last_actual_row = visa_monthly_wide %>%
    filter(month_date == max(month_date)) %>%
    rename(`Projected IV` = IV, `Projected NIV` = NIV)

fig3 = visa_monthly_wide %>%
    full_join(bind_rows(last_actual_row, visa_projected_wide), by = "month_date") %>%
    arrange(month_date) %>%
    rename(Month = month_date)

write_csv(fig3, "results/datawrapper/fig3.csv")

# fig.4 -- refugees and asylees monthly
refugees_monthly_table = refugees %>%
    group_by(year, month) %>%
    summarise(Refugees = sum(arrivals, na.rm = TRUE), .groups = "drop")

asylees_monthly_table = asylees %>%
    group_by(year, month) %>%
    summarise(Asylees = sum(encounters, na.rm = TRUE), .groups = "drop")

refugee_asylee_monthly_table = refugees_monthly_table %>%
    full_join(asylees_monthly_table, by = c("year", "month")) %>%
    mutate(Month = as.Date(sprintf("%d-%02d-01", year, month))) %>%
    arrange(Month) %>%
    select(Month, Refugees, Asylees)

write_csv(refugee_asylee_monthly_table, "results/datawrapper/fig4.csv")

# fig.5 -- missing workers by occupation
# add bins for color coding by immigrant share of workforce
occ_chart = cumulative_missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    arrange(missing_workers) %>%
    left_join(occ_group_totals %>% select(occ_group, pct_immigrant), by = "occ_group") %>%
    mutate(missing_workers = abs(missing_workers))

occ_total_pct_immigrant = acs %>%
    filter(empstat == 1) %>%
    mutate(immigrant = citizen %in% c(2, 3)) %>%
    summarise(pct_immigrant = sum(perwt[immigrant]) / sum(perwt) * 100) %>%
    pull(pct_immigrant)

occ_chart = cumulative_missing_workers_by_occ %>%
    filter(!is.na(occ_group)) %>%
    arrange(missing_workers) %>%
    left_join(occ_group_totals %>% select(occ_group, pct_immigrant), by = "occ_group") %>%
    mutate(missing_workers = round(abs(missing_workers)))

breaks = seq(min(occ_chart$pct_immigrant, na.rm = TRUE), max(occ_chart$pct_immigrant, na.rm = TRUE), length.out = 6)
bin_labels = paste0(round(breaks[-6]), "-", round(breaks[-1]), "%")

occ_chart = occ_chart %>%
    mutate(`Immigrant Share of Occupation` = cut(pct_immigrant, breaks = breaks, labels = bin_labels, include.lowest = TRUE)) %>%
    rename(`Occupation Group` = occ_group, `Missing Workers` = missing_workers, `% Immigrant` = pct_immigrant)

write_csv(occ_chart, "results/datawrapper/fig5.csv")

# fig.6 -- skilled workers / bachelor's degrees
degree_category_fn = function(educd) {
    case_when(
        educd < 62 ~ "Less than high school",
        educd %in% c(62, 63, 64) ~ "High school diploma or equivalent",
        educd >= 65 & educd <= 100 ~ "Some college or associate's degree",
        educd == 101 ~ "Bachelor's degree",
        educd > 101 ~ "Graduate or professional degree",
        TRUE ~ NA_character_)}

us_population_degrees = acs %>%
    filter(age >= 25) %>%
    mutate(degree = degree_category_fn(educd)) %>%
    group_by(degree) %>%
    summarise(pop = sum(perwt), .groups = "drop") %>%
    mutate(pct = pop / sum(pop) * 100, group = "US Population")

recent_immigrants_degrees = acs %>%
    filter(citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS, age >= 25) %>%
    mutate(degree = degree_category_fn(educd)) %>%
    group_by(degree) %>%
    summarise(pop = sum(perwt), .groups = "drop") %>%
    mutate(pct = pop / sum(pop) * 100, group = "Recent Immigrants")

bpl_degree_share = acs %>%
    filter(country != "United States", citizen %in% c(2, 3), yrimmig %in% RECENT_YEARS, age >= 25) %>%
    mutate(degree = degree_category_fn(educd)) %>%
    group_by(country) %>%
    mutate(pop = sum(perwt)) %>%
    group_by(country, pop, degree) %>%
    summarise(degree_workers = sum(perwt), .groups = "drop") %>%
    mutate(degree_share = degree_workers / pop)

# all-ages/25+ mismatch
missing_25plus_by_country = cumulative_missing %>%
    filter(!is.na(country)) %>%
    left_join(bpl_bachelors_share %>% select(country, pop, adults), by = "country") %>%
    mutate(missing_25plus = missing * (adults / pop)) %>%
    group_by(country) %>%
    summarise(missing_25plus = sum(missing_25plus, na.rm = TRUE), .groups = "drop")

missing_workers_degrees = missing_25plus_by_country %>%
    inner_join(bpl_degree_share %>% select(country, degree, degree_share), by = "country") %>%
    mutate(missing_degree = missing_25plus * degree_share) %>%
    group_by(degree) %>%
    summarise(missing_degree = sum(missing_degree, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct = missing_degree / sum(missing_degree) * 100, group = "Missing Workers")

DEGREE_LEVELS = c("Graduate or professional degree", "Bachelor's degree",
    "Some college or associate's degree", "High school diploma or equivalent",
    "Less than high school")

degree_chart = bind_rows(
        us_population_degrees %>% select(group, degree, pct),
        recent_immigrants_degrees %>% select(group, degree, pct),
        missing_workers_degrees %>% select(group, degree, pct)) %>%
    filter(!is.na(degree)) %>%
    mutate(degree = factor(degree, levels = DEGREE_LEVELS)) %>%
    pivot_wider(names_from = degree, values_from = pct, names_sort = TRUE) %>%
    rename(Group = group)

write_csv(degree_chart, "results/datawrapper/fig6.csv")

missing_workers_degrees %>%
    select(degree, missing_degree, pct) %>%
    arrange(factor(degree, levels = DEGREE_LEVELS))

# healthcare workers?
## missing healthcare workers by category, Jan 2025 - Sep 2026 cumulative ---------------------------
missing_healthcare_cumulative = cumulative_missing_by_country %>%
    select(country, missing) %>%
    inner_join(bpl_health_detail_share %>% select(country, health_category, category_share), by = "country") %>%
    mutate(missing_workers = missing * category_share) %>%
    group_by(health_category) %>%
    summarise(missing_workers = sum(missing_workers, na.rm = TRUE), .groups = "drop") %>%
    mutate(missing_workers = round(abs(missing_workers))) %>%
    arrange(desc(missing_workers)) %>%
    rename(`Health Category` = health_category, `Missing Workers` = missing_workers)

health_category_fn = function(occ) {
    occ_int = as.integer(occ)
    occ_str = sprintf("%04d", occ_int)
    occ_group_val = occ_group_fn(occ_int)
    case_when(
        occ_str %in% nurse_codes ~ "Nursing occupations",
        occ_group_val == "Healthcare practitioners & technical" ~ "Healthcare practitioners",
        occ_group_val == "Healthcare support" ~ "Other health support workers",
        TRUE ~ NA_character_)}

current_health_workforce = acs %>%
    filter(empstat == 1) %>%
    mutate(health_category = health_category_fn(zap_labels(occ))) %>%
    filter(!is.na(health_category)) %>%
    group_by(health_category) %>%
    summarise(current_workers = sum(perwt), .groups = "drop")

health_bar_chart = current_health_workforce %>%
    left_join(
        missing_healthcare_cumulative %>% rename(health_category = `Health Category`, missing_workers = `Missing Workers`),
        by = "health_category") %>%
    rename(`Health Category` = health_category, `Current Workforce` = current_workers, `Missing Workers` = missing_workers)

write_csv(health_bar_chart, "results/datawrapper/healthcare_workers_stacked.csv")

## range plot: current healthcare workforce vs. what it would have been without the decline --------
health_range_chart = current_health_workforce %>%
    left_join(
        missing_healthcare_cumulative %>% rename(health_category = `Health Category`, missing_workers = `Missing Workers`),
        by = "health_category") %>%
    mutate(`Would-Have-Been Workforce` = current_workers + missing_workers) %>%
    rename(`Health Category` = health_category, `Current Workforce` = current_workers) %>%
    select(`Health Category`, `Current Workforce`, `Would-Have-Been Workforce`)

write_csv(health_range_chart, "results/datawrapper/healthcare_workers_range.csv")
