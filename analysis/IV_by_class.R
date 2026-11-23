if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## IV by visa class ----------------------------------------------------------------------------
iv_class = read_csv("data/output/iv_issuances_by_country_visa_class.csv") %>%
    filter(country != "ALL COUNTRIES") %>%
    mutate(country = recode_visa_country(country),
        fy = if_else(month >= 10, year + 1L, year))

## 1. Jan-Feb 2026 vs. Jan-Feb 2024, by visa class (actual) ------------------------------------
iv_class_janfeb = iv_class %>%
    filter(month %in% 1:2, year %in% c(2024, 2026)) %>%
    group_by(visa_class, year) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = issuances,
        names_prefix = "janfeb_", values_fill = 0) %>%
    mutate(change = janfeb_2026 - janfeb_2024,
        pct_change = if_else(janfeb_2024 == 0, NA_real_, change / janfeb_2024)) %>%
    arrange(change)

iv_class_janfeb_total = iv_class_janfeb %>%
    summarise(visa_class = "TOTAL", janfeb_2024 = sum(janfeb_2024), janfeb_2026 = sum(janfeb_2026)) %>%
    mutate(change = janfeb_2026 - janfeb_2024, pct_change = change / janfeb_2024)

print(bind_rows(iv_class_janfeb, iv_class_janfeb_total), n = Inf)

## 2. Projected FY2026 by visa class ---------------------------------------------------------
# same method as the main analysis: each country's own Feb 2024 -> Feb 2026 change (all IV classes),
# applied to that country's Mar-Sep 2024 issuances in each class, so class totals sum to the IV total
iv_country_feb = iv_class %>%
    filter(month == 2, fy %in% c(2024, 2026)) %>%
    group_by(country, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, names_prefix = "visas_", values_fill = 0) %>%
    semi_join(acs_bpl, by = "country") %>%
    filter(!(visas_2024 == 0 & visas_2026 > 0)) %>%            # drop countries that went from zero to >=1
    mutate(pct_change = if_else(visas_2024 == 0, -1, (visas_2026 - visas_2024) / visas_2024))

iv_class_marsep = iv_class %>%
    filter(fy == 2024, month %in% 3:9) %>%
    inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
    mutate(projected = issuances * (1 + pct_change)) %>%
    group_by(visa_class) %>%
    summarise(marsep_2024 = sum(issuances), marsep_2026_proj = sum(projected), .groups = "drop")

iv_class_octfeb = iv_class %>%
    filter(fy %in% c(2024, 2026), month %in% c(10, 11, 12, 1, 2)) %>%
    semi_join(iv_country_feb, by = "country") %>%
    group_by(visa_class, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, names_prefix = "octfeb_", values_fill = 0)

iv_class_fy2024 = iv_class %>%
    filter(fy == 2024) %>%
    semi_join(iv_country_feb, by = "country") %>%
    group_by(visa_class) %>%
    summarise(fy2024 = sum(issuances), .groups = "drop")

iv_class_projected = iv_class_fy2024 %>%
    left_join(iv_class_octfeb, by = "visa_class") %>%
    left_join(iv_class_marsep, by = "visa_class") %>%
    mutate(across(c(octfeb_2024, octfeb_2026, marsep_2024, marsep_2026_proj), ~ coalesce(.x, 0)),
        fy2026_proj = octfeb_2026 + marsep_2026_proj,
        change = fy2026_proj - fy2024,
        pct_change = if_else(fy2024 == 0, NA_real_, change / fy2024)) %>%
    arrange(change)

iv_class_projected_total = iv_class_projected %>%
    summarise(visa_class = "TOTAL", fy2024 = sum(fy2024), fy2026_proj = sum(fy2026_proj)) %>%
    mutate(change = fy2026_proj - fy2024, pct_change = change / fy2024)

print(bind_rows(iv_class_projected, iv_class_projected_total), n = Inf)


## Spouses and children of US citizens (immediate relatives, IV) ---------------------------------
# IR1/CR1 = spouses; IR2/CR2 = unmarried children under 21; IR3/IR4/IH3/IH4 = adopted children
usc_family_groups = tibble(
    visa_class = c("IR1", "CR1", "IR2", "CR2", "IR3", "IR4", "IH3", "IH4"),
    group = c("Spouses", "Spouses", "Children", "Children", "Children", "Children", "Children", "Children"))

# Jan-Feb 2026 vs. Jan-Feb 2024 (actual)
usc_family_janfeb = iv_class_janfeb %>%
    inner_join(usc_family_groups, by = "visa_class") %>%
    select(group, visa_class, janfeb_2024, janfeb_2026, change, pct_change)

usc_family_janfeb_groups = usc_family_janfeb %>%
    group_by(group) %>%
    summarise(janfeb_2024 = sum(janfeb_2024), janfeb_2026 = sum(janfeb_2026), .groups = "drop") %>%
    bind_rows(summarise(., group = "Total", across(c(janfeb_2024, janfeb_2026), sum))) %>%
    mutate(change = janfeb_2026 - janfeb_2024, pct_change = change / janfeb_2024)

print(usc_family_janfeb, n = Inf)
print(usc_family_janfeb_groups)

write_csv(usc_family_janfeb, "results/IV_classes/family.csv")

write_csv(usc_family_janfeb_groups, "results/IV_classes/family_groups.csv")

# projected FY2026 vs. FY2024
usc_family_projected = iv_class_projected %>%
    inner_join(usc_family_groups, by = "visa_class") %>%
    select(group, visa_class, fy2024, octfeb_2026, marsep_2026_proj, fy2026_proj, change, pct_change)

usc_family_projected_groups = usc_family_projected %>%
    group_by(group) %>%
    summarise(across(c(fy2024, octfeb_2026, marsep_2026_proj, fy2026_proj), sum), .groups = "drop") %>%
    bind_rows(summarise(., group = "Total", across(c(fy2024, octfeb_2026, marsep_2026_proj, fy2026_proj), sum))) %>%
    mutate(change = fy2026_proj - fy2024, pct_change = change / fy2024)

print(usc_family_projected, n = Inf)

print(usc_family_projected_groups)

write_csv(usc_family_projected, "results/IV_classes/family_projected.CSV")

write_csv(usc_family_projected_groups, "results/IV_classes/family_projected_groups.CSV")

# total family of us citizens in 2025 (actual) and projected 2026

## Spouses and children of US citizens: FY2025 vs. FY2026 projected ------------------------------
usc_family_groups = tibble(
    visa_class = c("IR1", "CR1", "IB1", "IW", "IR2", "CR2", "IB2", "IR3", "IR4", "IH3", "IH4"),
    group = c("Spouses", "Spouses", "Spouses", "Spouses",
        "Children", "Children", "Children", "Children", "Children", "Children", "Children"))

usc_family = iv_class %>%
    inner_join(usc_family_groups, by = "visa_class") %>%
    semi_join(iv_country_feb, by = "country")

# FY2025 actual
usc_fy2025 = usc_family %>%
    filter(fy == 2025) %>%
    group_by(group) %>%
    summarise(fy2025 = sum(issuances), .groups = "drop")

# FY2026: actual Oct-Feb + projected Mar-Sep (each country's own Feb 2024 -> Feb 2026 change)
usc_octfeb_2026 = usc_family %>%
    filter(fy == 2026, month %in% c(10, 11, 12, 1, 2)) %>%
    group_by(group) %>%
    summarise(octfeb_2026 = sum(issuances), .groups = "drop")

usc_marsep_2026 = usc_family %>%
    filter(fy == 2024, month %in% 3:9) %>%
    inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
    mutate(projected = issuances * (1 + pct_change)) %>%
    group_by(group) %>%
    summarise(marsep_2026_proj = sum(projected), .groups = "drop")

usc_family_table = usc_fy2025 %>%
    left_join(usc_octfeb_2026, by = "group") %>%
    left_join(usc_marsep_2026, by = "group") %>%
    mutate(fy2026_proj = octfeb_2026 + marsep_2026_proj) %>%
    bind_rows(summarise(., group = "Total", across(where(is.numeric), sum))) %>%
    mutate(change = fy2026_proj - fy2025,
        pct_change = change / fy2025)

print(usc_family_table)

# change from fy2024
usc_family_table = usc_fy_actual %>%
    left_join(usc_octfeb_2026, by = "group") %>%
    left_join(usc_marsep_2026, by = "group") %>%
    mutate(fy2026_proj = octfeb_2026 + marsep_2026_proj) %>%
    bind_rows(summarise(., group = "TOTAL", across(where(is.numeric), sum))) %>%
    mutate(change = fy2026_proj - fy2024,
        pct_change = change / fy2024) %>%
    select(group,
        `FY2024` = fy2024,
        `FY2025` = fy2025,
        `FY2026 (including projected)` = fy2026_proj,
        change, pct_change)

print(usc_family_table)



## Spouses and children of US citizens: FY2024, FY2025, FY2026 projected ---------------------------
usc_family_groups = tibble(
    visa_class = c("IR1", "CR1", "IB1", "IW", "IR2", "CR2", "IB2", "IR3", "IR4", "IH3", "IH4"),
    group = c("spouses", "spouses", "spouses", "spouses",
        "children", "children", "children", "children", "children", "children", "children"))

usc_family = iv_class %>%
    inner_join(usc_family_groups, by = "visa_class") %>%
    semi_join(iv_country_feb, by = "country")

# FY2024 and FY2025 actual
usc_fy_actual = usc_family %>%
    filter(fy %in% c(2024, 2025)) %>%
    group_by(group, fy) %>%
    summarise(issuances = sum(issuances), .groups = "drop") %>%
    pivot_wider(names_from = fy, values_from = issuances, names_prefix = "fy")

# FY2026: actual Oct-Feb + projected Mar-Sep (each country's own Feb 2024 -> Feb 2026 change)
usc_octfeb_2026 = usc_family %>%
    filter(fy == 2026, month %in% c(10, 11, 12, 1, 2)) %>%
    group_by(group) %>%
    summarise(octfeb_2026 = sum(issuances), .groups = "drop")

usc_marsep_2026 = usc_family %>%
    filter(fy == 2024, month %in% 3:9) %>%
    inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
    mutate(projected = issuances * (1 + pct_change)) %>%
    group_by(group) %>%
    summarise(marsep_2026_proj = sum(projected), .groups = "drop")

# final table
usc_family_table = usc_fy_actual %>%
    left_join(usc_octfeb_2026, by = "group") %>%
    left_join(usc_marsep_2026, by = "group") %>%
    mutate(fy2026_proj = octfeb_2026 + marsep_2026_proj) %>%
    bind_rows(summarise(., group = "TOTAL", across(where(is.numeric), sum))) %>%
    mutate(change = fy2026_proj - fy2024,
        pct_change = change / fy2024) %>%
    select(group,
        `FY2024` = fy2024,
        `FY2025` = fy2025,
        `FY2026 (including projected)` = fy2026_proj,
        change, pct_change)

print(usc_family_table)


# missing


## Missing spouses and children of US citizens: FY2026 (incl. projected) vs. FY2024 ---------------
# missing = FY2024 - FY2026; positive = fewer than in FY2024

# by group (from the table you already built)
usc_missing_group = usc_family_table %>%
    transmute(group, fy2024 = `FY2024`, fy2026 = `FY2026 (including projected)`,
        missing = fy2024 - fy2026,
        pct_missing = missing / fy2024)

print(usc_missing_group)

# by country and visa class
usc_fy2024_cc = usc_family %>%
    filter(fy == 2024) %>%
    group_by(group, visa_class, country) %>%
    summarise(fy2024 = sum(issuances), .groups = "drop")

usc_octfeb_cc = usc_family %>%
    filter(fy == 2026, month %in% c(10, 11, 12, 1, 2)) %>%
    group_by(group, visa_class, country) %>%
    summarise(octfeb_2026 = sum(issuances), .groups = "drop")

usc_marsep_cc = usc_family %>%
    filter(fy == 2024, month %in% 3:9) %>%
    inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
    mutate(projected = issuances * (1 + pct_change)) %>%
    group_by(group, visa_class, country) %>%
    summarise(marsep_2026_proj = sum(projected), .groups = "drop")

usc_missing_cc = usc_fy2024_cc %>%
    full_join(usc_octfeb_cc, by = c("group", "visa_class", "country")) %>%
    full_join(usc_marsep_cc, by = c("group", "visa_class", "country")) %>%
    mutate(across(c(fy2024, octfeb_2026, marsep_2026_proj), ~ coalesce(.x, 0)),
        fy2026 = octfeb_2026 + marsep_2026_proj,
        missing = fy2024 - fy2026)

# check: should match the table above
usc_missing_cc %>%
    group_by(group) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop")

# by visa class
usc_missing_class = usc_missing_cc %>%
    group_by(group, visa_class) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop") %>%
    mutate(pct_missing = missing / fy2024) %>%
    arrange(desc(missing))

print(usc_missing_class, n = Inf)

# by country, spouses and children combined
usc_missing_country = usc_missing_cc %>%
    group_by(country) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop") %>%
    mutate(pct_missing = missing / fy2024,
        share_of_total_missing = missing / sum(missing)) %>%
    arrange(desc(missing))

print(usc_missing_country, n = 20)

# gross vs. net: the net total nets out countries that rose above FY2024 (e.g., Mexico, Canada)
usc_missing_cc %>%
    group_by(country) %>%
    summarise(missing = sum(missing), .groups = "drop") %>%
    summarise(net_missing = sum(missing),
        gross_missing = sum(pmax(missing, 0)),
        offset_by_increases = sum(pmin(missing, 0)))




        ## Missing spouses and children of US citizens: FY2026 (incl. projected) vs. FY2024 ---------------
usc_family_groups = tibble(
    visa_class = c("IR1", "CR1", "IB1", "IW", "IR2", "CR2", "IB2", "IR3", "IR4", "IH3", "IH4"),
    group = c("spouses", "spouses", "spouses", "spouses",
        "children", "children", "children", "children", "children", "children", "children"))

usc_family = iv_class %>%
    inner_join(usc_family_groups, by = "visa_class") %>%
    semi_join(iv_country_feb, by = "country")

# FY2024 actual, by group, class and country
usc_fy2024_cc = usc_family %>%
    filter(fy == 2024) %>%
    group_by(group, visa_class, country) %>%
    summarise(fy2024 = sum(issuances), .groups = "drop")

# FY2026 actual Oct-Feb
usc_octfeb_cc = usc_family %>%
    filter(fy == 2026, month %in% c(10, 11, 12, 1, 2)) %>%
    group_by(group, visa_class, country) %>%
    summarise(octfeb_2026 = sum(issuances), .groups = "drop")

# FY2026 projected Mar-Sep (each country's own Feb 2024 -> Feb 2026 change)
usc_marsep_cc = usc_family %>%
    filter(fy == 2024, month %in% 3:9) %>%
    inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
    mutate(projected = issuances * (1 + pct_change)) %>%
    group_by(group, visa_class, country) %>%
    summarise(marsep_2026_proj = sum(projected), .groups = "drop")

# missing = FY2024 - FY2026; positive = fewer than in FY2024
usc_missing_cc = usc_fy2024_cc %>%
    full_join(usc_octfeb_cc, by = c("group", "visa_class", "country")) %>%
    full_join(usc_marsep_cc, by = c("group", "visa_class", "country")) %>%
    mutate(across(c(fy2024, octfeb_2026, marsep_2026_proj), ~ coalesce(.x, 0)),
        fy2026 = octfeb_2026 + marsep_2026_proj,
        missing = fy2024 - fy2026)

# by group
usc_missing_group = usc_missing_cc %>%
    group_by(group) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop") %>%
    bind_rows(summarise(., group = "TOTAL", across(where(is.numeric), sum))) %>%
    mutate(pct_missing = missing / fy2024)

print(usc_missing_group)

# by visa class
usc_missing_class = usc_missing_cc %>%
    group_by(group, visa_class) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop") %>%
    mutate(pct_missing = missing / fy2024) %>%
    arrange(desc(missing))

print(usc_missing_class, n = Inf)

# by country, spouses and children combined
usc_missing_country = usc_missing_cc %>%
    group_by(country) %>%
    summarise(fy2024 = sum(fy2024), fy2026 = sum(fy2026), missing = sum(missing), .groups = "drop") %>%
    mutate(pct_missing = missing / fy2024,
        share_of_total_missing = missing / sum(missing)) %>%
    arrange(desc(missing))

print(usc_missing_country, n = 20)

# gross vs. net: the net total nets out countries that rose above FY2024
usc_missing_cc %>%
    group_by(country) %>%
    summarise(missing = sum(missing), .groups = "drop") %>%
    summarise(net_missing = sum(missing),
        gross_missing = sum(pmax(missing, 0)),
        offset_by_increases = sum(pmin(missing, 0)))

## Spouses and children of US citizens: 21-month cumulative, Jan 2025 - Sep 2026 ----------------
# each month compared with the same calendar month of FY2024; Mar-Sep 2026 projected

usc_family_groups = tibble(
    visa_class = c("IR1", "CR1", "IB1", "IW", "IR2", "CR2", "IB2", "IR3", "IR4", "IH3", "IH4"),
    group = c("spouses", "spouses", "spouses", "spouses",
        "children", "children", "children", "children", "children", "children", "children"))

window_months = tibble(year  = c(rep(2025L, 12), rep(2026L, 9)),
                       month = c(1:12, 1:9))

iv_matched = iv_class %>%
    semi_join(iv_country_feb, by = "country")

# function: cumulative missing for any set of visa classes (NULL = all IV classes)
cumulative_iv_missing = function(data, classes_df = NULL) {
    if (!is.null(classes_df)) {
        data = data %>% inner_join(classes_df, by = "visa_class")
    } else {
        data = data %>% mutate(group = "all IV")
    }

    baseline = data %>%
        filter(fy == 2024) %>%
        group_by(group, country, month) %>%
        summarise(baseline = sum(issuances), .groups = "drop")

    actual = data %>%
        filter(year == 2025 | (year == 2026 & month <= 2)) %>%
        group_by(group, country, year, month) %>%
        summarise(value = sum(issuances), .groups = "drop")

    projected = baseline %>%
        filter(month %in% 3:9) %>%
        inner_join(iv_country_feb %>% select(country, pct_change), by = "country") %>%
        transmute(group, country, year = 2026L, month, value = baseline * (1 + pct_change))

    data %>%
        distinct(group, country) %>%
        cross_join(window_months) %>%
        left_join(bind_rows(actual, projected), by = c("group", "country", "year", "month")) %>%
        left_join(baseline, by = c("group", "country", "month")) %>%
        mutate(value = coalesce(value, 0), baseline = coalesce(baseline, 0),
            missing = value - baseline)
}

usc_cum = cumulative_iv_missing(iv_matched, usc_family_groups)
iv_cum  = cumulative_iv_missing(iv_matched)

# by group (net)
usc_cum_table = usc_cum %>%
    group_by(group) %>%
    summarise(baseline = sum(baseline), value = sum(value), missing = sum(missing), .groups = "drop") %>%
    bind_rows(summarise(., group = "TOTAL", across(where(is.numeric), sum))) %>%
    mutate(pct_missing = missing / baseline)

print(usc_cum_table)

# check against the paper: all IV classes (should be close to 293,632)
iv_cum %>%
    summarise(baseline = sum(baseline), value = sum(value), missing = sum(missing))

# share of the IV decline, and of all legal entries (2,152,762 in the paper)
usc_total_missing = usc_cum_table %>% filter(group == "TOTAL") %>% pull(missing)
iv_total_missing  = sum(iv_cum$missing)

tibble(usc_missing = usc_total_missing,
    share_of_iv_missing = usc_total_missing / iv_total_missing,
    share_of_all_entries = usc_total_missing / -2152762)

# by year within the window: 2025 offsets 2026
usc_cum %>%
    group_by(group, year) %>%
    summarise(baseline = sum(baseline), value = sum(value), missing = sum(missing), .groups = "drop") %>%
    bind_rows(usc_cum %>%
            group_by(year) %>%
            summarise(group = "TOTAL", baseline = sum(baseline), value = sum(value),
                missing = sum(missing), .groups = "drop")) %>%
    arrange(year, group) %>%
    print(n = Inf)

# gross vs. net by country: net nets out countries that rose above FY2024
usc_cum %>%
    group_by(country) %>%
    summarise(missing = sum(missing), .groups = "drop") %>%
    summarise(net_missing = sum(missing),
        gross_missing = sum(pmax(missing, 0)),
        offset_by_increases = sum(pmin(missing, 0)))
