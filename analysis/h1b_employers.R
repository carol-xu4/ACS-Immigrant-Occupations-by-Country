## Preliminaries ----------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(matrixStats, tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, rvest)

setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/ACS Immigrant Occupations by Country")

## Read in and clean USCIS H-1B Employer Data Hub, FY2023-FY2026 -------------------------------
# file is UTF-16 and tab-delimited
h1b_raw = read_tsv("data/input/Employer Information (2).csv",
    locale = locale(encoding = "UTF-16"),
    col_types = cols(.default = "c"),
    show_col_types = FALSE)

# raw rows / fy
h1b_raw %>% count(`Fiscal Year`)

h1b = h1b_raw %>%
    select(-`Line by line`) %>%
    rename(fy = `Fiscal Year`,
        employer = `Employer (Petitioner) Name`,
        tax_id = `Tax ID`,
        naics = `Industry (NAICS) Code`,
        city = `Petitioner City`,
        state = `Petitioner State`,
        zip = `Petitioner Zip Code`,
        new_emp_approval = `New Employment Approval`,
        new_emp_denial = `New Employment Denial`,
        continuation_approval = `Continuation Approval`,
        continuation_denial = `Continuation Denial`,
        same_employer_approval = `Change with Same Employer Approval`,
        same_employer_denial = `Change with Same Employer Denial`,
        concurrent_approval = `New Concurrent Approval`,
        concurrent_denial = `New Concurrent Denial`,
        change_employer_approval = `Change of Employer Approval`,
        change_employer_denial = `Change of Employer Denial`,
        amended_approval = `Amended Approval`,
        amended_denial = `Amended Denial`) %>%
    mutate(across(c(ends_with("_approval"), ends_with("_denial")), ~ parse_number(.x)),
        fy = as.integer(fy),
        # trailing spaces (e.g., "MIDDLETON " vs "MIDDLETON") and double spaces in names split
        # the same employer location into two rows
        across(c(employer, tax_id, naics, city, state, zip), str_squish),
        employer = str_to_upper(employer),
        city = str_to_upper(city),
        naics = if_else(is.na(naics) | naics == "", "Unknown", naics),
        naics_code = if_else(naics == "Unknown", "Unknown", str_extract(naics, "^[0-9-]+")),
        naics_label = if_else(naics == "Unknown", "Unknown", str_remove(naics, "^[0-9-]+ - ")),
        all_approvals = rowSums(across(ends_with("_approval")), na.rm = TRUE),
        all_denials = rowSums(across(ends_with("_denial")), na.rm = TRUE))

# totals before collapsing duplicates
fy_totals_precollapse = h1b %>%
    group_by(fy) %>%
    summarise(new_emp_approval = sum(new_emp_approval, na.rm = TRUE),
        all_approvals = sum(all_approvals, na.rm = TRUE),
        .groups = "drop")

# duplicates: same employer location listed in more than one row within a fiscal year
h1b_dups = h1b %>%
    add_count(fy, employer, tax_id, naics, city, state, zip, name = "n_rows") %>%
    filter(n_rows > 1) %>%
    arrange(fy, employer)

print(h1b_dups %>% select(fy, employer, tax_id, city, state, new_emp_approval, new_emp_denial, n_rows), n = 40)

# collapse to one row per employer location per fiscal year (totals unchanged)
h1b = h1b %>%
    group_by(fy, employer, tax_id, naics, naics_code, naics_label, city, state, zip) %>%
    summarise(across(c(ends_with("_approval"), ends_with("_denial"), all_approvals, all_denials),
            ~ sum(.x, na.rm = TRUE)),
        .groups = "drop")

# sanity checks
# totals must be identical before and after collapsing
fy_totals_precollapse %>%
    left_join(h1b %>%
            group_by(fy) %>%
            summarise(new_emp_approval_after = sum(new_emp_approval),
                all_approvals_after = sum(all_approvals),
                .groups = "drop"),
        by = "fy") %>%
    mutate(same = new_emp_approval == new_emp_approval_after & all_approvals == all_approvals_after) %>%
    print(n = Inf)

# rows after collapse (same total)
h1b %>%
    group_by(fy) %>%
    summarise(rows = n(),
        unknown_naics = sum(naics == "Unknown"),
        pct_unknown_naics = unknown_naics / rows,
        missing_employer = sum(is.na(employer) | employer == ""),
        .groups = "drop") %>%
    print(n = Inf)

# no duplicates should remain
h1b %>%
    count(fy, employer, tax_id, naics, city, state, zip) %>%
    filter(n > 1) %>%
    nrow()

## Total H-1B petitions approved, by fiscal year -----------------------------------------------
# NOTE: counts are USCIS first decisions on petitions, not visas issued and not people
h1b_long = h1b %>%
    select(fy, ends_with("_approval"), ends_with("_denial")) %>%
    pivot_longer(-fy, names_to = c("category", "decision"),
        names_pattern = "(.*)_(approval|denial)", values_to = "petitions") %>%
    group_by(fy, category, decision) %>%
    summarise(petitions = sum(petitions, na.rm = TRUE), .groups = "drop")

# by petition category
fy_totals_by_category = h1b_long %>%
    pivot_wider(names_from = decision, values_from = petitions) %>%
    mutate(denial_rate = denial / (approval + denial)) %>%
    arrange(category, fy)

print(fy_totals_by_category, n = Inf)

# all categories combined
fy_totals = h1b %>%
    group_by(fy) %>%
    summarise(new_emp_approval = sum(new_emp_approval, na.rm = TRUE),
        new_emp_denial = sum(new_emp_denial, na.rm = TRUE),
        all_approvals = sum(all_approvals, na.rm = TRUE),
        all_denials = sum(all_denials, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(new_emp_denial_rate = new_emp_denial / (new_emp_approval + new_emp_denial),
        all_denial_rate = all_denials / (all_approvals + all_denials),
        new_emp_pct_vs_fy2024 = new_emp_approval / new_emp_approval[fy == 2024] - 1,
        all_pct_vs_fy2024 = all_approvals / all_approvals[fy == 2024] - 1)

print(fy_totals, n = Inf)

write_csv(fy_totals, "results/petitions/h1b_hub_fy_totals.csv")
write_csv(fy_totals_by_category, "results/petitions/h1b_hub_fy_totals_by_category.csv")

## By industry (NAICS sector), by fiscal year --------------------------------------------------
industry_fy = h1b %>%
    group_by(fy, naics_code, naics_label) %>%
    summarise(new_emp_approval = sum(new_emp_approval, na.rm = TRUE),
        new_emp_denial = sum(new_emp_denial, na.rm = TRUE),
        all_approvals = sum(all_approvals, na.rm = TRUE),
        .groups = "drop") %>%
    group_by(fy) %>%
    mutate(new_emp_share = new_emp_approval / sum(new_emp_approval),
        all_share = all_approvals / sum(all_approvals)) %>%
    ungroup()

# new employment approvals, wide by fiscal year
industry_new_emp_wide = industry_fy %>%
    select(naics_code, naics_label, fy, new_emp_approval) %>%
    pivot_wider(names_from = fy, values_from = new_emp_approval,
        names_prefix = "fy", values_fill = 0) %>%
    mutate(change_2024_2026 = fy2026 - fy2024,
        pct_change_2024_2026 = change_2024_2026 / fy2024) %>%
    arrange(desc(fy2024))

print(industry_new_emp_wide, n = Inf)

# all approvals, wide by fiscal year
industry_all_wide = industry_fy %>%
    select(naics_code, naics_label, fy, all_approvals) %>%
    pivot_wider(names_from = fy, values_from = all_approvals,
        names_prefix = "fy", values_fill = 0) %>%
    mutate(change_2024_2026 = fy2026 - fy2024,
        pct_change_2024_2026 = change_2024_2026 / fy2024) %>%
    arrange(desc(fy2024))

print(industry_all_wide, n = Inf)

# new employment denial rate by industry
industry_denial_rate = industry_fy %>%
    mutate(new_emp_denial_rate = new_emp_denial / (new_emp_approval + new_emp_denial)) %>%
    select(naics_code, naics_label, fy, new_emp_denial_rate) %>%
    pivot_wider(names_from = fy, values_from = new_emp_denial_rate, names_prefix = "fy")

print(industry_denial_rate, n = Inf)

# check: industry totals match fiscal year totals
industry_fy %>%
    group_by(fy) %>%
    summarise(new_emp_approval = sum(new_emp_approval), all_approvals = sum(all_approvals), .groups = "drop")

write_csv(industry_fy, "results/h1b_hub_industry_fy.csv")
write_csv(industry_new_emp_wide, "results/h1b_hub_industry_new_employment_wide.csv")
write_csv(industry_all_wide, "results/h1b_hub_industry_all_approvals_wide.csv")
