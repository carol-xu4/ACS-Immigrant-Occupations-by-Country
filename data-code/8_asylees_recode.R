## Asylees, by country and calendar month, FY2021-FY2026 ------------------------------------------
# Office of Field Operations, Southwest Land Border, Title 8 Inadmissibles only
# (excludes Title 42 expulsions -- people turned back, not people who entered the asylum pipeline)
read_encounters = function(path) {
    read_csv(path, show_col_types = FALSE) %>%
        rename(fy = `Fiscal Year`, month_abbv = `Month (abbv)`, component = Component,
            land_border_region = `Land Border Region`, citizenship = Citizenship,
            encounter_type = `Encounter Type`, encounters = `Encounter Count`) %>%
        filter(component == "Office of Field Operations",
            land_border_region == "Southwest Land Border",
            encounter_type == "Inadmissibles") %>%
        mutate(fy = as.integer(fy)) %>%
        group_by(fy, month_abbv, citizenship) %>%
        summarise(encounters = sum(encounters), .groups = "drop")
}

MONTH_LOOKUP = tibble(
    month_abbv = c("OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP"),
    month = c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    year_offset = c(-1L, -1L, -1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L))

encounters_old = read_encounters("data/input/nationwide-encounters-fy20-fy23-aor.csv") %>%
    filter(fy %in% c(2021, 2022))

encounters_new = read_encounters("data/input/nationwide-encounters-fy23-fy26-sep-aor (3).csv") %>%
    filter(fy %in% c(2023, 2024, 2025, 2026))

asylees_monthly = bind_rows(encounters_old, encounters_new) %>%
    left_join(MONTH_LOOKUP, by = "month_abbv") %>%
    mutate(year = fy + year_offset, country = str_to_title(citizenship))

# sanity check: totals by fy
asylees_monthly %>%
    group_by(fy) %>%
    summarise(total = sum(encounters), .groups = "drop")

asylees_recoded = asylees_monthly %>%
    mutate(country = recode_visa_country(country)) %>%
    group_by(fy, year, month, country) %>%
    summarise(encounters = sum(encounters), .groups = "drop")

# confirm clean recoding 
asylees_recoded %>%
    group_by(country) %>%
    summarise(encounters = sum(encounters), .groups = "drop") %>%
    anti_join(acs_bpl, by = "country")

write_csv(asylees_recoded, "data/output/asylee_encounters_by_country_fy2021_fy2026_recoded.csv")
