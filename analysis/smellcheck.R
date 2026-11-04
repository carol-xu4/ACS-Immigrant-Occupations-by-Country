# smell check single countries for FY2026 projection
check_country = "Afghanistan"

# raw monthly issuances, so you can see the actual Feb/Oct-Feb/Mar-Sep numbers behind everything below
iv_country_monthly %>%
  filter(country == check_country) %>%
  arrange(fy, month) %>%
  print(n = Inf)

# february actuals and the rate (r_c)
country_pct_change %>% filter(country == check_country)

# actual oct-feb totals, both fiscal years
octfeb_actual %>% filter(country == check_country)

# march-sep: fy2024 actual, fy2026 projected, and the change, by month
iv_country_projection %>% filter(country == check_country) %>% print(n = Inf)

# march-sep totals, summed
marsep_total %>% filter(country == check_country)

# full fy totals, change, pct_change, plus feb_2024/feb_2026 smell-check columns
missing_visas_by_country %>% filter(country == check_country)

# this country's healthcare occupation shares
bpl_occ_group %>%
  filter(country == check_country, occ_group %in% c("Healthcare practitioners & technical", "Healthcare support"))

# final missing healthcare workers for this country
missing_healthcare_workers %>% filter(country == check_country)

# recompute everything by hand, independent of the pipeline, to confirm it matches
feb2024      = country_pct_change %>% filter(country == check_country) %>% pull(visas_2024)
feb2026      = country_pct_change %>% filter(country == check_country) %>% pull(visas_2026)
rc           = country_pct_change %>% filter(country == check_country) %>% pull(pct_change)
marsep_2024  = iv_country_monthly %>% filter(country == check_country, fy == 2024, month >= 3, month <= 9) %>% summarise(v = sum(issuances)) %>% pull(v)
marsep_2026  = round(marsep_2024 * (1 + rc))
octfeb_2024  = octfeb_actual %>% filter(country == check_country) %>% pull(visas_2024)
octfeb_2026  = octfeb_actual %>% filter(country == check_country) %>% pull(visas_2026)
visas_2024_h = octfeb_2024 + marsep_2024
visas_2026_h = octfeb_2026 + marsep_2026
change_h     = visas_2026_h - visas_2024_h

cat("feb2024:", feb2024, " feb2026:", feb2026, " r_c:", round(rc, 4), "\n")
cat("marsep_2024:", marsep_2024, " marsep_2026 (by hand):", marsep_2026, "\n")
cat("octfeb_2024:", octfeb_2024, " octfeb_2026:", octfeb_2026, "\n")
cat("visas_2024 (by hand):", visas_2024_h, " visas_2026 (by hand):", visas_2026_h, " change (by hand):", change_h, "\n")
