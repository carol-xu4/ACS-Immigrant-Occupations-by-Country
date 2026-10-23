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
top3_occ = acs %>%
  filter(country != "United States", citizen %in% c(2, 3),
         age >= 18, age <= 64, empstat == 1) %>%
  mutate(occ_lbl = as.character(as_factor(occ))) %>%
  group_by(country, occ_lbl) %>%
  summarise(n = n(), pop = sum(perwt), .groups = "drop_last") %>%
  mutate(share = pop / sum(pop)) %>%                 # share of that country's workers
  slice_max(pop, n = 3, with_ties = FALSE) %>%
  mutate(rank  = row_number(),
         label = paste0(occ_lbl, " (", round(100 * share, 1), "%)")) %>%
  ungroup()

top3_wide = top3_occ %>%
  select(country, rank, label) %>%
  pivot_wider(names_from = rank, values_from = label, names_prefix = "occ_")

bpl_jobs = bpl_table_18_64 %>%
  left_join(top3_wide, by = "country") %>%
  arrange(desc(pop))

View(bpl_jobs)

write_csv(bpl_jobs, "results/top_10_jobs.csv")
