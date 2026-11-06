# Visa blans block XXX,XXX legal immigrant workers

## Methodology

### Visa types included
- **Immigrant visas (IV):** all countries/classes, State Department Monthly IV Issuance Statistics by Foreign State of Chargeability.
- **Nonimmigrant visas (NIV):** restricted to classes representing workers, students, and exchange visitors likely to appear in ACS as US residents:
  - Students: `F1`, `F2`, `M1`, `M2`
  - Temp/specialty workers: `H1B`, `H1B1`, `H1C`, `H2A`, `H2B`, `H3`, `H4`
  - Intracompany transferees: `L1`, `L2`
  - Extraordinary ability / athletes / entertainers: `O1`, `O2`, `O3`, `P1`–`P4`
  - Treaty traders/investors: `E1`, `E2`, `E2C`, `E3`, `E3D`, `E3R`
  - NAFTA/USMCA professionals: `TN`, `TD`
  - Religious workers: `R1`, `R2`
  - CNMI transitional workers: `CW1`, `CW2`
  - Exchange visitors: `J1`, `J2`
- **Excluded NIV classes:** tourism/business (`B1`, `B2`, `B1/B2`), transit/crew (`C`/`D` series), diplomatic/international org/NATO staff (`A`, `G`, `NATO` series), victim-protection visas (`T`, `U`, `N8`/`N9`), media (`I`), and fiancé(e)/spouse visas (`K1`–`K4` — immigrant-track, excluded here to avoid double-counting against IV).

### Data sources
- Visa issuances: [U.S. Department of State, Monthly Immigrant and Nonimmigrant Visa Issuance Statistics](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics.html)
- Population and occupation data: [IPUMS USA, American Community Survey (ACS) 5-year PUMS, 2020–2024](https://usa.ipums.org)

### Formulas
**1. Projected FY2026 visa decline, by country (c):**
- `r_c = (Feb_2026_c − Feb_2024_c) / Feb_2024_c`
- `Projected Mar–Sep 2026_c = Mar–Sep 2024_c × (1 + r_c)`
- `ΔV_c = (FY2026 actual + projected) − FY2024 actual` (Oct–Feb actual, Mar–Sep projected)
- Same formula applied separately to IV and to the included NIV classes, then summed per country.

**2. Occupation share, by country and occupation (o):**
- `s_{c,o}` = share of **all** immigrants (naturalized + noncitizen) born in country c who work in occupation o (ACS), using all immigrants — not just the employed — as the denominator, since visa data doesn't distinguish workers from family members.

**3. Missing workers:**
- `Missing_o = Σ_c ΔV_c × s_{c,o}`
- `Total missing workers = Σ_o Missing_o`