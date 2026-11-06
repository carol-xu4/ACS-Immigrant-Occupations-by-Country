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
- Immigrant Visa issuances: [U.S. Department of State, Monthly Immigrant Visa Issuance Statistics](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/immigrant-visa-statistics/monthly-immigrant-visa-issuances.html)
- Nonimmigrant Visa issuances: [U.S. Department of State, Monthly Nonimmigrant Visa Issuance Statistics](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html)
- Population and occupation data: [IPUMS USA, American Community Survey (ACS) 5-year PUMS, 2020–2024](https://usa.ipums.org)

### Formulas
**1. Projected FY2026 decline, by country $c$ and visa channel $k \in \{IV, NIV\}$:**

$$r_c^{k} = \frac{V_c^{k,\text{Feb2026}} - V_c^{k,\text{Feb2024}}}{V_c^{k,\text{Feb2024}}}$$

$$V_c^{k,\text{Mar-Sep2026, projected}} = V_c^{k,\text{Mar-Sep2024}} \times (1 + r_c^{k})$$

$$\Delta V_c^{k} = \left(V_c^{k,\text{Oct-Feb2026, actual}} + V_c^{k,\text{Mar-Sep2026, projected}}\right) - V_c^{k,\text{FY2024, actual}}$$

IV and NIV are projected separately, each using its own Feb-based rate, because NIV volume is far larger than IV — pooling the two series before computing a rate would let NIV's trend dominate and misstate IV's decline (and vice versa).

**2. Combined decline, by country:**

$$\Delta V_c = \Delta V_c^{IV} + \Delta V_c^{NIV}$$

**3. Occupation share, by country $c$ and occupation $o$:**

$$s_{c,o} = \frac{\text{immigrants from country } c \text{ working in occupation } o}{\text{all immigrants from country } c}$$

using all immigrants — not just the employed — as the denominator, since visa data doesn't distinguish workers from family members.

**4. Missing workers:**

$$\text{Missing}_o = \sum_c \Delta V_c \times s_{c,o}$$

$$\text{Total missing workers} = \sum_o \text{Missing}_o$$