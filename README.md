# Visa blans block 933,177 legal immigrant workers

## Methodology

### Data sources
- Immigrant Visa issuances: [U.S. Department of State, Monthly Immigrant Visa Issuance Statistics](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/immigrant-visa-statistics/monthly-immigrant-visa-issuances.html)
- Nonimmigrant Visa issuances: [U.S. Department of State, Monthly Nonimmigrant Visa Issuance Statistics](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html)
- Refugee Arrivals by Nationality: [U.S. Department of State | Bureau of Population, Refugees, and Migration | Refugee Admissions Report](https://www.rpc.state.gov/admissions-and-arrivals/)
- Asylees: [U.S. Customs and Border Protection | Nationwide Encounters](https://www.cbp.gov/document/stats/nationwide-encounters)
- Population and occupation data: [IPUMS USA, American Community Survey (ACS) 5-year PUMS, 2020–2024](https://usa.ipums.org)

### Formulas

**1. Projected FY2026 decline, by country $c$ and visa channel $k \in \{IV, NIV\}$:**

$$r_c^{k} = \frac{V_c^{k,\text{Feb2026}} - V_c^{k,\text{Feb2024}}}{V_c^{k,\text{Feb2024}}}$$

$$\hat{V}_c^{k,\text{Mar-Sep2026}} = V_c^{k,\text{Mar-Sep2024}} \times (1 + r_c^{k})$$

$$\Delta V_c^{k} = \left(V_c^{k,\text{Oct-Feb2026}} + \hat{V}_c^{k,\text{Mar-Sep2026}}\right) - V_c^{k,\text{FY2024}}$$

> $r_c^k$ is the rate of change from Feb 2024 to Feb 2026, and is negative when a country's visas declined. Multiplying the FY2024 base months by $(1 + r_c^k)$ applies that rate to project what FY2026 would look like if the Feb trend held.

> IV and NIV are projected separately, each using its own Feb-based rate, because NIV volume is far larger than IV — pooling the two series before computing a rate would let NIV's trend dominate and misstate IV's decline (and vice versa).

**2. Combined decline, by country:**

$$\Delta R_c = R_c^{\text{FY2026}} - R_c^{\text{FY2024}}$$

$$\Delta A_c = A_c^{\text{FY2026}} - A_c^{\text{FY2024}}$$

$$\Delta V_c = \Delta V_c^{IV} + \Delta V_c^{NIV} + \Delta R_c + \Delta A_c$$

where $R_c$ is refugee arrivals and $A_c$ is asylee admissions, proxied by CBP nationwide encounters at ports of entry (component = OFO, Southwest Land Border). Both are reported as actual full-year totals for FY2024 and FY2026 (no projection).

**2b. Cumulative decline, January 2025 - September 2026 (21 months), by country:**

For each source $k \in \{IV, NIV, Refugees, Asylees\}$ and calendar month $m$ from January 2025 through September 2026:

$$V_{c,m}^{k} = \begin{cases} \hat{V}_c^{k,\text{Mar-Sep2026}} & k \in \{IV, NIV\} \text{ and } m \in \{\text{Mar 2026}, \dots, \text{Sep 2026}\} \\ \text{actual value} & \text{otherwise} \end{cases}$$

$$\Delta V_{c,m}^{k} = V_{c,m}^{k} - V_{c,m}^{k,\text{FY2024}}$$

$$\Delta V_c^{\text{cumulative}} = \sum_{k} \sum_{m} \Delta V_{c,m}^{k}$$

> $V_{c,m}^{k,\text{FY2024}}$ is that source's value in the *same calendar month* $m$ during FY2024 — so January 2025 and January 2026 both compare to January 2024, consistent with how Mar-Sep 2026 is projected in Formula 1.
**3. Occupation share, by country $c$ and occupation $o$:**

$$s_{c,o} = \frac{\text{immigrants from country } c \text{ working in occupation } o}{\text{all immigrants from country } c}$$

> using all immigrants — not just the employed — as the denominator, since visa & arrivals data don't distinguish workers from family members.

**4. Missing workers:**

$$\text{Missing}_o = \sum_c \Delta V_c \times s_{c,o}$$

$$\text{Total missing workers} = \sum_o \text{Missing}_o$$

> Substituting $\Delta V_c^{\text{cumulative}}$ (Formula 2b) for $\Delta V_c$ (Formula 2) gives the cumulative missing-workers total for January 2025 - September 2026 instead of the FY2024-vs-FY2026 snapshot; both use the same occupation shares $s_{c,o}$.

### Visa types included
- **Immigrant visas (IV):** all countries/classes, State Department Monthly IV Issuance Statistics by Foreign State of Chargeability.
- **Nonimmigrant visas (NIV):** all classes whose holders would likely appear in the ACS as US residents (staying 2+ months at a US address within ACS geography). This excludes only visitors, transit/crew, CNMI-only classes, and defunct classes.
  - Students: `F1`, `F2`, `M1`, `M2`
  - Exchange visitors: `J1`, `J2`
  - Temp/specialty workers: `H1B`, `H1B1`, `H2A`, `H2B`, `H3`, `H4`
  - Intracompany transferees: `L1`, `L2`
  - Extraordinary ability / athletes / entertainers: `O1`, `O2`, `O3`, `P1`–`P4`
  - Treaty traders/investors and Australian specialty workers: `E1`, `E2`, `E3`, `E3D`, `E3R`
  - NAFTA/USMCA professionals: `TN`, `TD`
  - Religious workers: `R1`, `R2`
  - International cultural exchange: `Q1`
  - Foreign media: `I`
  - Diplomats, international organization staff, NATO, and their personal employees: `A1`–`A3`, `G1`–`G5`, `NATO1`–`NATO7`
  - Fiancé(e)s and spouses of US citizens and their children: `K1`–`K4` (K holders adjust status inside the US, so they do not also receive an IV and are not double-counted)
  - Derivatives of crime and trafficking victims: `U1`–`U5`, `T2`–`T6`
  - Parents/children of G-4 special immigrants: `N8`, `N9`
- **Excluded NIV classes:**
  - Visitors (tourism/business) and border crossing cards: `B1`, `B2`, `B1/B2`, `BBBCC`, `BBBCV`
  - Transit and crew: `C1`, `C1/D`, `C2`, `C3`, `C4`, `C4/D3`, `D`, `D3`
  - CNMI-only classes (CNMI is outside ACS geography): `CW1`, `CW2`, `E2C`
  - Defunct: `H1C` (program expired 2009)