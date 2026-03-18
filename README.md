# telework-productivity-spain
Analysis of telework impact on labour productivity in Spain (2006–2024)

# Telework and Labour Productivity in Spain (2006–2024)

Bachelor's thesis — Economics, Universidad Autónoma de Madrid (2025)  
Grade: 8/10

---

## Overview

Time series analysis of the relationship between telework intensity and labour 
productivity in Spain, focusing on the Information and Communications sector (CNAE-J).  
Data covers 2006–2024 using microdata from Spain's Labour Force Survey (EPA/INE).

**Main finding:** Both moderate and intensive telework show a short-term negative 
association with productivity in sector J, with effects that dissipate within 2–3 years.

---

## Methods

- Stationarity testing: Augmented Dickey-Fuller (ADF)
- Model: ARDL (Autoregressive Distributed Lag) estimated by OLS
- Tools: Transfer function and Impulse Response analysis
- Software: R

**Variables:**
- Dependent: Real labour productivity (VAB / employed workers, sector J)
- Main: Telework moderate (occasional) and telework intensive (>50% of days)
- Controls: Gross Fixed Capital Formation (GFCF), unemployment rate

---

## Repository structure
```
├── README.md
├── TFG_Ainhoa_Arranz_Martinez.pdf   ← full thesis document
├── code/
│   └── analysis.R                   ← R code for all models
└── data/
    └── HOW_TO_GET_DATA.md           ← instructions to request data from INE
```

---

## Data access

The microdata used in this analysis comes from the **EPA (Encuesta de Población Activa)** 
subsample, provided by Spain's National Statistics Institute (INE) under a formal data 
supply agreement (reference: TO045/2025).

Due to the data supply conditions, raw microdata cannot be redistributed.  
To reproduce this analysis, you can request the same data directly from INE:

- Annual subsample (2-digit level), 2006–2020
- Quarterly EPA microdata (2-digit level), 2020–2024  
- Quarterly DOMICI annex, 2020–2024

Request form: (https://www.ine.es/dyngs/DAB/index.htm?cid=1722).

Additional variables (VAB, GFCF, unemployment) are publicly available from:
- INE National Accounts: https://www.ine.es
- Eurostat: https://ec.europa.eu/eurostat
- World Bank (GDP deflator): https://data.worldbank.org

---

## Key results

| Variable | Effect on productivity | Significance |
|---|---|---|
| Telework moderate (Δ) | −2,773 €/worker | * (p=0.030) |
| Telework intensive (Δ) | −502 €/worker | ** (p=0.006) |
| Unemployment lag | −1,956 €/worker | ** (p=0.009) |

Model R² adjusted: 0.78 — F-statistic significant at 1% (p=0.0017)

Impulse response shows effects dissipate within 3 periods.

---

## Limitations

- Small sample: 19 annual observations (2006–2024)
- Telework only became structurally relevant after 2020
- Macroeconomic controls (GFCF, unemployment) are national aggregates, not sector-level
- 2020 observation is an outlier due to COVID-19 mandatory telework

---

## Author

**Ainhoa Arranz Martínez**  
Economics graduate, Universidad Autónoma de Madrid  
Currently studying MSc Big Data, Data Science & Business Analytics — UCM/NTIC  
[LinkedIn](https://linkedin.com/in/[tu-usuario]) · [GitHub](https://github.com/[tu-usuario])

---

*Primary data source: INE (Instituto Nacional de Estadística). 
Analytical conclusions are the author's own responsibility.*
