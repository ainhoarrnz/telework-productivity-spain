# ============================================================
# Telework and Labour Productivity in Spain (2006-2024)
# Bachelor's Thesis — UAM Economics, 2025
# Author: Ainhoa Arranz Martinez
#
# Description:
#   ARDL model estimating the short-term effect of telework
#   on labour productivity in Spain's ICT sector (CNAE-J)
#   and other selected sectors.
#
# Data: EPA subsample (INE, ref. TO045/2025),
#       Eurostat GFCF, INE unemployment rate,
#       INE National Accounts (VAB), World Bank GDP deflator
#
# Software: R
# Packages: readxl, dplyr, tidyr, ggplot2, urca,
#           dynlm, lmtest, sandwich, ARDL, car
# ============================================================


# ── 0. LIBRARIES ─────────────────────────────────────────────

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(data.table)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)
library(purrr)
library(urca)
library(dynlm)
library(lmtest)
library(sandwich)
library(ARDL)
library(car)


# ── 1. IMPORT RAW EPA DATA ───────────────────────────────────
# Each year is loaded from individual Excel files.
# Files must be placed in: Teletrabajo/EPA ANUAL/EPA_YYYY.xlsx

ruta  <- "Teletrabajo/EPA ANUAL"
años  <- 2006:2024

# Column positions differ between 2006-2020 and 2021-2024
get_columnas <- function(año) {
  if (año <= 2020) {
    return(c(8, 10, 15, 21, 35, 41, 42, 57, 59, 68, 138))
  } else {
    return(c(8, 10, 15, 21, 29, 35, 36, 51, 53, 65, 115))
  }
}

nombres_columnas <- c("EDAD1", "SEXO1", "ECIV1", "NFORMA", "TRAREM",
                      "OCUP",  "ACT",   "NUMTRA", "PARCO1", "DOMICI", "FACTOREL")

lista_dfs <- list()
for (año in años) {
  archivo <- file.path(ruta, paste0("EPA_", año, ".xlsx"))
  df      <- read_excel(archivo, col_names = FALSE)
  df      <- df[, get_columnas(año)]
  colnames(df) <- nombres_columnas
  df$AÑO  <- año

  df <- df %>%
    mutate(
      FACTOREL = FACTOREL / 1000,   # scale correction
      across(c(EDAD1, SEXO1, TRAREM, OCUP, ACT, NUMTRA, PARCO1, DOMICI), as.numeric)
    ) %>%
    filter(TRAREM == 1)             # employed workers only

  lista_dfs[[as.character(año)]] <- df
}

datos_completos <- bind_rows(lista_dfs)


# ── 2. CALIBRATE FACTOREL TO OFFICIAL TOTALS ─────────────────
# Calibration weights align sample totals to official INE figures

totales_oficiales <- c(
  "2006" = 19979987, "2007" = 20260310, "2008" = 20293306,
  "2009" = 17933285, "2010" = 17857447, "2011" = 17661530,
  "2012" = 16906284, "2013" = 16697974, "2014" = 16898898,
  "2015" = 17349392, "2016" = 17812746, "2017" = 18242488,
  "2018" = 18805321, "2019" = 19593924, "2020" = 19206081,
  "2021" = 19699490, "2022" = 20466206, "2023" = 20833857,
  "2024" = 21222597
)

totales_ponderados <- datos_completos %>%
  group_by(AÑO) %>%
  summarise(ponderado = sum(FACTOREL, na.rm = TRUE), .groups = "drop")

factores_calibracion <- sapply(totales_ponderados$AÑO, function(año) {
  totales_oficiales[as.character(año)] /
    totales_ponderados$ponderado[totales_ponderados$AÑO == año]
})
names(factores_calibracion) <- totales_ponderados$AÑO

datos_completos <- datos_completos %>%
  mutate(FACTOREL_CAL = FACTOREL * factores_calibracion[as.character(AÑO)])


# ── 3. SECTOR CLASSIFICATION (CNAE-93 → CNAE-2009) ───────────
# 2006-2007 use CNAE-93 codes; 2008+ use CNAE-2009

tabla_cnae93_2009 <- tibble(
  ACT = c(
    1, 2, 5, 74, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 23, 24, 25,
    26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 41, 45, 70, 50, 51, 52,
    60, 61, 62, 63, 55, 64, 65, 66, 67, 68, 72, 73, 71, 75, 80, 85, 92, 93,
    90, 91, 95, 97, 99
  ),
  sector_AU = c(
    rep("A", 4), rep("B", 4), rep("C", 20), "D", rep("E", 3), rep("F", 2),
    rep("G", 3), rep("H", 4), "I", "J", rep("K", 3), "L", rep("M", 2), "N",
    "O", "P", "Q", rep("R", 2), rep("S", 3), "T", "U"
  ),
  nombre_sector = c(
    rep("Agricultura, ganadería, silvicultura y pesca", 4),
    rep("Industrias extractivas", 4),
    rep("Industria manufacturera", 20),
    "Suministro de energía eléctrica, gas, vapor y aire acondicionado",
    rep("Suministro de agua; gestión de residuos y descontaminación", 3),
    rep("Construcción", 2),
    rep("Comercio al por mayor y al por menor; reparación de vehículos", 3),
    rep("Transporte y almacenamiento", 4),
    "Hostelería",
    "Información y comunicaciones",
    rep("Actividades financieras y de seguros", 3),
    "Actividades inmobiliarias",
    rep("Actividades profesionales, científicas y técnicas", 2),
    "Actividades administrativas y servicios auxiliares",
    "Administración pública y defensa; seguridad social obligatoria",
    "Educación",
    "Actividades sanitarias y de servicios sociales",
    rep("Actividades artísticas, recreativas y de entretenimiento", 2),
    rep("Otros servicios", 3),
    "Actividades de los hogares como empleadores",
    "Actividades de organizaciones y organismos extraterritoriales"
  )
) %>%
  add_row(ACT = 21, sector_AU = "C", nombre_sector = "Industria manufacturera") %>%
  add_row(ACT = 40, sector_AU = "D",
          nombre_sector = "Suministro de energía eléctrica, gas, vapor y aire acondicionado") %>%
  mutate(
    sector_AU    = case_when(ACT == 70 ~ "L", ACT == 95 ~ "T", TRUE ~ sector_AU),
    nombre_sector = case_when(
      ACT == 70 ~ "Actividades inmobiliarias",
      ACT == 95 ~ "Actividades de los hogares como empleadores",
      TRUE ~ nombre_sector
    )
  )

# CNAE-2009 mapping for 2008+
get_sector_label_auto <- function(act, año) {
  case_when(
    año >= 2008 & act %in% 1:3   ~ "Agricultura, ganadería, silvicultura y pesca",
    año >= 2008 & act %in% 5:9   ~ "Industrias extractivas",
    año >= 2008 & act %in% 10:33 ~ "Industria manufacturera",
    año >= 2008 & act == 35      ~ "Suministro de energía eléctrica, gas, vapor y aire acondicionado",
    año >= 2008 & act %in% 36:39 ~ "Suministro de agua; gestión de residuos y descontaminación",
    año >= 2008 & act %in% 41:43 ~ "Construcción",
    año >= 2008 & act %in% 45:47 ~ "Comercio al por mayor y al por menor; reparación de vehículos",
    año >= 2008 & act %in% 49:53 ~ "Transporte y almacenamiento",
    año >= 2008 & act %in% 55:56 ~ "Hostelería",
    año >= 2008 & act %in% 58:63 ~ "Información y comunicaciones",
    año >= 2008 & act %in% 64:66 ~ "Actividades financieras y de seguros",
    año >= 2008 & act == 68      ~ "Actividades inmobiliarias",
    año >= 2008 & act %in% 69:75 ~ "Actividades profesionales, científicas y técnicas",
    año >= 2008 & act %in% 77:82 ~ "Actividades administrativas y servicios auxiliares",
    año >= 2008 & act == 84      ~ "Administración pública y defensa; seguridad social obligatoria",
    año >= 2008 & act == 85      ~ "Educación",
    año >= 2008 & act %in% 86:88 ~ "Actividades sanitarias y de servicios sociales",
    año >= 2008 & act %in% 90:93 ~ "Actividades artísticas, recreativas y de entretenimiento",
    año >= 2008 & act %in% 94:96 ~ "Otros servicios",
    año >= 2008 & act == 97      ~ "Actividades de los hogares como empleadores",
    año >= 2008 & act == 99      ~ "Actividades de organizaciones y organismos extraterritoriales",
    TRUE ~ NA_character_
  )
}

# Assign sectors: 2006-2007 via CNAE-93 table; 2008+ via function
datos_completos <- datos_completos %>%
  left_join(tabla_cnae93_2009, by = "ACT", relationship = "many-to-one") %>%
  mutate(
    sector_AU    = if_else(AÑO %in% c(2006, 2007), sector_AU, NA_character_),
    nombre_sector = if_else(AÑO %in% c(2006, 2007), nombre_sector, NA_character_)
  ) %>%
  mutate(
    ACT         = as.numeric(ACT),
    sector_temp = get_sector_label_auto(ACT, AÑO),
    sector_AU   = if_else(is.na(sector_AU), sector_temp, sector_AU)
  ) %>%
  select(-sector_temp)

# Verification
table(is.na(datos_completos$sector_AU), datos_completos$AÑO)


# ── 4. LABOUR PRODUCTIVITY VARIABLE ──────────────────────────
# Productivity = Real VAB (sector) / Employed workers (sector)

# 4a. Load and format GDP deflator (base 2015 = 100)
Deflactor_PIB   <- read_excel("Variables/Productividad laboral/Deflactor PIB.xls",
                               sheet = "Hoja1")
deflactor_largo <- Deflactor_PIB %>%
  pivot_longer(cols = everything(), names_to = "AÑO", values_to = "deflactor") %>%
  mutate(AÑO = as.integer(AÑO), deflactor = as.numeric(deflactor))

# Estimate 2024 deflator (assumed +2.3% growth)
def_2024        <- deflactor_largo %>% filter(AÑO == 2023) %>% pull(deflactor) * 1.023
deflactor_largo <- bind_rows(deflactor_largo, tibble(AÑO = 2024, deflactor = def_2024)) %>%
  arrange(AÑO)

# 4b. Load VAB by sector (INE National Accounts)
VABA21 <- read_excel("Variables/Productividad laboral/VABA21.xlsx",
                     range = "B9:S29", col_names = FALSE) %>%
  mutate(sector_AU = LETTERS[1:21]) %>%
  select(sector_AU, everything())

colnames(VABA21)[-1] <- as.character(2023:2006)

vab_largo <- VABA21 %>%
  pivot_longer(cols = -sector_AU, names_to = "AÑO", values_to = "VAB") %>%
  mutate(AÑO = as.integer(AÑO)) %>%
  arrange(sector_AU, AÑO)

# Estimate 2024 VAB (assumed +3.2% GDP growth, sector weights from 2022-2023)
crecimientos <- vab_largo %>%
  filter(AÑO %in% c(2022, 2023)) %>%
  group_by(sector_AU, AÑO) %>%
  summarise(VAB = sum(VAB, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = AÑO, values_from = VAB, names_prefix = "VAB_") %>%
  mutate(
    tasa_sectorial              = (VAB_2023 - VAB_2022) / VAB_2022,
    peso_sectorial_en_crecimiento = tasa_sectorial /
      (sum(VAB_2023, na.rm = TRUE) / sum(VAB_2022, na.rm = TRUE) - 1)
  )

vab_2024  <- crecimientos %>%
  mutate(AÑO = 2024, VAB = VAB_2023 * (1 + 0.032 * peso_sectorial_en_crecimiento)) %>%
  select(sector_AU, AÑO, VAB)

vab_largo <- bind_rows(vab_largo, vab_2024) %>% arrange(sector_AU, AÑO)

# 4c. Calculate real VAB
vab_real_agrupado <- vab_largo %>%
  left_join(deflactor_largo, by = "AÑO") %>%
  mutate(VAB_real = VAB / (deflactor / 100)) %>%
  arrange(sector_AU, AÑO)

# 4d. Employed workers per sector and year
ocupados_sector <- datos_completos %>%
  filter(TRAREM == 1, !is.na(sector_AU)) %>%
  group_by(AÑO, sector_AU) %>%
  summarise(OCUPADOS = sum(FACTOREL_CAL, na.rm = TRUE), .groups = "drop")

# 4e. Productivity = VAB_real / OCUPADOS (in euros per worker)
productividad_sector <- vab_real_agrupado %>%
  left_join(ocupados_sector, by = c("AÑO", "sector_AU")) %>%
  mutate(
    productividad_real = ifelse(is.na(OCUPADOS) | OCUPADOS == 0, 0,
                                (VAB_real / OCUPADOS) * 1e6),
    log_productividad  = ifelse(productividad_real > 0, log(productividad_real), NA)
  ) %>%
  select(sector_AU, AÑO, VAB_real, OCUPADOS, productividad_real, log_productividad) %>%
  arrange(sector_AU, AÑO)


# ── 5. TELEWORK VARIABLES ─────────────────────────────────────
# DOMICI == 2: intensive telework (>50% of days)
# DOMICI == 1: moderate telework (occasional)

# 5a. National aggregate
teletrabajo_agg <- datos_completos %>%
  filter(AÑO >= 2006, AÑO <= 2024) %>%
  mutate(
    t_intenso  = ifelse(DOMICI == 2, FACTOREL_CAL, 0),
    t_moderado = ifelse(DOMICI == 1, FACTOREL_CAL, 0)
  ) %>%
  group_by(AÑO) %>%
  summarise(
    Teletrabajo_Intenso  = 100 * sum(t_intenso,  na.rm = TRUE) / sum(FACTOREL_CAL, na.rm = TRUE),
    Teletrabajo_Moderado = 100 * sum(t_moderado, na.rm = TRUE) / sum(FACTOREL_CAL, na.rm = TRUE),
    .groups = "drop"
  )

# 5b. By sector
teletrabajo_sector <- datos_completos %>%
  filter(AÑO >= 2006, AÑO <= 2024, !is.na(sector_AU)) %>%
  mutate(
    t_intenso  = ifelse(DOMICI == 2, FACTOREL_CAL, 0),
    t_moderado = ifelse(DOMICI == 1, FACTOREL_CAL, 0)
  ) %>%
  group_by(AÑO, sector_AU) %>%
  summarise(
    Teletrabajo_Intenso  = 100 * sum(t_intenso,  na.rm = TRUE) / sum(FACTOREL_CAL, na.rm = TRUE),
    Teletrabajo_Moderado = 100 * sum(t_moderado, na.rm = TRUE) / sum(FACTOREL_CAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(AÑO = 2006:2024, sector_AU = LETTERS[1:21],
           fill = list(Teletrabajo_Intenso = 0, Teletrabajo_Moderado = 0))


# ── 6. CONTROL VARIABLES (FBCF AND UNEMPLOYMENT) ─────────────

FBCFa <- read_excel("Variables/Variables control/FBCFa.xlsx", sheet = "Hoja1")
FBCFa <- as.data.frame(t(FBCFa))
colnames(FBCFa) <- "FBCF"
FBCFa <- FBCFa %>%
  mutate(FBCF = as.numeric(gsub(",", ".", FBCF)),
         AÑO  = as.numeric(rownames(FBCFa))) %>%
  select(AÑO, FBCF)

Tasa_paroa <- read_excel("Variables/Variables control/Tasa_paroa.xlsx", range = "B8:T9")
Tasa_paroa <- as.data.frame(t(Tasa_paroa))
colnames(Tasa_paroa) <- "Desempleo"
Tasa_paroa <- Tasa_paroa %>%
  mutate(Desempleo = as.numeric(gsub(",", ".", Desempleo)),
         AÑO       = as.numeric(rownames(Tasa_paroa))) %>%
  select(AÑO, Desempleo)

datos_macro <- left_join(FBCFa, Tasa_paroa, by = "AÑO")


# ── 7. STATIONARITY TESTS (ADF) ───────────────────────────────
# Variables tested for sector J and macro aggregates
# Selection criterion: BIC

# Macro variables
ts_fbcf     <- ts(datos_macro %>% filter(!is.na(FBCF)) %>% arrange(AÑO) %>% pull(FBCF),
                  start = min(datos_macro$AÑO), frequency = 1)
ts_desempleo <- ts(datos_macro %>% filter(!is.na(Desempleo)) %>% arrange(AÑO) %>% pull(Desempleo),
                   start = min(datos_macro$AÑO), frequency = 1)

adf_fbcf     <- ur.df(ts_fbcf,     type = "drift", selectlags = "BIC")
adf_desempleo <- ur.df(ts_desempleo, type = "trend", selectlags = "BIC")

# Sector J
ts_prod_j <- ts(productividad_sector %>%
                  filter(sector_AU == "J", !is.na(productividad_real)) %>%
                  arrange(AÑO) %>% pull(productividad_real),
                start = 2006, frequency = 1)

ts_tt_i_j <- ts(teletrabajo_sector %>%
                  filter(sector_AU == "J") %>%
                  arrange(AÑO) %>% pull(Teletrabajo_Intenso),
                start = 2006, frequency = 1)

ts_tt_m_j <- ts(teletrabajo_sector %>%
                  filter(sector_AU == "J") %>%
                  arrange(AÑO) %>% pull(Teletrabajo_Moderado),
                start = 2006, frequency = 1)

adf_prod_j <- ur.df(ts_prod_j, type = "drift", selectlags = "BIC")
adf_tt_i_j <- ur.df(ts_tt_i_j, type = "trend", selectlags = "BIC")
adf_tt_m_j <- ur.df(ts_tt_m_j, type = "trend", selectlags = "BIC")

# Summary table
tabla_adf_J <- tibble(
  Variable           = c("Productividad (J)", "Teletrabajo Intenso (J)",
                         "Teletrabajo Moderado (J)", "FBCF", "Desempleo"),
  Tau                = c(adf_prod_j@teststat[1], adf_tt_i_j@teststat[1],
                         adf_tt_m_j@teststat[1], adf_fbcf@teststat[1],
                         adf_desempleo@teststat[1]),
  VC_5pct            = c(adf_prod_j@cval[1, "5pct"], adf_tt_i_j@cval[1, "5pct"],
                         adf_tt_m_j@cval[1, "5pct"], adf_fbcf@cval[1, "5pct"],
                         adf_desempleo@cval[1, "5pct"]),
  Estacionaria_5pct  = Tau < VC_5pct
)
print(tabla_adf_J)
# Result: Productivity, FBCF, Unemployment are I(0); Telework variables are I(1)


# ── 8. BUILD SECTOR DATASETS ──────────────────────────────────
# Helper function to build a sector dataset

build_sector <- function(sector_code) {
  productividad_sector %>%
    filter(sector_AU == sector_code) %>%
    select(AÑO, productividad = productividad_real) %>%
    left_join(teletrabajo_sector %>% filter(sector_AU == sector_code), by = "AÑO") %>%
    left_join(datos_macro, by = "AÑO") %>%
    rename(teletrabajo_intenso  = Teletrabajo_Intenso,
           teletrabajo_moderado = Teletrabajo_Moderado,
           fbcf                 = FBCF,
           desempleo            = Desempleo) %>%
    arrange(AÑO)
}

sector_a <- build_sector("A")
sector_f <- build_sector("F")
sector_j <- build_sector("J")
sector_k <- build_sector("K")
sector_l <- build_sector("L")
sector_m <- build_sector("M")
sector_n <- build_sector("N")
sector_q <- build_sector("Q")


# ── 9. PREPARE DIFFERENCED TIME SERIES FOR ARDL ───────────────
# Telework variables are I(1) → use first differences
# Productivity, FBCF, Unemployment are I(0) → use levels

build_ts <- function(sector_df) {
  mixto <- sector_df %>%
    mutate(
      d_teletrabajo_intenso  = c(NA, diff(teletrabajo_intenso)),
      d_teletrabajo_moderado = c(NA, diff(teletrabajo_moderado))
    ) %>%
    drop_na()

  ts(mixto[, c("productividad", "d_teletrabajo_intenso",
               "d_teletrabajo_moderado", "fbcf", "desempleo")],
     start = min(mixto$AÑO), frequency = 1)
}

sector_j_ts <- build_ts(sector_j)
sector_m_ts <- build_ts(sector_m)
sector_a_ts <- build_ts(sector_a)
sector_l_ts <- build_ts(sector_l)
sector_n_ts <- build_ts(sector_n)
sector_q_ts <- build_ts(sector_q)


# ── 10. OLS ASSUMPTIONS CHECK ────────────────────────────────
# Run on auto_ardl models (exploratory)

modelo_auto_ardl_J <- auto_ardl(
  productividad ~ teletrabajo_intenso + teletrabajo_moderado + fbcf + desempleo,
  data = sector_j, max_order = 1, selection = "BIC", case = 3
)

# Autocorrelation
dwtest(modelo_auto_ardl_J$best_model)

# Normality of residuals
shapiro.test(residuals(modelo_auto_ardl_J$best_model))

# Linearity (RESET test)
resettest(modelo_auto_ardl_J$best_model)

# Multicollinearity (VIF)
vif(modelo_auto_ardl_J$best_model)


# ── 11. ARDL MODELS — SECTOR J ────────────────────────────────
# Multiple specifications estimated to find parsimonious model.
# Final model selected: modelo_2a_j (R² adj = 0.78, F-stat p < 0.01)

# Model 1 (full): all variables with lags including FBCF
modelo_1_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    fbcf + L(fbcf, 1) +
    desempleo + L(desempleo, 1),
  data = sector_j_ts
)
summary(modelo_1_j)

# Model 2a (SELECTED): lags on telework and unemployment, no FBCF
modelo_2a_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    desempleo + L(desempleo, 1),
  data = sector_j_ts
)
summary(modelo_2a_j)
coeftest(modelo_2a_j, vcov = sandwich::vcovHAC(modelo_2a_j))

# Model 2b: no unemployment lag
modelo_2b_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    desempleo,
  data = sector_j_ts
)
summary(modelo_2b_j)

# Model 3a: FBCF with lag, unemployment without lag
modelo_3a_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso +
    d_teletrabajo_moderado +
    desempleo +
    fbcf + L(fbcf, 1),
  data = sector_j_ts
)
summary(modelo_3a_j)

# Model 3b: telework lags + FBCF, no unemployment
modelo_3b_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    fbcf,
  data = sector_j_ts
)
summary(modelo_3b_j)

# Model 4: parsimonious, no lags except productivity
modelo_4_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso +
    d_teletrabajo_moderado +
    desempleo + fbcf,
  data = sector_j_ts
)
summary(modelo_4_j)

# Model 5: no productivity lag
modelo_5_j <- dynlm(
  productividad ~
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    fbcf + L(fbcf, 1) +
    desempleo + L(desempleo, 1),
  data = sector_j_ts
)
summary(modelo_5_j)

# Model 6: telework only (no macro controls)
modelo_6_j <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1),
  data = sector_j_ts
)
summary(modelo_6_j)


# ── 12. RESIDUAL DIAGNOSTICS — SECTOR J (MODEL 2a) ───────────

residuos_2a_j <- residuals(modelo_2a_j)

# Ljung-Box test for autocorrelation
Box.test(residuos_2a_j, lag = 4, type = "Ljung-Box")

# ACF plot — should show no significant spikes
acf(residuos_2a_j, main = "ACF de los residuos del modelo 2a_j (sector J)")

# Covariance matrix and eigenvalues (multicollinearity check)
X_j          <- model.matrix(modelo_2a_j)[, -1]
matriz_corr_j <- cor(X_j)
eigen_j       <- eigen(matriz_corr_j)
print(round(eigen_j$values, 3))

colnames(eigen_j$vectors) <- paste0("Componente_", 1:ncol(eigen_j$vectors))
rownames(eigen_j$vectors) <- colnames(X_j)
print(round(eigen_j$vectors, 3))


# ── 13. ARDL MODELS — SECTOR M ────────────────────────────────

modelo_2a_m <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    desempleo + L(desempleo, 1),
  data = sector_m_ts
)
summary(modelo_2a_m)
coeftest(modelo_2a_m, vcov = sandwich::vcovHAC(modelo_2a_m))

modelo_2b_m <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso  + L(d_teletrabajo_intenso, 1) +
    d_teletrabajo_moderado + L(d_teletrabajo_moderado, 1) +
    desempleo,
  data = sector_m_ts
)
summary(modelo_2b_m)

modelo_m_parsimonioso <- dynlm(
  productividad ~
    L(productividad, 1) +
    d_teletrabajo_intenso +
    d_teletrabajo_moderado +
    desempleo,
  data = sector_m_ts
)
coeftest(modelo_m_parsimonioso, vcov = sandwich::vcovHAC(modelo_m_parsimonioso))


# ── 14. RESIDUAL DIAGNOSTICS — SECTOR M ──────────────────────

residuos_m <- residuals(modelo_2a_m)
Box.test(residuos_m, lag = 4, type = "Ljung-Box")
acf(residuos_m, main = "ACF de los residuos del modelo 2a (sector M)")

X_m           <- model.matrix(modelo_2a_m)[, -1]
matriz_corr_m <- cor(X_m)
eigen_m       <- eigen(matriz_corr_m)
print(round(eigen_m$values, 3))


# ── 15. TRANSFER FUNCTION AND IMPULSE RESPONSE — SECTOR J ────
# Based on selected model 2a_j coefficients

# Coefficients from modelo_2a_j:
#   d_teletrabajo_moderado      = -2773
#   L(d_teletrabajo_moderado,1) = -4272
#   d_teletrabajo_intenso       = -502.2
#   L(d_teletrabajo_intenso,1)  = -288.5
#   L(productividad,1)          =  0.01264

phi1    <- coef(modelo_2a_j)["L(productividad, 1)"]
beta0_m <- coef(modelo_2a_j)["d_teletrabajo_moderado"]
beta1_m <- coef(modelo_2a_j)["L(d_teletrabajo_moderado, 1)"]
beta0_i <- coef(modelo_2a_j)["d_teletrabajo_intenso"]
beta1_i <- coef(modelo_2a_j)["L(d_teletrabajo_intenso, 1)"]

# Impulse response: effect of a 1pp shock in telework at horizons h = 0,1,2,3
h <- 0:3

# Moderate telework
IR_moderado <- sapply(h, function(t) {
  if (t == 0) return(beta0_m)
  if (t == 1) return(beta0_m * phi1 + beta1_m)
  beta0_m * phi1^t + beta1_m * phi1^(t - 1)
})

# Intensive telework
IR_intenso <- sapply(h, function(t) {
  if (t == 0) return(beta0_i)
  if (t == 1) return(beta0_i * phi1 + beta1_i)
  beta0_i * phi1^t + beta1_i * phi1^(t - 1)
})

cat("\nImpulse Response — Moderate Telework (euros/worker):\n")
print(round(data.frame(h, IR_moderado), 2))

cat("\nImpulse Response — Intensive Telework (euros/worker):\n")
print(round(data.frame(h, IR_intenso), 2))
