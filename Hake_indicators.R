#------------------------------------------------------------------------------
# Script to estimate misreporting and unreporting of Hake
# - Trip-by-trip cross-referencing between IFOP and Sernapesca
# - Calculation of deterministic and bootstrap indicators
# - Weighted expansion factors (cove, regional, national levels)
# - Reconstructed real catch estimator
#
# Developed by: Daniela Yepsen & José Zenteno (IFOP)
#------------------------------------------------------------------------------

rm(list = ls())

# 0) Initial Configuration & Package Loading
library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(writexl)

# Set seed for overall script reproducibility
set.seed(123)

#------------------------------------------------------------------------------
# 1) Data Loading and Base Preparation
#------------------------------------------------------------------------------

# Dynamic path resolution via here() ensures execution across different machines
maestro     <- read.csv(here("Data/Maestro_embarcaciones.csv"))
merluza_raw <- read.csv(here("Data/merluza_ifop_2014_2024.csv"))
serna_raw   <- read.csv(here("Data/merluza_sernapesca_2014_2024.csv"))

# Homogenize vessel registry (RPA) columns within the vessel master list
maestro <- maestro |>
  mutate(
    COD_BARCO              = as.character(COD_BARCO),
    RPA_VV                 = as.character(RPA_VV),
    SERNAP_EMBARCACION_rpa = as.character(SERNAP_EMBARCACION_rpa),
    RPA_2                  = as.character(RPA_2),
    `3_rpa`                = as.character(`X3_rpa`),
    `4_rpa`                = as.character(`X4_rpa`)
  )

# Process IFOP observed trips database
merluza_if <- merluza_raw |>
  mutate(
    COD_BARCO           = as.character(COD_BARCO),
    FECHA_HORA_RECALADA = as.POSIXct(FECHA_HORA_RECALADA),
    FECHA_HORA_ZARPE    = as.POSIXct(FECHA_HORA_ZARPE),
    año                 = as.integer(año),
    REGION_PUERTO_RECALADA = as.integer(REGION_PUERTO_RECALADA),
    NOMBRE_PUERTO       = as.character(NOMBRE_PUERTO)
  ) |>
  filter(COD_ESPECIE == 1) # Target species filter

# Quality control: check for unmapped landing dates
unmapped_dates <- merluza_if |>
  filter(is.na(FECHA_HORA_RECALADA)) |>
  distinct(FECHA_HORA_RECALADA)

# Filter out regions with fewer than 3 observed trips to avoid outlier noise
valid_regions <- merluza_if |>
  count(REGION_PUERTO_RECALADA) |>
  filter(n >= 3) |>
  pull(REGION_PUERTO_RECALADA)

merluza_if <- merluza_if |>
  filter(REGION_PUERTO_RECALADA %in% valid_regions)

# Process Sernapesca official landing declarations
serna_merlu <- serna_raw |>
  mutate(
    Cd_Nave    = as.character(Cd_Nave),
    Fc_Llegada = as.Date(Fc_Llegada),
    yr         = as.integer(yr)
  ) |>
  filter(Cd_Especie == 242, yr >= 2014, yr <= 2024)

# Consolidate Sernapesca records by unique trip identifiers
serna_merluza <- serna_merlu |>
  group_by(Nr_Folio, Cd_Arte) |>
  summarise(
    Desembarque   = sum(Desembarque, na.rm = TRUE),
    Cd_Nave       = first(Cd_Nave),
    Fc_Llegada    = first(Fc_Llegada),
    NM_ARTE       = first(NM_ARTE),
    yr            = first(yr),
    REGION_PUERTO_RECALADA = first(Region_Puerto),
    .groups = "drop"
  )

# Verify single-row consolidation per Folio-Arte
check_duplicates <- serna_merluza |> 
  count(Nr_Folio, Cd_Arte) |> 
  filter(n > 1)

if(nrow(check_duplicates) == 0) {
  message("Data integrity verified: Single row per Folio and Gear category.")
} else {
  warning("Data anomaly: Duplicate entries found at Folio-Gear level.")
}

totales_sernapesca <- serna_merluza |>
  count(año = yr, REGION_PUERTO_RECALADA, name = "Total_Sernapesca")

#--------------------------------------------------------------
# 2) Mapping Universe: Sernapesca vs Vessel Master List (RPA)
#--------------------------------------------------------------

rpa_serna <- serna_merluza |>
  distinct(RPA = Cd_Nave)

rpa_maestro_long <- maestro |>
  transmute(
    RPA_VV,
    RPA_2,
    RPA_SERNAP = SERNAP_EMBARCACION_rpa,
    RPA_3      = `3_rpa`,
    RPA_4      = `4_rpa`
  ) |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(
    cols      = everything(),
    names_to  = "RPA_type",
    values_to = "RPA"
  ) |>
  filter(!is.na(RPA) & RPA != "") |>
  distinct(RPA, RPA_type)

match_rpa_serna_maestro <- rpa_serna |>
  left_join(rpa_maestro_long, by = "RPA") |>
  mutate(has_master_match = !is.na(RPA_type))

global_master_coverage <- match_rpa_serna_maestro |>
  count(has_master_match, name = "n_vessels")

#--------------------------------------------------------------
# 3) IFOP Observed Trips Identification & Gear Harmonization
#--------------------------------------------------------------

maestro_rpa_prior <- maestro |>
  mutate(
    REGION_PUERTO_RECALADA = REGION_PUERTO,
    RPA_PRIOR = case_when(
      !is.na(RPA_VV)                 & RPA_VV                 != "" ~ RPA_VV,
      !is.na(SERNAP_EMBARCACION_rpa) & SERNAP_EMBARCACION_rpa != "" ~ SERNAP_EMBARCACION_rpa,
      !is.na(RPA_2)                  & RPA_2                  != "" ~ RPA_2,
      !is.na(`3_rpa`)                & `3_rpa`                != "" ~ `3_rpa`,
      !is.na(`4_rpa`)                & `4_rpa`                != "" ~ `4_rpa`,
      TRUE ~ NA_character_
    )
  ) |>
  arrange(COD_BARCO, desc(!is.na(RPA_PRIOR))) |>
  distinct(COD_BARCO, .keep_all = TRUE)

ifop_maestro <- merluza_if |>
  left_join(maestro_rpa_prior |> select(COD_BARCO, RPA_PRIOR), by = "COD_BARCO") |>
  mutate(
    RPA_PRIOR = as.character(RPA_PRIOR),
    tiene_RPA = !is.na(RPA_PRIOR) & RPA_PRIOR != ""
  )

# Gear categorization harmonization
merluza_if <- merluza_if |>
  mutate(
    ARTE_IFOP = case_when(
      Arte %in% c("Enmalle", "enmalle") ~ "ENMALLE",
      Arte %in% c("Espínel", "Espinel", "ESPINEL") ~ "ESPINEL",
      TRUE ~ "OTROS"
    )
  )

serna_merluza <- serna_merluza |>
  mutate(
    ARTE_SERNA = case_when(
      NM_ARTE %in% c("ENMALLE", "Enmalle") ~ "ENMALLE",
      NM_ARTE %in% c("ESPINEL", "Espinel") ~ "ESPINEL",
      TRUE ~ "OTROS"
    )
  )

#--------------------------------------------------------------
# 4) Trip-by-Trip Match Automation (IFOP vs Sernapesca)
#--------------------------------------------------------------

ifop_con_rpa <- merluza_if |>
  left_join(maestro_rpa_prior |> select(COD_BARCO, RPA_PRIOR), by = "COD_BARCO") |>
  mutate(
    RPA_PRIOR          = as.character(RPA_PRIOR),
    fecha_recalada_date = as.Date(FECHA_HORA_RECALADA)
  )

# Introduce temporal window constraint (+/- 2 days)
serna_pre_join <- serna_merluza |>
  mutate(
    fecha_min = Fc_Llegada - 2,
    fecha_max = Fc_Llegada + 2
  )

ifop_serna_join <- ifop_con_rpa |> 
  inner_join(
    serna_pre_join |> rename(REGION_SERNA = REGION_PUERTO_RECALADA),
    by = join_by(
      RPA_PRIOR == Cd_Nave,
      ARTE_IFOP == ARTE_SERNA,
      fecha_recalada_date >= fecha_min,
      fecha_recalada_date <= fecha_max
    ),
    relationship = "many-to-many"
  )

# Optimization: resolve tie-breaks using absolute minimum days difference
ifop_serna_cruce <- ifop_serna_join |> 
  mutate(dif_dias = abs(as.numeric(fecha_recalada_date - Fc_Llegada))) |>
  group_by(COD_BARCO, FECHA_HORA_RECALADA, ARTE_IFOP) |> 
  slice_min(order_by = dif_dias, n = 1, with_ties = FALSE) |> 
  ungroup()

# Consolidate weight data comparison metrics
ifop_serna_comp <- ifop_serna_cruce |> 
  group_by(año, COD_BARCO, FECHA_HORA_RECALADA, ARTE_IFOP, REGION_PUERTO_RECALADA, NOMBRE_PUERTO) |>
  summarise(
    PESO        = first(PESO), 
    Desembarque = sum(Desembarque, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(diff_peso = PESO - Desembarque)

#--------------------------------------------------------------
# 5) Strata Summaries Formulation
#--------------------------------------------------------------

res_global_anio <- ifop_serna_comp |>
  group_by(año) |>
  summarise(
    n_registros_cruce = n(),
    peso_ifop_total   = sum(PESO, na.rm = TRUE),
    peso_serna_total  = sum(Desembarque, na.rm = TRUE),
    diff_peso_total   = sum(diff_peso, na.rm = TRUE),
    .groups           = "drop"
  )

res_region_anio <- ifop_serna_comp |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(
    n_registros_cruce = n(),
    peso_ifop_total   = sum(PESO, na.rm = TRUE),
    peso_serna_total  = sum(Desembarque, na.rm = TRUE),
    diff_peso_total   = sum(diff_peso, na.rm = TRUE),
    .groups           = "drop"
  )

res_region_anio_puerto <- ifop_serna_comp |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO) |>
  summarise(
    n_registros_cruce = n(),
    peso_ifop_total   = sum(PESO, na.rm = TRUE),
    peso_serna_total  = sum(Desembarque, na.rm = TRUE),
    diff_peso_total   = sum(diff_peso, na.rm = TRUE),
    .groups           = "drop"
  )

res_region_anio_arte <- ifop_serna_comp |>
  group_by(año, REGION_PUERTO_RECALADA, ARTE_IFOP) |>
  summarise(
    n_viajes_ifop    = n_distinct(COD_BARCO, FECHA_HORA_RECALADA),
    peso_ifop_total  = sum(PESO, na.rm = TRUE),
    peso_serna_total = sum(Desembarque, na.rm = TRUE),
    .groups          = "drop"
  )

# Valid strata matrix criteria
res_region_anio_arte_validas <- res_region_anio_arte |>
  filter(n_viajes_ifop >= 5, peso_ifop_total > 0, peso_serna_total > 0)

#--------------------------------------------------------------
# 6) Analytical Assumption Checks (Statistical Tests)
#--------------------------------------------------------------

### Here include sensitivity andalysis of temporal window constraint


#--------------------------------------------------------------
# 7) Deterministic and Pondered Bias Ratios Calculations
#--------------------------------------------------------------

indicador_region_factor <- res_region_anio |>
  mutate(
    factor_ifop_serna = ifelse(peso_serna_total > 0, peso_ifop_total / peso_serna_total, NA_real_),
    REGION_PUERTO_RECALADA = factor(REGION_PUERTO_RECALADA, levels = 4:8)
  )

# Unweighted national mean overview
indicador_global_no_pond <- indicador_region_factor |>
  group_by(año) |>
  summarise(factor_ifop_serna = mean(factor_ifop_serna, na.rm = TRUE), .groups = "drop")

factor_puerto <- res_region_anio_puerto |>
  mutate(factor_ifop_serna = ifelse(peso_serna_total > 0, peso_ifop_total / peso_serna_total, NA_real_))

# 7.1 Regional expansion values pondered by coves volume
factor_region_pond_puerto <- factor_puerto |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(
    w_sum  = sum(peso_serna_total, na.rm = TRUE),
    factor_pond = ifelse(w_sum > 0, sum(factor_ifop_serna * peso_serna_total, na.rm = TRUE) / w_sum, NA_real_),
    var_pond    = ifelse(w_sum > 0, sum(peso_serna_total * (factor_ifop_serna - factor_pond)^2, na.rm = TRUE) / w_sum, NA_real_),
    sd_pond     = sqrt(var_pond),
    .groups     = "drop"
  ) |>
  left_join(
    factor_puerto |>
      group_by(año, REGION_PUERTO_RECALADA) |>
      summarise(n_caletas = sum(peso_serna_total > 0, na.rm = TRUE), .groups = "drop"),
    by = c("año", "REGION_PUERTO_RECALADA")
  ) |>
  mutate(
    se_pond = ifelse(!is.na(sd_pond) & n_caletas > 0, sd_pond / sqrt(n_caletas), NA_real_),
    ic_inf  = factor_pond - 1.96 * se_pond,
    ic_sup  = factor_pond + 1.96 * se_pond,
    REGION_PUERTO_RECALADA = factor(REGION_PUERTO_RECALADA, levels = 4:8)
  )

# 7.2 National expansion values pondered by official regional landings
factor_region_pond_puerto_num <- factor_region_pond_puerto |>
  mutate(REGION_PUERTO_RECALADA = as.integer(as.character(REGION_PUERTO_RECALADA)))

peso_region <- factor_puerto |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(peso_region_serna = sum(peso_serna_total, na.rm = TRUE), .groups = "drop")

factor_nacional_pond_region <- factor_region_pond_puerto_num |>
  left_join(peso_region, by = c("año", "REGION_PUERTO_RECALADA")) |>
  group_by(año) |>
  summarise(
    w_sum_reg       = sum(peso_region_serna, na.rm = TRUE),
    factor_nac_pond = ifelse(w_sum_reg > 0, sum(factor_pond * peso_region_serna, na.rm = TRUE) / w_sum_reg, NA_real_),
    var_nac_pond    = ifelse(w_sum_reg > 0, sum(peso_region_serna * (factor_pond - factor_nac_pond)^2, na.rm = TRUE) / w_sum_reg, NA_real_),
    sd_nac_pond     = sqrt(var_nac_pond),
    n_regiones      = sum(peso_region_serna > 0, na.rm = TRUE),
    .groups         = "drop"
  ) |>
  mutate(
    se_nac = ifelse(!is.na(sd_nac_pond) & n_regiones > 0, sd_nac_pond / sqrt(n_regiones), NA_real_),
    ic_inf = factor_nac_pond - 1.96 * se_nac,
    ic_sup = factor_nac_pond + 1.96 * se_nac
  )

#--------------------------------------------------------------
# 7.3 Strategic Key-Coves Indicator Extraction Module
#--------------------------------------------------------------
caletas_clave       <- c("PORTALES", "El MEMBRILLO", "DUAO", "CURANIPE", "CONSTITUCION")
factor_puerto_clave <- res_region_anio_puerto |>
  filter(NOMBRE_PUERTO %in% caletas_clave) |>
  mutate(factor_ifop_serna = ifelse(peso_serna_total > 0, peso_ifop_total / peso_serna_total, NA_real_))

factor_caletas_clave <- factor_puerto_clave |>
  group_by(año) |>
  summarise(
    w_sum       = sum(peso_serna_total, na.rm = TRUE),
    factor_pond = ifelse(w_sum > 0, sum(factor_ifop_serna * peso_serna_total, na.rm = TRUE) / w_sum, NA_real_),
    var_pond    = ifelse(w_sum > 0, sum(peso_serna_total * (factor_ifop_serna - factor_pond)^2, na.rm = TRUE) / w_sum, NA_real_),
    sd_pond     = sqrt(var_pond),
    n_caletas   = sum(peso_serna_total > 0, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(
    se_pond = ifelse(!is.na(sd_pond) & n_caletas > 0, sd_pond / sqrt(n_caletas), NA_real_),
    ic_inf  = factor_pond - 1.96 * se_pond,
    ic_sup  = factor_pond + 1.96 * se_pond
  )

# Export intermediate processed data frames safely to Excel sheet
export_region_pond <- factor_region_pond_puerto |>
  select(año, REGION_PUERTO_RECALADA, factor_ponderado = factor_pond, sd_ponderada = sd_pond, se_ponderado = se_pond, n_caletas, ic_inf, ic_sup)

export_nacional_pond <- factor_nacional_pond_region |>
  select(año, factor_nacional_ponderado = factor_nac_pond, sd_nacional_ponderada = sd_nac_pond, se_nacional = se_nac, n_regiones, ic_inf, ic_sup)

export_caleta <- factor_puerto |>
  select(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, peso_ifop_total, peso_serna_total, factor_ifop_serna)

write_xlsx(
  list(
    "Cove_Factors"             = export_caleta,
    "Weighted_Regional_Factor" = export_region_pond,
    "Weighted_National_Factor" = export_nacional_pond
  ),
  path = "factores_ponderados_merluza_con_IC.xlsx"
)

#--------------------------------------------------------------
# 8) Non-Parametric Bootstrap Framework (1,000 Replications)
#--------------------------------------------------------------
set.seed(123)
B <- 1000

bootstrap_coef <- function(df_sub) {
  map_dfr(1:B, function(b) {
    muestra    <- slice_sample(df_sub, prop = 1, replace = TRUE)
    peso_ifop  <- sum(muestra$PESO, na.rm = TRUE)
    peso_serna <- sum(muestra$Desembarque, na.rm = TRUE)
    tibble(boot = b, coef = ifelse(peso_serna > 0, peso_ifop / peso_serna, NA_real_))
  })
}

# 8.1 National Unweighted Bootstrapping Loop
boot_nal_coef <- ifop_serna_comp |>
  group_by(año) |>
  group_modify(~ bootstrap_coef(.x)) |>
  ungroup()

coef_sp_nal <- boot_nal_coef |>
  group_by(año) |>
  summarise(
    coef_mean = mean(coef, na.rm = TRUE),
    coef_sd   = sd(coef, na.rm = TRUE),
    coef_lo   = quantile(coef, 0.025, na.rm = TRUE),
    coef_hi   = quantile(coef, 0.975, na.rm = TRUE),
    .groups   = "drop"
  )

# 8.2 Regional Unweighted Bootstrapping Loop
boot_reg_coef <- ifop_serna_comp |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  group_modify(~ bootstrap_coef(.x)) |>
  ungroup()

coef_sp_reg <- boot_reg_coef |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(
    coef_mean = mean(coef, na.rm = TRUE),
    coef_sd   = sd(coef, na.rm = TRUE),
    coef_lo   = quantile(coef, 0.025, na.rm = TRUE),
    coef_hi   = quantile(coef, 0.975, na.rm = TRUE),
    .groups   = "drop"
  )

# 8.3 Gear-by-Region Unweighted Bootstrapping Loop
boot_reg_arte <- ifop_serna_comp |>
  semi_join(res_region_anio_arte_validas, by = c("año", "REGION_PUERTO_RECALADA", "ARTE_IFOP")) |>
  group_by(año, REGION_PUERTO_RECALADA, ARTE_IFOP) |>
  group_modify(~ bootstrap_coef(.x)) |>
  ungroup()

#--------------------------------------------------------------
# 9) Nested Stratified Multi-level Bootstrap Weights Calibration
#--------------------------------------------------------------

serna_reg_cal_art <- ifop_serna_comp |>
  semi_join(res_region_anio_arte_validas, by = c("año", "REGION_PUERTO_RECALADA", "ARTE_IFOP")) |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, ARTE_IFOP) |>
  summarise(des_serna = sum(Desembarque, na.rm = TRUE), .groups = "drop")

pesos_cal_arte <- serna_reg_cal_art |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO) |>
  mutate(w_arte = des_serna / sum(des_serna, na.rm = TRUE)) |>
  ungroup()

pesos_reg_cal <- serna_reg_cal_art |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO) |>
  summarise(des_reg_cal = sum(des_serna, na.rm = TRUE), .groups = "drop") |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  mutate(w_cal = des_reg_cal / sum(des_reg_cal, na.rm = TRUE)) |>
  ungroup() |>
  select(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, w_cal)

pesos_reg_nal <- serna_reg_cal_art |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(des_reg = sum(des_serna, na.rm = TRUE), .groups = "drop") |>
  group_by(año) |>
  mutate(w_reg = des_reg / sum(des_reg, na.rm = TRUE)) |>
  ungroup()

boot_cal_arte <- ifop_serna_comp |>
  semi_join(res_region_anio_arte_validas, by = c("año", "REGION_PUERTO_RECALADA", "ARTE_IFOP")) |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, ARTE_IFOP) |>
  group_modify(~ bootstrap_coef(.x)) |>
  ungroup()

boot_cal_pond <- boot_cal_arte |>
  inner_join(pesos_cal_arte, by = c("año", "REGION_PUERTO_RECALADA", "NOMBRE_PUERTO", "ARTE_IFOP")) |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, boot) |>
  summarise(coef = sum(w_arte * coef, na.rm = TRUE), .groups = "drop")

boot_reg_pond <- boot_cal_pond |>
  inner_join(pesos_reg_cal, by = c("año", "REGION_PUERTO_RECALADA", "NOMBRE_PUERTO")) |>
  group_by(año, REGION_PUERTO_RECALADA, boot) |>
  summarise(coef = sum(w_cal * coef, na.rm = TRUE), .groups = "drop")

coef_reg_pond <- boot_reg_pond |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(
    coef_mean = mean(coef, na.rm = TRUE),
    coef_sd   = sd(coef, na.rm = TRUE),
    coef_lo   = quantile(coef, 0.025, na.rm = TRUE),
    coef_hi   = quantile(coef, 0.975, na.rm = TRUE),
    .groups   = "drop"
  )

boot_nal_pond <- boot_reg_pond |>
  inner_join(pesos_reg_nal, by = c("año", "REGION_PUERTO_RECALADA")) |>
  group_by(año, boot) |>
  summarise(coef = sum(w_reg * coef, na.rm = TRUE), .groups = "drop")

coef_nal_pond <- boot_nal_pond |>
  group_by(año) |>
  summarise(
    coef_mean = mean(coef, na.rm = TRUE),
    coef_sd   = sd(coef, na.rm = TRUE),
    coef_lo   = quantile(coef, 0.025, na.rm = TRUE),
    coef_hi   = quantile(coef, 0.975, na.rm = TRUE),
    .groups   = "drop"
  )

#--------------------------------------------------------------
# 10) Under-declaration Metrics (Unreporting Calculation)
#--------------------------------------------------------------

viajes_ifop_base <- merluza_if |>
  select(año, COD_BARCO, FECHA_HORA_RECALADA, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, ARTE_IFOP, PESO)

viajes_ifop_match_flag <- viajes_ifop_base |>
  left_join(viajes_ifop_con_match, by = c("año", "COD_BARCO", "FECHA_HORA_RECALADA")) |>
  mutate(tiene_match = if_else(is.na(tiene_match), FALSE, TRUE))

subdecl_reg_arte_puerto <- viajes_ifop_match_flag |>
  group_by(año, REGION_PUERTO_RECALADA, NOMBRE_PUERTO, ARTE_IFOP) |>
  summarise(
    n_ifop_total   = n(),
    n_ifop_match   = sum(tiene_match),
    ton_ifop_total = sum(PESO, na.rm = TRUE),
    ton_ifop_match = sum(if_else(tiene_match, PESO, 0), na.rm = TRUE),
    p_match_viaje  = n_ifop_match / n_ifop_total,
    p_match_ton    = ton_ifop_match / ton_ifop_total,
    F_viaje        = if_else(p_match_viaje > 0, 1 / p_match_viaje, NA_real_),
    F_ton          = if_else(p_match_ton > 0, 1 / p_match_ton, NA_real_),
    .groups        = "drop"
  )

subdecl_reg_arte_merluza_agg <- subdecl_reg_arte_puerto |>
  group_by(año, REGION_PUERTO_RECALADA, ARTE_IFOP) |>
  summarise(
    ton_ifop_total = sum(ton_ifop_total, na.rm = TRUE),
    ton_ifop_match = sum(ton_ifop_match, na.rm = TRUE),
    p_match_ton    = ton_ifop_match / ton_ifop_total,
    F_ton          = if_else(p_match_ton > 0, 1 / p_match_ton, NA_real_),
    .groups        = "drop"
  )

# 10.1 Sampling coverage ratios metrics over official data
serna_total_anio_merluza <- serna_merluza |>
  group_by(yr) |>
  summarise(ton_serna_total = sum(Desembarque, na.rm = TRUE), .groups = "drop") |>
  rename(año = yr)

#--------------------------------------------------------------
# 11) National Landings Corrected Estimation Integration
#--------------------------------------------------------------

captura_real_nal_pond_merluza <- serna_total_anio_merluza |>
  left_join(coef_nal_pond |> select(año, coef_mean, coef_lo, coef_hi), by = "año") |>
  mutate(
    cap_real_mean = ton_serna_total * coef_mean,
    cap_real_lo   = ton_serna_total * coef_lo,
    cap_real_hi   = ton_serna_total * coef_hi
  )

# 11.1 Unreporting factor expansion bootstrap structure
bootstrap_F_subdecl <- function(df_sub) {
  map_dfr(1:B, function(b) {
    muestra       <- slice_sample(df_sub, prop = 1, replace = TRUE)
    n_ifop_total  <- nrow(muestra)
    n_ifop_match  <- sum(muestra$tiene_match, na.rm = TRUE)
    p_match_viaje <- ifelse(n_ifop_total > 0, n_ifop_match / n_ifop_total, NA_real_)
    tibble(boot = b, F_subdecl = ifelse(p_match_viaje > 0, 1 / p_match_viaje, NA_real_))
  })
}

boot_subdecl_nal <- viajes_ifop_match_flag |>
  group_by(año) |>
  group_modify(~ bootstrap_F_subdecl(.x)) |>
  ungroup()

subdecl_nal_ci <- boot_subdecl_nal |>
  group_by(año) |>
  summarise(
    F_mean  = mean(F_subdecl, na.rm = TRUE),
    F_sd    = sd(F_subdecl, na.rm = TRUE),
    F_lo    = quantile(F_subdecl, 0.025, na.rm = TRUE),
    F_hi    = quantile(F_subdecl, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

#--------------------------------------------------------------
# 12) Real Reconstructed Catch Strata Stratification (Main Model)
#--------------------------------------------------------------

boot_region_arte2_merluza <- boot_reg_arte

serna_total_reg_arte_merluza <- serna_merluza |>
  group_by(año = yr, REGION_PUERTO_RECALADA, ARTE_IFOP = NM_ARTE) |>
  summarise(ton_serna_total = sum(Desembarque, na.rm = TRUE), .groups = "drop")

cap_real_reg_arte_boot_merluza <- boot_region_arte2_merluza |>
  left_join(serna_total_reg_arte_merluza, by = c("año", "REGION_PUERTO_RECALADA", "ARTE_IFOP")) |>
  left_join(subdecl_reg_arte_merluza_agg, by = c("año", "REGION_PUERTO_RECALADA", "ARTE_IFOP")) |>
  mutate(
    F_subdecl     = F_ton,
    cap_real_boot = ton_serna_total * coef * F_subdecl
  )

cap_real_reg_arte_nal_merluza <- cap_real_reg_arte_boot_merluza |>
  group_by(año, boot) |>
  summarise(cap_real_boot = sum(cap_real_boot, na.rm = TRUE), .groups = "drop") |>
  group_by(año) |>
  summarise(
    cap_real_mean = mean(cap_real_boot, na.rm = TRUE),
    cap_real_lo   = quantile(cap_real_boot, 0.025, na.rm = TRUE),
    cap_real_hi   = quantile(cap_real_boot, 0.975, na.rm = TRUE),
    .groups       = "drop"
  )

factor_expansion_total_merluza <- cap_real_reg_arte_nal_merluza |>
  left_join(serna_total_anio_merluza, by = "año") |>
  mutate(
    F_expansion_mean = cap_real_mean / ton_serna_total,
    F_expansion_lo   = cap_real_lo / ton_serna_total,
    F_expansion_hi   = cap_real_hi / ton_serna_total
  )

#--------------------------------------------------------------
# 13) Regional Component Breakdown (Double Panel Infrastructure)
#--------------------------------------------------------------

bootstrap_F_subdecl_reg <- function(df_sub) {
  map_dfr(1:B, function(b) {
    muestra       <- slice_sample(df_sub, prop = 1, replace = TRUE)
    n_ifop_total  <- nrow(muestra)
    n_ifop_match  <- sum(muestra$tiene_match, na.rm = TRUE)
    p_match_viaje <- ifelse(n_ifop_total > 0, n_ifop_match / n_ifop_total, NA_real_)
    tibble(boot = b, F_subdecl = ifelse(p_match_viaje > 0, 1 / p_match_viaje, NA_real_))
  })
}

boot_subdecl_reg <- viajes_ifop_match_flag |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  group_modify(~ bootstrap_F_subdecl_reg(.x)) |>
  ungroup()

coef_subdecl_reg <- boot_subdecl_reg |>
  group_by(año, REGION_PUERTO_RECALADA) |>
  summarise(
    coef_mean = mean(F_subdecl, na.rm = TRUE),
    coef_sd   = sd(F_subdecl, na.rm = TRUE),
    coef_lo   = quantile(F_subdecl, 0.025, na.rm = TRUE),
    coef_hi   = quantile(F_subdecl, 0.975, na.rm = TRUE),
    .groups   = "drop"
  )

#--------------------------------------------------------------
# 14) Consolidated Parameters Final Exports Module
#--------------------------------------------------------------

fact_reporte_nal   <- coef_nal_pond |> select(año, F_reporte_mean = coef_mean, F_reporte_lo = coef_lo, F_reporte_hi = coef_hi)
fact_subdecl_nal   <- subdecl_nal_ci |> select(año, F_subdecl_mean = F_mean, F_subdecl_lo = F_lo, F_subdecl_hi = F_hi)
fact_expansion_nal <- factor_expansion_total_merluza |> select(año, F_expansion_mean, F_expansion_lo, F_expansion_hi)

factores_nal_ic <- fact_reporte_nal |>
  left_join(fact_subdecl_nal, by = "año") |>
  left_join(fact_expansion_nal, by = "año") |>
  arrange(año)

write_xlsx(
  list(
    "National_Correction_Factors" = factores_nal_ic,
    "Total_Expansion_Estimation"  = factor_expansion_total_merluza
  ),
  path = "factores_nacionales_con_IC_merluza.xlsx"
)

message("Process completed successfully. Reconstructed data frames and parameters saved.")
#------------------------------------------------------------------------------
# End of reproducible analytical script.
#------------------------------------------------------------------------------