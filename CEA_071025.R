#### CADe cost-effectiveness analysis ####
# Order: packages -> inputs/helpers -> main analysis -> OSA -> MWSA -> tables -> Excel

#### 0) PACKAGES ####
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(scales)
  library(writexl)
})

options(dplyr.summarise.inform = FALSE)

#### 1) INPUTS & HELPERS ####

## 1a) README (sheet descriptions)
readme_tbl <- tribble(
  ~Sheet, ~Description,
  "Baseline", "Discounted device (annualized) + follow-ups; outcomes include CRC incidence and death prevented and cost-per-case.",
  "One_way", "One-way sensitivity: (a) operational drivers, (b) CRC incidence/death ARR CIs, (c) Additional follow-up colonoscopies (per 10k) 95% CI from article. 'N/A' means ARR <= 0 (no cases prevented).",
  "FU_guideline", "Adds article-based DeltaHigh (US-MSTF/ESGE) to each scenario baseline additional follow-ups per 10k (low-risk FU unchanged). Shows Baseline FU, DeltaHigh, and Total FU.",
  "Multi_way", "Multi-way grid crossing operational ranges (incl. discount) with incidence and death risk CIs (with/without CADe).",
  "Environmental", "Device + follow-up carbon footprint and carbon cost; follow-up counts use each scenario baseline additional follow-ups per 10k (not guideline-adjusted)."
)

## 1b) Core scenarios (baseline inputs)
scenarios <- list(
  list(
    name = "Screening colonoscopy",
    # Costs
    device_cost = 37238, annual_volume = 1000, life_years = 10, discount = 0.03,
    fu_per10k = 835, fu_cost = 950,
    # Effects: absolute risks
    inc_no = 0.0082, inc_no_lo = 0.0067, inc_no_hi = 0.0100,
    inc_with = 0.0071, inc_with_lo = 0.0057, inc_with_hi = 0.0088,
    death_no = 0.0015, death_no_lo = 0.0010, death_no_hi = 0.0024,
    death_with = 0.0013, death_with_lo = 0.0008, death_with_hi = 0.0021
  ),
  list(
    name = "FIT-positive colonoscopy",
    # Costs
    device_cost = 37238, annual_volume = 1000, life_years = 10, discount = 0.03,
    fu_per10k = 108, fu_cost = 950,
    # Effects: absolute risks
    inc_no = 0.0582, inc_no_lo = 0.0560, inc_no_hi = 0.0608,
    inc_with = 0.0577, inc_with_lo = 0.0558, inc_with_hi = 0.0595,
    death_no = 0.0073, death_no_lo = 0.0068, death_no_hi = 0.0080,
    death_with = 0.0072, death_with_lo = 0.0065, death_with_hi = 0.0080
  )
)

## 1c) OWSA ranges 
operational_sens <- tribble(
  ~parameter, ~field, ~low, ~high,
  "Annual colonoscopies per CADe", "annual_volume", 500, 1500,
  "Device lifespan (years)", "life_years", 5, 15,
  "Discount rate", "discount", 0.00, 0.05,
  "Surveillance colonoscopy cost", "fu_cost", 100, 1800,
  "CADe purchase cost", "device_cost", 25896, 50373
)

## 1d) OWSA risk CIs expressed as ARR (delta between no-CADe and CADe)
arr_ci <- tribble(
  ~ScenarioName, ~Delta_inc_lo, ~Delta_inc_hi, ~Delta_death_lo, ~Delta_death_hi,
  "Screening colonoscopy", -0.0021, 0.0043, -0.0011, 0.0017,
  "FIT-positive colonoscopy", -0.0015, 0.0033, -0.0010, 0.0011
)

## 1e) OWSA additional follow-ups (per 10,000), 95% CI FROM MS model
fu_ci_counts <- tribble(
  ~ScenarioName,               ~FU_lo, ~FU_hi,
  "Screening colonoscopy",         719,    952,  # 7.19%–9.52%
  "FIT-positive colonoscopy",       64,    151   # 0.64%–1.51%
)

## 1f) Guideline high-risk DeltaFU add-on (absolute per 10,000 over 10y)
guideline_fu <- tribble(
  ~Guideline, ~Case, ~Delta_FU_per10k,
  "US-MSTF", "Low", 180,
  "US-MSTF", "Baseline", 650,
  "US-MSTF", "High", 1130,
  "ESGE", "Low", 80,
  "ESGE", "Baseline", 300,
  "ESGE", "High", 530
)

## 1g) Environmental constants
device_cf <- 3963        # kg CO2e over lifetime per device
followup_cf <- 28.4      # kg CO2e per colonoscopy follow-up
followups_screening <- 835   # per 10,000
followups_fit <- 108         # per 10,000
carbon_cost_epa <- 190       # USD/tonne
carbon_cost_eu <- 68.83      # USD/tonne

## 1h) Helpers
safe_divide <- function(num, den) ifelse(den > 0, num / den, NA_real_)
fmt_dollar_na <- function(x, accuracy = 1) {
  ifelse(is.na(x), "N/A", scales::dollar(x, accuracy = accuracy))
}

fu_grid_for <- function(scn_name, baseline) {
  if (scn_name == "Screening colonoscopy") c(low = 719, baseline = baseline, high = 952)
  else if (scn_name == "FIT-positive colonoscopy") c(low = 64, baseline = baseline, high = 151)
  else c(baseline = baseline)
}

#### 2) MAIN ANALYSIS FUNCTION ####
analyse <- function(sc) {
  yrs <- seq_len(sc$life_years)
  tot_cols <- sc$annual_volume * sc$life_years
  annual_fu <- (sc$fu_per10k / 1e4) * sc$annual_volume
  
  # Present value costs
  pv_device <- sum((sc$device_cost / sc$life_years) / (1 + sc$discount)^yrs)
  pv_followup <- sum(annual_fu * sc$fu_cost / (1 + sc$discount)^yrs)
  total_cost <- pv_device + pv_followup
  
  # Absolute risk reductions (truncate at 0)
  arr_inc <- pmax(sc$inc_no - sc$inc_with, 0)
  arr_death <- pmax(sc$death_no - sc$death_with, 0)
  
  # Cases prevented over horizon
  crc_prevented <- arr_inc * tot_cols
  death_prevented <- arr_death * tot_cols
  
  tibble(
    Scenario = sc$name,
    device_cost = sc$device_cost,
    annual_volume = sc$annual_volume,
    life_years = sc$life_years,
    discount = sc$discount,
    fu_cost = sc$fu_cost,
    fu_per10k = sc$fu_per10k,             
    risk_inc_no = sc$inc_no, risk_inc_with = sc$inc_with,
    risk_death_no = sc$death_no, risk_death_with = sc$death_with,
    pv_device = pv_device, pv_followup = pv_followup, total_cost = total_cost,
    arr_incidence = arr_inc, arr_death = arr_death,
    crc_prevented = crc_prevented, death_prevented = death_prevented,
    cost_per_crc_prevented = safe_divide(total_cost, crc_prevented),
    cost_per_death_prevented = safe_divide(total_cost, death_prevented),
    fee_per_case = safe_divide(pv_device, tot_cols)
  )
}

#### 3) BASELINE TABLE ####
baseline_tbl <- map_dfr(scenarios, analyse)

#### 4) ONE-WAY SENSITIVITY ANALYSIS (OSA / OWSA) ####

## 4a) Operational drivers
owsa_operational <- map_dfr(scenarios, function(sc0) {
  map_dfr(seq_len(nrow(operational_sens)), function(i) {
    row <- operational_sens[i, ]
    map2_dfr(c(row$low, row$high), c("Low", "High"), function(val, tag) {
      sc_mod <- sc0; sc_mod[[row$field]] <- val
      analyse(sc_mod) %>%
        transmute(
          Scenario, Parameter = row$parameter, Case = tag, Value = val,
          `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
          `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
        )
    })
  })
})

## 4b) Incidence and death ARR CIs
compute_ce_for_delta <- function(sc0, delta_inc = NULL, delta_death = NULL) {
  sc_mod <- sc0
  if (!is.null(delta_inc))   sc_mod$inc_with   <- pmax(sc0$inc_no   - delta_inc,   0)
  if (!is.null(delta_death)) sc_mod$death_with <- pmax(sc0$death_no - delta_death, 0)
  analyse(sc_mod)
}

owsa_risk <- purrr::map_dfr(scenarios, function(sc0) {
  r <- dplyr::filter(arr_ci, ScenarioName == sc0$name)
  if (nrow(r) == 0) return(tibble())
  r <- r[1, ]
  
  dplyr::bind_rows(
    compute_ce_for_delta(sc0, delta_inc = r$Delta_inc_lo) %>%
      dplyr::transmute(
        Scenario  = sc0$name,
        Parameter = "ARR: CRC incidence",
        Case      = "Low (CI)",
        Value     = r$Delta_inc_lo,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      ),
    compute_ce_for_delta(sc0, delta_inc = r$Delta_inc_hi) %>%
      dplyr::transmute(
        Scenario  = sc0$name,
        Parameter = "ARR: CRC incidence",
        Case      = "High (CI)",
        Value     = r$Delta_inc_hi,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      ),
    compute_ce_for_delta(sc0, delta_death = r$Delta_death_lo) %>%
      dplyr::transmute(
        Scenario  = sc0$name,
        Parameter = "ARR: CRC death",
        Case      = "Low (CI)",
        Value     = r$Delta_death_lo,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      ),
    compute_ce_for_delta(sc0, delta_death = r$Delta_death_hi) %>%
      dplyr::transmute(
        Scenario  = sc0$name,
        Parameter = "ARR: CRC death",
        Case      = "High (CI)",
        Value     = r$Delta_death_hi,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      )
  )
})

## 4c) Additional follow-ups (per 10k), 95% CI
owsa_fu <- map_dfr(scenarios, function(sc0) {
  r <- dplyr::filter(fu_ci_counts, ScenarioName == sc0$name)
  if (nrow(r) == 0) return(tibble())
  vals <- c(`Low (CI)` = r$FU_lo[1], `High (CI)` = r$FU_hi[1])
  imap_dfr(vals, function(v, tag) {
    sc_mod <- sc0; sc_mod$fu_per10k <- v
    analyse(sc_mod) %>%
      transmute(
        Scenario,
        Parameter = "Additional follow-ups (per 10k)",
        Case = tag,
        Value = v,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      )
  })
})

## 4d) Optional: vary HIGH-RISK follow-ups only (keeps LOW-RISK fixed) if fields exist
owsa_followups_high_only <- map_dfr(scenarios, function(sc0) {
  needed <- c("low_fu_per10k","high_fu_per10k","high_fu_per10k_lo","high_fu_per10k_hi")
  if (!all(needed %in% names(sc0))) return(tibble())
  low_fixed <- sc0$low_fu_per10k
  vals <- c(`Low` = sc0$high_fu_per10k_lo, `High` = sc0$high_fu_per10k_hi)
  imap_dfr(vals, function(v, tag) {
    sc_mod <- sc0; sc_mod$fu_per10k <- low_fixed + v
    analyse(sc_mod) %>%
      transmute(
        Scenario, Parameter = "High-risk follow-ups per 10k (low fixed)", Case = tag, Value = v,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      )
  })
})

## 4e) Final OWSA table
sens_tbl <- bind_rows(
  owsa_operational,
  owsa_risk,
  owsa_fu,
  owsa_followups_high_only
)

#### 5) MULTI-WAY SENSITIVITY (MWSA) ####

## 5a) Build operational value sets (low, baseline, high); discount uses 0-5% step 1%
baseline_oper_vals <- stats::setNames(
  unlist(scenarios[[1]][ operational_sens$field ]),
  operational_sens$field
)
oper_values <- map2(
  operational_sens$field,
  seq_len(nrow(operational_sens)),
  function(field, i) c(
    low = operational_sens$low[i],
    baseline = baseline_oper_vals[[field]],
    high = operational_sens$high[i]
  )
) |> purrr::set_names(operational_sens$field)
oper_values[["discount"]] <- seq(0, 0.05, by = 0.01)

## 5b) Cross operational sets with risk CIs (with/without CADe) AND FU counts
multiway_tbl <- map_dfr(scenarios, function(sc0) {
  risk_values <- list(
    inc_no     = c(low = sc0$inc_no_lo,     baseline = sc0$inc_no,     high = sc0$inc_no_hi),
    inc_with   = c(low = sc0$inc_with_lo,   baseline = sc0$inc_with,   high = sc0$inc_with_hi),
    death_no   = c(low = sc0$death_no_lo,   baseline = sc0$death_no,   high = sc0$death_no_hi),
    death_with = c(low = sc0$death_with_lo, baseline = sc0$death_with, high = sc0$death_with_hi)
  )
  fu_values <- fu_grid_for(sc0$name, sc0$fu_per10k)
  full_grid <- tidyr::crossing(!!!oper_values, !!!risk_values, fu_per10k = fu_values)
  purrr::pmap_dfr(full_grid, function(...) analyse(modifyList(sc0, list(...))))
})

## 5c) MWSA (COST VARIABLES ONLY) — Best/Worst for cost per CRC prevented
# Best (fixed): low device_cost, low fu_cost, HIGH discount, HIGH annual_volume (1500), LONG life_years (15)
# Worst (fixed): high device_cost, high fu_cost, LOW discount, LOW annual_volume (500), SHORT life_years (5)
# Clinical risks and FU counts are held at BASELINE for each scenario.

# Helper to fetch low/high for a given field from operational_sens
op_low  <- function(field) operational_sens$low [operational_sens$field == field][1]
op_high <- function(field) operational_sens$high[operational_sens$field == field][1]

mw_costonly_bestworst_crc <- function(mw_tbl, scenarios_list) {
  purrr::map_dfr(scenarios_list, function(sc0) {
    df <- mw_tbl %>%
      dplyr::filter(
        Scenario == sc0$name,
        risk_inc_no     == sc0$inc_no,
        risk_inc_with   == sc0$inc_with,
        risk_death_no   == sc0$death_no,
        risk_death_with == sc0$death_with,
        fu_per10k       == sc0$fu_per10k
      )
    
    if (!nrow(df)) return(tibble::tibble())
    
    best_row <- df %>%
      dplyr::filter(
        device_cost  == op_low("device_cost"),
        fu_cost      == op_low("fu_cost"),
        discount     == op_high("discount"),
        annual_volume== op_high("annual_volume"),
        life_years   == op_high("life_years")
      ) %>%
      dplyr::slice_min(cost_per_crc_prevented, n = 1, with_ties = FALSE)
    
    worst_row <- df %>%
      dplyr::filter(
        device_cost  == op_high("device_cost"),
        fu_cost      == op_high("fu_cost"),
        discount     == op_low("discount"),
        annual_volume== op_low("annual_volume"),
        life_years   == op_low("life_years")
      ) %>%
      dplyr::slice_max(cost_per_crc_prevented, n = 1, with_ties = FALSE)
    
    pack <- function(row, label) {
      row %>%
        dplyr::transmute(
          Scenario,
          Case = label,
          Outcome = "Cost per CRC prevented",
          Value_num = cost_per_crc_prevented,
          Value = fmt_dollar_na(cost_per_crc_prevented, 1),
          `Annual volume` = annual_volume,
          `Life years` = life_years,
          `Discount` = paste0(round(discount * 100, 1), "%"),
          `CADe cost` = scales::dollar(device_cost, accuracy = 1),
          `FU cost`   = scales::dollar(fu_cost, accuracy = 1)
        )
    }
    
    dplyr::bind_rows(
      pack(best_row,  "Best (cost-only)"),
      pack(worst_row, "Worst (cost-only)")
    )
  })
}

mw_costonly_bw_crc <- mw_costonly_bestworst_crc(multiway_tbl, scenarios)

#### 6) TABLES (non-CEA) ####

## 6a) Environmental table
device_emissions <- device_cf
followup_emissions_screening <- followups_screening * followup_cf
followup_emissions_fit <- followups_fit * followup_cf

total_emissions_screening <- device_emissions + followup_emissions_screening
total_emissions_fit <- device_emissions + followup_emissions_fit

total_tonnes_screening <- total_emissions_screening / 1000
total_tonnes_fit <- total_emissions_fit / 1000

per_proc_screening <- total_emissions_screening / 10000
per_proc_fit <- total_emissions_fit / 10000

cost_epa_screening <- total_tonnes_screening * carbon_cost_epa
cost_epa_fit <- total_tonnes_fit * carbon_cost_epa
cost_eu_screening <- total_tonnes_screening * carbon_cost_eu
cost_eu_fit <- total_tonnes_fit * carbon_cost_eu

environmental_tbl <- tibble(
  Metric = c(
    "Total device emissions (kg CO2e)",
    "Extra follow-up colonoscopies (n)",
    "Follow-up emissions (kg CO2e)",
    "Total emissions (kg CO2e)",
    "Total emissions (tonnes CO2e)",
    "Emissions per procedure (kg CO2e)",
    "Carbon cost @ $190/tonne (USD)",
    "Carbon cost @ $68.83/tonne (USD)"
  ),
  `Screening colonoscopy` = c(
    device_emissions,
    followups_screening,
    round(followup_emissions_screening, 0),
    round(total_emissions_screening, 0),
    round(total_tonnes_screening, 1),
    round(per_proc_screening, 1),
    round(cost_epa_screening, 0),
    round(cost_eu_screening, 0)
  ),
  `FIT-positive colonoscopy` = c(
    device_emissions,
    followups_fit,
    round(followup_emissions_fit, 0),
    round(total_emissions_fit, 0),
    round(total_tonnes_fit, 1),
    round(per_proc_fit, 1),
    round(cost_epa_fit, 0),
    round(cost_eu_fit, 0)
  )
)

## 6b) Guideline DeltaHigh added to baseline FU per 10k (low-risk unchanged)
sens_fu_guideline <- map_dfr(scenarios, function(sc0) {
  map_dfr(seq_len(nrow(guideline_fu)), function(i) {
    row <- guideline_fu[i, ]
    sc_mod <- sc0
    sc_mod$fu_per10k <- sc0$fu_per10k + row$Delta_FU_per10k
    analyse(sc_mod) %>%
      transmute(
        Scenario, Guideline = row$Guideline, Parameter = "Baseline FU per 10k + DeltaHigh",
        Case = row$Case,
        Baseline_FU_per10k = sc0$fu_per10k,
        Delta_FU_per10k = row$Delta_FU_per10k,
        Total_FU_per10k = sc_mod$fu_per10k,
        `Total cost per CRC prevented`       = fmt_dollar_na(cost_per_crc_prevented, accuracy = 1),
        `Total cost per CRC death prevented` = fmt_dollar_na(cost_per_death_prevented, accuracy = 1)
      )
  })
})

#### 7) WRITE EXCEL ####
sheets <- list(
  README = readme_tbl,
  Baseline = baseline_tbl,
  One_way = sens_tbl,
  FU_guideline = sens_fu_guideline,
  Multi_way = as.data.frame(multiway_tbl),
  MWSA_CostOnly_BestWorst_CRC = as.data.frame(mw_costonly_bw_crc),
  Environmental = as.data.frame(environmental_tbl)
)

write_xlsx(sheets, path = "CADe_analysis_final.xlsx")
