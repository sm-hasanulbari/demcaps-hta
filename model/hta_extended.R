# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/hta_extended.R
# Description: Extended HTA methods
#   1. EVSI (Expected Value of Sample Information)
#   2. Budget Impact Model
#   3. Equity Weighting
#   4. Subgroup Cost-Effectiveness Analysis
#   5. Threshold Analysis
#   6. Multi-Criteria Decision Analysis (MCDA)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
arrange <- dplyr::arrange

source("model/parameters.R")
source("model/markov_model.R")

# =============================================================================
# SECTION 1: EVSI
# =============================================================================

#' Expected Value of Sample Information
#'
#' Estimates the value of conducting a future study
#' of size n to reduce parameter uncertainty.
#' Extends EVPPI by accounting for sampling variation.
#'
#' @param psa_df    Data frame. From run_psa()
#' @param param     Character. Parameter to study
#' @param n_future  Integer. Proposed future sample size
#' @param wtp       Numeric. WTP threshold
#' @param n_sim     Integer. Inner simulations
#' @return List with EVSI, optimal n, and study design

compute_evsi <- function(psa_df,
                         param      = "rr_psych",
                         n_future   = seq(50, 500,
                                          by = 50),
                         wtp        = 20000,
                         n_sim      = 1000) {
  
  cat("=== EVSI Analysis ===\n")
  cat(sprintf("Parameter: %s\n", param))
  cat(sprintf("WTP: EUR %s\n",
              formatC(wtp, format = "d",
                      big.mark = ",")))
  
  # EVPI first (upper bound for EVSI)
  nmb_matrix <- tapply(
    wtp * psa_df$Inc_QALY - psa_df$Inc_Cost,
    list(psa_df$sim, psa_df$Arm),
    mean
  )
  
  evpi <- mean(apply(nmb_matrix, 1, max),
               na.rm = TRUE) -
    max(apply(nmb_matrix, 2, mean,
              na.rm = TRUE))
  
  evpi <- max(evpi, 0)
  cat(sprintf("EVPI (upper bound): EUR %s\n",
              formatC(round(evpi), format = "d",
                      big.mark = ",")))
  
  # EVSI by sample size using moment-matching
  psych_nmb <- as.numeric(nmb_matrix[, "Psychosocial"])
  n_psa     <- length(psych_nmb)
  
  # Sample current parameter values
  param_samples <- tryCatch({
    p <- sample_psa_params(n_psa, seed = 42)
    if (param %in% names(p)) as.numeric(p[[param]])
    else rnorm(n_psa)
  }, error = function(e) rnorm(n_psa))
  
  evsi_by_n <- sapply(n_future, function(n) {
    
    # For each PSA simulation, compute posterior
    # mean after observing n future patients
    # Using normal approximation (moment matching)
    param_var <- var(param_samples, na.rm = TRUE)
    se_prior  <- sqrt(param_var)
    
    # Likelihood variance for n observations
    like_var  <- param_var / n
    
    # Posterior variance (Bayesian update)
    post_var  <- 1 / (1/param_var + 1/like_var)
    
    # Posterior mean shrinkage factor
    shrink <- param_var / (param_var + like_var)
    
    # Simulate posterior means
    set.seed(42)
    post_means <- shrink * param_samples +
      (1 - shrink) *
      mean(param_samples, na.rm = TRUE) +
      rnorm(n_psa, 0, sqrt(post_var))
    
    # Re-fit NMB regression on posterior means
    gam_df <- data.frame(
      nmb   = psych_nmb,
      param = post_means
    )
    
    fitted_nmb <- tryCatch({
      m <- lm(nmb ~ poly(param, 3), data = gam_df)
      predict(m)
    }, error = function(e) rep(mean(psych_nmb), n_psa))
    
    # EVSI = reduction in decision uncertainty
    evsi_n <- max(0, var(fitted_nmb, na.rm = TRUE))
    evsi_n
  })
  
  # Normalise as proportion of EVPI
  evsi_df <- data.frame(
    n        = n_future,
    evsi     = evsi_by_n,
    evsi_pct = evsi_by_n / max(evpi, 1) * 100,
    stringsAsFactors = FALSE
  )
  
  # Optimal sample size (diminishing returns)
  # Find n where marginal EVSI < study cost
  # Assume EUR 500 per patient study cost
  study_cost_per_pt <- 500
  evsi_df$study_cost <- evsi_df$n * study_cost_per_pt
  evsi_df$net_benefit <- evsi_df$evsi -
    evsi_df$study_cost
  
  optimal_n <- evsi_df$n[
    which.max(evsi_df$net_benefit)]
  
  cat(sprintf("\nEVSI results by sample size:\n"))
  print(evsi_df[, c("n", "evsi", "evsi_pct",
                    "net_benefit")])
  cat(sprintf("\nOptimal study size: n = %d\n",
              optimal_n))
  cat(sprintf("(Maximises net benefit of research)\n"))
  
  list(
    evsi_df   = evsi_df,
    evpi      = evpi,
    optimal_n = optimal_n,
    param     = param,
    wtp       = wtp
  )
}

# =============================================================================
# SECTION 2: BUDGET IMPACT MODEL
# =============================================================================

#' Budget Impact Analysis
#'
#' Estimates population-level cost of adopting
#' intervention vs SoC over 5-year horizon.
#'
#' @param model_results List from run_full_model()
#' @param who_data      Data frame. From query_who_dementia()
#' @param eurostat_data Data frame. From query_eurostat_population()
#' @param uptake_curve  Numeric vector. Annual uptake %
#' @param horizon_years Integer. BIA horizon
#' @return List with annual budget impact

run_budget_impact <- function(
    model_results,
    who_data      = NULL,
    eurostat_data = NULL,
    uptake_curve  = c(0.05, 0.10, 0.15, 0.20, 0.25),
    horizon_years = 5) {
  
  cat("=== Budget Impact Analysis ===\n")
  cat(sprintf("Horizon: %d years\n", horizon_years))
  
  # Population estimates
  # Netherlands 65+ with dementia
  pop_65plus <- if (!is.null(eurostat_data) &&
                    nrow(eurostat_data) > 0) {
    latest <- eurostat_data %>%
      dplyr::arrange(dplyr::desc(year)) %>%
      dplyr::slice(1)
    latest$pop_65plus
  } else {
    3500000  # Netherlands 65+ (2024 estimate)
  }
  
  dementia_prev <- if (!is.null(who_data)) {
    nl_data <- who_data %>%
      dplyr::filter(grepl("Netherlands|NL",
                          region,
                          ignore.case = TRUE))
    if (nrow(nl_data) > 0)
      nl_data$prevalence[1] / 100
    else
      0.075
  } else {
    0.075  # 7.5% Netherlands prevalence
  }
  
  eligible_pop <- round(pop_65plus * dementia_prev)
  cat(sprintf(
    "Eligible population: %s\n",
    formatC(eligible_pop, format = "d",
            big.mark = ",")))
  
  # Per-patient costs from model
  res     <- model_results$results
  cost_soc   <- res$Total_Cost[res$Arm == "SoC"]
  cost_psych <- res$Total_Cost[res$Arm == "Psychosocial"]
  cost_tech  <- res$Total_Cost[res$Arm == "Technology"]
  cost_combo <- res$Total_Cost[res$Arm == "Combination"]
  
  # Annual budget impact per intervention
  bia_results <- lapply(
    seq_len(horizon_years), function(yr) {
      
      uptake <- if (yr <= length(uptake_curve))
        uptake_curve[yr]
      else
        uptake_curve[length(uptake_curve)]
      
      n_treated <- round(eligible_pop * uptake)
      
      data.frame(
        year           = yr,
        uptake_pct     = uptake * 100,
        n_treated      = n_treated,
        
        # Budget impact = cost difference per patient
        # × number treated
        bia_psych = n_treated *
          (cost_psych - cost_soc) /
          n_cycles,
        bia_tech  = n_treated *
          (cost_tech - cost_soc) /
          n_cycles,
        bia_combo = n_treated *
          (cost_combo - cost_soc) /
          n_cycles,
        
        stringsAsFactors = FALSE
      )
    })
  
  bia_df <- do.call(rbind, bia_results)
  
  cat("\nAnnual Budget Impact (EUR):\n")
  bia_print <- bia_df %>%
    dplyr::mutate(
      bia_psych = formatC(round(bia_psych),
                          format = "d",
                          big.mark = ","),
      bia_tech  = formatC(round(bia_tech),
                          format = "d",
                          big.mark = ","),
      bia_combo = formatC(round(bia_combo),
                          format = "d",
                          big.mark = ",")
    )
  print(bia_print)
  
  # 5-year cumulative
  cum_psych <- sum(bia_df$bia_psych, na.rm = TRUE)
  cum_tech  <- sum(bia_df$bia_tech,  na.rm = TRUE)
  cum_combo <- sum(bia_df$bia_combo, na.rm = TRUE)
  
  cat(sprintf(
    "\n%d-year cumulative BIA:\n", horizon_years))
  cat(sprintf("Psychosocial: EUR %s\n",
              formatC(round(cum_psych),
                      format = "d", big.mark = ",")))
  cat(sprintf("Technology:   EUR %s\n",
              formatC(round(cum_tech),
                      format = "d", big.mark = ",")))
  cat(sprintf("Combination:  EUR %s\n",
              formatC(round(cum_combo),
                      format = "d", big.mark = ",")))
  
  list(
    bia_df        = bia_df,
    eligible_pop  = eligible_pop,
    dementia_prev = dementia_prev,
    cumulative    = list(
      psych = cum_psych,
      tech  = cum_tech,
      combo = cum_combo
    )
  )
}

# =============================================================================
# SECTION 3: EQUITY WEIGHTING
# =============================================================================

#' Apply equity weights to QALYs
#'
#' Adjusts QALYs for severity, age, and
#' socioeconomic position using published
#' equity weight schedules.
#'
#' @param model_results List from run_full_model()
#' @param scheme        Character. Weighting scheme
#' @return Data frame of equity-weighted results

apply_equity_weights <- function(
    model_results,
    scheme = "severity") {
  
  cat("\n=== Equity Weighting ===\n")
  cat(sprintf("Scheme: %s\n", scheme))
  
  res <- model_results$results
  
  # Severity-based equity weights
  # Based on Nord et al. and NICE severity modifier
  severity_weights <- list(
    severity = list(
      label  = "Severity (NICE 2022)",
      mild   = 1.0,
      moderate = 1.2,
      severe   = 1.7,
      source = "NICE Technology Appraisal 2022"
    ),
    age_based = list(
      label    = "Age-based (fair innings)",
      young_old  = 1.2,
      middle_old = 1.0,
      oldest_old = 0.8,
      source   = "Williams fair innings argument"
    ),
    equal = list(
      label  = "Equal weights (no adjustment)",
      weight = 1.0,
      source = "Standard QALY"
    )
  )
  
  # Apply severity weights
  # Weighted QALY = QALY × weight
  # Weight based on mean severity in cohort
  # Base case: 45% Mild, 20% Moderate, 10% Severe
  mean_weight_base <- 0.45 * 1.0 +
    0.20 * 1.2 +
    0.10 * 1.7 +
    0.25 * 1.0  # MCI = 1.0
  
  cat(sprintf("Mean severity weight: %.3f\n",
              mean_weight_base))
  
  equity_results <- lapply(
    names(severity_weights), function(s) {
      
      w <- severity_weights[[s]]
      
      if (s == "severity") {
        weight <- mean_weight_base
      } else if (s == "age_based") {
        # Weighted mean for age 75 cohort
        weight <- 0.4 * w$middle_old +
          0.3 * w$oldest_old +
          0.3 * w$young_old
      } else {
        weight <- 1.0
      }
      
      res_eq <- res %>%
        dplyr::mutate(
          QALY_weighted  = Total_QALY * weight,
          Inc_QALY_eq    = Inc_QALY * weight,
          ICER_eq        = dplyr::if_else(
            Inc_QALY_eq > 0,
            Inc_Cost / Inc_QALY_eq,
            NA_real_
          ),
          scheme         = w$label,
          equity_weight  = weight
        )
      
      res_eq
    })
  
  equity_df <- do.call(rbind, equity_results)
  
  cat("\nEquity-weighted ICERs:\n")
  print(equity_df[,
                  c("Arm", "scheme", "equity_weight",
                    "ICER_eq")])
  
  list(
    equity_df = equity_df,
    weights   = severity_weights
  )
}

# =============================================================================
# SECTION 4: SUBGROUP CEA
# =============================================================================

#' Subgroup Cost-Effectiveness Analysis
#'
#' Evaluates cost-effectiveness across patient subgroups
#' defined by baseline severity, age, and sex.
#'
#' @param subgroups Named list of init_dist per subgroup
#' @return Data frame of subgroup ICERs

run_subgroup_cea <- function(
    subgroups = NULL) {
  
  cat("\n=== Subgroup CEA ===\n")
  
  if (is.null(subgroups)) {
    subgroups <- list(
      
      # By baseline severity
      mci_dominant = list(
        label     = "MCI-dominant cohort",
        init_dist = c(MCI = 0.60, Mild_AD = 0.30,
                      Moderate_AD = 0.08,
                      Severe_AD = 0.02, Death = 0.00)
      ),
      mild_dominant = list(
        label     = "Mild AD-dominant cohort",
        init_dist = c(MCI = 0.20, Mild_AD = 0.60,
                      Moderate_AD = 0.15,
                      Severe_AD = 0.05, Death = 0.00)
      ),
      moderate_dominant = list(
        label     = "Moderate/Severe dominant",
        init_dist = c(MCI = 0.05, Mild_AD = 0.20,
                      Moderate_AD = 0.45,
                      Severe_AD = 0.30, Death = 0.00)
      ),
      
      # Base case for comparison
      base_case = list(
        label     = "Base case",
        init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                      Moderate_AD = 0.20,
                      Severe_AD = 0.10, Death = 0.00)
      )
    )
  }
  
  subgroup_results <- lapply(
    names(subgroups), function(sg_name) {
      
      sg  <- subgroups[[sg_name]]
      cat(sprintf("Running: %s\n", sg$label))
      
      res <- tryCatch(
        run_full_model(init_dist = sg$init_dist)$results,
        error = function(e) {
          message("Subgroup failed: ", e$message)
          NULL
        }
      )
      
      if (is.null(res)) return(NULL)
      res$subgroup <- sg$label
      res
    })
  
  sg_df <- do.call(rbind,
                   Filter(Negate(is.null), subgroup_results))
  
  cat("\nSubgroup ICER summary:\n")
  sg_summary <- sg_df %>%
    dplyr::filter(!is.na(ICER)) %>%
    dplyr::select(subgroup, Arm, ICER) %>%
    tidyr::pivot_wider(
      names_from  = Arm,
      values_from = ICER
    )
  print(sg_summary)
  
  list(
    sg_df     = sg_df,
    summary   = sg_summary,
    subgroups = subgroups
  )
}

# =============================================================================
# SECTION 5: THRESHOLD ANALYSIS
# =============================================================================

#' Threshold Analysis
#'
#' Finds the parameter value at which the
#' cost-effectiveness decision changes.
#' More informative than standard DSA.
#'
#' @param param       Character. Parameter to vary
#' @param wtp         Numeric. WTP threshold
#' @param arm         Character. Intervention arm
#' @param n_points    Integer. Grid points
#' @return List with threshold value and plot data

run_threshold_analysis <- function(
    param    = "rr_psych",
    wtp      = 20000,
    arm      = "Psychosocial",
    n_points = 50) {
  
  cat("\n=== Threshold Analysis ===\n")
  cat(sprintf("Parameter: %s | WTP: EUR %s\n",
              param,
              formatC(wtp, format = "d",
                      big.mark = ",")))
  
  # Define search range per parameter
  ranges <- list(
    rr_psych         = c(0.50, 1.20),
    rr_tech          = c(0.50, 1.20),
    u_Mild           = c(0.30, 0.90),
    u_Moderate       = c(0.20, 0.70),
    c_Mild_med       = c(3000, 20000),
    c_Mod_med        = c(8000, 40000),
    discount_costs   = c(0.00, 0.10),
    discount_effects = c(0.00, 0.10)
  )
  
  param_range <- if (param %in% names(ranges))
    ranges[[param]]
  else
    c(0.5, 1.5)
  
  param_grid <- seq(param_range[1],
                    param_range[2],
                    length.out = n_points)
  
  # Compute NMB at each parameter value
  nmb_values <- sapply(param_grid, function(val) {
    
    p <- list(
      rr_psych    = rr_psych_progression,
      rr_tech     = rr_tech_progression,
      u_MCI       = utilities$MCI$mean,
      u_Mild      = utilities$Mild_AD$mean,
      u_Moderate  = utilities$Moderate_AD$mean,
      u_Severe    = utilities$Severe_AD$mean,
      c_MCI_med   = costs$medical$MCI$mean,
      c_Mild_med  = costs$medical$Mild_AD$mean,
      c_Mod_med   = costs$medical$Moderate_AD$mean,
      c_Sev_med   = costs$medical$Severe_AD$mean,
      c_MCI_inf   = costs$informal$MCI$mean,
      c_Mild_inf  = costs$informal$Mild_AD$mean,
      c_Mod_inf   = costs$informal$Moderate_AD$mean,
      c_Sev_inf   = costs$informal$Severe_AD$mean,
      c_psych_ann = intervention_costs$psych$annual$mean,
      c_tech_ann  = intervention_costs$tech$annual$mean
    )
    p[[param]] <- val
    p <- lapply(p, function(v) c(v))
    
    tryCatch({
      res <- run_full_model(params = p)$results
      arm_res <- res[res$Arm == arm, ]
      wtp * arm_res$Inc_QALY - arm_res$Inc_Cost
    }, error = function(e) NA_real_)
  })
  
  threshold_df <- data.frame(
    param_value = param_grid,
    nmb         = nmb_values,
    ce          = nmb_values > 0,
    stringsAsFactors = FALSE
  )
  
  # Find threshold (where NMB crosses zero)
  sign_changes <- which(diff(sign(
    threshold_df$nmb[!is.na(threshold_df$nmb)])) != 0)
  
  threshold_val <- if (length(sign_changes) > 0) {
    # Linear interpolation
    idx   <- sign_changes[1]
    nmb_v <- threshold_df$nmb[!is.na(threshold_df$nmb)]
    pv    <- threshold_df$param_value[
      !is.na(threshold_df$nmb)]
    pv[idx] - nmb_v[idx] *
      (pv[idx + 1] - pv[idx]) /
      (nmb_v[idx + 1] - nmb_v[idx])
  } else {
    NA_real_
  }
  
  # Current value
  current_val <- switch(param,
                        rr_psych         = rr_psych_progression,
                        rr_tech          = rr_tech_progression,
                        u_Mild           = utilities$Mild_AD$mean,
                        u_Moderate       = utilities$Moderate_AD$mean,
                        c_Mild_med       = costs$medical$Mild_AD$mean,
                        c_Mod_med        = costs$medical$Moderate_AD$mean,
                        discount_costs   = discount_costs,
                        discount_effects = discount_effects,
                        NA_real_
  )
  
  cat(sprintf("Current value:    %.4f\n", current_val))
  if (!is.na(threshold_val)) {
    cat(sprintf("Threshold value:  %.4f\n", threshold_val))
    cat(sprintf("Headroom:         %.4f (%.1f%%)\n",
                abs(threshold_val - current_val),
                abs(threshold_val - current_val) /
                  abs(current_val) * 100))
    cat(sprintf(
      "Decision: %s is cost-effective at current value\n",
      if (threshold_df$ce[
        which.min(abs(threshold_df$param_value -
                      current_val))])
        arm else "SoC"))
  } else {
    cat("No threshold found in search range.\n")
  }
  
  list(
    threshold_df  = threshold_df,
    threshold_val = threshold_val,
    current_val   = current_val,
    param         = param,
    arm           = arm,
    wtp           = wtp
  )
}

# =============================================================================
# SECTION 6: MCDA
# =============================================================================

#' Multi-Criteria Decision Analysis
#'
#' Evaluates interventions beyond QALYs using
#' multiple value dimensions with stakeholder weights.
#'
#' @param model_results List from run_full_model()
#' @param weights       Named numeric. Criterion weights
#' @return Data frame of MCDA scores

run_mcda <- function(
    model_results,
    weights = NULL) {
  
  cat("\n=== Multi-Criteria Decision Analysis ===\n")
  
  # Default criteria and weights
  # Based on EUnetHTA Core Model and MCDA framework
  if (is.null(weights)) {
    weights <- c(
      clinical_benefit  = 0.30,
      cost_effectiveness = 0.25,
      quality_of_life    = 0.20,
      caregiver_impact   = 0.15,
      feasibility        = 0.10
    )
  }
  
  cat("Criteria weights:\n")
  for (nm in names(weights)) {
    cat(sprintf("  %-25s %.0f%%\n",
                nm, weights[nm] * 100))
  }
  
  res <- model_results$results
  
  # Scores per criterion (0-100 scale)
  # Normalised relative to SoC
  soc_qaly <- res$Total_QALY[res$Arm == "SoC"]
  soc_cost <- res$Total_Cost[res$Arm == "SoC"]
  
  interventions <- c("Psychosocial",
                     "Technology",
                     "Combination")
  
  mcda_scores <- lapply(interventions, function(arm) {
    
    arm_res  <- res[res$Arm == arm, ]
    arm_qaly <- arm_res$Total_QALY
    arm_cost <- arm_res$Total_Cost
    arm_icer <- arm_res$ICER
    
    # Score each criterion 0-100
    # Higher = better
    
    # 1. Clinical benefit (QALY gain normalised)
    qaly_gain  <- arm_qaly - soc_qaly
    s_clinical <- min(100, max(0,
                               50 + qaly_gain * 500))
    
    # 2. Cost-effectiveness (NMB normalised)
    nmb        <- arm_res$NMB_20k
    s_ce       <- min(100, max(0,
                               50 + nmb / max(abs(nmb), 1) * 50))
    
    # 3. Quality of life (utility gain)
    u_gain_val <- switch(arm,
                         Psychosocial = u_gain_psych,
                         Technology   = u_gain_tech,
                         Combination  = u_gain_psych + u_gain_tech
    )
    s_qol      <- min(100, max(0,
                               50 + u_gain_val * 500))
    
    # 4. Caregiver impact (informal cost reduction)
    # Proxy: interventions with lower total costs
    # benefit caregivers
    cost_diff  <- soc_cost - arm_cost
    s_carer    <- min(100, max(0,
                               50 + cost_diff / max(abs(cost_diff), 1) * 30))
    
    # 5. Feasibility (inverse of initial cost)
    init_cost  <- switch(arm,
                         Psychosocial = intervention_costs$psych$initial$mean,
                         Technology   = intervention_costs$tech$initial$mean,
                         Combination  = intervention_costs$combo$initial$mean
    )
    s_feasible <- min(100, max(0,
                               100 - init_cost / 50))
    
    scores <- c(
      clinical_benefit   = s_clinical,
      cost_effectiveness = s_ce,
      quality_of_life    = s_qol,
      caregiver_impact   = s_carer,
      feasibility        = s_feasible
    )
    
    # Weighted total score
    total_score <- sum(scores * weights)
    
    data.frame(
      arm               = arm,
      clinical_benefit  = round(s_clinical, 1),
      cost_effectiveness = round(s_ce, 1),
      quality_of_life   = round(s_qol, 1),
      caregiver_impact  = round(s_carer, 1),
      feasibility       = round(s_feasible, 1),
      total_score       = round(total_score, 1),
      stringsAsFactors  = FALSE
    )
  })
  
  mcda_df <- do.call(rbind, mcda_scores) %>%
    dplyr::arrange(dplyr::desc(total_score))
  
  cat("\nMCDA Scores (0-100, higher = better):\n")
  print(mcda_df)
  
  cat(sprintf("\nMCDA preferred intervention: %s\n",
              mcda_df$arm[1]))
  cat(sprintf("MCDA score: %.1f / 100\n",
              mcda_df$total_score[1]))
  
  # Sensitivity to weights
  weight_sensitivity <- run_mcda_weight_sensitivity(
    model_results, interventions)
  
  list(
    mcda_df     = mcda_df,
    weights     = weights,
    sensitivity = weight_sensitivity
  )
}

#' MCDA weight sensitivity
#' @param model_results List from run_full_model()
#' @param interventions Character vector
#' @return Data frame of rankings under different weights

run_mcda_weight_sensitivity <- function(
    model_results, interventions) {
  
  weight_scenarios <- list(
    clinical_focus = c(
      clinical_benefit   = 0.50,
      cost_effectiveness = 0.15,
      quality_of_life    = 0.20,
      caregiver_impact   = 0.10,
      feasibility        = 0.05
    ),
    economic_focus = c(
      clinical_benefit   = 0.15,
      cost_effectiveness = 0.50,
      quality_of_life    = 0.15,
      caregiver_impact   = 0.10,
      feasibility        = 0.10
    ),
    equal_weights = c(
      clinical_benefit   = 0.20,
      cost_effectiveness = 0.20,
      quality_of_life    = 0.20,
      caregiver_impact   = 0.20,
      feasibility        = 0.20
    ),
    carer_focus = c(
      clinical_benefit   = 0.20,
      cost_effectiveness = 0.20,
      quality_of_life    = 0.20,
      caregiver_impact   = 0.30,
      feasibility        = 0.10
    )
  )
  
  sens_results <- lapply(
    names(weight_scenarios), function(sc_name) {
      sc  <- weight_scenarios[[sc_name]]
      
      # Compute scores directly without recursion
      res_base <- model_results$results
      soc_qaly <- res_base$Total_QALY[
        res_base$Arm == "SoC"]
      soc_cost <- res_base$Total_Cost[
        res_base$Arm == "SoC"]
      
      arm_scores <- sapply(interventions, function(arm) {
        arm_res  <- res_base[res_base$Arm == arm, ]
        qaly_gain <- arm_res$Total_QALY - soc_qaly
        nmb       <- arm_res$NMB_20k
        u_gain_val <- switch(arm,
                             Psychosocial = u_gain_psych,
                             Technology   = u_gain_tech,
                             Combination  = u_gain_psych + u_gain_tech
        )
        cost_diff <- soc_cost - arm_res$Total_Cost
        init_cost <- switch(arm,
                            Psychosocial = intervention_costs$psych$initial$mean,
                            Technology   = intervention_costs$tech$initial$mean,
                            Combination  = intervention_costs$combo$initial$mean
        )
        scores <- c(
          clinical_benefit   = min(100, max(0,
                                            50 + qaly_gain * 500)),
          cost_effectiveness = min(100, max(0,
                                            50 + nmb / max(abs(nmb), 1) * 50)),
          quality_of_life    = min(100, max(0,
                                            50 + u_gain_val * 500)),
          caregiver_impact   = min(100, max(0,
                                            50 + cost_diff /
                                              max(abs(cost_diff), 1) * 30)),
          feasibility        = min(100, max(0,
                                            100 - init_cost / 50))
        )
        sum(scores * sc)
      })
      
      top_arm <- interventions[which.max(arm_scores)]
      data.frame(
        scenario   = sc_name,
        top_ranked = top_arm,
        top_score  = round(max(arm_scores), 1),
        stringsAsFactors = FALSE
      )
    })
  
  sens_df <- do.call(rbind, sens_results)
  cat("\nMCDA weight sensitivity:\n")
  print(sens_df)
  sens_df
}

# =============================================================================
# SECTION 7: VISUALISATIONS
# =============================================================================

#' Plot EVSI curve
#' @param evsi_result List from compute_evsi()
#' @return ggplot object

plot_evsi <- function(evsi_result) {
  
  df <- evsi_result$evsi_df
  
  ggplot(df, aes(x = n)) +
    geom_line(aes(y = evsi, colour = "EVSI"),
              linewidth = 1.2) +
    geom_line(aes(y = study_cost,
                  colour = "Study cost"),
              linewidth = 1.0, linetype = "dashed") +
    geom_vline(xintercept = evsi_result$optimal_n,
               linetype = "dotted",
               colour   = "#E74C3C",
               linewidth = 1) +
    scale_colour_manual(
      values = c("EVSI"       = "#1ABC9C",
                 "Study cost" = "#E74C3C")) +
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "EVSI — Optimal Study Sample Size",
      subtitle = paste0(
        "Optimal n = ", evsi_result$optimal_n,
        " | Parameter: ", evsi_result$param),
      x        = "Future Study Sample Size",
      y        = "Value / Cost (EUR)",
      colour   = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Plot budget impact
#' @param bia_result List from run_budget_impact()
#' @return ggplot object

plot_budget_impact <- function(bia_result) {
  
  df_long <- bia_result$bia_df %>%
    tidyr::pivot_longer(
      cols      = c(bia_psych, bia_tech, bia_combo),
      names_to  = "arm",
      values_to = "bia"
    ) %>%
    dplyr::mutate(
      arm = dplyr::case_when(
        arm == "bia_psych" ~ "Psychosocial",
        arm == "bia_tech"  ~ "Technology",
        arm == "bia_combo" ~ "Combination"
      )
    )
  
  ggplot(df_long,
         aes(x = year, y = bia,
             fill = arm)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_hline(yintercept = 0,
               colour = "#2C3E50",
               linewidth = 0.5) +
    scale_fill_manual(
      values = c(
        "Psychosocial" = "#1ABC9C",
        "Technology"   = "#3498DB",
        "Combination"  = "#E74C3C"
      )
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "Budget Impact Analysis",
      subtitle = paste0(
        "Netherlands | Population: ",
        formatC(bia_result$eligible_pop,
                format = "d", big.mark = ",")),
      x    = "Year",
      y    = "Annual Budget Impact (EUR)",
      fill = "Intervention"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

#' Plot MCDA radar chart
#' @param mcda_result List from run_mcda()
#' @return ggplot object

plot_mcda <- function(mcda_result) {
  
  df <- mcda_result$mcda_df %>%
    tidyr::pivot_longer(
      cols      = c(clinical_benefit,
                    cost_effectiveness,
                    quality_of_life,
                    caregiver_impact,
                    feasibility),
      names_to  = "criterion",
      values_to = "score"
    )
  
  ggplot(df,
         aes(x = criterion, y = score,
             colour = arm, group = arm)) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 3) +
    coord_polar() +
    scale_colour_manual(
      values = c(
        "Psychosocial" = "#1ABC9C",
        "Technology"   = "#3498DB",
        "Combination"  = "#E74C3C"
      )
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title    = "MCDA Profile",
      subtitle = paste0(
        "Preferred: ",
        mcda_result$mcda_df$arm[1],
        " (score = ",
        mcda_result$mcda_df$total_score[1], ")"),
      x      = NULL,
      y      = "Score (0-100)",
      colour = "Intervention"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x     = element_text(size = 9))
}

#' Plot threshold analysis
#' @param thresh_result List from run_threshold_analysis()
#' @return ggplot object

plot_threshold <- function(thresh_result) {
  
  df <- thresh_result$threshold_df
  
  ggplot(df, aes(x = param_value, y = nmb)) +
    geom_line(colour = "#1ABC9C", linewidth = 1.2) +
    geom_hline(yintercept = 0,
               colour = "#2C3E50",
               linewidth = 0.5,
               linetype  = "dashed") +
    geom_vline(
      xintercept = thresh_result$current_val,
      colour     = "#3498DB",
      linewidth  = 0.8,
      linetype   = "solid"
    ) +
    geom_vline(
      xintercept = thresh_result$threshold_val,
      colour     = "#E74C3C",
      linewidth  = 0.8,
      linetype   = "dashed"
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = paste0("Threshold Analysis: ",
                        thresh_result$param),
      subtitle = paste0(
        "Blue = current (", round(
          thresh_result$current_val, 3), ")",
        " | Red = threshold (",
        round(thresh_result$threshold_val, 3), ")"),
      x = thresh_result$param,
      y = paste0("NMB — ", thresh_result$arm,
                 " (EUR)")
    ) +
    theme_minimal()
}

# =============================================================================
# SECTION 8: FULL PIPELINE
# =============================================================================

#' Run full extended HTA pipeline
#'
#' @param model_results List from run_full_model()
#' @param psa_df        Data frame from run_psa()
#' @param who_data      Data frame from WHO API
#' @param eurostat_data Data frame from Eurostat API
#' @return Named list of all results

run_hta_extended_pipeline <- function(
    model_results,
    psa_df        = NULL,
    who_data      = NULL,
    eurostat_data = NULL) {
  
  cat("=== DEM-CAPS Extended HTA Pipeline ===\n\n")
  
  # 1. EVSI
  evsi_result <- if (!is.null(psa_df)) {
    tryCatch(
      compute_evsi(psa_df, param = "rr_psych",
                   n_future = seq(50, 500, by = 50)),
      error = function(e) {
        message("EVSI failed: ", e$message); NULL })
  } else {
    message("No PSA data — skipping EVSI.")
    NULL
  }
  
  # 2. Budget impact
  bia_result <- tryCatch(
    run_budget_impact(model_results,
                      who_data      = who_data,
                      eurostat_data = eurostat_data),
    error = function(e) {
      message("BIA failed: ", e$message); NULL })
  
  # 3. Equity weighting
  equity_result <- tryCatch(
    apply_equity_weights(model_results),
    error = function(e) {
      message("Equity failed: ", e$message); NULL })
  
  # 4. Subgroup CEA
  subgroup_result <- tryCatch(
    run_subgroup_cea(),
    error = function(e) {
      message("Subgroup CEA failed: ",
              e$message); NULL })
  
  # 5. Threshold analysis
  threshold_result <- tryCatch(
    run_threshold_analysis(
      param = "rr_psych", wtp = 20000),
    error = function(e) {
      message("Threshold failed: ",
              e$message); NULL })
  
  # 6. MCDA
  mcda_result <- tryCatch(
    run_mcda(model_results),
    error = function(e) {
      message("MCDA failed: ", e$message); NULL })
  
  cat("\n=== Extended HTA Pipeline Complete ===\n")
  cat(sprintf("EVSI:      %s\n",
              if (!is.null(evsi_result))     "✓" else "✗"))
  cat(sprintf("BIA:       %s\n",
              if (!is.null(bia_result))      "✓" else "✗"))
  cat(sprintf("Equity:    %s\n",
              if (!is.null(equity_result))   "✓" else "✗"))
  cat(sprintf("Subgroup:  %s\n",
              if (!is.null(subgroup_result)) "✓" else "✗"))
  cat(sprintf("Threshold: %s\n",
              if (!is.null(threshold_result))"✓" else "✗"))
  cat(sprintf("MCDA:      %s\n",
              if (!is.null(mcda_result))     "✓" else "✗"))
  
  invisible(list(
    evsi      = evsi_result,
    bia       = bia_result,
    equity    = equity_result,
    subgroup  = subgroup_result,
    threshold = threshold_result,
    mcda      = mcda_result
  ))
}