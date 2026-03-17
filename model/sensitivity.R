# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/sensitivity.R
# BUG FIXES APPLIED:
#   1. DSA discount rates now correctly passed to model (not just computed)
#   2. Scenarios now actually implement their logic (not all returning base-case)
#   3. n_cycles_override now works for short horizon scenario
# =============================================================================

source("model/markov_model.R")

# -----------------------------------------------------------------------------
# ONE-WAY DSA
# -----------------------------------------------------------------------------

run_dsa <- function(init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                                  Moderate_AD = 0.20, Severe_AD = 0.10,
                                  Death = 0.00)) {
  
  base      <- run_full_model(init_dist = init_dist)$results
  base_icer <- base$ICER[base$Arm == "Psychosocial"]
  
  dsa_rows <- lapply(dsa_params, function(dp) {
    
    run_dsa_arm <- function(param_value, disc_c, disc_e) {
      
      # Build parameter list with this one value changed
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
      p[[dp$param]] <- param_value
      p <- lapply(p, function(v) c(v))
      
      # FIX: temporarily override discount rates in environment
      old_disc_c <- discount_costs
      old_disc_e <- discount_effects
      assign("discount_costs",   disc_c, envir = environment(run_arm))
      assign("discount_effects", disc_e, envir = environment(run_arm))
      
      res <- tryCatch(
        run_full_model(params = p, init_dist = init_dist)$results,
        error = function(e) NULL
      )
      
      assign("discount_costs",   old_disc_c, envir = environment(run_arm))
      assign("discount_effects", old_disc_e, envir = environment(run_arm))
      res
    }
    
    disc_c_low  <- if (dp$param == "discount_costs")   dp$low  else discount_costs
    disc_c_high <- if (dp$param == "discount_costs")   dp$high else discount_costs
    disc_e_low  <- if (dp$param == "discount_effects") dp$low  else discount_effects
    disc_e_high <- if (dp$param == "discount_effects") dp$high else discount_effects
    
    res_low  <- run_dsa_arm(dp$low,  disc_c_low,  disc_e_low)
    res_high <- run_dsa_arm(dp$high, disc_c_high, disc_e_high)
    
    icer_low  <- if (!is.null(res_low))
      res_low$ICER[res_low$Arm   == "Psychosocial"] else NA
    icer_high <- if (!is.null(res_high))
      res_high$ICER[res_high$Arm == "Psychosocial"] else NA
    
    data.frame(
      Parameter  = dp$label,
      Low_Value  = dp$low,
      Base_Value = dp$base,
      High_Value = dp$high,
      ICER_Low   = icer_low,
      ICER_High  = icer_high,
      ICER_Range = abs(icer_high - icer_low),
      Reference  = dp$ref,
      stringsAsFactors = FALSE
    )
  })
  
  dsa_df <- do.call(rbind, Filter(Negate(is.null), dsa_rows))
  dsa_df[order(dsa_df$ICER_Range, decreasing = TRUE), ]
}

# -----------------------------------------------------------------------------
# SCENARIO ANALYSIS
# FIX: each scenario now correctly implements its intended logic
# -----------------------------------------------------------------------------

run_scenarios <- function() {
  
  # --- 1. Base case ---
  s1 <- run_full_model(
    init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                  Moderate_AD = 0.20, Severe_AD = 0.10, Death = 0.00)
  )$results
  s1$Scenario <- "Base case"
  
  # --- 2. Early-stage dominant cohort ---
  s2 <- run_full_model(
    init_dist = c(MCI = 0.40, Mild_AD = 0.45,
                  Moderate_AD = 0.10, Severe_AD = 0.05, Death = 0.00)
  )$results
  s2$Scenario <- "Early-stage dominant (MCI/Mild)"
  
  # --- 3. Late-stage dominant cohort ---
  s3 <- run_full_model(
    init_dist = c(MCI = 0.05, Mild_AD = 0.20,
                  Moderate_AD = 0.45, Severe_AD = 0.30, Death = 0.00)
  )$results
  s3$Scenario <- "Late-stage dominant (Moderate/Severe)"
  
  # --- 4. Healthcare perspective (no informal costs) ---
  # FIX: actually removes informal costs, was identical to base before
  old_informal <- costs$informal
  costs$informal <- lapply(costs$informal, function(x) list(mean = 0, se = 0))
  s4 <- run_full_model(
    init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                  Moderate_AD = 0.20, Severe_AD = 0.10, Death = 0.00)
  )$results
  costs$informal <- old_informal
  s4$Scenario <- "Healthcare perspective (no informal costs)"
  
  # --- 5. Shortened horizon (10 years) ---
  # FIX: actually overrides n_cycles, was ignored before
  old_cycles <- n_cycles
  assign("n_cycles", 10, envir = .GlobalEnv)
  s5 <- run_full_model(
    init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                  Moderate_AD = 0.20, Severe_AD = 0.10, Death = 0.00)
  )$results
  assign("n_cycles", old_cycles, envir = .GlobalEnv)
  s5$Scenario <- "Shortened horizon (10 years)"
  
  # --- 6. Technology cost halved ---
  # FIX: actually halves tech costs, was identical to base before
  old_tech_costs <- intervention_costs$tech
  intervention_costs$tech$initial$mean <- old_tech_costs$initial$mean / 2
  intervention_costs$tech$annual$mean  <- old_tech_costs$annual$mean  / 2
  s6 <- run_full_model(
    init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                  Moderate_AD = 0.20, Severe_AD = 0.10, Death = 0.00)
  )$results
  intervention_costs$tech <- old_tech_costs
  s6$Scenario <- "Technology cost halved (scale-up scenario)"
  
  do.call(rbind, list(s1, s2, s3, s4, s5, s6))
}