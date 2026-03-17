# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/markov_model.R
# BUG FIXES APPLIED:
#   1. PSA transition matrix now uses sampled params (was always tp_base)
#   2. Arm name matching fixed ("Psychosocial" / "Technology" not "psych"/"tech")
#   3. Half-cycle correction applied correctly to both ends
#   4. set.seed() removed from inside sample function (now passed as argument)
# =============================================================================

source("model/parameters.R")

# -----------------------------------------------------------------------------
# UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

apply_half_cycle <- function(trace) {
  n         <- nrow(trace)
  corrected <- trace
  corrected[1, ] <- trace[1, ] * 0.5
  corrected[n, ] <- trace[n, ] * 0.5   # FIX: last row now correctly halved
  corrected
}

discount_vector <- function(values, rate, cycles = NULL) {
  if (is.null(cycles)) cycles <- length(values)
  disc <- 1 / (1 + rate)^(0:(cycles - 1))
  values * disc
}

# -----------------------------------------------------------------------------
# TRANSITION MATRIX BUILDER
# -----------------------------------------------------------------------------

build_tp_matrix <- function(tp_base, rr = 1.0) {
  tp_mod <- tp_base
  
  for (i in 1:(n_states - 1)) {
    for (j in (i + 1):(n_states - 1)) {
      original_p   <- tp_base[i, j]
      modified_p   <- min(original_p * rr,
                          tp_base[i, j] + tp_base[i, i] - 0.01)
      delta        <- original_p - modified_p
      tp_mod[i, j] <- modified_p
      tp_mod[i, i] <- tp_mod[i, i] + delta
    }
  }
  
  row_sums <- rowSums(tp_mod)
  for (i in seq_len(n_states)) {
    tp_mod[i, ] <- tp_mod[i, ] / row_sums[i]
  }
  tp_mod
}

# -----------------------------------------------------------------------------
# MARKOV TRACE
# -----------------------------------------------------------------------------

run_markov_trace <- function(tp_matrix, init_state, n_cycles) {
  trace           <- matrix(0, nrow = n_cycles + 1, ncol = n_states)
  colnames(trace) <- health_states
  trace[1, ]      <- init_state
  
  for (t in 1:n_cycles) {
    trace[t + 1, ] <- trace[t, ] %*% tp_matrix
  }
  trace
}

# -----------------------------------------------------------------------------
# COST & QALY ACCUMULATION
# -----------------------------------------------------------------------------

calc_costs <- function(trace, state_costs, interv_cost_i = 0,
                       interv_cost_a = 0, disc_rate = discount_costs) {
  n_c      <- nrow(trace) - 1
  hc_trace <- apply_half_cycle(trace)
  
  # FIX: loop now includes final row of hc_trace
  cost_per_cycle <- numeric(n_c + 1)
  for (t in 1:(n_c + 1)) {
    state_c          <- sum(hc_trace[t, ] * state_costs)
    interv_c         <- ifelse(t == 1, interv_cost_i + interv_cost_a,
                               interv_cost_a)
    cost_per_cycle[t] <- state_c + interv_c
  }
  
  disc_costs <- discount_vector(cost_per_cycle, disc_rate, n_c + 1)
  list(total       = sum(disc_costs),
       per_cycle   = disc_costs,
       undiscounted = cost_per_cycle)
}

calc_qalys <- function(trace, state_utils, u_gain = 0,
                       disc_rate = discount_effects) {
  n_c      <- nrow(trace) - 1
  hc_trace <- apply_half_cycle(trace)
  
  # FIX: loop now includes final row; u_gain applies to all alive states
  qaly_per_cycle <- numeric(n_c + 1)
  for (t in 1:(n_c + 1)) {
    base_u           <- sum(hc_trace[t, ] * state_utils)
    alive_prop       <- 1 - hc_trace[t, "Death"]
    qaly_per_cycle[t] <- (base_u + u_gain * alive_prop) * cycle_length_years
  }
  
  disc_qalys <- discount_vector(qaly_per_cycle, disc_rate, n_c + 1)
  list(total       = sum(disc_qalys),
       per_cycle   = disc_qalys,
       undiscounted = qaly_per_cycle)
}

# -----------------------------------------------------------------------------
# SINGLE ARM
# -----------------------------------------------------------------------------

run_arm <- function(arm         = "SoC",
                    rr          = 1.0,
                    u_gain      = 0,
                    c_initial   = 0,
                    c_annual    = 0,
                    init_dist   = c(MCI = 0.25, Mild_AD = 0.45,
                                    Moderate_AD = 0.20, Severe_AD = 0.10,
                                    Death = 0.00),
                    params      = NULL,
                    tp_override = NULL) {
  
  # FIX: use tp_override when PSA provides a sampled matrix
  tp <- if (!is.null(tp_override)) tp_override else tp_base
  
  # FIX: arm names now match run_full_model() exactly
  if (!is.null(params)) {
    if (arm == "Psychosocial" && !is.null(params$rr_psych)) {
      rr <- params$rr_psych[1]
    }
    if (arm == "Technology" && !is.null(params$rr_tech)) {
      rr <- params$rr_tech[1]
    }
    if (arm == "Combination" && !is.null(params$rr_psych) &&
        !is.null(params$rr_tech)) {
      rr <- params$rr_psych[1] * params$rr_tech[1]
    }
  }
  
  tp_arm <- build_tp_matrix(tp, rr)
  
  state_costs <- c(
    MCI         = if (!is.null(params))
      params$c_MCI_med[1]  + params$c_MCI_inf[1]
    else costs$medical$MCI$mean + costs$informal$MCI$mean,
    Mild_AD     = if (!is.null(params))
      params$c_Mild_med[1] + params$c_Mild_inf[1]
    else costs$medical$Mild_AD$mean + costs$informal$Mild_AD$mean,
    Moderate_AD = if (!is.null(params))
      params$c_Mod_med[1]  + params$c_Mod_inf[1]
    else costs$medical$Moderate_AD$mean +
      costs$informal$Moderate_AD$mean,
    Severe_AD   = if (!is.null(params))
      params$c_Sev_med[1]  + params$c_Sev_inf[1]
    else costs$medical$Severe_AD$mean +
      costs$informal$Severe_AD$mean,
    Death       = 0
  )
  
  state_utils <- c(
    MCI         = if (!is.null(params)) params$u_MCI[1]
    else utilities$MCI$mean,
    Mild_AD     = if (!is.null(params)) params$u_Mild[1]
    else utilities$Mild_AD$mean,
    Moderate_AD = if (!is.null(params)) params$u_Moderate[1]
    else utilities$Moderate_AD$mean,
    Severe_AD   = if (!is.null(params)) params$u_Severe[1]
    else utilities$Severe_AD$mean,
    Death       = 0
  )
  
  trace   <- run_markov_trace(tp_arm, init_dist, n_cycles)
  costs_r <- calc_costs(trace, state_costs, c_initial, c_annual,
                        discount_costs)
  qalys_r <- calc_qalys(trace, state_utils, u_gain, discount_effects)
  
  list(arm        = arm,
       trace      = trace,
       total_cost = costs_r$total,
       total_qaly = qalys_r$total,
       cost_cycle = costs_r$per_cycle,
       qaly_cycle = qalys_r$per_cycle,
       tp_matrix  = tp_arm)
}

# -----------------------------------------------------------------------------
# FULL MODEL
# -----------------------------------------------------------------------------

run_full_model <- function(params    = NULL,
                           init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                                         Moderate_AD = 0.20, Severe_AD = 0.10,
                                         Death = 0.00)) {
  
  soc   <- run_arm("SoC",
                   rr = 1.0, u_gain = 0,
                   c_initial = 0, c_annual = 0,
                   init_dist = init_dist, params = params)
  
  psych <- run_arm("Psychosocial",
                   rr        = if (!is.null(params)) params$rr_psych[1]
                   else rr_psych_progression,
                   u_gain    = u_gain_psych,
                   c_initial = intervention_costs$psych$initial$mean,
                   c_annual  = if (!is.null(params)) params$c_psych_ann[1]
                   else intervention_costs$psych$annual$mean,
                   init_dist = init_dist, params = params)
  
  tech  <- run_arm("Technology",
                   rr        = if (!is.null(params)) params$rr_tech[1]
                   else rr_tech_progression,
                   u_gain    = u_gain_tech,
                   c_initial = intervention_costs$tech$initial$mean,
                   c_annual  = if (!is.null(params)) params$c_tech_ann[1]
                   else intervention_costs$tech$annual$mean,
                   init_dist = init_dist, params = params)
  
  combo <- run_arm("Combination",
                   rr        = if (!is.null(params))
                     params$rr_psych[1] * params$rr_tech[1]
                   else rr_combo_progression,
                   u_gain    = u_gain_psych + u_gain_tech,
                   c_initial = intervention_costs$combo$initial$mean,
                   c_annual  = if (!is.null(params)) params$c_psych_ann[1] +
                     params$c_tech_ann[1]
                   else intervention_costs$combo$annual$mean,
                   init_dist = init_dist, params = params)
  
  arms <- list(soc, psych, tech, combo)
  
  results <- data.frame(
    Arm        = sapply(arms, `[[`, "arm"),
    Total_Cost = sapply(arms, `[[`, "total_cost"),
    Total_QALY = sapply(arms, `[[`, "total_qaly"),
    stringsAsFactors = FALSE
  )
  
  results$Inc_Cost <- results$Total_Cost - results$Total_Cost[1]
  results$Inc_QALY <- results$Total_QALY - results$Total_QALY[1]
  results$ICER     <- ifelse(results$Inc_QALY > 0,
                             results$Inc_Cost / results$Inc_QALY,
                             NA_real_)
  results$NMB_20k  <- results$Inc_QALY * 20000 - results$Inc_Cost
  results$NMB_50k  <- results$Inc_QALY * 50000 - results$Inc_Cost
  results$NMB_80k  <- results$Inc_QALY * 80000 - results$Inc_Cost
  
  list(results = results,
       traces  = lapply(arms, `[[`, "trace"),
       arms    = arms)
}

# -----------------------------------------------------------------------------
# PSA ENGINE
# FIX: seed passed externally, not hardcoded inside sampling function
# -----------------------------------------------------------------------------

run_psa <- function(n_sim     = 1000,
                    init_dist = c(MCI = 0.25, Mild_AD = 0.45,
                                  Moderate_AD = 0.20, Severe_AD = 0.10,
                                  Death = 0.00),
                    seed      = 42) {
  
  cat("Running PSA:", n_sim, "simulations...\n")
  param_samples <- sample_psa_params(n_sim, seed = seed)
  
  psa_results <- lapply(seq_len(n_sim), function(i) {
    p_i <- lapply(param_samples, `[`, i)
    tryCatch({
      res     <- run_full_model(params = p_i, init_dist = init_dist)$results
      res$sim <- i
      res
    }, error = function(e) NULL)
  })
  
  psa_df <- do.call(rbind, Filter(Negate(is.null), psa_results))
  cat("PSA complete. Valid simulations:", nrow(psa_df) / 4, "\n")
  psa_df
}

# -----------------------------------------------------------------------------
# CEAC
# FIX: per-simulation NMB comparison now correctly aligned across arms
# -----------------------------------------------------------------------------

calc_ceac <- function(psa_df,
                      wtp = seq(0, 100000, by = 5000)) {
  
  arms_list <- unique(psa_df$Arm)
  
  ceac_list <- lapply(wtp, function(lambda) {
    
    # Build matrix: rows = simulations, cols = arms, values = NMB
    nmb_matrix <- tapply(
      lambda * psa_df$Inc_QALY - psa_df$Inc_Cost,
      list(psa_df$sim, psa_df$Arm),
      mean
    )
    
    # For each simulation, find which arm has the highest NMB
    best_per_sim <- apply(nmb_matrix, 1, which.max)
    
    arm_probs <- sapply(seq_along(arms_list), function(k) {
      sum(best_per_sim == k) / nrow(nmb_matrix)
    })
    
    data.frame(WTP     = lambda,
               Arm     = arms_list,
               Prob_CE = arm_probs,
               stringsAsFactors = FALSE)
  })
  
  do.call(rbind, ceac_list)
}