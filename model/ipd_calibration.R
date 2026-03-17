# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/ipd_calibration.R
# Description: IPD calibration using synthetic data
# Note: Uses synthetic data generated from published parameters.
#       Swap generate_synthetic_ipd() for real data when available.
# =============================================================================

library(dplyr)
library(tidyr)
library(MASS)
library(nnet)
library(survival)
library(ggplot2)
library(purrr)

source("model/parameters.R")

# Ensure dplyr takes priority over MASS
select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
arrange <- dplyr::arrange
rename  <- dplyr::rename

# -----------------------------------------------------------------------------
# SECTION 1: SYNTHETIC DATA GENERATOR
# -----------------------------------------------------------------------------

#' Generate synthetic IPD for calibration pipeline
#'
#' @param n_patients Integer. Number of simulated patients
#' @param n_years    Integer. Follow-up duration in years
#' @param seed       Integer. Random seed for reproducibility
#' @return Data frame in IPD long format

generate_synthetic_ipd <- function(n_patients = 500,
                                   n_years    = 5,
                                   seed       = 2024) {
  set.seed(seed)
  cat(sprintf(
    "Generating synthetic IPD: %d patients, %d years\n",
    n_patients, n_years))
  cat("NOTE: Synthetic data only — for pipeline testing.\n\n")
  
  mmse_ranges <- list(
    MCI         = c(24, 30),
    Mild_AD     = c(20, 23),
    Moderate_AD = c(10, 19),
    Severe_AD   = c(0,   9)
  )
  
  eq5d_means <- c(MCI = 0.74, Mild_AD = 0.65,
                  Moderate_AD = 0.47, Severe_AD = 0.28)
  eq5d_sds   <- c(MCI = 0.12, Mild_AD = 0.14,
                  Moderate_AD = 0.16, Severe_AD = 0.18)
  
  care_hours <- c(MCI = 10,  Mild_AD = 25,
                  Moderate_AD = 45, Severe_AD = 65)
  care_sds   <- c(MCI = 8,   Mild_AD = 15,
                  Moderate_AD = 20, Severe_AD = 25)
  
  records <- lapply(seq_len(n_patients), function(pid) {
    
    start_state <- sample(
      c("MCI", "Mild_AD", "Moderate_AD", "Severe_AD"),
      1, prob = c(0.25, 0.45, 0.20, 0.10)
    )
    age_0 <- round(rnorm(1, 75, 8))
    sex   <- sample(c("M", "F"), 1, prob = c(0.35, 0.65))
    
    state_now  <- start_state
    visit_list <- list()
    
    for (yr in 0:n_years) {
      if (state_now == "Death") break
      
      mmse_val <- runif(1,
                        mmse_ranges[[state_now]][1],
                        mmse_ranges[[state_now]][2])
      
      eq5d_val <- rnorm(1,
                        eq5d_means[state_now],
                        eq5d_sds[state_now])
      eq5d_val <- max(-0.594, min(1, eq5d_val))
      
      care_val <- rnorm(1,
                        care_hours[state_now],
                        care_sds[state_now])
      care_val <- max(0, care_val)
      
      visit_list[[yr + 1]] <- data.frame(
        patient_id     = pid,
        visit_time     = yr,
        mmse           = round(mmse_val, 1),
        eq5d_index     = round(eq5d_val, 3),
        age            = age_0 + yr,
        sex            = sex,
        informal_hours = round(care_val, 1),
        vital_status   = "Alive",
        country        = sample(c("NL", "SE", "DE"), 1,
                                prob = c(0.5, 0.3, 0.2)),
        arm            = sample(c("SoC", "Psychosocial"), 1),
        stringsAsFactors = FALSE
      )
      
      tp_now     <- tp_base[state_now, ]
      next_state <- sample(health_states, 1, prob = tp_now)
      
      if (next_state == "Death") {
        death_rec              <- visit_list[[yr + 1]]
        death_rec$visit_time   <- yr + 0.5
        death_rec$vital_status <- "Dead"
        visit_list[[yr + 2]]   <- death_rec
        break
      }
      state_now <- next_state
    }
    
    do.call(rbind, visit_list)
  })
  
  ipd <- do.call(rbind, records)
  cat(sprintf("Generated: %d patients, %d records\n\n",
              n_patients, nrow(ipd)))
  ipd
}

# -----------------------------------------------------------------------------
# SECTION 2: HEALTH STATE CLASSIFICATION
# -----------------------------------------------------------------------------

#' Classify MMSE score into DEM-CAPS health states
#' @param mmse Numeric vector
#' @return Character vector of health state labels

classify_health_state <- function(mmse) {
  dplyr::case_when(
    is.na(mmse)  ~ NA_character_,
    mmse >= 24   ~ "MCI",
    mmse >= 20   ~ "Mild_AD",
    mmse >= 10   ~ "Moderate_AD",
    mmse >= 0    ~ "Severe_AD",
    TRUE         ~ NA_character_
  )
}

#' Derive state transitions from longitudinal IPD
#' @param ipd Data frame in long format
#' @return Data frame with transitions added

derive_transitions <- function(ipd) {
  
  names(ipd) <- tolower(names(ipd))
  
  ipd %>%
    dplyr::arrange(patient_id, visit_time) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::mutate(
      health_state = classify_health_state(mmse),
      state_from   = dplyr::lag(health_state),
      state_to     = health_state,
      time_diff    = visit_time - dplyr::lag(visit_time),
      cycle        = floor(visit_time)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(state_from)) %>%
    dplyr::mutate(
      state_to = dplyr::if_else(
        !is.na(vital_status) & vital_status == "Dead",
        "Death",
        state_to
      )
    )
}

# -----------------------------------------------------------------------------
# SECTION 3: TRANSITION PROBABILITY ESTIMATION
# -----------------------------------------------------------------------------

#' Estimate transition probability matrix from IPD
#'
#' @param ipd_trans  Data frame from derive_transitions()
#' @param method     Character. "counts" or "multinomial"
#' @return List with tp_matrix and diagnostics

estimate_transition_matrix <- function(ipd_trans,
                                       method = "counts") {
  
  cat(sprintf("Estimating transitions: method='%s'\n", method))
  
  ipd_annual <- ipd_trans %>%
    dplyr::filter(time_diff > 0.5 & time_diff < 2.0)
  
  if (method == "counts") {
    
    count_mat <- table(
      from = ipd_annual$state_from,
      to   = ipd_annual$state_to
    )
    
    full_states <- health_states
    count_full  <- matrix(0,
                          nrow     = length(full_states),
                          ncol     = length(full_states),
                          dimnames = list(full_states, full_states)
    )
    
    shared_rows <- intersect(rownames(count_mat), full_states)
    shared_cols <- intersect(colnames(count_mat), full_states)
    count_full[shared_rows, shared_cols] <-
      count_mat[shared_rows, shared_cols]
    
    count_full["Death", "Death"] <- 1
    
    alpha  <- 0.5
    tp_ipd <- t(apply(count_full + alpha, 1,
                      function(r) r / sum(r)))
    
    for (i in seq_len(length(full_states) - 1)) {
      for (j in seq_len(i - 1)) {
        surplus       <- tp_ipd[i, j]
        tp_ipd[i, j]  <- 0
        tp_ipd[i, i]  <- tp_ipd[i, i] + surplus
      }
    }
    
    tp_ipd <- t(apply(tp_ipd, 1,
                      function(r) r / sum(r)))
    
    return(list(
      tp_matrix = tp_ipd,
      method    = "counts",
      n_trans   = sum(count_full)
    ))
  }
  
  if (method == "multinomial") {
    
    ipd_sub <- ipd_annual %>%
      dplyr::filter(!is.na(state_from),
                    !is.na(state_to)) %>%
      dplyr::mutate(
        state_to   = factor(state_to,
                            levels = health_states),
        state_from = factor(state_from,
                            levels = health_states)
      )
    
    model_mn <- nnet::multinom(
      state_to ~ state_from + age + sex,
      data    = ipd_sub,
      MaxNWts = 5000,
      trace   = FALSE
    )
    
    mean_age     <- mean(ipd_sub$age, na.rm = TRUE)
    tp_ipd       <- matrix(0,
                           nrow     = length(health_states),
                           ncol     = length(health_states),
                           dimnames = list(health_states, health_states)
    )
    
    alive_states <- health_states[health_states != "Death"]
    for (from_s in alive_states) {
      nd  <- data.frame(
        state_from = factor(from_s, levels = health_states),
        age        = mean_age,
        sex        = "F"
      )
      pp  <- predict(model_mn, newdata = nd, type = "probs")
      tp_ipd[from_s, names(pp)] <- as.numeric(pp)
    }
    
    tp_ipd["Death", "Death"] <- 1
    tp_ipd <- t(apply(tp_ipd, 1, function(r) {
      if (sum(r) > 0) r / sum(r) else r
    }))
    
    return(list(
      tp_matrix = tp_ipd,
      method    = "multinomial",
      model     = model_mn
    ))
  }
  
  stop("Unknown method. Use 'counts' or 'multinomial'.")
}

# -----------------------------------------------------------------------------
# SECTION 4: UTILITY ESTIMATION
# -----------------------------------------------------------------------------

#' Estimate state utilities from IPD EQ-5D data
#'
#' @param ipd       Data frame. Full IPD
#' @param ipd_trans Data frame. IPD with health_state
#' @return Named list of utilities per state

estimate_utilities_ipd <- function(ipd, ipd_trans) {
  
  ipd_u <- ipd %>%
    dplyr::left_join(
      ipd_trans %>%
        dplyr::select(patient_id, visit_time, health_state),
      by = c("patient_id", "visit_time")
    ) %>%
    dplyr::filter(!is.na(health_state),
                  !is.na(eq5d_index))
  
  state_utils <- ipd_u %>%
    dplyr::group_by(health_state) %>%
    dplyr::summarise(
      mean_util = mean(eq5d_index,  na.rm = TRUE),
      se_util   = sd(eq5d_index,    na.rm = TRUE) /
        sqrt(dplyr::n()),
      n         = dplyr::n(),
      .groups   = "drop"
    )
  
  cat("\nIPD-estimated utilities:\n")
  print(state_utils)
  
  result <- setNames(
    lapply(seq_len(nrow(state_utils)), function(i) {
      list(
        mean = max(0, min(1, state_utils$mean_util[i])),
        se   = state_utils$se_util[i]
      )
    }),
    as.character(state_utils$health_state)
  )
  result$Death <- list(mean = 0, se = 0)
  result
}

# -----------------------------------------------------------------------------
# SECTION 5: INFORMAL CARE COST ESTIMATION
# -----------------------------------------------------------------------------

#' Estimate informal care costs from IPD hours data
#'
#' @param ipd         Data frame. IPD with informal_hours
#' @param ipd_trans   Data frame. IPD with health_state
#' @param hourly_rate Numeric. EUR per hour (ZIN 2024)
#' @return Named list of annual costs per state

estimate_informal_costs_ipd <- function(ipd,
                                        ipd_trans,
                                        hourly_rate = 16.70) {
  
  ipd_c <- ipd %>%
    dplyr::left_join(
      ipd_trans %>%
        dplyr::select(patient_id, visit_time, health_state),
      by = c("patient_id", "visit_time")
    ) %>%
    dplyr::filter(!is.na(health_state),
                  !is.na(informal_hours)) %>%
    dplyr::mutate(
      annual_cost = informal_hours * 52 * hourly_rate)
  
  cost_summary <- ipd_c %>%
    dplyr::group_by(health_state) %>%
    dplyr::summarise(
      mean_cost = mean(annual_cost, na.rm = TRUE),
      se_cost   = sd(annual_cost,   na.rm = TRUE) /
        sqrt(dplyr::n()),
      n         = dplyr::n(),
      .groups   = "drop"
    )
  
  cat("\nIPD-estimated informal care costs (EUR/year):\n")
  print(cost_summary)
  
  result <- setNames(
    lapply(seq_len(nrow(cost_summary)), function(i) {
      list(
        mean = cost_summary$mean_cost[i],
        se   = cost_summary$se_cost[i]
      )
    }),
    as.character(cost_summary$health_state)
  )
  result$Death <- list(mean = 0, se = 0)
  result
}

# -----------------------------------------------------------------------------
# SECTION 6: GOODNESS-OF-FIT
# -----------------------------------------------------------------------------

#' Compute GOF: predicted vs observed state prevalence
#'
#' @param tp_cal    Matrix. Calibrated TP matrix
#' @param ipd_trans Data frame. Observed transitions
#' @return List with RMSE, MAE and detail data frame

compute_gof <- function(tp_cal, ipd_trans) {
  
  obs_prev <- ipd_trans %>%
    dplyr::group_by(cycle, state_to) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(cycle) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup()
  
  init_obs <- ipd_trans %>%
    dplyr::filter(cycle == min(cycle)) %>%
    dplyr::count(state_to) %>%
    dplyr::mutate(prop = n / sum(n))
  
  init_vec <- setNames(
    rep(0, length(health_states)), health_states)
  for (i in seq_len(nrow(init_obs))) {
    s <- init_obs$state_to[i]
    if (s %in% names(init_vec))
      init_vec[s] <- init_obs$prop[i]
  }
  if (sum(init_vec) > 0)
    init_vec <- init_vec / sum(init_vec)
  
  max_cycle  <- max(obs_prev$cycle, na.rm = TRUE)
  pred_trace <- matrix(0,
                       nrow = max_cycle + 1,
                       ncol = length(health_states)
  )
  colnames(pred_trace) <- health_states
  pred_trace[1, ]      <- init_vec
  
  for (t in seq_len(max_cycle)) {
    pred_trace[t + 1, ] <- pred_trace[t, ] %*% tp_cal
  }
  
  pred_long <- as.data.frame(pred_trace) %>%
    dplyr::mutate(cycle = 0:max_cycle) %>%
    tidyr::pivot_longer(-cycle,
                        names_to  = "state_to",
                        values_to = "pred_prop")
  
  gof_df <- obs_prev %>%
    dplyr::inner_join(pred_long,
                      by = c("cycle", "state_to")) %>%
    dplyr::mutate(sq_error = (prop - pred_prop)^2)
  
  rmse <- sqrt(mean(gof_df$sq_error, na.rm = TRUE))
  mae  <- mean(abs(gof_df$prop - gof_df$pred_prop),
               na.rm = TRUE)
  
  cat(sprintf("\nGOF: RMSE = %.4f | MAE = %.4f\n",
              rmse, mae))
  list(rmse = rmse, mae = mae, detail = gof_df)
}

# -----------------------------------------------------------------------------
# SECTION 7: DIAGNOSTIC PLOTS
# -----------------------------------------------------------------------------

#' Plot observed vs predicted state occupancy
#' @param gof_result List from compute_gof()
#' @return ggplot object

plot_gof <- function(gof_result) {
  
  ggplot(gof_result$detail, aes(x = cycle)) +
    geom_point(aes(y = prop, colour = "Observed"),
               size = 2, alpha = 0.7) +
    geom_line(aes(y = pred_prop, colour = "Predicted"),
              linewidth = 1.0) +
    facet_wrap(~ state_to, scales = "free_y", ncol = 2) +
    scale_colour_manual(
      values = c("Observed"  = "#E74C3C",
                 "Predicted" = "#1ABC9C")) +
    labs(
      title    = "GOF: Observed vs Predicted State Occupancy",
      subtitle = paste0(
        "RMSE = ", round(gof_result$rmse, 4),
        " | MAE = ", round(gof_result$mae, 4)),
      x        = "Cycle (Years)",
      y        = "Proportion",
      colour   = NULL
    ) +
    theme_minimal() +
    theme(strip.text      = element_text(face = "bold"),
          legend.position = "bottom")
}

#' Plot TP matrix heatmap comparison
#' @param tp_lit Matrix. Literature-based (tp_base)
#' @param tp_ipd Matrix. IPD-calibrated

plot_tp_comparison <- function(tp_lit = tp_base,
                               tp_ipd) {
  
  melt_tp <- function(mat, label) {
    df           <- as.data.frame(as.table(mat))
    colnames(df) <- c("From", "To", "Probability")
    df$Source    <- label
    df
  }
  
  df_plot <- rbind(
    melt_tp(tp_lit, "Literature (Handels 2017)"),
    melt_tp(tp_ipd, "IPD-Calibrated (Synthetic)")
  )
  
  ggplot(df_plot, aes(x = To, y = From,
                      fill = Probability)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.2f", Probability)),
              size = 3, colour = "white") +
    scale_fill_gradient2(
      low = "#AED6F1", mid = "#2E86C1",
      high = "#1A252F", midpoint = 0.3,
      limits = c(0, 1)) +
    facet_wrap(~ Source, ncol = 2) +
    scale_x_discrete(position = "top") +
    labs(title = "TP Matrix: Literature vs IPD-Calibrated",
         x = "To State", y = "From State") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30,
                                     hjust = 0),
          strip.text  = element_text(face = "bold"))
}

# -----------------------------------------------------------------------------
# SECTION 8: APPLY CALIBRATED PARAMS
# -----------------------------------------------------------------------------

#' Apply calibrated parameters to model environment
#' @param cal_path Character. Path to saved .rds file

apply_calibrated_params <- function(
    cal_path = "data/processed/calibrated_params.rds") {
  
  if (!file.exists(cal_path))
    stop(paste("File not found:", cal_path))
  
  cal <- readRDS(cal_path)
  cat(sprintf(
    "Loading: %d patients, calibrated %s\n",
    cal$n_patients, cal$calibration_date))
  
  assign("tp_base",   cal$tp_matrix, envir = .GlobalEnv)
  assign("utilities", cal$utilities, envir = .GlobalEnv)
  
  cat("Calibrated parameters applied.\n")
  invisible(cal)
}

#' Reset to literature parameters
reset_to_literature_params <- function() {
  source("model/parameters.R")
  cat("Reset to literature parameters.\n")
}

# -----------------------------------------------------------------------------
# SECTION 9: MASTER CALIBRATION PIPELINE
# -----------------------------------------------------------------------------

#' Run full IPD calibration pipeline
#'
#' @param ipd_path  Character. Path to IPD CSV
#'                  Use NULL to generate synthetic data
#' @param method_tp Character. "counts" or "multinomial"
#' @param save_path Character. Output .rds path
#' @param n_synth   Integer. Patients if synthetic
#' @return List of calibrated parameters

run_ipd_calibration <- function(
    ipd_path  = NULL,
    method_tp = "counts",
    save_path = "data/processed/calibrated_params.rds",
    n_synth   = 500) {
  
  cat("=== DEM-CAPS IPD Calibration Pipeline ===\n")
  cat(sprintf("Method: %s\n\n", method_tp))
  
  # 1. Load or generate data
  if (is.null(ipd_path)) {
    cat("No IPD path — using synthetic data.\n")
    ipd <- generate_synthetic_ipd(
      n_patients = n_synth, seed = 2024)
  } else {
    ipd <- if (grepl("\\.rds$", ipd_path,
                     ignore.case = TRUE)) {
      readRDS(ipd_path)
    } else {
      read.csv(ipd_path, stringsAsFactors = TRUE)
    }
    names(ipd) <- tolower(names(ipd))
    cat(sprintf("Loaded: %d rows, %d patients\n",
                nrow(ipd),
                length(unique(ipd$patient_id))))
  }
  
  # 2. Derive transitions
  ipd_trans <- derive_transitions(ipd)
  cat(sprintf("Transitions: %d pairs\n",
              nrow(ipd_trans)))
  
  # 3. Estimate TP matrix
  tp_result <- estimate_transition_matrix(
    ipd_trans, method = method_tp)
  tp_cal    <- tp_result$tp_matrix
  
  # 4. Estimate utilities
  utils_cal <- estimate_utilities_ipd(ipd, ipd_trans)
  
  # 5. Estimate informal costs
  informal_cal <- estimate_informal_costs_ipd(
    ipd, ipd_trans)
  
  # 6. Goodness of fit
  gof <- compute_gof(tp_cal, ipd_trans)
  
  # 7. Package
  calibrated <- list(
    tp_matrix        = tp_cal,
    utilities        = utils_cal,
    informal_costs   = informal_cal,
    method_tp        = method_tp,
    n_patients       = length(unique(ipd$patient_id)),
    n_transitions    = nrow(ipd_trans),
    calibration_date = Sys.Date(),
    data_source      = if (is.null(ipd_path))
      "synthetic" else
        basename(ipd_path),
    gof              = gof
  )
  
  # 8. Save
  dir.create(dirname(save_path),
             showWarnings = FALSE, recursive = TRUE)
  saveRDS(calibrated, save_path)
  cat(sprintf("\nSaved: %s\n", save_path))
  
  # 9. Summary
  cat("\n=== Calibration Summary ===\n")
  cat(sprintf("Patients:    %d\n", calibrated$n_patients))
  cat(sprintf("Transitions: %d\n", calibrated$n_transitions))
  cat(sprintf("RMSE:        %.4f\n", gof$rmse))
  cat(sprintf("MAE:         %.4f\n", gof$mae))
  cat(sprintf("Data source: %s\n\n", calibrated$data_source))
  cat("Calibrated TP matrix:\n")
  print(round(tp_cal, 3))
  
  invisible(calibrated)
}

# -----------------------------------------------------------------------------
# SECTION 10: QUICK TEST
# -----------------------------------------------------------------------------

#' Test full pipeline on synthetic data
test_ipd_pipeline <- function() {
  
  cat("===== IPD PIPELINE TEST =====\n\n")
  
  cal <- run_ipd_calibration(
    ipd_path  = NULL,
    method_tp = "counts",
    save_path = "data/processed/calibrated_params.rds",
    n_synth   = 300
  )
  
  cat("\nLiterature TP matrix:\n")
  print(round(tp_base, 3))
  
  cat("\nCalibrated TP matrix:\n")
  print(round(cal$tp_matrix, 3))
  
  cat("\nDifference (calibrated - literature):\n")
  print(round(cal$tp_matrix - tp_base, 3))
  
  apply_calibrated_params()
  source("model/markov_model.R")
  results <- run_full_model()
  
  cat("\nResults with calibrated parameters:\n")
  print(results$results[,
                        c("Arm", "Total_Cost", "Total_QALY", "ICER")])
  
  reset_to_literature_params()
  cat("\n===== TEST COMPLETE =====\n")
  invisible(cal)
}