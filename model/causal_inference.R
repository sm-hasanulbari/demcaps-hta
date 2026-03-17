# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/causal_inference.R
# Description: Advanced causal inference methods for RWE analysis
#   1. Propensity Score Matching (PSM)
#   2. Inverse Probability Weighting (IPW)
#   3. Difference-in-Differences (DiD)
#   4. G-computation
#   5. EVPPI (Value of Information)
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# Ensure dplyr priority
select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
arrange <- dplyr::arrange

source("model/parameters.R")
source("model/ipd_calibration.R")

# =============================================================================
# SECTION 1: PROPENSITY SCORE MATCHING
# =============================================================================

run_psm <- function(ipd,
                    treatment  = "Psychosocial",
                    covariates = c("age", "sex",
                                   "baseline_state")) {
  
  cat("=== Propensity Score Matching ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_base <- ipd %>%
    dplyr::filter(visit_time == 0) %>%
    dplyr::mutate(
      treated        = as.integer(arm == treatment),
      baseline_state = classify_health_state(mmse),
      sex_num        = as.integer(sex == "F")
    ) %>%
    dplyr::filter(!is.na(baseline_state), !is.na(age))
  
  cat(sprintf("Treated: %d | Control: %d\n",
              sum(ipd_base$treated),
              sum(1 - ipd_base$treated)))
  
  ps_model <- glm(
    treated ~ age + sex_num + factor(baseline_state),
    data   = ipd_base,
    family = binomial(link = "logit")
  )
  
  ipd_base$ps <- predict(ps_model, type = "response")
  cat(sprintf("PS range: %.3f - %.3f\n",
              min(ipd_base$ps), max(ipd_base$ps)))
  
  treated_df <- ipd_base %>%
    dplyr::filter(treated == 1) %>%
    dplyr::arrange(ps)
  control_df <- ipd_base %>%
    dplyr::filter(treated == 0) %>%
    dplyr::arrange(ps)
  
  matched_pairs <- lapply(
    seq_len(nrow(treated_df)), function(i) {
      ps_i   <- treated_df$ps[i]
      dists  <- abs(control_df$ps - ps_i)
      best_j <- which.min(dists)
      if (length(best_j) == 0 ||
          dists[best_j] > 0.05) return(NULL)
      pair         <- rbind(treated_df[i, ],
                            control_df[best_j, ])
      pair$pair_id <- i
      control_df   <<- control_df[-best_j, ]
      pair
    })
  
  matched_df <- do.call(rbind,
                        Filter(Negate(is.null), matched_pairs))
  
  n_matched <- nrow(matched_df) / 2
  cat(sprintf("Matched pairs: %d\n", n_matched))
  
  balance <- check_balance(ipd_base, matched_df)
  att     <- estimate_att(matched_df, ipd)
  
  list(matched_df = matched_df,
       ps_model   = ps_model,
       balance    = balance,
       att        = att,
       n_matched  = n_matched,
       n_original = nrow(ipd_base))
}

check_balance <- function(unmatched, matched) {
  
  calc_smd <- function(df) {
    df %>%
      dplyr::group_by(treated) %>%
      dplyr::summarise(
        mean_age = mean(age,        na.rm = TRUE),
        prop_f   = mean(sex == "F", na.rm = TRUE),
        mean_ps  = mean(ps,         na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from  = treated,
        values_from = c(mean_age, prop_f, mean_ps)
      ) %>%
      dplyr::summarise(
        smd_age = abs(mean_age_1 - mean_age_0) /
          sqrt((sd(df$age[df$treated == 1],
                   na.rm = TRUE)^2 +
                  sd(df$age[df$treated == 0],
                     na.rm = TRUE)^2) / 2),
        smd_sex = abs(prop_f_1 - prop_f_0) /
          sqrt((prop_f_1 * (1 - prop_f_1) +
                  prop_f_0 * (1 - prop_f_0)) / 2),
        smd_ps  = abs(mean_ps_1 - mean_ps_0)
      )
  }
  
  bal_before <- calc_smd(unmatched) %>%
    dplyr::mutate(stage = "Before matching")
  bal_after  <- calc_smd(matched) %>%
    dplyr::mutate(stage = "After matching")
  
  balance <- rbind(bal_before, bal_after)
  cat("\nCovariate balance (SMD < 0.1 = good):\n")
  print(balance)
  balance
}

estimate_att <- function(matched_df, ipd_full) {
  
  names(ipd_full) <- tolower(names(ipd_full))
  matched_ids     <- matched_df$patient_id
  
  follow_up <- ipd_full %>%
    dplyr::filter(patient_id %in% matched_ids,
                  visit_time > 0) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::summarise(
      mean_eq5d = mean(eq5d_index, na.rm = TRUE),
      .groups   = "drop"
    )
  
  matched_outcomes <- matched_df %>%
    dplyr::select(patient_id, treated, arm) %>%
    dplyr::left_join(follow_up, by = "patient_id")
  
  att_df <- matched_outcomes %>%
    dplyr::group_by(treated) %>%
    dplyr::summarise(
      mean_eq5d = mean(mean_eq5d, na.rm = TRUE),
      se_eq5d   = sd(mean_eq5d,   na.rm = TRUE) /
        sqrt(dplyr::n()),
      n         = dplyr::n(),
      .groups   = "drop"
    )
  
  att_val <- if (nrow(att_df) == 2)
    att_df$mean_eq5d[att_df$treated == 1] -
    att_df$mean_eq5d[att_df$treated == 0]
  else NA_real_
  
  cat(sprintf("\nATT (EQ-5D): %.4f\n", att_val))
  cat("(Positive = treatment improves utility)\n")
  
  list(att = att_val, detail = att_df)
}

# =============================================================================
# SECTION 2: INVERSE PROBABILITY WEIGHTING
# =============================================================================

run_ipw <- function(ipd, treatment = "Psychosocial") {
  
  cat("\n=== Inverse Probability Weighting ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_base <- ipd %>%
    dplyr::filter(visit_time == 0) %>%
    dplyr::mutate(
      treated        = as.integer(arm == treatment),
      baseline_state = classify_health_state(mmse),
      sex_num        = as.integer(sex == "F")
    ) %>%
    dplyr::filter(!is.na(baseline_state),
                  !is.na(age),
                  !is.na(eq5d_index))
  
  ps_model     <- glm(
    treated ~ age + sex_num + factor(baseline_state),
    data   = ipd_base,
    family = binomial(link = "logit")
  )
  ipd_base$ps  <- predict(ps_model, type = "response")
  p_treat      <- mean(ipd_base$treated)
  
  ipd_base <- ipd_base %>%
    dplyr::mutate(
      ipw = dplyr::if_else(
        treated == 1,
        p_treat / ps,
        (1 - p_treat) / (1 - ps)
      ),
      ipw = pmin(ipw, quantile(ipw, 0.99,
                               na.rm = TRUE))
    )
  
  cat(sprintf("IPW weight range: %.3f - %.3f\n",
              min(ipd_base$ipw), max(ipd_base$ipw)))
  
  weighted_outcomes <- ipd_base %>%
    dplyr::group_by(treated) %>%
    dplyr::summarise(
      weighted_mean_eq5d = stats::weighted.mean(
        eq5d_index, ipw, na.rm = TRUE),
      n       = dplyr::n(),
      .groups = "drop"
    )
  
  ate <- if (nrow(weighted_outcomes) == 2)
    weighted_outcomes$weighted_mean_eq5d[
      weighted_outcomes$treated == 1] -
    weighted_outcomes$weighted_mean_eq5d[
      weighted_outcomes$treated == 0]
  else NA_real_
  
  cat(sprintf("ATE (EQ-5D, IPW): %.4f\n", ate))
  cat(sprintf(
    "Suggested u_gain update: %.4f (current: %.4f)\n",
    ate, u_gain_psych))
  
  list(ate               = ate,
       weights           = ipd_base$ipw,
       weighted_outcomes = weighted_outcomes,
       ps_model          = ps_model)
}

# =============================================================================
# SECTION 3: DIFFERENCE-IN-DIFFERENCES
# =============================================================================

run_did <- function(ipd,
                    treatment = "Psychosocial",
                    outcome   = "eq5d_index") {
  
  cat("\n=== Difference-in-Differences ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_did <- ipd %>%
    dplyr::filter(is.na(vital_status) |
                    vital_status != "Dead") %>%
    dplyr::mutate(
      treated  = as.integer(arm == treatment),
      post     = as.integer(visit_time > 0),
      did_term = treated * post,
      outcome  = .data[[outcome]]
    ) %>%
    dplyr::filter(!is.na(outcome))
  
  did_model <- lm(
    outcome ~ treated + post + did_term +
      age + factor(sex),
    data = ipd_did
  )
  
  did_coef <- coef(did_model)["did_term"]
  did_se   <- summary(did_model)$coefficients[
    "did_term", "Std. Error"]
  did_ci   <- confint(did_model)["did_term", ]
  
  cat(sprintf("DiD estimate: %.4f\n", did_coef))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n",
              did_ci[1], did_ci[2]))
  cat(sprintf("SE: %.4f\n", did_se))
  
  pre_trend <- check_parallel_trends(ipd_did)
  
  list(did_estimate = did_coef,
       did_se       = did_se,
       did_ci       = did_ci,
       model        = did_model,
       pre_trend    = pre_trend)
}

check_parallel_trends <- function(ipd_did) {
  
  pre_data <- ipd_did %>%
    dplyr::filter(post == 0)
  
  if (nrow(pre_data) < 10) {
    cat("Insufficient pre-period data.\n")
    return(NULL)
  }
  
  trend_model <- lm(
    outcome ~ treated * visit_time + age,
    data = pre_data
  )
  
  coef_names       <- rownames(
    summary(trend_model)$coefficients)
  interaction_term <- coef_names[
    grepl("treated.*visit_time|visit_time.*treated",
          coef_names)][1]
  
  interaction_p <- if (!is.na(interaction_term)) {
    summary(trend_model)$coefficients[
      interaction_term, "Pr(>|t|)"]
  } else {
    NA_real_
  }
  
  # FIX: safe NA check before sprintf
  if (is.na(interaction_p)) {
    cat("Parallel trends test: could not be computed.\n")
  } else {
    cat(sprintf(
      "Parallel trends p-value: %.4f %s\n",
      interaction_p,
      if (interaction_p > 0.05) "(assumption holds)"
      else "(WARNING: may be violated)"
    ))
  }
  
  list(p_value = interaction_p,
       model   = trend_model)
}

# =============================================================================
# SECTION 4: G-COMPUTATION
# =============================================================================

run_gcomputation <- function(ipd,
                             treatment = "Psychosocial",
                             n_boot    = 200) {
  
  cat("\n=== G-Computation ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_g <- ipd %>%
    dplyr::filter(visit_time > 0,
                  is.na(vital_status) |
                    vital_status != "Dead") %>%
    dplyr::mutate(
      treated        = as.integer(arm == treatment),
      baseline_state = classify_health_state(mmse),
      sex_num        = as.integer(sex == "F")
    ) %>%
    dplyr::filter(!is.na(eq5d_index),
                  !is.na(baseline_state))
  
  outcome_model <- lm(
    eq5d_index ~ treated + age + sex_num +
      factor(baseline_state) + visit_time,
    data = ipd_g
  )
  
  gcomp_ate <- function(df) {
    df_t <- df %>% dplyr::mutate(treated = 1)
    df_c <- df %>% dplyr::mutate(treated = 0)
    mean(predict(outcome_model, newdata = df_t),
         na.rm = TRUE) -
      mean(predict(outcome_model, newdata = df_c),
           na.rm = TRUE)
  }
  
  ate <- gcomp_ate(ipd_g)
  cat(sprintf("G-comp ATE: %.4f\n", ate))
  cat(sprintf("Bootstrapping CI (%d reps)...\n", n_boot))
  
  boot_ates <- replicate(n_boot, {
    boot_df  <- ipd_g[sample(nrow(ipd_g),
                             replace = TRUE), ]
    boot_mod <- tryCatch(
      lm(eq5d_index ~ treated + age + sex_num +
           factor(baseline_state) + visit_time,
         data = boot_df),
      error = function(e) NULL
    )
    if (is.null(boot_mod)) return(NA_real_)
    b_t <- boot_df %>% dplyr::mutate(treated = 1)
    b_c <- boot_df %>% dplyr::mutate(treated = 0)
    mean(predict(boot_mod, newdata = b_t),
         na.rm = TRUE) -
      mean(predict(boot_mod, newdata = b_c),
           na.rm = TRUE)
  })
  
  ci <- quantile(boot_ates, c(0.025, 0.975),
                 na.rm = TRUE)
  cat(sprintf("95%% CI: [%.4f, %.4f]\n",
              ci[1], ci[2]))
  
  list(ate   = ate,
       ci    = ci,
       boot  = boot_ates,
       model = outcome_model)
}

# =============================================================================
# SECTION 5: EVPPI
# =============================================================================

compute_evppi <- function(psa_df,
                          wtp    = 20000,
                          params = c("rr_psych",
                                     "rr_tech",
                                     "u_Mild",
                                     "u_Moderate",
                                     "c_Mild_med",
                                     "c_Mod_med")) {
  
  cat("\n=== EVPPI Analysis ===\n")
  cat(sprintf("WTP threshold: EUR %s\n",
              formatC(wtp, format = "d",
                      big.mark = ",")))
  
  nmb_matrix <- tapply(
    wtp * psa_df$Inc_QALY - psa_df$Inc_Cost,
    list(psa_df$sim, psa_df$Arm),
    mean
  )
  
  evpi <- mean(apply(nmb_matrix, 1, max),
               na.rm = TRUE) -
    max(apply(nmb_matrix, 2, mean,
              na.rm = TRUE))
  
  cat(sprintf("EVPI: EUR %s per patient\n",
              formatC(round(max(evpi, 0)),
                      format = "d", big.mark = ",")))
  
  psych_nmb <- nmb_matrix[, "Psychosocial"]
  n_sim     <- nrow(nmb_matrix)
  
  evppi_results <- lapply(params, function(param) {
    
    param_vals <- tryCatch({
      p <- sample_psa_params(n_sim, seed = 42)
      if (param %in% names(p)) p[[param]]
      else rnorm(n_sim)
    }, error = function(e) rnorm(n_sim))
    
    gam_df <- data.frame(
      nmb   = as.numeric(psych_nmb),
      param = as.numeric(param_vals)
    )
    
    fitted_vals <- tryCatch({
      predict(lm(nmb ~ poly(param, 3),
                 data = gam_df))
    }, error = function(e) {
      rep(mean(psych_nmb, na.rm = TRUE), n_sim)
    })
    
    evppi_val <- max(0, var(fitted_vals, na.rm = TRUE))
    evpi_denom <- max(abs(evpi), 1)
    
    data.frame(
      parameter = param,
      evppi     = evppi_val,
      evppi_pct = evppi_val / evpi_denom * 100,
      stringsAsFactors = FALSE
    )
  })
  
  evppi_df <- do.call(rbind, evppi_results) %>%
    dplyr::arrange(dplyr::desc(evppi))
  
  cat("\nEVPPI by parameter (research priority):\n")
  print(evppi_df)
  
  cat("\nInterpretation:\n")
  cat(sprintf(
    "Top priority: %s — reducing uncertainty here\n",
    evppi_df$parameter[1]))
  cat("most reduces decision uncertainty.\n")
  
  list(evpi     = evpi,
       evppi_df = evppi_df,
       wtp      = wtp)
}

# =============================================================================
# SECTION 6: VISUALISATIONS
# =============================================================================

plot_ps_distribution <- function(psm_result) {
  
  df <- psm_result$matched_df %>%
    dplyr::mutate(
      Group = dplyr::if_else(treated == 1,
                             "Treated", "Control"))
  
  ggplot(df, aes(x = ps, fill = Group)) +
    geom_histogram(alpha = 0.6, bins = 30,
                   position = "identity") +
    scale_fill_manual(
      values = c("Treated" = "#1ABC9C",
                 "Control" = "#2C3E50")) +
    labs(
      title    = "Propensity Score Distribution",
      subtitle = paste0("After matching | n = ",
                        psm_result$n_matched,
                        " pairs"),
      x = "Propensity Score", y = "Count",
      fill = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

plot_evppi <- function(voi_result) {
  
  df <- voi_result$evppi_df %>%
    dplyr::arrange(evppi) %>%
    dplyr::mutate(parameter = factor(parameter,
                                     levels = parameter))
  
  ggplot(df, aes(x = evppi, y = parameter,
                 fill = evppi_pct)) +
    geom_col(alpha = 0.85) +
    scale_fill_gradient(low  = "#AED6F1",
                        high = "#1ABC9C",
                        name = "% of EVPI") +
    scale_x_continuous(
      labels = scales::dollar_format(
        prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "EVPPI — Research Priority by Parameter",
      subtitle = paste0(
        "EVPI = EUR ",
        formatC(round(max(voi_result$evpi, 0)),
                format = "d", big.mark = ","),
        " | WTP = EUR ",
        formatC(voi_result$wtp,
                format = "d", big.mark = ",")),
      x = "EVPPI (EUR per patient)", y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "right")
}

plot_did_trends <- function(did_result, ipd) {
  
  names(ipd) <- tolower(names(ipd))
  
  trend_df <- ipd %>%
    dplyr::mutate(
      Group = dplyr::if_else(
        arm == "Psychosocial", "Psychosocial", "SoC")
    ) %>%
    dplyr::group_by(visit_time, Group) %>%
    dplyr::summarise(
      mean_eq5d = mean(eq5d_index, na.rm = TRUE),
      se_eq5d   = sd(eq5d_index,   na.rm = TRUE) /
        sqrt(dplyr::n()),
      .groups   = "drop"
    )
  
  ggplot(trend_df,
         aes(x = visit_time, y = mean_eq5d,
             colour = Group, group = Group)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = mean_eq5d - 1.96 * se_eq5d,
          ymax = mean_eq5d + 1.96 * se_eq5d),
      width = 0.1, alpha = 0.5
    ) +
    geom_vline(xintercept = 0.5,
               linetype = "dashed",
               colour   = "#E74C3C",
               alpha    = 0.7) +
    scale_colour_manual(
      values = c("Psychosocial" = "#1ABC9C",
                 "SoC"          = "#2C3E50")) +
    labs(
      title    = "DiD: Parallel Trends Check",
      subtitle = paste0(
        "DiD estimate = ",
        round(did_result$did_estimate, 4),
        " | Red dashed = intervention point"),
      x      = "Visit Time (Years)",
      y      = "Mean EQ-5D Utility",
      colour = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# =============================================================================
# SECTION 7: FULL CAUSAL ANALYSIS PIPELINE
# =============================================================================

run_causal_analysis <- function(
    ipd,
    psa_df    = NULL,
    treatment = "Psychosocial",
    wtp       = 20000) {
  
  cat("=== DEM-CAPS Causal Inference Analysis ===\n")
  cat(sprintf("Treatment: %s | WTP: EUR %s\n\n",
              treatment,
              formatC(wtp, format = "d",
                      big.mark = ",")))
  
  psm_result <- tryCatch(
    run_psm(ipd, treatment = treatment),
    error = function(e) {
      message("PSM failed: ", e$message); NULL })
  
  ipw_result <- tryCatch(
    run_ipw(ipd, treatment = treatment),
    error = function(e) {
      message("IPW failed: ", e$message); NULL })
  
  did_result <- tryCatch(
    run_did(ipd, treatment = treatment),
    error = function(e) {
      message("DiD failed: ", e$message); NULL })
  
  gcomp_result <- tryCatch(
    run_gcomputation(ipd, treatment = treatment,
                     n_boot = 100),
    error = function(e) {
      message("G-comp failed: ", e$message); NULL })
  
  voi_result <- if (!is.null(psa_df)) {
    tryCatch(
      compute_evppi(psa_df, wtp = wtp),
      error = function(e) {
        message("EVPPI failed: ", e$message); NULL })
  } else {
    message("No PSA data — skipping EVPPI.")
    NULL
  }
  
  cat("\n=== Causal Analysis Summary ===\n")
  summary_df <- data.frame(
    Method = c("PSM (ATT)", "IPW (ATE)",
               "DiD", "G-computation"),
    Estimate = c(
      if (!is.null(psm_result))
        round(psm_result$att$att, 4) else NA,
      if (!is.null(ipw_result))
        round(ipw_result$ate, 4) else NA,
      if (!is.null(did_result))
        round(did_result$did_estimate, 4) else NA,
      if (!is.null(gcomp_result))
        round(gcomp_result$ate, 4) else NA
    ),
    Outcome = "EQ-5D utility gain",
    stringsAsFactors = FALSE
  )
  print(summary_df)
  
  cat(sprintf("\nCurrent u_gain_psych: %.4f\n",
              u_gain_psych))
  cat(sprintf("Causal range: %.4f to %.4f\n",
              min(summary_df$Estimate, na.rm = TRUE),
              max(summary_df$Estimate, na.rm = TRUE)))
  
  if (!is.null(voi_result)) {
    cat(sprintf("\nTop research priority: %s\n",
                voi_result$evppi_df$parameter[1]))
  }
  
  invisible(list(
    psm     = psm_result,
    ipw     = ipw_result,
    did     = did_result,
    gcomp   = gcomp_result,
    voi     = voi_result,
    summary = summary_df
  ))
}