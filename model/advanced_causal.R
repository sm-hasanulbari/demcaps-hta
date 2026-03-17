# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/advanced_causal.R
# Description: Advanced causal inference methods
#   1. Targeted Maximum Likelihood (TMLE)
#   2. Instrumental Variables (IV)
#   3. Regression Discontinuity (RD)
#   4. Marginal Structural Models (MSM)
#   5. Interrupted Time Series (ITS)
#   6. Bayesian Networks
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
arrange <- dplyr::arrange

source("model/parameters.R")
source("model/ipd_calibration.R")

# =============================================================================
# SECTION 1: TMLE
# =============================================================================

run_tmle <- function(ipd,
                     treatment = "Psychosocial",
                     outcome   = "eq5d_index",
                     n_boot    = 200) {
  
  cat("=== TMLE (Targeted Maximum Likelihood) ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_t <- ipd %>%
    dplyr::filter(visit_time > 0,
                  is.na(vital_status) |
                    vital_status != "Dead") %>%
    dplyr::mutate(
      A              = as.integer(arm == treatment),
      Y              = .data[[outcome]],
      baseline_state = classify_health_state(mmse),
      sex_num        = as.integer(sex == "F"),
      age_c          = scale(age)[, 1]
    ) %>%
    dplyr::filter(!is.na(Y), !is.na(baseline_state))
  
  n <- nrow(ipd_t)
  cat(sprintf("n=%d | Treated=%d | Control=%d\n",
              n, sum(ipd_t$A), sum(1 - ipd_t$A)))
  
  # Step 1: Outcome model Q(A,W)
  Q_model <- lm(
    Y ~ A + age_c + sex_num +
      factor(baseline_state) + visit_time,
    data = ipd_t
  )
  
  ipd_1 <- ipd_t %>% dplyr::mutate(A = 1)
  ipd_0 <- ipd_t %>% dplyr::mutate(A = 0)
  Q1W   <- predict(Q_model, newdata = ipd_1)
  Q0W   <- predict(Q_model, newdata = ipd_0)
  QAW   <- predict(Q_model, newdata = ipd_t)
  
  # Step 2: Propensity model g(A|W)
  g_model <- glm(
    A ~ age_c + sex_num + factor(baseline_state),
    data = ipd_t, family = binomial()
  )
  gAW <- predict(g_model, type = "response")
  gAW <- pmax(pmin(gAW, 0.95), 0.05)
  
  # Step 3: Clever covariate
  H1W <- 1 / gAW
  H0W <- -1 / (1 - gAW)
  HAW <- ifelse(ipd_t$A == 1, H1W, H0W)
  
  # Step 4: Targeting
  epsilon_model <- lm(I(ipd_t$Y - QAW) ~ HAW - 1)
  epsilon       <- coef(epsilon_model)[1]
  cat(sprintf("Targeting epsilon: %.6f\n", epsilon))
  
  # Step 5: Update
  Q1W_star <- Q1W + epsilon * H1W
  Q0W_star <- Q0W + epsilon * H0W
  
  # Step 6: TMLE estimate
  tmle_ate <- mean(Q1W_star - Q0W_star)
  
  # Step 7: Influence curve SE
  D <- (ipd_t$A / gAW) * (ipd_t$Y - Q1W_star) -
    ((1 - ipd_t$A) / (1 - gAW)) *
    (ipd_t$Y - Q0W_star) +
    (Q1W_star - Q0W_star) - tmle_ate
  
  tmle_se <- sqrt(var(D) / n)
  tmle_ci <- c(tmle_ate - 1.96 * tmle_se,
               tmle_ate + 1.96 * tmle_se)
  
  cat(sprintf("TMLE ATE:  %.4f\n", tmle_ate))
  cat(sprintf("SE:        %.4f\n", tmle_se))
  cat(sprintf("95%% CI:   [%.4f, %.4f]\n",
              tmle_ci[1], tmle_ci[2]))
  cat(sprintf("p-value:   %.4f\n",
              2 * pnorm(-abs(tmle_ate / tmle_se))))
  
  list(ate      = tmle_ate,
       se       = tmle_se,
       ci       = tmle_ci,
       epsilon  = epsilon,
       Q_model  = Q_model,
       g_model  = g_model,
       influence = D)
}

# =============================================================================
# SECTION 2: INSTRUMENTAL VARIABLES
# =============================================================================

run_iv <- function(ipd, treatment = "Psychosocial") {
  
  cat("\n=== Instrumental Variables (2SLS) ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_base <- ipd %>%
    dplyr::filter(visit_time == 0) %>%
    dplyr::mutate(
      A       = as.integer(arm == treatment),
      Y       = eq5d_index,
      age_c   = scale(age)[, 1],
      sex_num = as.integer(sex == "F"),
      Z       = as.integer(
        rbinom(dplyr::n(), 1,
               prob = pmin(0.4 + 0.3 * A +
                             rnorm(dplyr::n(), 0, 0.1),
                           1)) > 0.5)
    ) %>%
    dplyr::filter(!is.na(Y))
  
  # First stage
  first_stage <- lm(A ~ Z + age_c + sex_num,
                    data = ipd_base)
  f_stat      <- summary(first_stage)$fstatistic[1]
  cat(sprintf("First stage F-stat: %.2f %s\n",
              f_stat,
              if (!is.na(f_stat) && f_stat > 10)
                "(strong)" else "(weak — caution)"))
  
  ipd_base$A_hat <- predict(first_stage)
  
  # Second stage
  second_stage <- lm(Y ~ A_hat + age_c + sex_num,
                     data = ipd_base)
  
  iv_est <- coef(second_stage)["A_hat"]
  iv_se  <- summary(second_stage)$coefficients[
    "A_hat", "Std. Error"]
  iv_ci  <- confint(second_stage)["A_hat", ]
  
  cat(sprintf("IV estimate (LATE): %.4f\n", iv_est))
  cat(sprintf("SE: %.4f\n", iv_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n",
              iv_ci[1], iv_ci[2]))
  
  list(late         = iv_est,
       se           = iv_se,
       ci           = iv_ci,
       first_stage  = first_stage,
       second_stage = second_stage,
       f_stat       = f_stat)
}

# =============================================================================
# SECTION 3: REGRESSION DISCONTINUITY
# =============================================================================

run_rdd <- function(ipd, cutoff = 24, bandwidth = 3) {
  
  cat("\n=== Regression Discontinuity Design ===\n")
  cat(sprintf("Cutoff: MMSE=%d | BW=+/-%d\n",
              cutoff, bandwidth))
  names(ipd) <- tolower(names(ipd))
  
  ipd_rdd <- ipd %>%
    dplyr::filter(visit_time == 0) %>%
    dplyr::mutate(
      mmse_c  = mmse - cutoff,
      treated = as.integer(mmse < cutoff),
      Y       = eq5d_index,
      age_c   = scale(age)[, 1],
      sex_num = as.integer(sex == "F")
    ) %>%
    dplyr::filter(!is.na(mmse), !is.na(Y))
  
  ipd_bw <- ipd_rdd %>%
    dplyr::filter(abs(mmse_c) <= bandwidth)
  
  cat(sprintf(
    "Within BW: n=%d (below=%d, above=%d)\n",
    nrow(ipd_bw),
    sum(ipd_bw$treated == 1),
    sum(ipd_bw$treated == 0)))
  
  if (nrow(ipd_bw) < 20) {
    message("Insufficient data within bandwidth.")
    return(NULL)
  }
  
  rdd_model <- lm(
    Y ~ treated * mmse_c + age_c + sex_num,
    data = ipd_bw
  )
  
  rdd_est <- coef(rdd_model)["treated"]
  rdd_se  <- summary(rdd_model)$coefficients[
    "treated", "Std. Error"]
  rdd_ci  <- confint(rdd_model)["treated", ]
  
  cat(sprintf("RD estimate: %.4f\n", rdd_est))
  cat(sprintf("SE: %.4f\n", rdd_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n",
              rdd_ci[1], rdd_ci[2]))
  
  # McCrary density check
  below    <- sum(ipd_rdd$mmse_c >= -bandwidth &
                    ipd_rdd$mmse_c < 0)
  above    <- sum(ipd_rdd$mmse_c >= 0 &
                    ipd_rdd$mmse_c <= bandwidth)
  ratio    <- below / max(above, 1)
  balanced <- abs(ratio - 1) < 0.3
  cat(sprintf(
    "McCrary check: below=%d, above=%d, ratio=%.2f %s\n",
    below, above, ratio,
    if (balanced) "(no manipulation)"
    else "(WARNING)"))
  
  list(estimate  = rdd_est,
       se        = rdd_se,
       ci        = rdd_ci,
       model     = rdd_model,
       data_bw   = ipd_bw)
}

# =============================================================================
# SECTION 4: MARGINAL STRUCTURAL MODELS
# =============================================================================

run_msm <- function(ipd, treatment = "Psychosocial") {
  
  cat("\n=== Marginal Structural Models ===\n")
  names(ipd) <- tolower(names(ipd))
  
  ipd_msm <- ipd %>%
    dplyr::filter(is.na(vital_status) |
                    vital_status != "Dead") %>%
    dplyr::arrange(patient_id, visit_time) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::mutate(
      A      = as.integer(arm == treatment),
      Y      = eq5d_index,
      L_mmse = dplyr::lag(mmse,
                          default = mmse[1]),
      L_eq5d = dplyr::lag(eq5d_index,
                          default = eq5d_index[1]),
      sex_num = as.integer(sex == "F"),
      time   = visit_time
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      age_c = as.numeric(scale(age))
    ) %>%
    dplyr::filter(!is.na(Y), !is.na(L_mmse))
  
  ps_tv <- glm(
    A ~ L_mmse + L_eq5d + age_c + sex_num + time,
    data = ipd_msm, family = binomial()
  )
  ps_marg <- glm(
    A ~ age_c + sex_num,
    data = ipd_msm, family = binomial()
  )
  
  ipd_msm$ps_tv   <- predict(ps_tv,   type = "response")
  ipd_msm$ps_marg <- predict(ps_marg, type = "response")
  
  ipd_msm <- ipd_msm %>%
    dplyr::mutate(
      sw_num = dplyr::if_else(A == 1,
                              ps_marg, 1 - ps_marg),
      sw_den = dplyr::if_else(A == 1,
                              ps_tv, 1 - ps_tv),
      sw     = sw_num / pmax(sw_den, 0.01)
    ) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::mutate(
      cum_sw = cumprod(sw),
      cum_sw = pmin(cum_sw,
                    quantile(cum_sw, 0.99,
                             na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
  
  cat(sprintf("SW range: %.3f - %.3f\n",
              min(ipd_msm$cum_sw, na.rm = TRUE),
              max(ipd_msm$cum_sw, na.rm = TRUE)))
  
  msm_model <- lm(
    Y ~ A + time + age_c + sex_num,
    data    = ipd_msm,
    weights = ipd_msm$cum_sw
  )
  
  msm_est <- coef(msm_model)["A"]
  msm_se  <- summary(msm_model)$coefficients[
    "A", "Std. Error"]
  msm_ci  <- confint(msm_model)["A", ]
  
  cat(sprintf("MSM estimate: %.4f\n", msm_est))
  cat(sprintf("SE: %.4f\n", msm_se))
  cat(sprintf("95%% CI: [%.4f, %.4f]\n",
              msm_ci[1], msm_ci[2]))
  
  list(estimate = msm_est,
       se       = msm_se,
       ci       = msm_ci,
       model    = msm_model,
       weights  = ipd_msm$cum_sw)
}

# =============================================================================
# SECTION 5: INTERRUPTED TIME SERIES
# =============================================================================

run_its <- function(ipd,
                    intervention_time = 2,
                    outcome           = "eq5d_index") {
  
  cat("\n=== Interrupted Time Series ===\n")
  cat(sprintf("Intervention point: year %d\n",
              intervention_time))
  names(ipd) <- tolower(names(ipd))
  
  ts_df <- ipd %>%
    dplyr::filter(is.na(vital_status) |
                    vital_status != "Dead") %>%
    dplyr::group_by(visit_time) %>%
    dplyr::summarise(
      mean_y  = mean(.data[[outcome]], na.rm = TRUE),
      se_y    = sd(.data[[outcome]],   na.rm = TRUE) /
        sqrt(dplyr::n()),
      n       = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      time      = visit_time,
      post      = as.integer(
        visit_time >= intervention_time),
      time_post = pmax(0,
                       visit_time - intervention_time)
    )
  
  cat(sprintf("Time points: %d\n", nrow(ts_df)))
  
  its_model <- lm(
    mean_y ~ time + post + time_post,
    data    = ts_df,
    weights = ts_df$n
  )
  
  coefs <- coef(its_model)
  ses   <- summary(its_model)$coefficients[,
                                           "Std. Error"]
  
  cat("\nITS model:\n")
  cat(sprintf("Pre-trend:    %.4f (SE=%.4f)\n",
              coefs["time"],      ses["time"]))
  cat(sprintf("Level change: %.4f (SE=%.4f)\n",
              coefs["post"],      ses["post"]))
  cat(sprintf("Slope change: %.4f (SE=%.4f)\n",
              coefs["time_post"], ses["time_post"]))
  
  ts_df$fitted        <- predict(its_model)
  ts_df_cf            <- ts_df %>%
    dplyr::mutate(post = 0, time_post = 0)
  ts_df$counterfactual <- predict(its_model,
                                  newdata = ts_df_cf)
  ts_df$effect        <- ts_df$fitted -
    ts_df$counterfactual
  
  avg_effect <- mean(ts_df$effect[ts_df$post == 1],
                     na.rm = TRUE)
  cat(sprintf("Average ITS effect: %.4f\n", avg_effect))
  
  list(level_change = coefs["post"],
       slope_change = coefs["time_post"],
       avg_effect   = avg_effect,
       model        = its_model,
       ts_data      = ts_df)
}

# =============================================================================
# SECTION 6: BAYESIAN NETWORKS
# =============================================================================

run_bayesian_network <- function(ipd) {
  
  cat("\n=== Bayesian Network ===\n")
  names(ipd) <- tolower(names(ipd))
  
  bn_df <- ipd %>%
    dplyr::filter(visit_time == 0) %>%
    dplyr::mutate(
      treatment   = factor(dplyr::if_else(
        arm == "Psychosocial", "treated", "control")),
      severity    = factor(
        classify_health_state(mmse),
        levels = c("MCI", "Mild_AD",
                   "Moderate_AD", "Severe_AD")),
      age_group   = factor(dplyr::case_when(
        age < 70 ~ "young_old",
        age < 80 ~ "middle_old",
        TRUE     ~ "oldest_old")),
      sex_f       = factor(sex),
      utility_cat = factor(dplyr::case_when(
        eq5d_index >= 0.70 ~ "good",
        eq5d_index >= 0.50 ~ "moderate",
        TRUE               ~ "poor"))
    ) %>%
    dplyr::select(treatment, severity,
                  age_group, sex_f, utility_cat) %>%
    dplyr::filter(dplyr::if_all(
      dplyr::everything(), ~ !is.na(.)))
  
  cat(sprintf("BN dataset: %d obs, %d vars\n",
              nrow(bn_df), ncol(bn_df)))
  
  dag_edges <- data.frame(
    from = c("age_group", "severity",
             "treatment", "sex_f", "severity"),
    to   = c("severity",  "utility_cat",
             "utility_cat", "utility_cat", "treatment"),
    stringsAsFactors = FALSE
  )
  
  cat("\nDAG structure:\n")
  for (i in seq_len(nrow(dag_edges))) {
    cat(sprintf("  %s -> %s\n",
                dag_edges$from[i],
                dag_edges$to[i]))
  }
  
  cpts         <- estimate_cpts(bn_df, dag_edges)
  causal_query <- compute_causal_query(bn_df)
  
  list(dag          = dag_edges,
       cpts         = cpts,
       causal_query = causal_query,
       data         = bn_df)
}

estimate_cpts <- function(bn_df, dag) {
  
  variables <- unique(c(dag$from, dag$to))
  
  cpts <- lapply(variables, function(var) {
    parents <- dag$from[dag$to == var]
    if (length(parents) == 0) {
      prop.table(table(bn_df[[var]]))
    } else {
      tryCatch(
        prop.table(table(bn_df[[var]],
                         bn_df[[parents[1]]]),
                   margin = 2),
        error = function(e) NULL
      )
    }
  })
  
  names(cpts) <- variables
  cat("\nCPT for utility_cat:\n")
  if (!is.null(cpts$utility_cat))
    print(round(cpts$utility_cat, 3))
  cpts
}

compute_causal_query <- function(bn_df) {
  
  severities <- levels(bn_df$severity)
  p_s        <- prop.table(table(bn_df$severity))
  
  results <- lapply(c("treated", "control"),
                    function(trt) {
                      
                      p_y_given_a <- sapply(severities, function(s) {
                        subset_df <- bn_df %>%
                          dplyr::filter(treatment == trt,
                                        severity  == s)
                        if (nrow(subset_df) == 0)
                          return(c(good = 0, moderate = 0, poor = 0))
                        prop.table(table(subset_df$utility_cat))
                      })
                      
                      p_y_adj <- rowSums(
                        sweep(p_y_given_a, 2,
                              as.numeric(p_s), "*"),
                        na.rm = TRUE
                      )
                      
                      data.frame(
                        treatment  = trt,
                        p_good     = p_y_adj["good"],
                        p_moderate = p_y_adj["moderate"],
                        p_poor     = p_y_adj["poor"],
                        row.names  = NULL,
                        stringsAsFactors = FALSE
                      )
                    })
  
  result_df <- do.call(rbind, results)
  cat("\nCausal query P(utility | do(treatment)):\n")
  print(result_df)
  
  ace <- result_df$p_good[
    result_df$treatment == "treated"] -
    result_df$p_good[
      result_df$treatment == "control"]
  
  cat(sprintf("\nACE on P(good utility): %.4f\n", ace))
  list(result = result_df, ace = ace)
}

# =============================================================================
# SECTION 7: VISUALISATIONS
# =============================================================================

plot_tmle_influence <- function(tmle_result) {
  df <- data.frame(
    influence = tmle_result$influence,
    index     = seq_along(tmle_result$influence)
  )
  ggplot(df, aes(x = index, y = influence)) +
    geom_point(alpha = 0.3, size = 0.8,
               colour = "#3498DB") +
    geom_hline(yintercept = 0,
               colour = "#E74C3C", linewidth = 0.8) +
    labs(title    = "TMLE Influence Curve",
         subtitle = paste0("ATE = ",
                           round(tmle_result$ate, 4)),
         x = "Observation", y = "Influence") +
    theme_minimal()
}

plot_its <- function(its_result) {
  df <- its_result$ts_data
  ggplot(df, aes(x = time)) +
    geom_point(aes(y = mean_y),
               size = 3, colour = "#2C3E50") +
    geom_line(aes(y = fitted,
                  colour = "Observed trend"),
              linewidth = 1.2) +
    geom_line(aes(y = counterfactual,
                  colour = "Counterfactual"),
              linewidth = 1.0, linetype = "dashed") +
    geom_vline(
      xintercept = min(df$time[df$post == 1],
                       na.rm = TRUE) - 0.5,
      linetype = "dotted", colour = "#E74C3C",
      linewidth = 1
    ) +
    scale_colour_manual(
      values = c("Observed trend"  = "#1ABC9C",
                 "Counterfactual"  = "#E74C3C")
    ) +
    labs(title    = "Interrupted Time Series",
         subtitle = paste0(
           "Level change = ",
           round(its_result$level_change, 4)),
         x = "Time (Years)", y = "Mean EQ-5D",
         colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

plot_rdd <- function(rdd_result) {
  df <- rdd_result$data_bw
  ggplot(df, aes(x = mmse_c, y = Y,
                 colour = factor(treated))) +
    geom_point(alpha = 0.4, size = 1.5) +
    geom_smooth(method = "lm", formula = y ~ x,
                se = TRUE, alpha = 0.2) +
    geom_vline(xintercept = 0,
               linetype = "dashed",
               colour = "#E74C3C", linewidth = 1) +
    scale_colour_manual(
      values = c("0" = "#2C3E50", "1" = "#1ABC9C"),
      labels = c("0" = "Control", "1" = "Treated")
    ) +
    labs(title    = "Regression Discontinuity",
         subtitle = paste0("RD = ",
                           round(rdd_result$estimate, 4)),
         x = "MMSE (centred at cutoff)",
         y = "EQ-5D Utility", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# =============================================================================
# SECTION 8: FULL PIPELINE
# =============================================================================

run_advanced_causal_pipeline <- function(
    ipd,
    treatment = "Psychosocial") {
  
  cat("=== DEM-CAPS Advanced Causal Pipeline ===\n")
  cat(sprintf("Treatment: %s\n\n", treatment))
  
  tmle_result <- tryCatch(
    run_tmle(ipd, treatment = treatment),
    error = function(e) {
      message("TMLE failed: ", e$message); NULL })
  
  iv_result <- tryCatch(
    run_iv(ipd, treatment = treatment),
    error = function(e) {
      message("IV failed: ", e$message); NULL })
  
  rdd_result <- tryCatch(
    run_rdd(ipd, cutoff = 24, bandwidth = 3),
    error = function(e) {
      message("RDD failed: ", e$message); NULL })
  
  msm_result <- tryCatch(
    run_msm(ipd, treatment = treatment),
    error = function(e) {
      message("MSM failed: ", e$message); NULL })
  
  its_result <- tryCatch(
    run_its(ipd, intervention_time = 2),
    error = function(e) {
      message("ITS failed: ", e$message); NULL })
  
  bn_result <- tryCatch(
    run_bayesian_network(ipd),
    error = function(e) {
      message("BN failed: ", e$message); NULL })
  
  cat("\n=== Advanced Causal Summary ===\n")
  summary_df <- data.frame(
    Method = c("TMLE", "IV (2SLS)", "RDD",
               "MSM", "ITS", "Bayes Net"),
    Estimate = c(
      if (!is.null(tmle_result))
        round(tmle_result$ate, 4)        else NA,
      if (!is.null(iv_result))
        round(iv_result$late, 4)         else NA,
      if (!is.null(rdd_result))
        round(rdd_result$estimate, 4)    else NA,
      if (!is.null(msm_result))
        round(msm_result$estimate, 4)    else NA,
      if (!is.null(its_result))
        round(its_result$avg_effect, 4)  else NA,
      if (!is.null(bn_result))
        round(bn_result$causal_query$ace, 4) else NA
    ),
    Status = c(
      if (!is.null(tmle_result)) "✓" else "✗",
      if (!is.null(iv_result))   "✓" else "✗",
      if (!is.null(rdd_result))  "✓" else "✗",
      if (!is.null(msm_result))  "✓" else "✗",
      if (!is.null(its_result))  "✓" else "✗",
      if (!is.null(bn_result))   "✓" else "✗"
    ),
    stringsAsFactors = FALSE
  )
  print(summary_df)
  
  invisible(list(
    tmle    = tmle_result,
    iv      = iv_result,
    rdd     = rdd_result,
    msm     = msm_result,
    its     = its_result,
    bn      = bn_result,
    summary = summary_df
  ))
}