# =============================================================================
# DEM-CAPS Health-Economic Model
# File: tests/test_model.R
# Description: Unit tests
# =============================================================================

library(testthat)

# Set working directory to project root before sourcing
# Run this file from the project root: source("tests/test_model.R")
source("model/parameters.R")
source("model/markov_model.R")

# -----------------------------------------------------------------------------
# 1. PARAMETER TESTS
# -----------------------------------------------------------------------------

test_that("Transition matrix rows sum to 1", {
  row_sums <- rowSums(tp_base)
  expect_true(all(abs(row_sums - 1) < 1e-9))
})

test_that("Transition probabilities are between 0 and 1", {
  expect_true(all(tp_base >= 0 & tp_base <= 1))
})

test_that("Death is absorbing state", {
  expect_equal(tp_base["Death", "Death"], 1.0)
  expect_true(all(tp_base["Death", -5] == 0))
})

test_that("Utilities are in [0, 1]", {
  u_vals <- sapply(utilities, `[[`, "mean")
  expect_true(all(u_vals >= 0 & u_vals <= 1))
})

test_that("Utility ordering holds MCI > Mild > Moderate > Severe", {
  u <- sapply(utilities, `[[`, "mean")
  expect_true(u["MCI"]         > u["Mild_AD"])
  expect_true(u["Mild_AD"]     > u["Moderate_AD"])
  expect_true(u["Moderate_AD"] > u["Severe_AD"])
})

test_that("All costs are non-negative", {
  for (cat in names(costs)) {
    for (state in names(costs[[cat]])) {
      expect_true(costs[[cat]][[state]]$mean >= 0)
    }
  }
})

# -----------------------------------------------------------------------------
# 2. TRANSITION MATRIX BUILDER TESTS
# -----------------------------------------------------------------------------

test_that("Intervention TP matrix rows sum to 1", {
  tp_mod   <- build_tp_matrix(tp_base, rr = 0.82)
  row_sums <- rowSums(tp_mod)
  expect_true(all(abs(row_sums - 1) < 1e-6))
})

test_that("RR = 1.0 produces unchanged TP matrix", {
  tp_mod <- build_tp_matrix(tp_base, rr = 1.0)
  expect_equal(tp_mod, tp_base, tolerance = 1e-9)
})

test_that("RR < 1 reduces progression probabilities", {
  tp_mod <- build_tp_matrix(tp_base, rr = 0.82)
  # MCI to Mild_AD should be lower with RR < 1
  expect_true(tp_mod["MCI", "Mild_AD"] < tp_base["MCI", "Mild_AD"])
})

# -----------------------------------------------------------------------------
# 3. MARKOV TRACE TESTS
# -----------------------------------------------------------------------------

test_that("Markov trace rows sum to 1 at every cycle", {
  init  <- c(MCI = 0.25, Mild_AD = 0.45,
             Moderate_AD = 0.20, Severe_AD = 0.10,
             Death = 0.00)
  trace <- run_markov_trace(tp_base, init, n_cycles = 20)
  expect_true(all(abs(rowSums(trace) - 1) < 1e-9))
})

test_that("Death proportion is monotonically non-decreasing", {
  init  <- c(MCI = 0.25, Mild_AD = 0.45,
             Moderate_AD = 0.20, Severe_AD = 0.10,
             Death = 0.00)
  trace <- run_markov_trace(tp_base, init, n_cycles = 20)
  diffs <- diff(trace[, "Death"])
  expect_true(all(diffs >= -1e-9))
})

test_that("Trace starts with correct initial distribution", {
  init  <- c(MCI = 0.25, Mild_AD = 0.45,
             Moderate_AD = 0.20, Severe_AD = 0.10,
             Death = 0.00)
  trace <- run_markov_trace(tp_base, init, n_cycles = 20)
  expect_equal(trace[1, ], init, tolerance = 1e-9)
})

# -----------------------------------------------------------------------------
# 4. FULL MODEL TESTS
# -----------------------------------------------------------------------------

test_that("Full model runs without error", {
  expect_no_error(run_full_model())
})

test_that("Full model returns 4 strategy rows", {
  res <- run_full_model()
  expect_equal(nrow(res$results), 4)
  expect_setequal(res$results$Arm,
                  c("SoC", "Psychosocial",
                    "Technology", "Combination"))
})

test_that("SoC incremental cost and QALY are zero", {
  res <- run_full_model()$results
  soc <- res[res$Arm == "SoC", ]
  expect_equal(soc$Inc_Cost, 0, tolerance = 1e-6)
  expect_equal(soc$Inc_QALY, 0, tolerance = 1e-6)
})

test_that("Interventions produce >= QALYs than SoC", {
  res      <- run_full_model()$results
  soc_qaly <- res$Total_QALY[res$Arm == "SoC"]
  for (arm in c("Psychosocial", "Technology", "Combination")) {
    arm_qaly <- res$Total_QALY[res$Arm == arm]
    expect_true(arm_qaly >= soc_qaly)
  }
})

test_that("ICER values are finite for non-dominated arms", {
  res   <- run_full_model()$results
  icers <- res$ICER[res$Arm != "SoC"]
  valid <- icers[!is.na(icers)]
  expect_true(all(is.finite(valid)))
})

# -----------------------------------------------------------------------------
# 5. PSA TESTS
# -----------------------------------------------------------------------------

test_that("PSA returns correct structure", {
  psa <- run_psa(n_sim = 50, seed = 42)
  expect_true(is.data.frame(psa))
  expect_true(nrow(psa) > 0)
  expect_true(all(c("Arm", "ICER", "sim") %in% colnames(psa)))
})

test_that("PSA returns 4 rows per simulation", {
  psa <- run_psa(n_sim = 50, seed = 42)
  expect_equal(nrow(psa), 50 * 4)
})

test_that("PSA sampled utilities are in valid range", {
  params <- sample_psa_params(100, seed = 42)
  expect_true(all(params$u_MCI  >= 0 & params$u_MCI  <= 1))
  expect_true(all(params$u_Mild >= 0 & params$u_Mild <= 1))
})

test_that("PSA sampled RRs are positive", {
  params <- sample_psa_params(100, seed = 42)
  expect_true(all(params$rr_psych > 0))
  expect_true(all(params$rr_tech  > 0))
})

test_that("PSA with same seed produces identical results", {
  psa1 <- run_psa(n_sim = 20, seed = 99)
  psa2 <- run_psa(n_sim = 20, seed = 99)
  expect_equal(psa1$Total_Cost, psa2$Total_Cost)
})

test_that("PSA with different seeds produces different results", {
  psa1 <- run_psa(n_sim = 20, seed = 1)
  psa2 <- run_psa(n_sim = 20, seed = 2)
  expect_false(identical(psa1$Total_Cost, psa2$Total_Cost))
})

# -----------------------------------------------------------------------------
# RUN ALL TESTS
# -----------------------------------------------------------------------------
cat("\n=== DEM-CAPS Model Test Suite ===\n")
cat("All tests complete.\n")