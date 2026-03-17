# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/parameters.R
# =============================================================================

library(MASS)
library(truncnorm)

health_states <- c("MCI", "Mild_AD", "Moderate_AD", "Severe_AD", "Death")
n_states      <- length(health_states)

time_horizon_years <- 20
cycle_length_years <- 1
n_cycles           <- time_horizon_years / cycle_length_years

discount_costs   <- 0.04
discount_effects <- 0.015
cohort_age_start <- 75
cohort_size      <- 1000

tp_base <- matrix(
  c(0.55, 0.25, 0.08, 0.02, 0.10,
    0.00, 0.50, 0.28, 0.07, 0.15,
    0.00, 0.00, 0.48, 0.31, 0.21,
    0.00, 0.00, 0.00, 0.65, 0.35,
    0.00, 0.00, 0.00, 0.00, 1.00),
  nrow = n_states, byrow = TRUE,
  dimnames = list(health_states, health_states)
)
stopifnot(all(abs(rowSums(tp_base) - 1) < 1e-9))

rr_psych_progression <- 0.82
se_rr_psych          <- 0.07
rr_tech_progression  <- 0.88
se_rr_tech           <- 0.065
rr_combo_progression <- rr_psych_progression * rr_tech_progression

utilities <- list(
  MCI         = list(mean = 0.74, se = 0.03),
  Mild_AD     = list(mean = 0.65, se = 0.04),
  Moderate_AD = list(mean = 0.47, se = 0.05),
  Severe_AD   = list(mean = 0.28, se = 0.06),
  Death       = list(mean = 0.00, se = 0.00)
)

u_gain_psych <- 0.04
u_gain_tech  <- 0.02

costs <- list(
  medical = list(
    MCI         = list(mean = 2800,  se = 420),
    Mild_AD     = list(mean = 8500,  se = 1275),
    Moderate_AD = list(mean = 18200, se = 2730),
    Severe_AD   = list(mean = 38500, se = 5775),
    Death       = list(mean = 0,     se = 0)
  ),
  informal = list(
    MCI         = list(mean = 3200,  se = 640),
    Mild_AD     = list(mean = 12400, se = 2480),
    Moderate_AD = list(mean = 22800, se = 4560),
    Severe_AD   = list(mean = 31200, se = 6240),
    Death       = list(mean = 0,     se = 0)
  )
)

intervention_costs <- list(
  psych = list(
    initial = list(mean = 1200, se = 240),
    annual  = list(mean = 800,  se = 160)
  ),
  tech = list(
    initial = list(mean = 2500, se = 500),
    annual  = list(mean = 600,  se = 120)
  ),
  combo = list(
    initial = list(mean = 3400, se = 680),
    annual  = list(mean = 1200, se = 240)
  )
)

sample_psa_params <- function(n_sim = 1000, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  sample_beta <- function(mu, se) {
    if (mu == 0 || se == 0) return(rep(0, n_sim))
    alpha <- mu * ((mu * (1 - mu) / se^2) - 1)
    beta  <- (1 - mu) * ((mu * (1 - mu) / se^2) - 1)
    alpha <- max(alpha, 0.01)
    beta  <- max(beta,  0.01)
    rbeta(n_sim, alpha, beta)
  }
  
  sample_gamma <- function(mu, se) {
    if (mu == 0) return(rep(0, n_sim))
    shape <- (mu / se)^2
    scale <- se^2 / mu
    rgamma(n_sim, shape = shape, scale = scale)
  }
  
  sample_lognorm <- function(mu, se) {
    sigma2 <- log(1 + (se / mu)^2)
    mulog  <- log(mu) - sigma2 / 2
    rlnorm(n_sim, meanlog = mulog, sdlog = sqrt(sigma2))
  }
  
  list(
    u_MCI       = sample_beta(utilities$MCI$mean,         utilities$MCI$se),
    u_Mild      = sample_beta(utilities$Mild_AD$mean,     utilities$Mild_AD$se),
    u_Moderate  = sample_beta(utilities$Moderate_AD$mean, utilities$Moderate_AD$se),
    u_Severe    = sample_beta(utilities$Severe_AD$mean,   utilities$Severe_AD$se),
    
    rr_psych    = sample_lognorm(rr_psych_progression, se_rr_psych),
    rr_tech     = sample_lognorm(rr_tech_progression,  se_rr_tech),
    
    c_MCI_med   = sample_gamma(costs$medical$MCI$mean,         costs$medical$MCI$se),
    c_Mild_med  = sample_gamma(costs$medical$Mild_AD$mean,     costs$medical$Mild_AD$se),
    c_Mod_med   = sample_gamma(costs$medical$Moderate_AD$mean, costs$medical$Moderate_AD$se),
    c_Sev_med   = sample_gamma(costs$medical$Severe_AD$mean,   costs$medical$Severe_AD$se),
    
    c_MCI_inf   = sample_gamma(costs$informal$MCI$mean,         costs$informal$MCI$se),
    c_Mild_inf  = sample_gamma(costs$informal$Mild_AD$mean,     costs$informal$Mild_AD$se),
    c_Mod_inf   = sample_gamma(costs$informal$Moderate_AD$mean, costs$informal$Moderate_AD$se),
    c_Sev_inf   = sample_gamma(costs$informal$Severe_AD$mean,   costs$informal$Severe_AD$se),
    
    c_psych_ann = sample_gamma(intervention_costs$psych$annual$mean,
                               intervention_costs$psych$annual$se),
    c_tech_ann  = sample_gamma(intervention_costs$tech$annual$mean,
                               intervention_costs$tech$annual$se)
  )
}

dsa_params <- list(
  list(param = "rr_psych",         low = 0.71, base = 0.82,   high = 0.94,
       label = "RR progression - psychosocial", ref = "7"),
  list(param = "rr_tech",          low = 0.76, base = 0.88,   high = 1.01,
       label = "RR progression - technology",   ref = "9"),
  list(param = "u_Mild",           low = 0.57, base = 0.65,   high = 0.73,
       label = "Utility - Mild AD",              ref = "6"),
  list(param = "u_Moderate",       low = 0.38, base = 0.47,   high = 0.56,
       label = "Utility - Moderate AD",          ref = "6"),
  list(param = "c_Mild_med",       low = 6800, base = 8500,   high = 10200,
       label = "Medical cost - Mild AD (EUR)",   ref = "14"),
  list(param = "c_Mod_med",        low = 14560,base = 18200,  high = 21840,
       label = "Medical cost - Moderate AD (EUR)", ref = "14"),
  list(param = "discount_costs",   low = 0.00, base = 0.04,   high = 0.06,
       label = "Discount rate - costs",          ref = "3"),
  list(param = "discount_effects", low = 0.00, base = 0.015,  high = 0.04,
       label = "Discount rate - effects",        ref = "3")
)