# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/assumptions.R
# Description: Model assumptions register — NICE/EMA grade
# =============================================================================

# Traffic light: Green=Low, Amber=Moderate, Red=High impact
# Format: each assumption has id, category, assumption,
#         justification, reference, impact, sensitivity

get_assumptions <- function() {
  
  data.frame(
    id = 1:32,
    
    category = c(
      # Structural (1-6)
      rep("Structural", 6),
      # Transition probabilities (7-11)
      rep("Transition Probabilities", 5),
      # Intervention effects (12-17)
      rep("Intervention Effects", 6),
      # Costs (18-23)
      rep("Costs", 6),
      # Utilities (24-28)
      rep("Utilities", 5),
      # Model settings (29-32)
      rep("Model Settings", 4)
    ),
    
    assumption = c(
      # Structural
      "Markov cohort model used (not patient-level microsimulation)",
      "Annual cycle length of 1 year",
      "Half-cycle correction applied to costs and QALYs",
      "No tunnel states required — health states are mutually exclusive",
      "Homogeneous cohort — no individual heterogeneity modelled",
      "No backward transitions (no improvement in severity)",
      
      # Transition probabilities
      "Transition probabilities sourced from Handels et al. 2017 (IPECAD model)",
      "Annual transition probabilities remain constant over 20-year horizon",
      "Transition probabilities applied equally to all intervention arms",
      "MCI state includes clinical MCI only — not subjective cognitive decline",
      "Death is an absorbing state — no revival possible",
      
      # Intervention effects
      "Relative risk applied uniformly to all disease progression transitions",
      "Combination intervention effect = product of individual RRs (additive log scale)",
      "Intervention effect is constant and does not wane over time",
      "RR for psychosocial based on Olazaran et al. 2010 systematic review",
      "RR for technology based on Imbeault et al. 2014 review",
      "No adverse events or dis-utility associated with interventions",
      
      # Costs
      "Societal perspective adopted (medical + informal care costs)",
      "Replacement cost method used for informal care valuation",
      "Medical costs remain constant within each health state over time",
      "No learning curve or implementation costs beyond Year 1 initial cost",
      "Social care costs excluded (insufficient published data)",
      "All costs inflated to EUR 2025 using Dutch healthcare CPI",
      
      # Utilities
      "EQ-5D-3L utility weights derived from GERAS-EU cohort (proxy-rated)",
      "Utility gain from intervention applied to all alive health states",
      "No disutility associated with adverse events (interventions assumed safe)",
      "No age adjustment applied to utility weights",
      "Caregiver utility decrement not included in base-case QALY calculation",
      
      # Model settings
      "20-year lifetime horizon captures majority of dementia disease course",
      "Discount rate for costs: 4% per annum (ZIN 2025 guidelines)",
      "Discount rate for effects: 1.5% per annum (ZIN 2025 guidelines)",
      "Starting cohort age: 75 years (EURODEM population-based estimate)"
    ),
    
    justification = c(
      # Structural
      "Markov cohort is standard for dementia HTA; consistent with IPECAD guidelines and published models",
      "Annual cycles align with available data on disease progression and clinical follow-up intervals",
      "Half-cycle correction reduces bias from assuming transitions occur at cycle start; recommended by NICE DSU",
      "MMSE-based severity classification yields mutually exclusive states; tunnel states not warranted",
      "Individual heterogeneity not estimable without patient-level data; standard cohort approach justified",
      "Clinical evidence does not support sustained cognitive improvement in AD; conservative assumption",
      
      # TP
      "Handels 2017 IPECAD model is the most cited and validated dementia HTA model in Europe",
      "No published evidence of time-varying transition rates over 20-year horizon; sensitivity tested",
      "No intervention modifies natural history differently across states beyond the progression RR",
      "Clinical MCI definition used (Winblad 2004); consistent with MMSE 24-30 threshold",
      "Standard Markov model assumption; consistent with all published dementia models",
      
      # Intervention effects
      "Single RR applied across transitions is conservative and consistent with published HTA models",
      "Additive log-scale combination is the most conservative assumption; multiplicative would be larger",
      "No long-term trial data available beyond 2 years; constant effect is standard assumption",
      "Olazaran 2010 is the most comprehensive systematic review (n=1200, 30 RCTs)",
      "Imbeault 2014 is the most relevant technology review for assistive digital interventions",
      "Psychosocial and technology interventions have no documented adverse events in systematic reviews",
      
      # Costs
      "Societal perspective recommended for dementia by IPECAD and ZIN for full burden capture",
      "Replacement cost method is ZIN-recommended approach for informal care valuation",
      "Insufficient longitudinal cost data to model within-state cost progression",
      "Implementation costs are one-off and captured in initial cost parameter",
      "Social care data too heterogeneous across studies; sensitivity analysis recommended",
      "Inflation applied using CBS healthcare price index; standard Dutch pharmacoeconomic practice",
      
      # Utilities
      "GERAS-EU is the largest European dementia cohort study with EQ-5D measurement",
      "Conservative assumption; intervention benefit primarily through slowing progression",
      "No evidence of adverse events in included intervention trials",
      "Age adjustment would require additional data; no standard approach exists for dementia",
      "Caregiver utility included in sensitivity analysis (scenario: societal QALY)",
      
      # Settings
      "20-year horizon captures >95% of remaining life expectancy for 75-year-old cohort",
      "ZIN 2025 Kostenhandleiding mandates 4% for costs in Dutch economic evaluations",
      "ZIN 2025 Kostenhandleiding mandates 1.5% for effects in Dutch economic evaluations",
      "Based on Hofman et al. EURODEM age distribution for community dementia prevalence"
    ),
    
    reference = c(
      "[15][16][2]", "[2][15]", "[15][18]",
      "[1][5]", "[15][16]", "[5]",
      "[5][2]", "[15]", "[7][9]",
      "[1]", "[15]",
      "[7][9][15]", "Assumed", "[7][9]",
      "[7]", "[9]", "[7][9]",
      "[3][2]", "[3]", "[14]",
      "[14]", "[14]", "[3]",
      "[6]", "[7][9]", "[7][9]",
      "Assumed", "[12]",
      "[4]", "[3]", "[3]", "[4]"
    ),
    
    impact = c(
      # Structural
      "Low", "Low", "Low",
      "Low", "Moderate", "Low",
      # TP
      "High", "High", "Moderate",
      "Moderate", "Low",
      # Intervention
      "High", "High", "High",
      "High", "High", "Low",
      # Costs
      "Moderate", "Moderate", "Moderate",
      "Low", "Moderate", "Low",
      # Utilities
      "High", "Moderate", "Low",
      "Low", "Moderate",
      # Settings
      "Moderate", "High", "Moderate", "Low"
    ),
    
    dsa_param = c(
      # Structural
      NA, NA, NA, NA, NA, NA,
      # TP
      "rr_psych", NA, "rr_psych",
      NA, NA,
      # Intervention
      "rr_psych", "rr_psych", "rr_psych",
      "rr_psych", "rr_tech", NA,
      # Costs
      NA, NA, "c_Mild_med",
      NA, NA, NA,
      # Utilities
      "u_Mild", "u_Mild", NA,
      NA, NA,
      # Settings
      NA, "discount_costs",
      "discount_effects", NA
    ),
    
    stringsAsFactors = FALSE
  )
}

#' Export assumptions to Word document
#' @param assumptions_df Data frame from get_assumptions()
#' @param filter_impact  Character. NULL or "High"/"Moderate"/"Low"
#' @return Path to saved Word document

export_assumptions_word <- function(
    assumptions_df,
    filter_impact = NULL) {
  
  if (!requireNamespace("officer", quietly = TRUE)) {
    message("Installing officer package...")
    install.packages("officer")
  }
  library(officer)
  library(flextable)
  
  if (!is.null(filter_impact)) {
    assumptions_df <- assumptions_df %>%
      dplyr::filter(impact == filter_impact)
  }
  
  doc <- officer::read_docx()
  
  # Title
  doc <- doc %>%
    officer::body_add_par(
      "DEM-CAPS Health-Economic Model",
      style = "heading 1") %>%
    officer::body_add_par(
      "Model Assumptions Register",
      style = "heading 2") %>%
    officer::body_add_par(
      paste0("Generated: ", Sys.Date(),
             " | Maastricht University | EUR 2025"),
      style = "Normal") %>%
    officer::body_add_par("", style = "Normal")
  
  # Summary
  n_high <- sum(assumptions_df$impact == "High")
  n_mod  <- sum(assumptions_df$impact == "Moderate")
  n_low  <- sum(assumptions_df$impact == "Low")
  
  doc <- doc %>%
    officer::body_add_par(
      paste0("Total assumptions: ", nrow(assumptions_df),
             " | High impact: ", n_high,
             " | Moderate: ", n_mod,
             " | Low: ", n_low),
      style = "Normal") %>%
    officer::body_add_par("", style = "Normal")
  
  # Table per category
  categories <- unique(assumptions_df$category)
  
  for (cat in categories) {
    cat_df <- assumptions_df %>%
      dplyr::filter(category == cat) %>%
      dplyr::select(assumption, justification,
                    reference, impact)
    
    names(cat_df) <- c("Assumption", "Justification",
                       "Reference", "Impact")
    
    doc <- doc %>%
      officer::body_add_par(cat, style = "heading 3")
    
    ft <- flextable::flextable(cat_df) %>%
      flextable::set_table_properties(
        width = 1, layout = "autofit") %>%
      flextable::bg(
        i = ~ Impact == "High",
        j = ~ Impact,
        bg = "#FADBD8") %>%
      flextable::bg(
        i = ~ Impact == "Moderate",
        j = ~ Impact,
        bg = "#FDEBD0") %>%
      flextable::bg(
        i = ~ Impact == "Low",
        j = ~ Impact,
        bg = "#D5F5E3") %>%
      flextable::bold(part = "header") %>%
      flextable::fontsize(size = 9, part = "all") %>%
      flextable::theme_vanilla()
    
    doc <- doc %>%
      officer::body_add_flextable(ft) %>%
      officer::body_add_par("", style = "Normal")
  }
  
  save_path <- "data/processed/assumptions_register.docx"
  dir.create(dirname(save_path),
             showWarnings = FALSE,
             recursive    = TRUE)
  print(doc, target = save_path)
  cat(sprintf("Saved: %s\n", save_path))
  save_path
}