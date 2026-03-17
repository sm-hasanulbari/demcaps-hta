# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/visualizations.R
# Description: Publication-quality plots
# =============================================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)
library(ggrepel)

# -----------------------------------------------------------------------------
# COLOUR PALETTES
# -----------------------------------------------------------------------------

demcaps_palette <- c(
  "SoC"          = "#2C3E50",
  "Psychosocial" = "#1ABC9C",
  "Technology"   = "#3498DB",
  "Combination"  = "#E74C3C"
)

demcaps_fill <- c(
  "MCI"         = "#A8D5E2",
  "Mild_AD"     = "#5DADE2",
  "Moderate_AD" = "#2E86C1",
  "Severe_AD"   = "#1A5276",
  "Death"       = "#717D7E"
)

# -----------------------------------------------------------------------------
# THEME
# -----------------------------------------------------------------------------

theme_demcaps <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2,
                                      colour = "#1C2833",
                                      margin = margin(b = 8)),
      plot.subtitle    = element_text(size = base_size - 1,
                                      colour = "#566573",
                                      margin = margin(b = 12)),
      plot.caption     = element_text(size = base_size - 3,
                                      colour = "#99A3A4",
                                      hjust = 0,
                                      margin = margin(t = 10)),
      axis.title       = element_text(face = "bold", colour = "#2C3E50",
                                      size = base_size - 1),
      axis.text        = element_text(colour = "#566573"),
      panel.grid.major = element_line(colour = "#EAECEE", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold"),
      strip.text       = element_text(face = "bold", colour = "#2C3E50"),
      plot.background  = element_rect(fill = "#FDFEFE", colour = NA),
      panel.background = element_rect(fill = "#FDFEFE", colour = NA)
    )
}

# -----------------------------------------------------------------------------
# 1. MARKOV TRACE PLOT
# -----------------------------------------------------------------------------

plot_trace <- function(trace, arm = "Standard of Care",
                       use_plotly = TRUE) {
  
  df       <- as.data.frame(trace)
  df$Cycle <- 0:n_cycles
  df_long  <- pivot_longer(df, cols = -Cycle,
                           names_to  = "State",
                           values_to = "Proportion")
  
  df_long$State <- factor(df_long$State,
                          levels = c("MCI", "Mild_AD", "Moderate_AD",
                                     "Severe_AD", "Death"),
                          labels = c("MCI", "Mild AD", "Moderate AD",
                                     "Severe AD", "Death"))
  
  p <- ggplot(df_long,
              aes(x = Cycle, y = Proportion, fill = State)) +
    geom_area(alpha = 0.85, position = "stack") +
    scale_fill_manual(values = demcaps_fill) +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, n_cycles, 5)) +
    labs(
      title    = paste("Disease State Trajectory -", arm),
      subtitle = "Proportion of cohort in each health state per annual cycle",
      x        = "Model Cycle (Years)",
      y        = "Proportion of Cohort",
      fill     = "Health State",
      caption  = "DEM-CAPS HTA Model | Handels et al. (2017)[5] | GERAS-EU[6]"
    ) +
    theme_demcaps()
  
  if (use_plotly) {
    ggplotly(p, tooltip = c("x", "fill", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  } else p
}

# -----------------------------------------------------------------------------
# 2. COST-EFFECTIVENESS PLANE
# -----------------------------------------------------------------------------

plot_ce_plane <- function(psa_df, wtp_line = 20000,
                          use_plotly = TRUE) {
  
  df <- psa_df %>%
    filter(Arm != "SoC") %>%
    mutate(Arm = factor(Arm,
                        levels = c("Psychosocial",
                                   "Technology",
                                   "Combination")))
  
  p <- ggplot(df,
              aes(x = Inc_QALY, y = Inc_Cost,
                  colour = Arm,
                  text   = paste(
                    "Arm:", Arm,
                    "<br>Inc. QALY:", round(Inc_QALY, 3),
                    "<br>Inc. Cost: EUR",
                    formatC(Inc_Cost, format = "f",
                            digits = 0, big.mark = ",")
                  ))) +
    geom_abline(slope = wtp_line, intercept = 0,
                linetype = "dashed", colour = "#E67E22",
                linewidth = 0.8, alpha = 0.7) +
    geom_hline(yintercept = 0, colour = "#7F8C8D", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "#7F8C8D", linewidth = 0.4) +
    geom_point(alpha = 0.35, size = 1.5, shape = 16) +
    stat_summary(aes(group = Arm), fun = mean, geom = "point",
                 size = 5, shape = 18, show.legend = FALSE) +
    scale_colour_manual(
      values = demcaps_palette[c("Psychosocial",
                                 "Technology",
                                 "Combination")]) +
    scale_y_continuous(
      labels = dollar_format(prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "Cost-Effectiveness Plane",
      subtitle = paste0("PSA scatter (n=",
                        length(unique(psa_df$sim)),
                        " simulations); dashed = WTP threshold"),
      x        = "Incremental QALYs",
      y        = "Incremental Costs (EUR)",
      colour   = "Intervention",
      caption  = "WTP reference: Dutch guidelines (ZIN 2024)[3]"
    ) +
    theme_demcaps()
  
  if (use_plotly) {
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
  } else p
}

# -----------------------------------------------------------------------------
# 3. CEAC PLOT
# -----------------------------------------------------------------------------

plot_ceac <- function(ceac_df, use_plotly = TRUE) {
  
  p <- ggplot(ceac_df,
              aes(x = WTP, y = Prob_CE,
                  colour = Arm, group = Arm)) +
    geom_line(linewidth = 1.0) +
    geom_hline(yintercept = 0.5, linetype = "dotted",
               colour = "#95A5A6") +
    scale_colour_manual(values = demcaps_palette) +
    scale_x_continuous(
      labels = dollar_format(prefix = "EUR ", big.mark = ",")) +
    scale_y_continuous(labels = percent_format(),
                       limits = c(0, 1)) +
    labs(
      title    = "Cost-Effectiveness Acceptability Curves",
      subtitle = "Probability each intervention is cost-effective at each WTP threshold",
      x        = "Willingness-to-Pay Threshold (EUR/QALY)",
      y        = "Probability Cost-Effective",
      colour   = "Strategy",
      caption  = "Fenwick et al. (2001)[20] | Lothgren & Zethraeus (2000)[21]"
    ) +
    theme_demcaps()
  
  if (use_plotly) {
    ggplotly(p, tooltip = c("x", "colour", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.2))
  } else p
}

# -----------------------------------------------------------------------------
# 4. TORNADO DIAGRAM
# -----------------------------------------------------------------------------

plot_tornado <- function(dsa_df, use_plotly = TRUE) {
  
  df <- dsa_df %>%
    arrange(ICER_Range) %>%
    mutate(Parameter = factor(Parameter, levels = Parameter))
  
  base_icer <- dsa_df$Base_Value[1]
  
  p <- ggplot(df) +
    geom_segment(aes(x    = ICER_Low,  xend = ICER_High,
                     y    = Parameter, yend = Parameter),
                 linewidth = 6, colour = "#3498DB", alpha = 0.7) +
    geom_vline(xintercept = base_icer,
               linetype = "dashed", colour = "#E74C3C",
               linewidth = 0.8) +
    scale_x_continuous(
      labels = dollar_format(prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "Tornado Diagram - One-Way Sensitivity Analysis",
      subtitle = "Impact of individual parameter uncertainty on Psychosocial ICER",
      x        = "ICER (EUR per QALY gained)",
      y        = NULL,
      caption  = "Red dashed line = base-case ICER"
    ) +
    theme_demcaps() +
    theme(legend.position = "none")
  
  if (use_plotly) ggplotly(p) else p
}

# -----------------------------------------------------------------------------
# 5. NMB BAR CHART
# -----------------------------------------------------------------------------

plot_nmb <- function(results_df, wtp = 20000,
                     use_plotly = TRUE) {
  
  wtp_col <- paste0("NMB_", wtp / 1000, "k")
  
  if (!wtp_col %in% names(results_df)) {
    wtp_col <- "NMB_20k"
  }
  
  df <- results_df %>%
    filter(Arm != "SoC") %>%
    mutate(
      Arm       = factor(Arm,
                         levels = c("Psychosocial",
                                    "Technology",
                                    "Combination")),
      NMB       = .data[[wtp_col]],
      CE        = NMB > 0,
      NMB_label = paste0("EUR ",
                         formatC(round(NMB), format = "d",
                                 big.mark = ","))
    )
  
  p <- ggplot(df,
              aes(x = Arm, y = NMB, fill = Arm,
                  text = paste("Arm:", Arm,
                               "<br>NMB:", NMB_label))) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_hline(yintercept = 0, colour = "#2C3E50",
               linewidth = 0.6) +
    geom_text(aes(label = NMB_label,
                  vjust = ifelse(NMB >= 0, -0.4, 1.2)),
              size = 4, fontface = "bold",
              colour = "#2C3E50") +
    scale_fill_manual(
      values = demcaps_palette[c("Psychosocial",
                                 "Technology",
                                 "Combination")]) +
    scale_y_continuous(
      labels = dollar_format(prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = paste0("Net Monetary Benefit at EUR ",
                        formatC(wtp, format = "d",
                                big.mark = ","), "/QALY"),
      subtitle = "Positive NMB = cost-effective vs Standard of Care",
      x        = NULL,
      y        = "NMB (EUR)",
      caption  = "NMB = (Inc. QALY x WTP) - Inc. Cost"
    ) +
    theme_demcaps() +
    theme(legend.position = "none")
  
  if (use_plotly) ggplotly(p, tooltip = "text") else p
}

# -----------------------------------------------------------------------------
# 6. COST BREAKDOWN
# -----------------------------------------------------------------------------

plot_cost_breakdown <- function(results_df,
                                use_plotly = TRUE) {
  
  df <- results_df %>%
    mutate(
      Arm          = factor(Arm,
                            levels = c("SoC", "Psychosocial",
                                       "Technology", "Combination")),
      Cost_label   = paste0("EUR ",
                            formatC(round(Total_Cost),
                                    format = "d",
                                    big.mark = ","))
    )
  
  p <- ggplot(df,
              aes(x    = Arm,
                  y    = Total_Cost,
                  fill = Arm,
                  text = paste("Arm:", Arm,
                               "<br>Total Cost:", Cost_label))) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_text(aes(label = Cost_label),
              vjust  = -0.4, size = 3.5,
              fontface = "bold", colour = "#2C3E50") +
    scale_fill_manual(values = demcaps_palette) +
    scale_y_continuous(
      labels = dollar_format(prefix = "EUR ", big.mark = ",")) +
    labs(
      title    = "Total Discounted Costs by Strategy",
      subtitle = paste0("20-year horizon | Discount rate: ",
                        discount_costs * 100, "% | ",
                        "Societal perspective"),
      x        = NULL,
      y        = "Total Discounted Cost (EUR)",
      caption  = "Costs: ZIN 2024[3] | Alzheimer Nederland[14]"
    ) +
    theme_demcaps() +
    theme(legend.position = "none")
  
  if (use_plotly) ggplotly(p, tooltip = "text") else p
}