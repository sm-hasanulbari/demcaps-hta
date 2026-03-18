# =============================================================================
# DEM-CAPS Health-Economic Model — model/visualizations.R
# =============================================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)

# =============================================================================
# COLOUR PALETTE
# =============================================================================
demcaps_fill <- c(
  "MCI"         = "#2ECC71",
  "Mild AD"     = "#3498DB",
  "Moderate AD" = "#F39C12",
  "Severe AD"   = "#E74C3C",
  "Death"       = "#95A5A6"
)

demcaps_arms <- c(
  "SoC"          = "#95A5A6",
  "Psychosocial" = "#2ECC71",
  "Technology"   = "#3498DB",
  "Combination"  = "#8E44AD"
)

dark_theme <- theme_minimal(base_size=13) +
  theme(
    plot.background   = element_rect(fill="#161B22", colour=NA),
    panel.background  = element_rect(fill="#161B22", colour=NA),
    panel.grid.major  = element_line(colour="#21262D", linewidth=0.4),
    panel.grid.minor  = element_line(colour="#0D1117", linewidth=0.2),
    axis.text         = element_text(colour="#8B949E", size=10),
    axis.title        = element_text(colour="#E6EDF3", size=11, face="bold"),
    plot.title        = element_text(colour="#E6EDF3", size=14, face="bold",
                                     margin=margin(b=8)),
    plot.subtitle     = element_text(colour="#8B949E", size=10),
    legend.background = element_rect(fill="#161B22", colour=NA),
    legend.text       = element_text(colour="#E6EDF3", size=10),
    legend.title      = element_text(colour="#1ABC9C", size=10, face="bold"),
    legend.key        = element_rect(fill="#161B22", colour=NA),
    strip.background  = element_rect(fill="#21262D", colour=NA),
    strip.text        = element_text(colour="#1ABC9C", face="bold"),
    plot.margin       = margin(10,15,10,10)
  )

dark_plotly_layout <- function(p,
                               legend_y  = -0.28,
                               margin_t  = 50,
                               margin_b  = 110,
                               margin_l  = 70,
                               margin_r  = 20) {
  p %>% plotly::layout(
    paper_bgcolor = "#161B22",
    plot_bgcolor  = "#161B22",
    font          = list(color="#E6EDF3"),
    legend = list(
      orientation = "h",
      y           = legend_y,
      x           = 0,
      font        = list(color="#E6EDF3", size=11),
      bgcolor     = "rgba(22,27,34,0.9)",
      bordercolor = "#30363D",
      borderwidth = 1
    ),
    margin = list(t=margin_t, b=margin_b, l=margin_l, r=margin_r)
  )
}

# =============================================================================
# 1. DISEASE STATE TRAJECTORY
#    FIX: handles matrix input (traces[[i]] is often a matrix)
# =============================================================================
plot_trace <- function(trace_df, arm = "SoC", use_plotly = TRUE) {
  
  # ── Convert matrix / array to data.frame ──────────────────────────────────
  if (is.matrix(trace_df) || is.array(trace_df)) {
    trace_df <- as.data.frame(trace_df)
  }
  if (!is.data.frame(trace_df)) {
    trace_df <- as.data.frame(trace_df)
  }
  
  # ── Ensure Cycle column exists ────────────────────────────────────────────
  if (!"Cycle" %in% names(trace_df)) {
    trace_df$Cycle <- seq_len(nrow(trace_df)) - 1
  }
  
  # ── Normalise column names ─────────────────────────────────────────────────
  names(trace_df) <- gsub("Mild_AD",     "Mild AD",     names(trace_df))
  names(trace_df) <- gsub("Moderate_AD", "Moderate AD", names(trace_df))
  names(trace_df) <- gsub("Severe_AD",   "Severe AD",   names(trace_df))
  
  # ── Keep only known states + Cycle ────────────────────────────────────────
  state_cols <- intersect(names(trace_df),
                          c("MCI","Mild AD","Moderate AD","Severe AD","Death"))
  if (length(state_cols) == 0) {
    # Try numeric columns as fallback
    num_cols <- names(trace_df)[sapply(trace_df, is.numeric) &
                                  names(trace_df) != "Cycle"]
    state_cols <- num_cols[seq_len(min(5, length(num_cols)))]
    state_names <- c("MCI","Mild AD","Moderate AD","Severe AD","Death")
    names(trace_df)[names(trace_df) %in% state_cols] <-
      state_names[seq_along(state_cols)]
    state_cols <- state_names[seq_along(state_cols)]
  }
  
  long <- trace_df %>%
    dplyr::select(Cycle, dplyr::all_of(state_cols)) %>%
    tidyr::pivot_longer(cols=-Cycle,
                        names_to="State",
                        values_to="Proportion") %>%
    dplyr::mutate(
      State      = factor(State,
                          levels=c("MCI","Mild AD","Moderate AD",
                                   "Severe AD","Death")),
      Proportion = pmax(0, pmin(1, Proportion))
    )
  
  p <- ggplot(long, aes(x=Cycle, y=Proportion, fill=State)) +
    geom_area(alpha=0.85, colour="#0D1117", linewidth=0.3) +
    scale_fill_manual(values=demcaps_fill, name="Health State") +
    scale_y_continuous(labels=percent_format(accuracy=1),
                       limits=c(0,1),
                       expand=expansion(mult=c(0,0.02))) +
    scale_x_continuous(expand=expansion(mult=c(0,0.01))) +
    labs(title=paste("Disease State Trajectory —", arm),
         x="Model Cycle (Years)", y="Proportion of Cohort") +
    dark_theme +
    theme(legend.position="bottom", legend.direction="horizontal",
          legend.margin=margin(t=12))
  
  if (use_plotly) {
    ggplotly(p, tooltip=c("x","fill","y")) %>%
      dark_plotly_layout(legend_y=-0.30, margin_t=50,
                         margin_b=120, margin_l=70, margin_r=20) %>%
      plotly::config(displayModeBar=FALSE, responsive=TRUE)
  } else {
    p
  }
}

# =============================================================================
# 2. COST-EFFECTIVENESS PLANE
# =============================================================================
plot_ce_plane <- function(psa_df, wtp_line=20000, use_plotly=TRUE) {
  
  # Flexible column name handling
  if (!"Inc_QALY" %in% names(psa_df)) {
    possible_qaly <- c("delta_qaly","inc_qaly","DELTA_QALY","IncrementalQALY")
    possible_cost <- c("delta_cost","inc_cost","DELTA_COST","IncrementalCost")
    for (col in possible_qaly)
      if (col %in% names(psa_df)) { psa_df$Inc_QALY <- psa_df[[col]]; break }
    for (col in possible_cost)
      if (col %in% names(psa_df)) { psa_df$Inc_Cost <- psa_df[[col]]; break }
  }
  if (!"Arm" %in% names(psa_df)) {
    for (col in c("arm","intervention","Strategy","strategy"))
      if (col %in% names(psa_df)) { psa_df$Arm <- psa_df[[col]]; break }
  }
  
  # Remove SoC (reference — zero incremental)
  psa_plot <- psa_df %>% dplyr::filter(Arm != "SoC")
  
  p <- ggplot(psa_plot,
              aes(x=Inc_QALY, y=Inc_Cost, colour=Arm,
                  text=paste0("Arm: ",Arm,
                              "<br>Inc QALY: ",round(Inc_QALY,3),
                              "<br>Inc Cost: EUR ",
                              formatC(round(Inc_Cost),
                                      format="d",big.mark=",")))) +
    geom_point(alpha=0.5, size=1.6, shape=18) +
    geom_abline(slope=wtp_line, intercept=0, linetype="dashed",
                colour="#F39C12", linewidth=0.8, aes(text=NULL)) +
    geom_hline(yintercept=0, colour="#566573", linewidth=0.4) +
    geom_vline(xintercept=0, colour="#566573", linewidth=0.4) +
    scale_colour_manual(
      values = demcaps_arms[names(demcaps_arms) != "SoC"],
      name   = "Intervention") +
    scale_y_continuous(
      labels=dollar_format(prefix="EUR ", big.mark=",")) +
    scale_x_continuous(labels=number_format(accuracy=0.01)) +
    labs(title="Cost-Effectiveness Plane",
         x="Incremental QALYs", y="Incremental Costs (EUR)") +
    dark_theme + theme(legend.position="bottom")
  
  if (use_plotly) {
    ggplotly(p, tooltip="text") %>%
      dark_plotly_layout(legend_y=-0.28, margin_t=50,
                         margin_b=100, margin_l=80, margin_r=20) %>%
      plotly::config(displayModeBar=TRUE, responsive=TRUE)
  } else p
}

# =============================================================================
# 3. CALC CEAC — base R only, no purrr dependency
#    FIX: rewritten without purrr::map_dfr
# =============================================================================
calc_ceac <- function(psa_df, wtp_range=seq(0, 100000, by=2500)) {
  
  # Flexible column handling
  if (!"Inc_QALY" %in% names(psa_df)) {
    for (col in c("delta_qaly","inc_qaly","IncrementalQALY"))
      if (col %in% names(psa_df)) { psa_df$Inc_QALY <- psa_df[[col]]; break }
  }
  if (!"Inc_Cost" %in% names(psa_df)) {
    for (col in c("delta_cost","inc_cost","IncrementalCost"))
      if (col %in% names(psa_df)) { psa_df$Inc_Cost <- psa_df[[col]]; break }
  }
  if (!"Arm" %in% names(psa_df)) {
    for (col in c("arm","intervention","Strategy","strategy"))
      if (col %in% names(psa_df)) { psa_df$Arm <- psa_df[[col]]; break }
  }
  
  arms   <- unique(psa_df$Arm)
  result <- vector("list", length(wtp_range) * length(arms))
  idx    <- 1L
  
  for (wtp in wtp_range) {
    for (a in arms) {
      d   <- psa_df[psa_df$Arm == a, ]
      nmb <- wtp * d$Inc_QALY - d$Inc_Cost
      result[[idx]] <- data.frame(
        WTP      = wtp,
        Strategy = a,
        Prob_CE  = mean(nmb >= 0, na.rm=TRUE),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  
  do.call(rbind, result)
}

# =============================================================================
# 4. PLOT CEAC
# =============================================================================
plot_ceac <- function(ceac_df, use_plotly=TRUE) {
  
  if (is.null(ceac_df) || nrow(ceac_df) == 0) return(NULL)
  
  # Ensure Strategy column exists
  if (!"Strategy" %in% names(ceac_df) && "Arm" %in% names(ceac_df))
    ceac_df$Strategy <- ceac_df$Arm
  
  wtp_lines <- data.frame(
    wtp   = c(20000, 50000, 80000),
    label = c("EUR 20k","EUR 50k","EUR 80k"),
    col   = c("#E67E22","#E74C3C","#8E44AD"),
    stringsAsFactors = FALSE
  )
  
  # Colours: arms + WTP line labels
  all_colours <- c(demcaps_arms,
                   setNames(wtp_lines$col, wtp_lines$label))
  
  p <- ggplot(ceac_df, aes(x=WTP, y=Prob_CE, colour=Strategy)) +
    geom_line(linewidth=1.2) +
    geom_hline(yintercept=0.5, linetype="dashed",
               colour="#566573", linewidth=0.6) +
    geom_vline(data=wtp_lines,
               aes(xintercept=wtp, colour=label),
               linetype="dashed", linewidth=0.8,
               inherit.aes=FALSE) +
    scale_colour_manual(values=all_colours, name="Strategy") +
    scale_y_continuous(labels=percent_format(accuracy=1),
                       limits=c(0,1)) +
    scale_x_continuous(labels=dollar_format(prefix="EUR ", big.mark=",")) +
    labs(title="Cost-Effectiveness Acceptability Curves",
         x="Willingness-to-Pay (EUR/QALY)",
         y="Probability Cost-Effective") +
    dark_theme + theme(legend.position="bottom")
  
  if (use_plotly) {
    ggplotly(p, tooltip=c("x","colour","y")) %>%
      dark_plotly_layout(legend_y=-0.30, margin_t=50,
                         margin_b=110, margin_l=70, margin_r=20) %>%
      plotly::config(displayModeBar=TRUE, responsive=TRUE)
  } else p
}

# =============================================================================
# 5. TORNADO DIAGRAM
#    FIX: flexible column name detection
# =============================================================================
plot_tornado <- function(dsa_df, n_params=15, use_plotly=TRUE) {
  
  if (is.null(dsa_df)) return(NULL)
  
  # Handle list output from run_dsa
  if (is.list(dsa_df) && !is.data.frame(dsa_df)) {
    if ("dsa_results" %in% names(dsa_df)) dsa_df <- dsa_df$dsa_results
    else if ("results"     %in% names(dsa_df)) dsa_df <- dsa_df$results
    else if ("tornado"     %in% names(dsa_df)) dsa_df <- dsa_df$tornado
    else {
      # Try first data.frame element
      df_elems <- Filter(is.data.frame, dsa_df)
      if (length(df_elems) > 0) dsa_df <- df_elems[[1]]
      else return(NULL)
    }
  }
  
  if (!is.data.frame(dsa_df) || nrow(dsa_df) == 0) return(NULL)
  
  # ── Flexible column name mapping ──────────────────────────────────────────
  # Parameter column
  param_col <- intersect(names(dsa_df),
                         c("Parameter","parameter","param","Param","Variable","variable"))[1]
  if (is.na(param_col)) return(NULL)
  
  # ICER columns
  low_col  <- intersect(names(dsa_df),
                        c("ICER_low","icer_low","ICER_Low","low","Low","ICER_low_psych"))[1]
  high_col <- intersect(names(dsa_df),
                        c("ICER_high","icer_high","ICER_High","high","High","ICER_high_psych"))[1]
  base_col <- intersect(names(dsa_df),
                        c("ICER_base","icer_base","ICER_Base","base","Base","Base_ICER"))[1]
  
  # If no explicit base col, use midpoint
  if (is.na(low_col) || is.na(high_col)) return(NULL)
  
  dsa_df$Parameter <- dsa_df[[param_col]]
  dsa_df$ICER_low  <- as.numeric(dsa_df[[low_col]])
  dsa_df$ICER_high <- as.numeric(dsa_df[[high_col]])
  
  if (!is.na(base_col)) {
    dsa_df$ICER_base <- as.numeric(dsa_df[[base_col]])
  } else {
    dsa_df$ICER_base <- (dsa_df$ICER_low + dsa_df$ICER_high) / 2
  }
  
  # Filter to one arm if multiple
  if ("Arm" %in% names(dsa_df) && length(unique(dsa_df$Arm)) > 1) {
    for (arm_name in c("Psychosocial","Technology","Combination")) {
      sub <- dsa_df[dsa_df$Arm == arm_name, ]
      if (nrow(sub) > 0) { dsa_df <- sub; break }
    }
  }
  
  # Remove NA rows
  dsa_df <- dsa_df[!is.na(dsa_df$ICER_low) & !is.na(dsa_df$ICER_high), ]
  if (nrow(dsa_df) == 0) return(NULL)
  
  df <- dsa_df %>%
    dplyr::mutate(swing = abs(ICER_high - ICER_low)) %>%
    dplyr::arrange(desc(swing)) %>%
    dplyr::slice_head(n=n_params) %>%
    dplyr::mutate(Parameter=factor(Parameter, levels=rev(Parameter)))
  
  base_val <- median(df$ICER_base, na.rm=TRUE)
  
  p <- ggplot(df) +
    geom_segment(
      aes(x=ICER_low, xend=ICER_high,
          y=Parameter, yend=Parameter,
          text=paste0(Parameter,
                      "<br>Low:  EUR ",
                      formatC(round(ICER_low), format="d",big.mark=","),
                      "<br>High: EUR ",
                      formatC(round(ICER_high),format="d",big.mark=","),
                      "<br>Base: EUR ",
                      formatC(round(ICER_base),format="d",big.mark=","))),
      colour="#3498DB", linewidth=5, lineend="round") +
    geom_vline(xintercept=base_val,
               colour="#1ABC9C", linetype="dashed", linewidth=0.8) +
    scale_x_continuous(
      labels=dollar_format(prefix="EUR ", big.mark=",")) +
    labs(title="Tornado Diagram — Deterministic Sensitivity Analysis",
         subtitle="ICER range per parameter (base-case = teal dashed line)",
         x="ICER (EUR/QALY)", y=NULL) +
    dark_theme +
    theme(panel.grid.major.y=element_blank(),
          axis.text.y=element_text(size=9))
  
  if (use_plotly) {
    ggplotly(p, tooltip="text") %>%
      dark_plotly_layout(margin_t=50, margin_b=80,
                         margin_l=180, margin_r=20) %>%
      plotly::layout(showlegend=FALSE) %>%
      plotly::config(displayModeBar=TRUE, responsive=TRUE)
  } else p
}

# =============================================================================
# 6. COST BREAKDOWN
# =============================================================================
plot_cost_breakdown <- function(results_df, use_plotly=TRUE) {
  
  cost_cols <- intersect(
    c("Medical_Cost","Informal_Cost","Intervention_Cost"),
    names(results_df))
  
  if (length(cost_cols)==0) return(if(use_plotly) plotly::plot_ly() else ggplot())
  
  long <- results_df %>%
    dplyr::select(Arm, dplyr::all_of(cost_cols)) %>%
    tidyr::pivot_longer(-Arm, names_to="Component", values_to="Cost") %>%
    dplyr::mutate(Component=gsub("_Cost","",Component))
  
  p <- ggplot(long, aes(x=Arm, y=Cost, fill=Component,
                        text=paste0(Arm,"\n",Component,
                                    "\nEUR ",formatC(round(Cost),
                                                     format="d",big.mark=",")))) +
    geom_col(position="stack", width=0.6) +
    scale_fill_manual(
      values=c(Medical="#3498DB",Informal="#F39C12",Intervention="#2ECC71")) +
    scale_y_continuous(labels=dollar_format(prefix="EUR ", big.mark=",")) +
    labs(title="Cost Breakdown by Component", x=NULL, y="Total Cost (EUR)") +
    dark_theme + theme(legend.position="bottom")
  
  if (use_plotly) {
    ggplotly(p, tooltip="text") %>%
      dark_plotly_layout(legend_y=-0.25, margin_b=100) %>%
      plotly::config(displayModeBar=FALSE, responsive=TRUE)
  } else p
}

# =============================================================================
# 7. NMB BAR CHART
# =============================================================================
plot_nmb <- function(results_df, wtp=20000, use_plotly=TRUE) {
  
  col_name <- paste0("NMB_",wtp/1000,"k")
  if (!col_name %in% names(results_df)) col_name <- "NMB_20k"
  
  df <- results_df %>%
    dplyr::filter(Arm != "SoC") %>%
    dplyr::mutate(col=ifelse(.data[[col_name]]>=0,"#2ECC71","#E74C3C"))
  
  p <- ggplot(df, aes(x=reorder(Arm, .data[[col_name]]),
                      y=.data[[col_name]], fill=col,
                      text=paste0(Arm,"\nNMB: EUR ",
                                  formatC(round(.data[[col_name]]),
                                          format="d",big.mark=",")))) +
    geom_col(width=0.55) +
    geom_hline(yintercept=0, colour="#566573", linewidth=0.8) +
    scale_fill_identity() +
    scale_y_continuous(labels=dollar_format(prefix="EUR ", big.mark=",")) +
    labs(title=paste0("Net Monetary Benefit @ EUR ",
                      formatC(wtp,format="d",big.mark=","),"/QALY"),
         x=NULL, y="NMB (EUR)") +
    dark_theme + theme(legend.position="none")
  
  if (use_plotly) {
    ggplotly(p, tooltip="text") %>%
      dark_plotly_layout(margin_b=80) %>%
      plotly::config(displayModeBar=FALSE, responsive=TRUE)
  } else p
}

# =============================================================================
# 8. GOF PLOT
# =============================================================================
plot_gof <- function(gof_obj) {
  if (is.null(gof_obj) || is.null(gof_obj$obs_pred)) return(NULL)
  df <- gof_obj$obs_pred
  ggplot(df, aes(x=Observed, y=Predicted, colour=State)) +
    geom_point(size=3, alpha=0.8) +
    geom_abline(slope=1, intercept=0, colour="#1ABC9C",
                linetype="dashed", linewidth=0.8) +
    scale_colour_manual(values=demcaps_fill) +
    scale_x_continuous(labels=percent_format(accuracy=1)) +
    scale_y_continuous(labels=percent_format(accuracy=1)) +
    labs(title="GOF: Observed vs Predicted",
         subtitle=paste0("RMSE = ",round(gof_obj$rmse,4),
                         "  |  MAE = ",round(gof_obj$mae,4)),
         x="Observed Proportion", y="Predicted Proportion") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 9. TP COMPARISON HEATMAP
# =============================================================================
plot_tp_comparison <- function(tp_lit, tp_ipd) {
  if (is.null(tp_lit)||is.null(tp_ipd)) return(NULL)
  diff_mat <- tp_ipd - tp_lit
  df <- as.data.frame(as.table(diff_mat)) %>%
    dplyr::rename(From=Var1, To=Var2, Diff=Freq)
  ggplot(df, aes(x=To, y=From, fill=Diff)) +
    geom_tile(colour="#0D1117", linewidth=0.5) +
    geom_text(aes(label=round(Diff,3)), colour="white", size=3.5) +
    scale_fill_gradient2(low="#3498DB", mid="#161B22", high="#E74C3C",
                         midpoint=0, name="Diff") +
    labs(title="Transition Probability Difference\n(IPD calibrated − Literature)",
         x="To State", y="From State") +
    dark_theme +
    theme(axis.text.x=element_text(angle=30,hjust=1),
          legend.position="right")
}

# =============================================================================
# 10. EVPPI
# =============================================================================
plot_evppi <- function(evppi_df) {
  if (is.null(evppi_df)) return(NULL)
  top_n <- evppi_df %>%
    dplyr::arrange(desc(EVPPI)) %>%
    dplyr::slice_head(n=10) %>%
    dplyr::mutate(Parameter=factor(Parameter, levels=rev(Parameter)))
  
  ggplot(top_n, aes(x=Parameter, y=EVPPI,
                    fill=EVPPI > mean(EVPPI))) +
    geom_col(width=0.65) +
    geom_text(
      aes(label=paste0("EUR ",
                       formatC(round(EVPPI/1000), format="d", big.mark=","),
                       "k")),
      hjust=-0.1, colour="#E6EDF3", size=3.2) +
    scale_fill_manual(values=c("FALSE"="#3498DB","TRUE"="#1ABC9C")) +
    scale_y_continuous(
      labels=function(x) paste0("EUR ",
                                formatC(round(x/1000), format="d", big.mark=","),
                                "k"),
      expand=expansion(mult=c(0, 0.4))) +
    coord_flip() +
    labs(title="EVPPI — Expected Value of Partial Perfect Information",
         subtitle="Top parameters by research value",
         x=NULL, y="EVPPI (EUR per patient)") +
    dark_theme +
    theme(legend.position="none",
          plot.margin=ggplot2::margin(10, 20, 30, 10),
          axis.text.x=ggplot2::element_text(colour="#8B949E", size=9))
}

# =============================================================================
# 11. PROPENSITY SCORE DISTRIBUTION
# =============================================================================
plot_ps_distribution <- function(psm_obj) {
  if (is.null(psm_obj)||is.null(psm_obj$ps_df)) return(NULL)
  df <- psm_obj$ps_df
  ggplot(df, aes(x=ps, fill=factor(treatment),
                 colour=factor(treatment))) +
    geom_density(alpha=0.4, linewidth=0.8) +
    scale_fill_manual(values=c("0"="#95A5A6","1"="#1ABC9C"),
                      labels=c("Control","Treated"), name="Group") +
    scale_colour_manual(values=c("0"="#95A5A6","1"="#1ABC9C"),
                        labels=c("Control","Treated"), name="Group") +
    scale_x_continuous(limits=c(0,1)) +
    labs(title="Propensity Score Distribution",
         subtitle="Before matching",
         x="Propensity Score", y="Density") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 12. SURVIVAL EXTRAPOLATION
# =============================================================================
plot_survival_extrapolation <- function(surv_obj) {
  if (is.null(surv_obj)||is.null(surv_obj$pred_df)) return(NULL)
  df  <- surv_obj$pred_df
  obs <- surv_obj$km_df
  ggplot() +
    geom_line(data=obs,
              aes(x=time,y=surv,colour="Kaplan-Meier"),
              linewidth=1) +
    geom_ribbon(data=obs,
                aes(x=time,ymin=lower,ymax=upper),
                fill="#566573",alpha=0.15) +
    geom_line(data=df,
              aes(x=time,y=surv,colour=model,linetype=model),
              linewidth=0.8) +
    scale_colour_manual(
      values=c("Kaplan-Meier"="#E6EDF3","Weibull"="#1ABC9C",
               "Exponential"="#3498DB","Gompertz"="#F39C12",
               "Log-logistic"="#E74C3C","Log-normal"="#8E44AD")) +
    scale_linetype_manual(
      values=c("Kaplan-Meier"="solid","Weibull"="dashed",
               "Exponential"="dotted","Gompertz"="dotdash",
               "Log-logistic"="longdash","Log-normal"="twodash")) +
    scale_y_continuous(labels=percent_format(accuracy=1),limits=c(0,1)) +
    labs(title="Parametric Survival Extrapolation",
         subtitle="KM = observed  |  Lines = extrapolated models",
         x="Time (years)",y="Survival Probability",
         colour="Model",linetype="Model") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 13. COMPETING RISKS (CIF)
# =============================================================================
plot_cif <- function(comp_risk_obj) {
  if (is.null(comp_risk_obj)||is.null(comp_risk_obj$cif_df)) return(NULL)
  df <- comp_risk_obj$cif_df
  ggplot(df, aes(x=time,y=cif,colour=cause,fill=cause)) +
    geom_line(linewidth=1) +
    geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.15) +
    scale_colour_manual(values=c("#E74C3C","#3498DB","#F39C12","#2ECC71")) +
    scale_fill_manual(values=c("#E74C3C","#3498DB","#F39C12","#2ECC71")) +
    scale_y_continuous(labels=percent_format(accuracy=1)) +
    labs(title="Cumulative Incidence Functions",
         subtitle="Competing risks model",
         x="Time (years)",y="Cumulative Incidence",
         colour="Cause",fill="Cause") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 14. NMA FOREST PLOT
# =============================================================================
plot_nma_forest <- function(nma_obj) {
  if (is.null(nma_obj)||is.null(nma_obj$forest_df)) return(NULL)
  df <- nma_obj$forest_df %>%
    dplyr::filter(treatment2 != "SoC") %>%
    dplyr::mutate(treatment2=reorder(treatment2,md))
  ggplot(df, aes(x=md,y=treatment2,xmin=lower,xmax=upper,
                 colour=md<0)) +
    geom_point(size=4) +
    geom_errorbarh(height=0.25,linewidth=0.8) +
    geom_vline(xintercept=0,colour="#566573",
               linetype="dashed",linewidth=0.8) +
    scale_colour_manual(
      values=c("TRUE"="#E74C3C","FALSE"="#2ECC71"),
      labels=c("TRUE"="Unfavourable","FALSE"="Favourable")) +
    labs(title="NMA Forest Plot — vs SoC",
         subtitle="Mean difference in MMSE score",
         x="Mean Difference (95% CrI)",y=NULL,colour="Direction") +
    dark_theme +
    theme(legend.position="bottom",
          panel.grid.major.y=element_blank())
}

# =============================================================================
# 15. EVSI
# =============================================================================
plot_evsi <- function(evsi_obj) {
  if (is.null(evsi_obj)||is.null(evsi_obj$evsi_df)) return(NULL)
  df <- evsi_obj$evsi_df
  ggplot(df, aes(x=n,y=EVSI,colour=study_type)) +
    geom_line(linewidth=1.2) +
    geom_hline(aes(yintercept=evoi,linetype="EVPI"),
               colour="#F39C12",linewidth=0.8) +
    scale_colour_manual(
      values=c("#3498DB","#2ECC71","#8E44AD","#E74C3C"),
      name="Study Design") +
    scale_linetype_manual(values="dashed",name=NULL) +
    scale_y_continuous(labels=dollar_format(prefix="EUR ",big.mark=",")) +
    scale_x_continuous(labels=scales::comma) +
    labs(title="Expected Value of Sample Information (EVSI)",
         subtitle="Value of further research by study size",
         x="Sample Size per Arm",y="EVSI (EUR)") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 16. BUDGET IMPACT
# =============================================================================
plot_budget_impact <- function(bia_obj) {
  if (is.null(bia_obj)||is.null(bia_obj$bia_df)) return(NULL)
  df <- bia_obj$bia_df
  ggplot(df, aes(x=Year,y=Budget_Impact,fill=Arm)) +
    geom_col(position="dodge",width=0.7) +
    geom_hline(yintercept=0,colour="#566573") +
    scale_fill_manual(
      values=demcaps_arms[names(demcaps_arms)!="SoC"]) +
    scale_y_continuous(labels=dollar_format(prefix="EUR ",big.mark=",")) +
    labs(title="Budget Impact Analysis — Netherlands",
         subtitle="Incremental budget impact vs SoC over 5 years",
         x="Year",y="Budget Impact (EUR)",fill="Intervention") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 17. MCDA
# =============================================================================
plot_mcda <- function(mcda_obj) {
  if (is.null(mcda_obj)||is.null(mcda_obj$mcda_df)) return(NULL)
  df <- mcda_obj$mcda_df
  ggplot(df, aes(x=reorder(Arm,Weighted_Score),
                 y=Weighted_Score,fill=Arm)) +
    geom_col(width=0.6,show.legend=FALSE) +
    geom_text(aes(label=round(Weighted_Score,2)),
              vjust=-0.4,colour="#E6EDF3",size=4) +
    scale_fill_manual(values=demcaps_arms) +
    scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
    labs(title="MCDA — Multi-Criteria Decision Analysis",
         subtitle="Weighted composite score across all criteria",
         x=NULL,y="Weighted Score") +
    dark_theme
}

# =============================================================================
# 18. THRESHOLD ANALYSIS
# =============================================================================
plot_threshold <- function(thresh_obj) {
  if (is.null(thresh_obj)||is.null(thresh_obj$threshold_df)) return(NULL)
  df     <- thresh_obj$threshold_df
  breaks <- thresh_obj$threshold_vals
  ggplot(df, aes(x=WTP,y=ICER,colour=Arm)) +
    geom_line(linewidth=1.2) +
    geom_hline(
      data=data.frame(
        y=breaks,
        lab=paste0("EUR ",formatC(breaks,format="d",big.mark=","),"/QALY")),
      aes(yintercept=y,linetype=lab),
      colour="#F39C12",linewidth=0.7,inherit.aes=FALSE) +
    scale_colour_manual(values=demcaps_arms) +
    scale_y_continuous(labels=dollar_format(prefix="EUR ",big.mark=",")) +
    scale_x_continuous(labels=dollar_format(prefix="EUR ",big.mark=",")) +
    labs(title="Threshold Analysis",
         subtitle="ICER as a function of WTP threshold",
         x="WTP Threshold (EUR/QALY)",y="ICER (EUR/QALY)",
         colour="Intervention") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 19. ITS
# =============================================================================
plot_its <- function(its_obj) {
  if (is.null(its_obj)||is.null(its_obj$its_df)) return(NULL)
  df     <- its_obj$its_df
  int_pt <- its_obj$intervention_point
  ggplot(df, aes(x=time,y=outcome,colour=phase)) +
    geom_point(alpha=0.5,size=1.5) +
    geom_line(aes(y=fitted),linewidth=1) +
    geom_vline(xintercept=int_pt,colour="#F39C12",
               linetype="dashed",linewidth=0.8) +
    annotate("text",x=int_pt,y=max(df$outcome,na.rm=TRUE)*0.95,
             label="Intervention",colour="#F39C12",hjust=-0.1,size=3.5) +
    scale_colour_manual(values=c("pre"="#95A5A6","post"="#1ABC9C")) +
    labs(title="Interrupted Time Series Analysis",
         x="Time",y="Outcome",colour="Phase") +
    dark_theme + theme(legend.position="bottom")
}

# =============================================================================
# 20. RDD
# =============================================================================
plot_rdd <- function(rdd_obj) {
  if (is.null(rdd_obj)||is.null(rdd_obj$rdd_df)) return(NULL)
  df        <- rdd_obj$rdd_df
  threshold <- rdd_obj$cutpoint
  ggplot(df, aes(x=running,y=outcome,colour=factor(treatment))) +
    geom_point(alpha=0.4,size=1.5) +
    geom_smooth(method="lm",formula=y~x,se=TRUE,linewidth=1) +
    geom_vline(xintercept=threshold,colour="#F39C12",
               linetype="dashed",linewidth=0.8) +
    annotate("text",x=threshold,y=max(df$outcome,na.rm=TRUE)*0.95,
             label=paste("Cutpoint:",threshold),
             colour="#F39C12",hjust=-0.1,size=3.5) +
    scale_colour_manual(values=c("0"="#95A5A6","1"="#1ABC9C"),
                        labels=c("Control","Treated")) +
    labs(title="Regression Discontinuity Design",
         x="Running Variable",y="Outcome",colour="Assignment") +
    dark_theme + theme(legend.position="bottom")
}