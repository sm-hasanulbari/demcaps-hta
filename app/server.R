# =============================================================================
# DEM-CAPS Health-Economic Model
# File: app/server.R
# =============================================================================

library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(waiter)

source("model/parameters.R")
source("model/markov_model.R")
source("model/sensitivity.R")
source("model/visualizations.R")

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # REACTIVE: Initial state distribution
  # --------------------------------------------------------------------------
  init_dist_r <- reactive({
    vals <- c(
      MCI         = input$i_mci      / 100,
      Mild_AD     = input$i_mild     / 100,
      Moderate_AD = input$i_moderate / 100,
      Severe_AD   = input$i_severe   / 100,
      Death       = 0
    )
    total         <- sum(vals)
    vals["Death"] <- max(0, 1 - total)
    vals / sum(vals)
  })
  
  output$dist_check_msg <- renderUI({
    total <- input$i_mci + input$i_mild +
      input$i_moderate + input$i_severe
    if (abs(total - 100) > 0.5) {
      tags$div(
        style = "color:#E74C3C; font-size:11px; font-weight:600;",
        icon("triangle-exclamation"),
        paste0(" Sum = ", total, "% (auto-normalised)")
      )
    } else {
      tags$div(
        style = "color:#1ABC9C; font-size:11px;",
        icon("check"), " Distribution sums to 100%"
      )
    }
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Base-case model
  # --------------------------------------------------------------------------
  model_results <- eventReactive(input$run_model, {
    w <- Waiter$new(html = tagList(
      spin_loaders(15, color = "#1ABC9C"),
      tags$br(),
      tags$span(style = "color:#FFFFFF; font-size:14px;",
                "Running base-case model...")
    ))
    w$show()
    on.exit(w$hide())
    
    old_vals <- list(
      rr_psych = rr_psych_progression,
      rr_tech  = rr_tech_progression,
      disc_c   = discount_costs,
      disc_e   = discount_effects,
      n_cyc    = n_cycles,
      u_p      = u_gain_psych,
      u_t      = u_gain_tech
    )
    
    rr_psych_progression  <<- input$i_rr_psych
    rr_tech_progression   <<- input$i_rr_tech
    rr_combo_progression  <<- input$i_rr_psych * input$i_rr_tech
    discount_costs        <<- input$i_disc_c / 100
    discount_effects      <<- input$i_disc_e / 100
    n_cycles              <<- input$i_horizon
    u_gain_psych          <<- input$i_u_psych
    u_gain_tech           <<- input$i_u_tech
    
    result <- run_full_model(init_dist = init_dist_r())
    
    rr_psych_progression  <<- old_vals$rr_psych
    rr_tech_progression   <<- old_vals$rr_tech
    rr_combo_progression  <<- old_vals$rr_psych * old_vals$rr_tech
    discount_costs        <<- old_vals$disc_c
    discount_effects      <<- old_vals$disc_e
    n_cycles              <<- old_vals$n_cyc
    u_gain_psych          <<- old_vals$u_p
    u_gain_tech           <<- old_vals$u_t
    
    result
  }, ignoreNULL = FALSE)
  
  # --------------------------------------------------------------------------
  # REACTIVE: PSA
  # --------------------------------------------------------------------------
  psa_results <- eventReactive(input$run_psa_btn, {
    w <- Waiter$new(html = tagList(
      spin_loaders(15, color = "#1ABC9C"),
      tags$br(),
      tags$span(style = "color:#FFFFFF; font-size:14px;",
                paste("Running", input$i_n_sim, "PSA simulations..."))
    ))
    w$show()
    on.exit(w$hide())
    run_psa(n_sim     = input$i_n_sim,
            init_dist = init_dist_r(),
            seed      = 42)
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: DSA
  # --------------------------------------------------------------------------
  dsa_results <- eventReactive(input$run_dsa_btn, {
    w <- Waiter$new(html = tagList(
      spin_loaders(15, color = "#1ABC9C"),
      tags$br(),
      tags$span(style = "color:#FFFFFF; font-size:14px;",
                "Running sensitivity analysis...")
    ))
    w$show()
    on.exit(w$hide())
    run_dsa(init_dist = init_dist_r())
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Scenarios
  # --------------------------------------------------------------------------
  scenario_results <- eventReactive(input$run_scenarios_btn, {
    w <- Waiter$new(html = tagList(
      spin_loaders(15, color = "#1ABC9C"),
      tags$br(),
      tags$span(style = "color:#FFFFFF; font-size:14px;",
                "Running scenario analysis...")
    ))
    w$show()
    on.exit(w$hide())
    run_scenarios()
  })
  
  # --------------------------------------------------------------------------
  # VALUE BOXES
  # --------------------------------------------------------------------------
  format_icer <- function(icer) {
    if (is.na(icer) || is.null(icer)) return("Dominated")
    if (icer < 0)                     return("Dominant")
    paste0("EUR ", formatC(round(icer), format = "d", big.mark = ","))
  }
  
  output$vbox_icer_psych <- renderValueBox({
    res  <- model_results()$results
    icer <- res$ICER[res$Arm == "Psychosocial"]
    valueBox(format_icer(icer), "ICER - Psychosocial",
             icon = icon("brain"), color = "teal")
  })
  
  output$vbox_icer_tech <- renderValueBox({
    res  <- model_results()$results
    icer <- res$ICER[res$Arm == "Technology"]
    valueBox(format_icer(icer), "ICER - Technology",
             icon = icon("microchip"), color = "blue")
  })
  
  output$vbox_icer_combo <- renderValueBox({
    res  <- model_results()$results
    icer <- res$ICER[res$Arm == "Combination"]
    valueBox(format_icer(icer), "ICER - Combination",
             icon = icon("circle-plus"), color = "red")
  })
  
  output$vbox_nmb_psych <- renderValueBox({
    res <- model_results()$results
    nmb <- res$NMB_20k[res$Arm == "Psychosocial"]
    valueBox(
      paste0("EUR ", formatC(round(nmb),
                             format = "d", big.mark = ",")),
      "NMB Psychosocial @ EUR 20k",
      icon  = icon("euro-sign"),
      color = if (nmb > 0) "green" else "red"
    )
  })
  
  # --------------------------------------------------------------------------
  # BASE-CASE TABLE
  # --------------------------------------------------------------------------
  output$tbl_basecase <- renderDT({
    res             <- model_results()$results
    res$Total_Cost  <- round(res$Total_Cost)
    res$Total_QALY  <- round(res$Total_QALY, 3)
    res$Inc_Cost    <- round(res$Inc_Cost)
    res$Inc_QALY    <- round(res$Inc_QALY, 3)
    res$ICER        <- round(res$ICER)
    res$NMB_20k     <- round(res$NMB_20k)
    res$NMB_50k     <- round(res$NMB_50k)
    res$NMB_80k     <- round(res$NMB_80k)
    datatable(res,
              options  = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
    ) %>%
      formatCurrency(c("Total_Cost", "Inc_Cost", "ICER",
                       "NMB_20k", "NMB_50k", "NMB_80k"),
                     currency = "EUR ", digits = 0)
  })
  
  # --------------------------------------------------------------------------
  # BASE-CASE PLOTS
  # --------------------------------------------------------------------------
  output$plt_costs <- renderPlotly({
    plot_cost_breakdown(model_results()$results)
  })
  
  output$plt_nmb <- renderPlotly({
    plot_nmb(model_results()$results, wtp = 20000)
  })
  
  # --------------------------------------------------------------------------
  # PSA PLOTS
  # --------------------------------------------------------------------------
  output$plt_ce_plane <- renderPlotly({
    req(psa_results())
    plot_ce_plane(psa_results(), wtp_line = 20000)
  })
  
  output$plt_ceac <- renderPlotly({
    req(psa_results())
    ceac_df <- calc_ceac(psa_results())
    plot_ceac(ceac_df)
  })
  
  # --------------------------------------------------------------------------
  # DSA
  # --------------------------------------------------------------------------
  output$plt_tornado <- renderPlotly({
    req(dsa_results())
    plot_tornado(dsa_results())
  })
  
  output$tbl_dsa <- renderDT({
    req(dsa_results())
    datatable(dsa_results(),
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  # --------------------------------------------------------------------------
  # SCENARIOS
  # --------------------------------------------------------------------------
  output$tbl_scenarios <- renderDT({
    req(scenario_results())
    datatable(scenario_results(),
              options  = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  # --------------------------------------------------------------------------
  # DISEASE TRACE
  # --------------------------------------------------------------------------
  output$plt_trace <- renderPlotly({
    res     <- model_results()
    arm_idx <- switch(input$trace_arm,
                      "SoC"          = 1,
                      "Psychosocial" = 2,
                      "Technology"   = 3,
                      "Combination"  = 4)
    plot_trace(res$traces[[arm_idx]],
               arm = input$trace_arm)
  })
  
  # --------------------------------------------------------------------------
  # EVIDENCE TAB
  # --------------------------------------------------------------------------
  evidence_data <- reactiveVal(NULL)
  
  empty_dt <- function(msg = "Click Refresh Evidence to load data") {
    datatable(
      data.frame(Message = msg),
      rownames = FALSE,
      options  = list(dom = "t")
    )
  }
  
  # Load cache on startup
  observe({
    cache_path <- "data/processed/clinicaltrials_cache.rds"
    if (file.exists(cache_path)) {
      tryCatch({
        source("model/api_clinicaltrials.R", local = TRUE)
        cached  <- load_trial_cache(cache_path)
        summary <- summarise_trial_evidence(cached)
        evidence_data(list(
          trials         = cached,
          trial_summary  = summary,
          pubmed_psych   = NULL,
          pubmed_tech    = NULL,
          who_prevalence = NULL,
          eurostat_pop   = NULL,
          pull_date      = Sys.Date()
        ))
      }, error = function(e) NULL)
    }
  })
  
  # Refresh button — pulls all APIs live
  observeEvent(input$run_evidence_btn, {
    w <- Waiter$new(html = tagList(
      spin_loaders(15, color = "#1ABC9C"),
      tags$br(),
      tags$span(style = "color:#FFFFFF; font-size:14px;",
                "Pulling live evidence from APIs...")
    ))
    w$show()
    on.exit(w$hide())
    source("model/api_clinicaltrials.R", local = TRUE)
    ev <- pull_all_evidence(use_cache = FALSE)
    evidence_data(ev)
  })
  
  output$evidence_last_updated <- renderUI({
    ev <- evidence_data()
    if (is.null(ev)) return(NULL)
    tags$span(
      style = "color:#1ABC9C; font-size:12px;",
      icon("check"),
      paste("Last updated:", ev$pull_date)
    )
  })
  
  # Value boxes
  output$vbox_n_trials <- renderValueBox({
    ev <- evidence_data()
    n  <- if (!is.null(ev) && !is.null(ev$trials))
      nrow(ev$trials) else 0
    valueBox(n, "Total Trials",
             icon = icon("flask"), color = "teal")
  })
  
  output$vbox_n_rct <- renderValueBox({
    ev <- evidence_data()
    n  <- if (!is.null(ev) && !is.null(ev$trials))
      sum(grepl("RANDOMIZED|INTERVENTIONAL",
                toupper(ev$trials$study_type)),
          na.rm = TRUE) else 0
    valueBox(n, "Randomised Trials",
             icon = icon("shuffle"), color = "blue")
  })
  
  output$vbox_n_enrolled <- renderValueBox({
    ev <- evidence_data()
    n  <- if (!is.null(ev) && !is.null(ev$trials))
      formatC(sum(ev$trials$enrollment, na.rm = TRUE),
              format = "d", big.mark = ",") else "0"
    valueBox(n, "Total Enrolled",
             icon = icon("users"), color = "green")
  })
  
  output$vbox_n_pubmed <- renderValueBox({
    ev <- evidence_data()
    n  <- 0
    if (!is.null(ev)) {
      n1 <- if (!is.null(ev$pubmed_psych))
        nrow(ev$pubmed_psych) else 0
      n2 <- if (!is.null(ev$pubmed_tech))
        nrow(ev$pubmed_tech) else 0
      n  <- n1 + n2
    }
    valueBox(n, "PubMed Articles",
             icon = icon("book"), color = "purple")
  })
  
  # Trials table
  output$tbl_trials <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$trials) ||
        nrow(ev$trials) == 0)
      return(empty_dt("No trial data — click Refresh Evidence"))
    
    df <- ev$trials
    if (!is.null(input$ev_category) &&
        input$ev_category != "All")
      df <- df[df$search_category == input$ev_category, ]
    if (!is.null(input$ev_status) &&
        input$ev_status != "All")
      df <- df[df$status == input$ev_status, ]
    
    df <- df[, c("nct_id", "title", "status", "phase",
                 "enrollment", "start_date", "completion",
                 "search_category")]
    datatable(df,
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  # Evidence summary
  output$tbl_evidence_summary <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$trial_summary))
      return(empty_dt())
    datatable(ev$trial_summary,
              options  = list(pageLength = 10),
              rownames = FALSE
    )
  })
  
  # PubMed tables
  output$tbl_pubmed_psych <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$pubmed_psych))
      return(empty_dt())
    datatable(ev$pubmed_psych,
              caption  = "Psychosocial Interventions",
              options  = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  output$tbl_pubmed_tech <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$pubmed_tech))
      return(empty_dt())
    datatable(ev$pubmed_tech,
              caption  = "Technology Interventions",
              options  = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
    )
  })
  
  # WHO table
  output$tbl_who <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$who_prevalence))
      return(empty_dt())
    datatable(ev$who_prevalence,
              options  = list(pageLength = 10),
              rownames = FALSE
    )
  })
  
  # Eurostat table
  output$tbl_eurostat <- renderDT({
    ev <- evidence_data()
    if (is.null(ev) || is.null(ev$eurostat_pop))
      return(empty_dt())
    datatable(ev$eurostat_pop,
              options  = list(pageLength = 10),
              rownames = FALSE
    )
  })
  
} # end server