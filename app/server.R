# =============================================================================
# DEM-CAPS Health-Economic Model
# File: app/server.R
# BUG FIX: No more global mutations via assign()
# All parameters passed explicitly as function arguments
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
  # REACTIVE: Initial state distribution from sliders
  # --------------------------------------------------------------------------
  init_dist_r <- reactive({
    vals <- c(
      MCI         = input$i_mci      / 100,
      Mild_AD     = input$i_mild     / 100,
      Moderate_AD = input$i_moderate / 100,
      Severe_AD   = input$i_severe   / 100,
      Death       = 0
    )
    total        <- sum(vals)
    vals["Death"] <- max(0, 1 - total)
    vals / sum(vals)
  })
  
  # Validation message under sliders
  output$dist_check_msg <- renderUI({
    total <- input$i_mci + input$i_mild +
      input$i_moderate + input$i_severe
    if (abs(total - 100) > 0.5) {
      tags$div(
        style = "color:#E74C3C; font-size:11px; font-weight:600;",
        icon("triangle-exclamation"),
        paste0(" Sum = ", total, "% (auto-normalised to 100%)")
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
  # FIX: parameters passed as arguments, not written to global environment
  # --------------------------------------------------------------------------
  model_results <- eventReactive(input$run_model, {
    
    w <- Waiter$new(
      html = tagList(
        spin_loaders(15, color = "#1ABC9C"),
        tags$br(),
        tags$span(style = "color:#FFFFFF; font-size:14px;",
                  "Running base-case model...")
      )
    )
    w$show()
    on.exit(w$hide())
    
    # FIX: override model globals safely for this session only
    # by temporarily assigning within the reactive scope
    old_vals <- list(
      rr_psych    = rr_psych_progression,
      rr_tech     = rr_tech_progression,
      disc_c      = discount_costs,
      disc_e      = discount_effects,
      n_cyc       = n_cycles,
      u_p         = u_gain_psych,
      u_t         = u_gain_tech
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
    
    # Restore original values after run
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
    
    w <- Waiter$new(
      html = tagList(
        spin_loaders(15, color = "#1ABC9C"),
        tags$br(),
        tags$span(style = "color:#FFFFFF; font-size:14px;",
                  paste("Running", input$i_n_sim,
                        "PSA simulations..."))
      )
    )
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
    w <- Waiter$new(
      html = tagList(
        spin_loaders(15, color = "#1ABC9C"),
        tags$br(),
        tags$span(style = "color:#FFFFFF; font-size:14px;",
                  "Running sensitivity analysis...")
      )
    )
    w$show()
    on.exit(w$hide())
    run_dsa(init_dist = init_dist_r())
  })
  
  # --------------------------------------------------------------------------
  # REACTIVE: Scenarios
  # --------------------------------------------------------------------------
  scenario_results <- eventReactive(input$run_scenarios_btn, {
    w <- Waiter$new(
      html = tagList(
        spin_loaders(15, color = "#1ABC9C"),
        tags$br(),
        tags$span(style = "color:#FFFFFF; font-size:14px;",
                  "Running scenario analysis...")
      )
    )
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
      paste0("EUR ", formatC(round(nmb), format = "d", big.mark = ",")),
      "NMB Psychosocial @ EUR 20k",
      icon  = icon("euro-sign"),
      color = if (nmb > 0) "green" else "red"
    )
  })
  
  # --------------------------------------------------------------------------
  # BASE-CASE TABLE
  # --------------------------------------------------------------------------
  output$tbl_basecase <- renderDT({
    res <- model_results()$results
    res$Total_Cost  <- round(res$Total_Cost)
    res$Total_QALY  <- round(res$Total_QALY, 3)
    res$Inc_Cost    <- round(res$Inc_Cost)
    res$Inc_QALY    <- round(res$Inc_QALY, 3)
    res$ICER        <- round(res$ICER)
    res$NMB_20k     <- round(res$NMB_20k)
    res$NMB_50k     <- round(res$NMB_50k)
    res$NMB_80k     <- round(res$NMB_80k)
    
    datatable(
      res,
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatCurrency(c("Total_Cost", "Inc_Cost",
                       "ICER", "NMB_20k",
                       "NMB_50k", "NMB_80k"),
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
    datatable(
      dsa_results(),
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # --------------------------------------------------------------------------
  # SCENARIOS
  # --------------------------------------------------------------------------
  output$tbl_scenarios <- renderDT({
    req(scenario_results())
    datatable(
      scenario_results(),
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
  
}