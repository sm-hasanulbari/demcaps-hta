# =============================================================================
# DEM-CAPS Health-Economic Model — app/server.R
# =============================================================================

library(shiny)
library(DT)
library(plotly)
library(dplyr)
library(waiter)

cat("Loading DEM-CAPS model files...\n")
cat("  parameters.R...\n");        source("model/parameters.R")
cat("  markov_model.R...\n");      source("model/markov_model.R")
cat("  sensitivity.R...\n");       source("model/sensitivity.R")
cat("  visualizations.R...\n");    source("model/visualizations.R")
cat("  ipd_calibration.R...\n");   source("model/ipd_calibration.R")
cat("  causal_inference.R...\n");  source("model/causal_inference.R")
cat("  survival_rwe.R...\n");      source("model/survival_rwe.R")
cat("  advanced_causal.R...\n");   source("model/advanced_causal.R")
cat("  hta_extended.R...\n");      source("model/hta_extended.R")
cat("  api_clinicaltrials.R...\n");source("model/api_clinicaltrials.R")
cat("  assumptions.R...\n");       source("model/assumptions.R")
cat("All files loaded.\n")

empty_plotly <- function(msg = "Click the Run button above to load") {
  plotly::plot_ly() %>%
    plotly::layout(
      xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
      paper_bgcolor="#161B22", plot_bgcolor="#161B22",
      annotations=list(list(text=msg,x=0.5,y=0.5,xref="paper",yref="paper",
                            showarrow=FALSE,font=list(color="#566573",size=13))))
}

empty_dt <- function(msg = "Click the Run button above to load") {
  datatable(data.frame(Status=msg),rownames=FALSE,
            options=list(dom="t",columnDefs=list(list(className="dt-center",targets=0))))
}

# Comprehensive dark theme — also clips legend inside plot area
dark_plot <- function(p) {
  if (is.null(p)) return(NULL)
  if (inherits(p,"gg"))
    p <- p + ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill="#161B22",colour=NA),
      panel.background  = ggplot2::element_rect(fill="#161B22",colour=NA),
      text              = ggplot2::element_text(colour="#E6EDF3"),
      axis.text         = ggplot2::element_text(colour="#8B949E"),
      axis.title        = ggplot2::element_text(colour="#E6EDF3"),
      plot.title        = ggplot2::element_text(colour="#E6EDF3",face="bold"),
      plot.subtitle     = ggplot2::element_text(colour="#8B949E"),
      legend.background = ggplot2::element_rect(fill="#161B22",colour=NA),
      legend.text       = ggplot2::element_text(colour="#E6EDF3"),
      legend.title      = ggplot2::element_text(colour="#1ABC9C"),
      legend.key        = ggplot2::element_rect(fill="#161B22",colour=NA),
      legend.margin     = ggplot2::margin(2,2,2,2),
      legend.box.margin = ggplot2::margin(0,0,0,0),
      strip.background  = ggplot2::element_rect(fill="#21262D",colour=NA),
      strip.text        = ggplot2::element_text(colour="#1ABC9C",face="bold"),
      panel.grid.major  = ggplot2::element_line(colour="#21262D"),
      panel.grid.minor  = ggplot2::element_line(colour="#0D1117"),
      plot.margin       = ggplot2::margin(8,8,8,8))
  p
}

server <- function(input, output, session) {
  
  spinner_waiter <- function(msg="Running...") {
    Waiter$new(html=tagList(spin_loaders(15,color="#1ABC9C"),tags$br(),
                            tags$span(style="color:#FFFFFF;font-size:14px;",msg)))
  }
  
  # --------------------------------------------------------------------------
  # MODEL STATUS & RESET
  # --------------------------------------------------------------------------
  model_run_time <- reactiveVal(NULL)
  
  output$model_status <- renderUI({
    t <- model_run_time()
    if (is.null(t))
      tags$span(icon("circle",style="color:#F39C12;font-size:8px;")," Ready — click Run Model")
    else
      tags$span(icon("circle",style="color:#1ABC9C;font-size:8px;"),paste(" Last run:",t))
  })
  
  observeEvent(input$reset_model, {
    updateSliderInput(session,"i_mci",value=25); updateSliderInput(session,"i_mild",value=45)
    updateSliderInput(session,"i_moderate",value=20); updateSliderInput(session,"i_severe",value=10)
    updateSliderInput(session,"i_rr_psych",value=0.82); updateSliderInput(session,"i_rr_tech",value=0.88)
    updateSliderInput(session,"i_u_psych",value=0.04); updateSliderInput(session,"i_u_tech",value=0.02)
    updateSliderInput(session,"i_horizon",value=20); updateSliderInput(session,"i_disc_c",value=4)
    updateSliderInput(session,"i_disc_e",value=1.5); updateSliderInput(session,"i_n_sim",value=500)
    showNotification("Reset to default values.",type="message")
  })
  
  # --------------------------------------------------------------------------
  # KEY FIX: redraw assumptions DT when its tab becomes active
  # --------------------------------------------------------------------------
  observeEvent(input$active_tab, {
    if (!is.null(input$active_tab) && input$active_tab == "assumptions") {
      session$sendCustomMessage("redrawDT","tbl_assumptions")
    }
  })
  
  # --------------------------------------------------------------------------
  # INIT DIST
  # --------------------------------------------------------------------------
  init_dist_r <- reactive({
    vals <- c(MCI=input$i_mci/100,Mild_AD=input$i_mild/100,
              Moderate_AD=input$i_moderate/100,Severe_AD=input$i_severe/100,Death=0)
    vals["Death"] <- max(0,1-sum(vals))
    vals/sum(vals)
  })
  
  output$dist_check_msg <- renderUI({
    total <- input$i_mci+input$i_mild+input$i_moderate+input$i_severe
    if(abs(total-100)>0.5)
      tags$div(style="color:#E74C3C;font-size:11px;",icon("triangle-exclamation"),paste0(" Sum=",total,"% (auto-normalised)"))
    else
      tags$div(style="color:#1ABC9C;font-size:11px;",icon("check")," Sums to 100%")
  })
  
  # --------------------------------------------------------------------------
  # BASE-CASE MODEL
  # --------------------------------------------------------------------------
  model_results <- eventReactive(input$run_model, {
    w <- spinner_waiter("Running base-case model..."); w$show(); on.exit(w$hide())
    old <- list(rr_p=rr_psych_progression,rr_t=rr_tech_progression,
                dc=discount_costs,de=discount_effects,nc=n_cycles,
                up=u_gain_psych,ut=u_gain_tech)
    rr_psych_progression <<- input$i_rr_psych; rr_tech_progression <<- input$i_rr_tech
    rr_combo_progression <<- input$i_rr_psych*input$i_rr_tech
    discount_costs <<- input$i_disc_c/100; discount_effects <<- input$i_disc_e/100
    n_cycles <<- input$i_horizon; u_gain_psych <<- input$i_u_psych; u_gain_tech <<- input$i_u_tech
    res <- run_full_model(init_dist=init_dist_r())
    rr_psych_progression <<- old$rr_p; rr_tech_progression <<- old$rr_t
    rr_combo_progression <<- old$rr_p*old$rr_t
    discount_costs <<- old$dc; discount_effects <<- old$de
    n_cycles <<- old$nc; u_gain_psych <<- old$up; u_gain_tech <<- old$ut
    model_run_time(format(Sys.time(),"%H:%M:%S")); res
  }, ignoreNULL=FALSE)
  
  fmt_icer <- function(x) {
    if(is.na(x)||is.null(x)) return("Dominated")
    if(x<0) return("Dominant")
    paste0("EUR ",formatC(round(x),format="d",big.mark=","))
  }
  
  output$vbox_icer_psych <- renderValueBox({ res<-model_results()$results; valueBox(fmt_icer(res$ICER[res$Arm=="Psychosocial"]),"ICER Psychosocial",icon=icon("brain"),color="teal") })
  output$vbox_icer_tech  <- renderValueBox({ res<-model_results()$results; valueBox(fmt_icer(res$ICER[res$Arm=="Technology"]),"ICER Technology",icon=icon("microchip"),color="blue") })
  output$vbox_icer_combo <- renderValueBox({ res<-model_results()$results; valueBox(fmt_icer(res$ICER[res$Arm=="Combination"]),"ICER Combination",icon=icon("circle-plus"),color="red") })
  output$vbox_nmb_psych  <- renderValueBox({
    res<-model_results()$results; nmb<-res$NMB_20k[res$Arm=="Psychosocial"]
    valueBox(paste0("EUR ",formatC(round(nmb),format="d",big.mark=",")),"NMB Psychosocial @20k",
             icon=icon("euro-sign"),color=if(nmb>0)"green" else "red")
  })
  
  output$tbl_basecase <- renderDT({
    res <- model_results()$results
    cols <- c("Total_Cost","Total_QALY","Inc_Cost","Inc_QALY","ICER","NMB_20k","NMB_50k","NMB_80k")
    res[,cols] <- lapply(res[,cols],function(x) round(x,2))
    datatable(res,options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)
  })
  
  output$plt_trace <- renderPlotly({
    res <- model_results()
    arm_idx <- switch(input$trace_arm,"SoC"=1,"Psychosocial"=2,"Technology"=3,"Combination"=4)
    plot_trace(res$traces[[arm_idx]],arm=input$trace_arm)
  })
  
  # --------------------------------------------------------------------------
  # PSA
  # --------------------------------------------------------------------------
  psa_store <- reactiveVal(NULL)
  
  observeEvent(input$run_psa_btn, {
    w <- spinner_waiter(paste("Running",input$i_n_sim,"PSA simulations...")); w$show(); on.exit(w$hide())
    tryCatch({
      res <- run_psa(n_sim=input$i_n_sim,init_dist=init_dist_r(),seed=42)
      psa_store(res)
      showNotification(paste0("PSA complete — ",input$i_n_sim," sims."),duration=4,type="message")
    }, error=function(e) showNotification(paste("PSA error:",e$message),type="error"))
  })
  
  observeEvent(input$run_all_btn, {
    w <- spinner_waiter("Running full analysis pipeline..."); w$show(); on.exit(w$hide())
    tryCatch({
      res <- run_psa(n_sim=500,seed=42,init_dist=init_dist_r())
      psa_store(res)
      showNotification("Full analysis complete — PSA loaded.",duration=5,type="message")
    }, error=function(e) showNotification(paste("Error:",e$message),type="error"))
  })
  
  output$plt_ce_plane <- renderPlotly({ d<-psa_store(); if(is.null(d)) return(empty_plotly("Click Run PSA to load")); plot_ce_plane(d,wtp_line=20000) })
  output$plt_ceac     <- renderPlotly({ d<-psa_store(); if(is.null(d)) return(empty_plotly("Click Run PSA to load")); plot_ceac(calc_ceac(d)) })
  output$plt_ce_plane_dash <- renderPlotly({ d<-psa_store(); if(is.null(d)) return(empty_plotly("Run PSA or Full Analysis to populate")); plot_ce_plane(d,wtp_line=20000) })
  output$plt_ceac_dash     <- renderPlotly({ d<-psa_store(); if(is.null(d)) return(empty_plotly("Run PSA or Full Analysis to populate")); plot_ceac(calc_ceac(d)) })
  
  # --------------------------------------------------------------------------
  # DSA
  # --------------------------------------------------------------------------
  dsa_store <- reactiveVal(NULL)
  
  observeEvent(input$run_dsa_btn, {
    w <- spinner_waiter("Running DSA..."); w$show(); on.exit(w$hide())
    tryCatch({ dsa_store(run_dsa(init_dist=init_dist_r())) },
             error=function(e) showNotification(paste("DSA error:",e$message),type="error"))
  })
  
  output$plt_tornado <- renderPlotly({ d<-dsa_store(); if(is.null(d)) return(empty_plotly("Click Run DSA to load")); plot_tornado(d) })
  output$tbl_dsa <- renderDT({ d<-dsa_store(); if(is.null(d)) return(empty_dt("Click Run DSA to load")); datatable(d,options=list(pageLength=15,scrollX=TRUE),rownames=FALSE) })
  
  # --------------------------------------------------------------------------
  # SCENARIOS
  # --------------------------------------------------------------------------
  scen_store <- reactiveVal(NULL)
  
  observeEvent(input$run_scenarios_btn, {
    w <- spinner_waiter("Running 6 scenarios..."); w$show(); on.exit(w$hide())
    tryCatch({
      res    <- run_scenarios()
      n_arms <- if("Arm" %in% names(res)) length(unique(res$Arm)) else 4
      n_scen <- nrow(res)/n_arms
      labels <- c("S1 — Base-case","S2 — Early cohort","S3 — Late cohort",
                  "S4 — No informal care","S5 — 10-year horizon","S6 — Tech scale-up")
      res$Scenario <- rep(if(n_scen<=6) labels[seq_len(n_scen)] else paste0("S",seq_len(n_scen)),each=n_arms)
      cols <- intersect(c("Total_Cost","Total_QALY","Inc_Cost","Inc_QALY","ICER"),names(res))
      res[,cols] <- lapply(res[,cols],function(x) round(x,2))
      res <- res[,c("Scenario",setdiff(names(res),"Scenario"))]
      scen_store(res)
    }, error=function(e) showNotification(paste("Scenario error:",e$message),type="error"))
  })
  
  output$tbl_scenarios <- renderDT({
    d<-scen_store(); if(is.null(d)) return(empty_dt("Click Run Scenarios to load"))
    datatable(d,options=list(pageLength=24,scrollX=TRUE),rownames=FALSE) %>%
      formatStyle("Scenario",
                  backgroundColor=styleEqual(
                    c("S1 — Base-case","S2 — Early cohort","S3 — Late cohort",
                      "S4 — No informal care","S5 — 10-year horizon","S6 — Tech scale-up"),
                    c("rgba(26,188,156,0.10)","rgba(52,152,219,0.10)","rgba(243,156,18,0.10)",
                      "rgba(231,76,60,0.10)","rgba(142,68,173,0.10)","rgba(39,174,96,0.10)")),
                  fontWeight="bold")
  })
  
  # --------------------------------------------------------------------------
  # EVIDENCE
  # --------------------------------------------------------------------------
  evidence_data <- reactiveVal(NULL)
  
  observe({
    cache_path <- "data/processed/clinicaltrials_cache.rds"
    if(file.exists(cache_path)) {
      tryCatch({
        cached <- load_trial_cache(cache_path)
        evidence_data(list(trials=cached,trial_summary=summarise_trial_evidence(cached),
                           pubmed_psych=NULL,pubmed_tech=NULL,who_prevalence=NULL,eurostat_pop=NULL,pull_date=Sys.Date()))
      }, error=function(e) NULL)
    }
  })
  
  observeEvent(input$run_evidence_btn, {
    w <- spinner_waiter("Pulling live evidence..."); w$show(); on.exit(w$hide())
    evidence_data(pull_all_evidence(use_cache=FALSE))
  })
  
  output$evidence_last_updated <- renderUI({
    ev<-evidence_data(); if(is.null(ev)) return(NULL)
    tags$span(style="color:#1ABC9C;font-size:12px;",icon("check"),paste("Updated:",ev$pull_date))
  })
  
  output$vbox_n_trials   <- renderValueBox({ ev<-evidence_data(); valueBox(if(!is.null(ev)&&!is.null(ev$trials)) nrow(ev$trials) else 0,"Trials",icon("flask"),color="teal") })
  output$vbox_n_rct      <- renderValueBox({ ev<-evidence_data(); n<-if(!is.null(ev)&&!is.null(ev$trials)) sum(grepl("RANDOMIZED|INTERVENTIONAL",toupper(ev$trials$study_type)),na.rm=TRUE) else 0; valueBox(n,"RCTs",icon("shuffle"),color="blue") })
  output$vbox_n_enrolled <- renderValueBox({ ev<-evidence_data(); n<-if(!is.null(ev)&&!is.null(ev$trials)) formatC(sum(ev$trials$enrollment,na.rm=TRUE),format="d",big.mark=",") else "0"; valueBox(n,"Enrolled",icon("users"),color="green") })
  output$vbox_n_pubmed   <- renderValueBox({
    ev<-evidence_data(); n<-0
    if(!is.null(ev)) n<-(if(!is.null(ev$pubmed_psych)) nrow(ev$pubmed_psych) else 0)+(if(!is.null(ev$pubmed_tech)) nrow(ev$pubmed_tech) else 0)
    valueBox(n,"PubMed",icon("book"),color="purple")
  })
  
  output$tbl_trials <- renderDT({
    ev<-evidence_data(); if(is.null(ev)||is.null(ev$trials)) return(empty_dt())
    df<-ev$trials
    if(!is.null(input$ev_category)&&input$ev_category!="All") df<-df[df$search_category==input$ev_category,]
    if(!is.null(input$ev_status)&&input$ev_status!="All") df<-df[df$status==input$ev_status,]
    datatable(df[,c("nct_id","title","status","phase","enrollment","search_category")],options=list(pageLength=15,scrollX=TRUE),rownames=FALSE)
  })
  output$tbl_evidence_summary <- renderDT({ ev<-evidence_data(); if(is.null(ev)||is.null(ev$trial_summary)) return(empty_dt()); datatable(ev$trial_summary,options=list(pageLength=10),rownames=FALSE) })
  output$tbl_pubmed_psych <- renderDT({ ev<-evidence_data(); if(is.null(ev)||is.null(ev$pubmed_psych)) return(empty_dt()); datatable(ev$pubmed_psych,options=list(pageLength=10,scrollX=TRUE),rownames=FALSE) })
  output$tbl_pubmed_tech  <- renderDT({ ev<-evidence_data(); if(is.null(ev)||is.null(ev$pubmed_tech))  return(empty_dt()); datatable(ev$pubmed_tech, options=list(pageLength=10,scrollX=TRUE),rownames=FALSE) })
  output$tbl_who      <- renderDT({ ev<-evidence_data(); if(is.null(ev)||is.null(ev$who_prevalence)) return(empty_dt()); datatable(ev$who_prevalence,options=list(pageLength=10),rownames=FALSE) })
  output$tbl_eurostat <- renderDT({ ev<-evidence_data(); if(is.null(ev)||is.null(ev$eurostat_pop))  return(empty_dt()); datatable(ev$eurostat_pop,  options=list(pageLength=10),rownames=FALSE) })
  
  # --------------------------------------------------------------------------
  # IPD CALIBRATION
  # --------------------------------------------------------------------------
  ipd_cal <- reactiveVal(NULL)
  
  observeEvent(input$run_ipd_btn, {
    w <- spinner_waiter("Running IPD calibration..."); w$show(); on.exit(w$hide())
    tryCatch({
      ipd <- generate_synthetic_ipd(n_patients=input$ipd_n_patients,seed=2024)
      cal <- run_ipd_calibration(ipd_path=NULL,method_tp=input$ipd_method,n_synth=input$ipd_n_patients)
      cal$tp_matrix["Death",] <- c(0,0,0,0,1)
      ipd_cal(cal)
    }, error=function(e) showNotification(paste("IPD error:",e$message),type="error"))
  })
  
  output$vbox_ipd_patients <- renderValueBox({ cal<-ipd_cal(); valueBox(if(!is.null(cal)) cal$n_patients else "—","Patients",icon("users"),color="teal") })
  output$vbox_ipd_trans    <- renderValueBox({ cal<-ipd_cal(); valueBox(if(!is.null(cal)) cal$n_transitions else "—","Transitions",icon("shuffle"),color="blue") })
  output$vbox_ipd_rmse     <- renderValueBox({ cal<-ipd_cal(); valueBox(if(!is.null(cal)) round(cal$gof$rmse,4) else "—","RMSE",icon("chart-line"),color="green") })
  output$vbox_ipd_mae      <- renderValueBox({ cal<-ipd_cal(); valueBox(if(!is.null(cal)) round(cal$gof$mae,4) else "—","MAE",icon("bullseye"),color="yellow") })
  output$plt_gof        <- renderPlot({ cal<-ipd_cal(); if(is.null(cal)) return(NULL); dark_plot(plot_gof(cal$gof)) },                                              bg="#161B22")
  output$plt_tp_compare <- renderPlot({ cal<-ipd_cal(); if(is.null(cal)) return(NULL); dark_plot(plot_tp_comparison(tp_lit=tp_base,tp_ipd=cal$tp_matrix)) },        bg="#161B22")
  output$tbl_tp_cal     <- renderDT({  cal<-ipd_cal(); if(is.null(cal)) return(empty_dt()); datatable(as.data.frame(round(cal$tp_matrix,3)),options=list(pageLength=5)) })
  
  # --------------------------------------------------------------------------
  # CAUSAL INFERENCE
  # --------------------------------------------------------------------------
  causal_store <- reactiveVal(NULL)
  
  observeEvent(input$run_causal_btn, {
    w <- spinner_waiter("Running causal analysis..."); w$show(); on.exit(w$hide())
    tryCatch({
      ipd <- generate_synthetic_ipd(200,seed=2024)
      psa <- run_psa(n_sim=100,seed=42)
      causal_store(run_causal_analysis(ipd,psa_df=psa,wtp=20000))
    }, error=function(e) showNotification(paste("Causal error:",e$message),type="error"))
  })
  
  output$tbl_causal  <- renderDT({   d<-causal_store(); if(is.null(d)) return(empty_dt("Click Run Causal Analysis to load")); datatable(d$summary,options=list(pageLength=10),rownames=FALSE) })
  output$plt_ps_dist <- renderPlot({ d<-causal_store(); if(is.null(d)||is.null(d$psm)) return(NULL); dark_plot(plot_ps_distribution(d$psm)) }, bg="#161B22")
  output$plt_evppi   <- renderPlot({ d<-causal_store(); if(is.null(d)||is.null(d$voi)) return(NULL); dark_plot(plot_evppi(d$voi)) },           bg="#161B22")
  
  # --------------------------------------------------------------------------
  # ADVANCED CAUSAL
  # --------------------------------------------------------------------------
  advanced_store <- reactiveVal(NULL)
  
  observeEvent(input$run_advanced_btn, {
    w <- spinner_waiter("Running advanced causal methods..."); w$show(); on.exit(w$hide())
    tryCatch({
      advanced_store(run_advanced_causal_pipeline(generate_synthetic_ipd(200,seed=2024)))
    }, error=function(e) showNotification(paste("Advanced error:",e$message),type="error"))
  })
  
  output$tbl_advanced <- renderDT({   d<-advanced_store(); if(is.null(d)) return(empty_dt("Click Run Advanced Causal to load")); datatable(d$summary,options=list(pageLength=10),rownames=FALSE) })
  output$plt_its      <- renderPlot({ d<-advanced_store(); if(is.null(d)||is.null(d$its)) return(NULL); dark_plot(plot_its(d$its)) }, bg="#161B22")
  output$plt_rdd      <- renderPlot({ d<-advanced_store(); if(is.null(d)||is.null(d$rdd)) return(NULL); dark_plot(plot_rdd(d$rdd)) }, bg="#161B22")
  
  # --------------------------------------------------------------------------
  # SURVIVAL & RWE
  # --------------------------------------------------------------------------
  survival_store <- reactiveVal(NULL)
  
  observeEvent(input$run_survival_btn, {
    w <- spinner_waiter("Running survival & RWE pipeline..."); w$show(); on.exit(w$hide())
    tryCatch({
      survival_store(run_survival_rwe_pipeline(generate_synthetic_ipd(200,seed=2024)))
    }, error=function(e) showNotification(paste("Survival error:",e$message),type="error"))
  })
  
  output$plt_survival <- renderPlot({ d<-survival_store(); if(is.null(d)||is.null(d$survival))  return(NULL); dark_plot(plot_survival_extrapolation(d$survival)) }, bg="#161B22")
  output$plt_cif      <- renderPlot({ d<-survival_store(); if(is.null(d)||is.null(d$comp_risk)) return(NULL); dark_plot(plot_cif(d$comp_risk)) },                   bg="#161B22")
  output$plt_nma      <- renderPlot({ d<-survival_store(); if(is.null(d)||is.null(d$nma))       return(NULL); dark_plot(plot_nma_forest(d$nma)) },                   bg="#161B22")
  output$tbl_nma <- renderDT({
    d<-survival_store(); if(is.null(d)||is.null(d$nma)) return(empty_dt("Click Run Survival & RWE to load"))
    league <- d$nma$league %>% dplyr::filter(treatment1=="SoC") %>% dplyr::mutate(across(where(is.numeric),~round(.,3)))
    datatable(league,options=list(pageLength=10),rownames=FALSE)
  })
  output$tbl_multistate <- renderDT({
    d<-survival_store(); if(is.null(d)||is.null(d$multistate)) return(empty_dt("Click Run Survival & RWE to load"))
    datatable(d$multistate$transitions %>% dplyr::mutate(across(where(is.numeric),~round(.,4))),options=list(pageLength=10),rownames=FALSE)
  })
  
  # --------------------------------------------------------------------------
  # EXTENDED HTA
  # --------------------------------------------------------------------------
  hta_store <- reactiveVal(NULL)
  
  observeEvent(input$run_hta_btn, {
    w <- spinner_waiter("Running extended HTA pipeline..."); w$show(); on.exit(w$hide())
    tryCatch({
      psa <- run_psa(n_sim=200,seed=42)
      hta_store(run_hta_extended_pipeline(model_results=model_results(),psa_df=psa))
    }, error=function(e) showNotification(paste("HTA error:",e$message),type="error"))
  })
  
  output$plt_evsi      <- renderPlot({ d<-hta_store(); if(is.null(d)||is.null(d$evsi))      return(NULL); dark_plot(plot_evsi(d$evsi)) },           bg="#161B22")
  output$plt_bia       <- renderPlot({ d<-hta_store(); if(is.null(d)||is.null(d$bia))        return(NULL); dark_plot(plot_budget_impact(d$bia)) },   bg="#161B22")
  output$plt_mcda      <- renderPlot({ d<-hta_store(); if(is.null(d)||is.null(d$mcda))       return(NULL); dark_plot(plot_mcda(d$mcda)) },           bg="#161B22")
  output$plt_threshold <- renderPlot({ d<-hta_store(); if(is.null(d)||is.null(d$threshold))  return(NULL); dark_plot(plot_threshold(d$threshold)) }, bg="#161B22")
  output$tbl_mcda <- renderDT({ d<-hta_store(); if(is.null(d)||is.null(d$mcda)) return(empty_dt("Click Run Extended HTA to load")); datatable(d$mcda$mcda_df,options=list(pageLength=5),rownames=FALSE) })
  output$tbl_subgroup <- renderDT({
    d<-hta_store(); if(is.null(d)||is.null(d$subgroup)) return(empty_dt("Click Run Extended HTA to load"))
    datatable(d$subgroup$sg_df %>% dplyr::mutate(across(where(is.numeric),~round(.,2))),options=list(pageLength=15,scrollX=TRUE),rownames=FALSE)
  })
  output$tbl_equity <- renderDT({
    d<-hta_store(); if(is.null(d)||is.null(d$equity)) return(empty_dt("Click Run Extended HTA to load"))
    df <- d$equity$equity_df %>% dplyr::select(Arm,scheme,equity_weight,ICER_eq) %>% dplyr::mutate(across(where(is.numeric),~round(.,2)))
    datatable(df,options=list(pageLength=10),rownames=FALSE)
  })
  
  # --------------------------------------------------------------------------
  # ASSUMPTIONS
  # --------------------------------------------------------------------------
  all_assumptions <- get_assumptions()
  
  output$n_high_inline <- renderUI({ tags$span(sum(all_assumptions$impact=="High")) })
  output$n_mod_inline  <- renderUI({ tags$span(sum(all_assumptions$impact=="Moderate")) })
  output$n_low_inline  <- renderUI({ tags$span(sum(all_assumptions$impact=="Low")) })
  
  filtered_assumptions <- reactive({
    df <- all_assumptions
    if(!is.null(input$assumption_filter)&&input$assumption_filter!="All") df<-df[df$impact==input$assumption_filter,]
    if(!is.null(input$assumption_cat)&&input$assumption_cat!="All")       df<-df[df$category==input$assumption_cat,]
    df
  })
  
  output$tbl_assumptions <- renderDT({
    df <- filtered_assumptions()
    df$traffic <- dplyr::case_when(
      df$impact=="High"     ~ "Red - High",
      df$impact=="Moderate" ~ "Amber - Moderate",
      df$impact=="Low"      ~ "Green - Low")
    display <- df[,c("id","category","assumption","justification","reference","traffic")]
    names(display) <- c("#","Category","Assumption","Justification","Ref","Impact")
    datatable(display, selection="single",
              options=list(
                pageLength=15, scrollX=TRUE,
                autoWidth=FALSE,
                # KEY FIX: adjust columns once DT has finished drawing
                initComplete=JS("function(settings,json){
        var api=this.api();
        setTimeout(function(){ api.columns.adjust().draw(false); }, 250);
      }"),
                columnDefs=list(
                  list(width="30px", targets=0), list(width="120px",targets=1),
                  list(width="250px",targets=2), list(width="300px",targets=3),
                  list(width="80px", targets=4), list(width="100px",targets=5))),
              rownames=FALSE) %>%
      formatStyle("Impact",
                  backgroundColor=styleEqual(
                    c("Red - High","Amber - Moderate","Green - Low"),
                    c("rgba(192,57,43,0.2)","rgba(211,84,0,0.2)","rgba(30,132,73,0.2)")),
                  color=styleEqual(
                    c("Red - High","Amber - Moderate","Green - Low"),
                    c("#E74C3C","#E67E22","#1ABC9C")),
                  fontWeight="bold")
  })
  
  output$assumption_detail <- renderUI({
    idx <- input$tbl_assumptions_rows_selected
    if(is.null(idx)||length(idx)==0) {
      return(tags$div(
        style="text-align:center;padding:30px;color:#566573;font-size:13px;border:1px dashed #30363D;border-radius:6px;",
        icon("hand-pointer",style="font-size:28px;color:#30363D;display:block;margin:0 auto 10px;"),
        tags$br(),"Click any row in the table above to view full assumption details."))
    }
    df  <- filtered_assumptions()
    if(idx>nrow(df)) return(NULL)
    row <- df[idx,]
    impact_col <- switch(row$impact,"High"="#E74C3C","Moderate"="#E67E22","Low"="#1ABC9C","#1ABC9C")
    rgba_bg    <- switch(row$impact,"High"="192,57,43","Moderate"="211,84,0","Low"="30,132,73","30,132,73")
    tags$div(
      style=paste0("background:#21262D;border:1px solid #30363D;border-left:4px solid ",impact_col,";border-radius:6px;padding:20px;"),
      fluidRow(
        column(8,
               tags$h4(style=paste0("color:",impact_col,";margin-top:0;"),paste0("#",row$id," — ",row$assumption)),
               tags$p(style="color:#E6EDF3;font-size:13px;",tags$b("Category: "),tags$span(style="color:#8B949E;",row$category)),
               tags$p(style="color:#E6EDF3;font-size:13px;",tags$b("Justification: "),tags$br(),tags$span(style="color:#C9D1D9;",row$justification)),
               tags$p(style="color:#E6EDF3;font-size:13px;",tags$b("References: "),
                      tags$span(style="background:rgba(26,188,156,0.15);border:1px solid #1ABC9C;color:#1ABC9C;border-radius:4px;padding:2px 8px;font-size:12px;",row$reference))
        ),
        column(4,
               tags$div(
                 style=paste0("text-align:center;background:rgba(",rgba_bg,",0.1);border:1px solid ",impact_col,";border-radius:8px;padding:20px;"),
                 tags$div(style=paste0("font-size:14px;font-weight:700;color:",impact_col,";text-transform:uppercase;"),paste("Impact:",row$impact)),
                 tags$br(),
                 if(!is.na(row$dsa_param))
                   tags$div(style="color:#8B949E;font-size:12px;","DSA parameter:",tags$br(),tags$span(style="color:#1ABC9C;font-family:monospace;",row$dsa_param))
                 else
                   tags$div(style="color:#566573;font-size:12px;","No DSA parameter")
               )
        )
      )
    )
  })
  
  observeEvent(input$goto_dsa, { session$sendCustomMessage("switchTabJS","sensitivity") })
  
  observeEvent(input$export_assumptions, {
    w <- spinner_waiter("Exporting to Word..."); w$show(); on.exit(w$hide())
    tryCatch({
      filter_val <- if(input$assumption_filter=="All") NULL else input$assumption_filter
      path <- export_assumptions_word(filtered_assumptions(),filter_impact=filter_val)
      showNotification(paste("Exported:",basename(path)),type="message",duration=5)
    }, error=function(e) showNotification(paste("Export failed:",e$message),type="error"))
  })
  
  # --------------------------------------------------------------------------
  # KEY FIX: suspendWhenHidden=FALSE — MUST be AFTER all outputs are defined
  # --------------------------------------------------------------------------
  all_outputs <- c(
    "plt_ce_plane","plt_ceac","plt_ce_plane_dash","plt_ceac_dash",
    "plt_trace","tbl_basecase",
    "plt_tornado","tbl_dsa",
    "tbl_scenarios",
    "tbl_trials","tbl_evidence_summary",
    "tbl_pubmed_psych","tbl_pubmed_tech","tbl_who","tbl_eurostat",
    "vbox_n_trials","vbox_n_rct","vbox_n_enrolled","vbox_n_pubmed",
    "plt_gof","plt_tp_compare","tbl_tp_cal",
    "vbox_ipd_patients","vbox_ipd_trans","vbox_ipd_rmse","vbox_ipd_mae",
    "tbl_causal","plt_ps_dist","plt_evppi",
    "tbl_advanced","plt_its","plt_rdd",
    "plt_survival","plt_cif","plt_nma","tbl_nma","tbl_multistate",
    "plt_evsi","plt_bia","plt_mcda","tbl_mcda",
    "plt_threshold","tbl_subgroup","tbl_equity",
    "tbl_assumptions","assumption_detail"
  )
  for (oid in all_outputs) {
    local({ o<-oid; outputOptions(output,o,suspendWhenHidden=FALSE) })
  }
  
} # end server