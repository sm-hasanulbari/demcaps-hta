# =============================================================================
# DEM-CAPS Health-Economic Model
# File: app/ui.R
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)

ui <- dashboardPage(
  skin = "black",
  
  # --------------------------------------------------------------------------
  # HEADER
  # --------------------------------------------------------------------------
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/fluency/24/brain.png"),
      " DEM-CAPS HTA"
    ),
    titleWidth = 220
  ),
  
  # --------------------------------------------------------------------------
  # SIDEBAR
  # --------------------------------------------------------------------------
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",      tabName = "tab_overview",
               icon = icon("house")),
      menuItem("Model Inputs",  tabName = "tab_inputs",
               icon = icon("sliders")),
      menuItem("Base-Case",     tabName = "tab_basecase",
               icon = icon("table")),
      menuItem("PSA",           tabName = "tab_psa",
               icon = icon("chart-line")),
      menuItem("Sensitivity",   tabName = "tab_sensitivity",
               icon = icon("tornado")),
      menuItem("Scenarios",     tabName = "tab_scenarios",
               icon = icon("layer-group")),
      menuItem("Disease Trace", tabName = "tab_trace",
               icon = icon("chart-area")),
      menuItem("Evidence",      tabName = "tab_evidence",
               icon = icon("database")),
      menuItem("References",    tabName = "tab_refs",
               icon = icon("book"))
    ),
    tags$hr(),
    tags$div(
      style = "padding: 10px; font-size: 11px; color: #AAB7B8;",
      tags$b("DEM-CAPS Consortium"),
      tags$br(), "Maastricht University",
      tags$br(), "Model v1.0 | EUR 2024"
    )
  ),
  
  # --------------------------------------------------------------------------
  # BODY
  # --------------------------------------------------------------------------
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo { background-color: #1C2833; }
      .skin-black .main-header .navbar { background-color: #1C2833; }
      .skin-black .main-sidebar { background-color: #212F3D; }
      .content-wrapper { background-color: #F4F6F7; }
      .value-box .inner h3 { font-size: 22px; }
      .nav-tabs-custom { margin-bottom: 0px; }
      .ref-badge {
        background: #1ABC9C; color: white;
        border-radius: 3px; padding: 1px 5px;
        font-size: 10px; font-weight: bold;
        cursor: help; margin-left: 3px;
      }
    "))),
    
    tabItems(
      
      # -----------------------------------------------------------------------
      # TAB 1: OVERVIEW
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_overview",
              fluidRow(
                box(width = 12, status = "primary",
                    solidHeader = TRUE,
                    title = "DEM-CAPS Health-Economic Model",
                    tags$div(
                      tags$h4("About this model"),
                      tags$p("The DEM-CAPS model is a Markov state-transition
                health-economic model evaluating psychosocial and
                technology-based dementia support interventions
                against Standard of Care (SoC) over a 20-year
                lifetime horizon."),
                      tags$h4("Health States"),
                      tags$p("Five mutually exclusive states based on MMSE
                severity classification",
                             tags$sup(tags$span(class = "ref-badge",
                                                title = "Winblad et al. J Intern Med. 2004", "1")),
                             ":"),
                      tags$ul(
                        tags$li("MCI — Mild Cognitive Impairment (MMSE 24-30)"),
                        tags$li("Mild AD — (MMSE 20-23)"),
                        tags$li("Moderate AD — (MMSE 10-19)"),
                        tags$li("Severe AD — (MMSE 0-9)"),
                        tags$li("Death — absorbing state")
                      ),
                      tags$h4("Interventions"),
                      tags$ul(
                        tags$li(tags$b("Psychosocial:"),
                                " RR = 0.82 (Olazaran et al.)",
                                tags$sup(tags$span(class = "ref-badge",
                                                   title = "Olazaran et al. 2010", "7"))),
                        tags$li(tags$b("Technology:"),
                                " RR = 0.88 (Imbeault et al.)",
                                tags$sup(tags$span(class = "ref-badge",
                                                   title = "Imbeault et al. 2014", "9"))),
                        tags$li(tags$b("Combination:"),
                                " RR = 0.82 x 0.88 (additive assumption)")
                      ),
                      tags$h4("Perspective & Discounting"),
                      tags$p("Societal perspective (medical + informal care costs).
                Discount rates: costs 4%, effects 1.5% per Dutch
                pharmacoeconomic guidelines",
                             tags$sup(tags$span(class = "ref-badge",
                                                title = "ZIN Kostenhandleiding 2024", "3")), ".")
                    )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 2: MODEL INPUTS
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_inputs",
              fluidRow(
                box(width = 4, title = "Starting Cohort Distribution",
                    status = "primary", solidHeader = TRUE,
                    sliderInput("i_mci",      "MCI (%)",
                                min = 0, max = 100, value = 25, step = 1),
                    sliderInput("i_mild",     "Mild AD (%)",
                                min = 0, max = 100, value = 45, step = 1),
                    sliderInput("i_moderate", "Moderate AD (%)",
                                min = 0, max = 100, value = 20, step = 1),
                    sliderInput("i_severe",   "Severe AD (%)",
                                min = 0, max = 100, value = 10, step = 1),
                    uiOutput("dist_check_msg")
                ),
                box(width = 4, title = "Intervention Parameters",
                    status = "primary", solidHeader = TRUE,
                    sliderInput("i_rr_psych",
                                "RR Progression - Psychosocial",
                                min = 0.5, max = 1.2,
                                value = 0.82, step = 0.01),
                    sliderInput("i_rr_tech",
                                "RR Progression - Technology",
                                min = 0.5, max = 1.2,
                                value = 0.88, step = 0.01),
                    sliderInput("i_u_psych",
                                "Utility Gain - Psychosocial",
                                min = 0, max = 0.2,
                                value = 0.04, step = 0.01),
                    sliderInput("i_u_tech",
                                "Utility Gain - Technology",
                                min = 0, max = 0.2,
                                value = 0.02, step = 0.01)
                ),
                box(width = 4, title = "Model Settings",
                    status = "primary", solidHeader = TRUE,
                    sliderInput("i_horizon",
                                "Time Horizon (years)",
                                min = 5, max = 30,
                                value = 20, step = 1),
                    sliderInput("i_disc_c",
                                "Discount Rate - Costs (%)",
                                min = 0, max = 10,
                                value = 4, step = 0.5),
                    sliderInput("i_disc_e",
                                "Discount Rate - Effects (%)",
                                min = 0, max = 10,
                                value = 1.5, step = 0.5),
                    sliderInput("i_n_sim",
                                "PSA Simulations",
                                min = 500, max = 5000,
                                value = 1000, step = 500),
                    tags$br(),
                    actionButton("run_model", "Run Base-Case",
                                 class = "btn-success btn-block",
                                 icon  = icon("play"),
                                 style = "font-weight: bold;")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 3: BASE-CASE
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_basecase",
              fluidRow(
                valueBoxOutput("vbox_icer_psych", width = 3),
                valueBoxOutput("vbox_icer_tech",  width = 3),
                valueBoxOutput("vbox_icer_combo", width = 3),
                valueBoxOutput("vbox_nmb_psych",  width = 3)
              ),
              fluidRow(
                box(width = 12, title = "Base-Case Results Table",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(DTOutput("tbl_basecase"), color = "#1ABC9C")
                )
              ),
              fluidRow(
                box(width = 6, title = "Total Costs by Strategy",
                    status = "info", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_costs"), color = "#1ABC9C")
                ),
                box(width = 6, title = "Net Monetary Benefit",
                    status = "info", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_nmb"), color = "#1ABC9C")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 4: PSA
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_psa",
              fluidRow(
                box(width = 12, status = "warning",
                    tags$p(icon("triangle-exclamation"),
                           tags$b(" Note:"),
                           " PSA with 1000+ simulations may take 30-60 seconds."),
                    actionButton("run_psa_btn", "Run PSA",
                                 class = "btn-warning btn-lg",
                                 icon  = icon("play"))
                )
              ),
              fluidRow(
                box(width = 6, title = "Cost-Effectiveness Plane",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_ce_plane"),
                                color = "#1ABC9C")
                ),
                box(width = 6, title = "CEAC",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_ceac"),
                                color = "#1ABC9C")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 5: SENSITIVITY
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_sensitivity",
              fluidRow(
                box(width = 12,
                    actionButton("run_dsa_btn", "Run DSA",
                                 class = "btn-info btn-lg",
                                 icon  = icon("play"))
                )
              ),
              fluidRow(
                box(width = 12, title = "Tornado Diagram",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_tornado",
                                             height = "500px"),
                                color = "#1ABC9C")
                )
              ),
              fluidRow(
                box(width = 12, title = "DSA Results Table",
                    status = "info", solidHeader = TRUE,
                    withSpinner(DTOutput("tbl_dsa"), color = "#1ABC9C")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 6: SCENARIOS
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_scenarios",
              fluidRow(
                box(width = 12,
                    actionButton("run_scenarios_btn",
                                 "Run Scenario Analysis",
                                 class = "btn-info btn-lg",
                                 icon  = icon("play"))
                )
              ),
              fluidRow(
                box(width = 12, title = "Scenario Results",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(DTOutput("tbl_scenarios"),
                                color = "#1ABC9C")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 7: DISEASE TRACE
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_trace",
              fluidRow(
                box(width = 12, status = "primary",
                    selectInput("trace_arm", "Select Strategy:",
                                choices  = c("SoC", "Psychosocial",
                                             "Technology", "Combination"),
                                selected = "SoC")
                )
              ),
              fluidRow(
                box(width = 12, title = "State Occupancy Over Time",
                    status = "primary", solidHeader = TRUE,
                    withSpinner(plotlyOutput("plt_trace",
                                             height = "450px"),
                                color = "#1ABC9C")
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 8: EVIDENCE
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_evidence",
              fluidRow(
                box(width = 12, status = "primary",
                    tags$p(
                      icon("circle-info"),
                      tags$b(" Live data from ClinicalTrials.gov,
                      PubMed, WHO and Eurostat."),
                      " Click Refresh to pull latest evidence.
                Cached data used by default."
                    ),
                    actionButton("run_evidence_btn",
                                 "Refresh Evidence",
                                 class = "btn-primary btn-lg",
                                 icon  = icon("rotate")),
                    tags$span(style = "margin-left:15px;",
                              uiOutput("evidence_last_updated"))
                )
              ),
              fluidRow(
                valueBoxOutput("vbox_n_trials",   width = 3),
                valueBoxOutput("vbox_n_rct",      width = 3),
                valueBoxOutput("vbox_n_enrolled", width = 3),
                valueBoxOutput("vbox_n_pubmed",   width = 3)
              ),
              fluidRow(
                tabBox(width = 12, id = "evidence_tabs",
                       tabPanel("Clinical Trials",
                                icon = icon("flask"),
                                fluidRow(
                                  column(4,
                                         selectInput("ev_category",
                                                     "Filter by Category:",
                                                     choices = c("All",
                                                                 "Psychosocial interventions",
                                                                 "Technology interventions",
                                                                 "Combination interventions",
                                                                 "Caregiver support",
                                                                 "MCI interventions"),
                                                     selected = "All")
                                  ),
                                  column(4,
                                         selectInput("ev_status",
                                                     "Filter by Status:",
                                                     choices  = c("All", "COMPLETED",
                                                                  "ACTIVE_NOT_RECRUITING"),
                                                     selected = "All")
                                  ),
                                  column(4,
                                         selectInput("ev_phase",
                                                     "Filter by Phase:",
                                                     choices  = c("All", "Phase 2",
                                                                  "Phase 3", "Phase 4"),
                                                     selected = "All")
                                  )
                                ),
                                withSpinner(DTOutput("tbl_trials"),
                                            color = "#1ABC9C")
                       ),
                       tabPanel("Evidence Summary",
                                icon = icon("chart-bar"),
                                withSpinner(DTOutput("tbl_evidence_summary"),
                                            color = "#1ABC9C")
                       ),
                       tabPanel("PubMed Articles",
                                icon = icon("book-open"),
                                fluidRow(
                                  column(6,
                                         withSpinner(DTOutput("tbl_pubmed_psych"),
                                                     color = "#1ABC9C")
                                  ),
                                  column(6,
                                         withSpinner(DTOutput("tbl_pubmed_tech"),
                                                     color = "#1ABC9C")
                                  )
                                )
                       ),
                       tabPanel("WHO & Eurostat",
                                icon = icon("globe"),
                                fluidRow(
                                  box(width = 6,
                                      title = "WHO Dementia Prevalence",
                                      status = "primary", solidHeader = TRUE,
                                      withSpinner(DTOutput("tbl_who"),
                                                  color = "#1ABC9C")
                                  ),
                                  box(width = 6,
                                      title = "Eurostat Population (NL, 65+)",
                                      status = "info", solidHeader = TRUE,
                                      withSpinner(DTOutput("tbl_eurostat"),
                                                  color = "#1ABC9C")
                                  )
                                )
                       )
                )
              )
      ),
      
      # -----------------------------------------------------------------------
      # TAB 9: REFERENCES
      # -----------------------------------------------------------------------
      tabItem(tabName = "tab_refs",
              fluidRow(
                box(width = 12, title = "Bibliography (Vancouver)",
                    status = "primary", solidHeader = TRUE,
                    tags$ol(
                      tags$li("Winblad B et al. J Intern Med. 2004;256(3):240-246."),
                      tags$li("IPECAD. Guidelines for HE modelling in AD. 2022."),
                      tags$li("Zorginstituut Nederland. Kostenhandleiding 2024."),
                      tags$li("Hofman A et al. Neuroepidemiology. 1991;10(5-6):286-295."),
                      tags$li("Handels RLH et al. Alzheimers Dement. 2017;13(6):627-638."),
                      tags$li("Wimo A et al. J Alzheimers Dis. 2017;57(3):793-803."),
                      tags$li("Olazaran J et al. Dement Geriatr Cogn Disord. 2010;30(2):161-178."),
                      tags$li("Brodaty H, Arasaratnam C. JAMA. 2012;308(9):911-922."),
                      tags$li("Imbeault H et al. Neuropsychol Rehabil. 2014;24(3-4):522-538."),
                      tags$li("Klimova B et al. Curr Alzheimer Res. 2018;15(10):975-983."),
                      tags$li("Pinquart M, Sorensen S. Psychol Aging. 2006;21(2):395-410."),
                      tags$li("Krol M et al. Value Health. 2016;19(6):713-723."),
                      tags$li("Livingston G et al. Lancet. 2020;396(10248):413-446."),
                      tags$li("Alzheimer Nederland. Economische impact 2023."),
                      tags$li("Briggs AH et al. Decision Modelling for HEE. OUP; 2006."),
                      tags$li("Drummond MF et al. Methods for Economic Evaluation. OUP; 2015.")
                    )
                )
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage