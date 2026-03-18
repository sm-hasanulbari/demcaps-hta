# =============================================================================
# DEM-CAPS Health-Economic Model — app/ui.R
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(
      tags$img(src="https://img.icons8.com/fluency/24/brain.png",
               style="margin-right:6px;vertical-align:middle;"),
      tags$b("DEM-CAPS"), " HTA"
    ),
    titleWidth = 300,
    tags$li(class="dropdown",
            tags$span(
              style="padding:15px;color:#1ABC9C;font-size:12px;font-weight:600;",
              icon("circle", style="color:#1ABC9C;font-size:8px;"),
              " v1.0 | Costs: EUR 2025 | Model: 2026"
            )
    )
  ),
  
  dashboardSidebar(
    width = 300,
    tags$style(HTML("
      .sidebar-section { padding:8px 15px 4px 15px; font-size:10px; font-weight:700; letter-spacing:1.5px; color:#1ABC9C; text-transform:uppercase; border-top:1px solid #21262D; margin-top:6px; }
      .skin-black .main-sidebar { background-color:#0D1117; }
      .irs-grid-text { color:#8B949E !important; font-size:10px !important; }
      .irs-min, .irs-max { color:#8B949E !important; background:transparent !important; font-size:10px !important; }
      .irs-single { background:#1ABC9C !important; color:white !important; font-size:11px !important; }
      .irs-bar, .irs-bar-edge { background:#1ABC9C !important; border-color:#1ABC9C !important; }
      .irs-line { background:#30363D !important; }
      .irs-handle { background:#1ABC9C !important; border-color:#1ABC9C !important; }
      .sidebar .shiny-input-container { margin-bottom:2px; }
      .sidebar label { color:#E6EDF3 !important; font-size:12px !important; }
      .status-bar { padding:6px 15px; background:#0D1117; font-size:11px; color:#8B949E; border-bottom:1px solid #21262D; }
    ")),
    tags$div(
      style="padding:12px 15px 8px 15px;background:#0D1117;",
      actionButton("run_model","Run Model",
                   style="width:100%;background:#1ABC9C;color:white;border:none;border-radius:6px;padding:10px;font-size:14px;font-weight:700;margin-bottom:8px;"),
      actionButton("run_all_btn","Run Full Analysis",
                   style="width:100%;background:#21262D;color:#F39C12;border:1px solid #F39C12;border-radius:6px;padding:8px;font-size:12px;font-weight:700;margin-bottom:8px;"),
      actionButton("reset_model","Reset Defaults",
                   style="width:100%;background:transparent;color:#8B949E;border:1px solid #30363D;border-radius:6px;padding:8px;font-size:12px;")
    ),
    tags$div(class="status-bar", uiOutput("model_status")),
    tags$div(class="sidebar-section","Cohort"),
    tags$div(style="padding:0 10px;",
             sliderInput("i_mci","MCI (%)",0,100,25,1,width="100%"),
             sliderInput("i_mild","Mild AD (%)",0,100,45,1,width="100%"),
             sliderInput("i_moderate","Moderate (%)",0,100,20,1,width="100%"),
             sliderInput("i_severe","Severe (%)",0,100,10,1,width="100%"),
             uiOutput("dist_check_msg")
    ),
    tags$div(class="sidebar-section","Interventions"),
    tags$div(style="padding:0 10px;",
             sliderInput("i_rr_psych","RR — Psychosocial",0.5,1.2,0.82,0.01,width="100%"),
             sliderInput("i_rr_tech","RR — Technology",0.5,1.2,0.88,0.01,width="100%"),
             sliderInput("i_u_psych","Utility gain — Psych",0,0.2,0.04,0.01,width="100%"),
             sliderInput("i_u_tech","Utility gain — Tech",0,0.2,0.02,0.01,width="100%")
    ),
    tags$div(class="sidebar-section","Model Settings"),
    tags$div(style="padding:0 10px;",
             sliderInput("i_horizon","Time horizon (years)",5,30,20,1,width="100%"),
             sliderInput("i_disc_c","Discount — costs (%)",0,10,4,0.5,width="100%"),
             sliderInput("i_disc_e","Discount — effects (%)",0,10,1.5,0.5,width="100%"),
             sliderInput("i_n_sim","PSA simulations",500,5000,500,500,width="100%")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo { background-color:#0D1117; border-right:1px solid #1ABC9C; color:#E6EDF3 !important; }
      .skin-black .main-header .navbar { background-color:#0D1117; }
      .content-wrapper { background-color:#0D1117; }
      .box { border-radius:8px; background:#161B22 !important; border:1px solid #30363D; border-top:3px solid #1ABC9C; box-shadow:0 2px 8px rgba(0,0,0,0.3); }
      .box.box-primary { border-top-color:#1ABC9C; } .box.box-info { border-top-color:#3498DB; }
      .box.box-warning { border-top-color:#F39C12; } .box.box-success { border-top-color:#27AE60; }
      .box-header { background:#161B22 !important; border-bottom:1px solid #21262D; }
      .box-header .box-title { color:#E6EDF3 !important; font-weight:600; font-size:14px; }
      .box-body { background:#161B22 !important; color:#E6EDF3 !important; }
      .value-box { border-radius:8px; }
      .value-box .inner h3 { font-size:24px; font-weight:700; }
      .value-box .inner p  { font-size:12px; font-weight:600; }
      .nav-tabs-custom { background:#161B22 !important; border-color:#30363D !important; }
      .nav-tabs-custom > .nav-tabs { border-bottom-color:#30363D !important; background:#161B22 !important; }
      .nav-tabs-custom > .nav-tabs > li > a { color:#8B949E !important; background:#21262D !important; border-color:#30363D !important; }
      .nav-tabs-custom > .nav-tabs > li.active > a { background:#1ABC9C !important; color:white !important; border:none !important; }
      .nav-tabs-custom > .tab-content { background:#161B22 !important; border:none !important; }
      .nav-tabs-custom > .tab-content > .tab-pane { background:#161B22 !important; color:#E6EDF3 !important; }
      .tab-content { background:#161B22 !important; } .tab-pane { background:#161B22 !important; }
      .dataTables_wrapper { background:#161B22 !important; color:#E6EDF3 !important; }
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { background:#161B22 !important; color:#E6EDF3 !important; }
      .dataTables_wrapper .dataTables_length label, .dataTables_wrapper .dataTables_filter label,
      .dataTables_wrapper .dataTables_info { color:#E6EDF3 !important; }
      .dataTables_wrapper input[type=search], .dataTables_wrapper select { background:#21262D !important; color:#E6EDF3 !important; border:1px solid #30363D !important; }
      table.dataTable thead th { background:#21262D !important; color:#1ABC9C !important; border-bottom:2px solid #1ABC9C !important; font-size:13px; }
      table.dataTable thead td { background:#21262D !important; }
      table.dataTable tbody tr { background:#161B22 !important; color:#E6EDF3 !important; }
      table.dataTable tbody tr.odd { background:#161B22 !important; }
      table.dataTable tbody tr.even { background:#1a2030 !important; }
      table.dataTable tbody tr:hover { background:#21262D !important; color:#1ABC9C !important; }
      table.dataTable tbody td { border-color:#21262D !important; }
      .paginate_button { background:#21262D !important; color:#E6EDF3 !important; border:1px solid #30363D !important; border-radius:4px !important; }
      .paginate_button:hover { background:#1ABC9C !important; color:white !important; border-color:#1ABC9C !important; }
      .paginate_button.current, .paginate_button.current:hover { background:#1ABC9C !important; color:white !important; border-color:#1ABC9C !important; }
      .paginate_button.disabled { background:#0D1117 !important; color:#566573 !important; }
      p,li,label,.shiny-text-output { color:#E6EDF3; }
      h1,h2,h3,h4,h5 { color:#E6EDF3; }
      select { background:#21262D !important; color:#E6EDF3 !important; border:1px solid #30363D !important; }
      .selectize-input { background:#21262D !important; color:#E6EDF3 !important; border-color:#30363D !important; }
      .selectize-dropdown { background:#21262D !important; color:#E6EDF3 !important; border-color:#30363D !important; }
      .selectize-dropdown .option:hover { background:#1ABC9C !important; color:white !important; }
      .well { background:#21262D !important; border-color:#30363D !important; }
      .method-badge { display:inline-block; background:rgba(26,188,156,0.15); border:1px solid #1ABC9C; color:#1ABC9C; border-radius:4px; padding:3px 10px; font-size:11px; font-weight:700; margin:2px; }
      .diagram-box { background:#161B22; border:1px solid #30363D; border-radius:8px; padding:20px; margin-bottom:15px; }
      .diagram-box h4 { color:#1ABC9C; font-size:13px; font-weight:700; text-transform:uppercase; letter-spacing:1px; margin:0 0 15px 0; padding-bottom:8px; border-bottom:1px solid #21262D; }
      .profile-strip { background:linear-gradient(90deg,#0D1117 0%,#161B22 40%,#0D1117 100%); border-bottom:1px solid #21262D; padding:8px 20px; display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:8px; }
      .profile-strip-name { color:#E6EDF3; font-weight:700; font-size:13px; }
      .profile-strip-creds { color:#8B949E; font-size:11px; }
      .profile-strip-tag { display:inline-block; background:rgba(52,152,219,0.12); border:1px solid #3498DB; color:#3498DB; border-radius:3px; padding:1px 6px; font-size:10px; font-weight:600; margin:1px; }
      .profile-strip-btn { display:inline-flex; align-items:center; padding:4px 10px; border-radius:4px; font-size:11px; font-weight:700; text-decoration:none; margin-left:4px; }
      .tab-nav-wrapper { background:#0D1117; padding:10px 15px 0 15px; border-bottom:2px solid #21262D; margin-bottom:0; }
      .tab-row { display:grid; grid-template-columns:repeat(6,1fr); gap:4px; margin-bottom:4px; }
      .htab-btn { background:#161B22; border:1px solid #30363D; border-bottom:3px solid transparent; border-radius:6px 6px 0 0; color:#8B949E; padding:8px 4px; font-size:11px; font-weight:600; cursor:pointer; text-align:center; transition:all 0.15s; width:100%; line-height:1.4; }
      .htab-btn:hover { background:rgba(26,188,156,0.08); color:#1ABC9C; border-color:#1ABC9C; border-bottom-color:#1ABC9C; }
      .htab-btn.active { background:rgba(26,188,156,0.15); color:#1ABC9C; border-color:#1ABC9C; border-bottom-color:#1ABC9C; }
      .badge-live { display:inline-block; background:#27AE60; color:white; border-radius:8px; padding:1px 4px; font-size:9px; font-weight:700; margin-left:2px; }
      .badge-red  { display:inline-block; background:#E74C3C; color:white; border-radius:8px; padding:1px 4px; font-size:9px; font-weight:700; margin-left:2px; }
      .tab-panel { display:none; } .tab-panel.active-panel { display:block; }
      .tab-content-area { padding-top:20px; }
      .how-to-step { display:flex; align-items:flex-start; gap:14px; padding:10px 0; border-bottom:1px solid #21262D; }
      .how-to-step:last-child { border-bottom:none; }
      .step-num { width:28px; height:28px; border-radius:50%; background:#1ABC9C; color:white; font-weight:700; font-size:13px; display:flex; align-items:center; justify-content:center; flex-shrink:0; margin-top:2px; }
      .step-body { flex:1; }
      .step-title { color:#E6EDF3; font-weight:700; font-size:13px; margin-bottom:3px; }
      .step-desc { color:#8B949E; font-size:12px; line-height:1.6; }
      .step-tag { display:inline-block; background:rgba(26,188,156,0.12); border:1px solid #1ABC9C; color:#1ABC9C; border-radius:3px; padding:1px 6px; font-size:10px; font-weight:600; margin:1px; }
      .shiny-plot-output { background:#161B22 !important; overflow:hidden; }
      ::-webkit-scrollbar { width:5px; height:5px; }
      ::-webkit-scrollbar-track { background:#0D1117; }
      ::-webkit-scrollbar-thumb { background:#30363D; border-radius:3px; }
      ::-webkit-scrollbar-thumb:hover { background:#1ABC9C; }
    "))),
    
    tags$script(HTML("
      function switchTab(name) {
        document.querySelectorAll('.tab-panel').forEach(function(p){ p.classList.remove('active-panel'); });
        document.querySelectorAll('.htab-btn').forEach(function(b){ b.classList.remove('active'); });
        var panel = document.getElementById('panel_' + name);
        if (panel) panel.classList.add('active-panel');
        var btn = document.getElementById('htab_' + name);
        if (btn) btn.classList.add('active');
        Shiny.setInputValue('active_tab', name, {priority:'event'});
        // Force DataTable redraw in newly visible panel after DOM settles
        setTimeout(function() {
          if (panel) {
            $(panel).find('table.dataTable').each(function() {
              if ($.fn.DataTable.isDataTable(this)) {
                $(this).DataTable().columns.adjust().draw(false);
              }
            });
          }
        }, 200);
      }
      document.addEventListener('DOMContentLoaded', function(){ switchTab('overview'); });
      Shiny.addCustomMessageHandler('switchTabJS', function(name){ switchTab(name); });
      // Force a specific DT to redraw (used for assumptions tab)
      Shiny.addCustomMessageHandler('redrawDT', function(id) {
        // Try multiple times with increasing delays
        [300, 600, 1000].forEach(function(delay) {
          setTimeout(function() {
            var container = document.getElementById(id);
            if (container) {
              var tbls = $(container).find('table');
              tbls.each(function() {
                if ($.fn.DataTable && $.fn.DataTable.isDataTable(this)) {
                  $(this).DataTable().columns.adjust().draw(false);
                }
              });
            }
          }, delay);
        });
      });
    ")),
    
    # ── PERSISTENT PROFILE STRIP ──────────────
    tags$div(class="profile-strip",
             tags$div(style="display:flex;align-items:center;gap:14px;",
                      tags$div(style="width:38px;height:38px;border-radius:50%;background:linear-gradient(135deg,#1ABC9C 0%,#3498DB 100%);display:flex;align-items:center;justify-content:center;font-size:13px;font-weight:700;color:white;border:2px solid #1ABC9C;flex-shrink:0;","SH"),
                      tags$div(
                        tags$div(class="profile-strip-name","Sm Hasan ul Bari"),
                        tags$div(class="profile-strip-creds","MBBS · MSc Health Economics & Decision Modelling (Merit) · MSc Advanced Biostatistics & Epidemiology (Distinction) · Erasmus Mundus Scholar · WHO Research Fellow · Health Economist · NICE · LSHTM")
                      )
             ),
             tags$div(style="display:flex;align-items:center;flex-wrap:wrap;gap:4px;",
                      tags$span(class="profile-strip-tag","HTA"),
                      tags$span(class="profile-strip-tag","Pharmacoeconomics"),
                      tags$span(class="profile-strip-tag","Market Access"),
                      tags$span(class="profile-strip-tag","HEOR"),
                      tags$span(class="profile-strip-tag","NICE · LSHTM"),
                      tags$a(href="https://www.linkedin.com/in/hasanbari/",target="_blank",class="profile-strip-btn",style="background:#0A66C2;color:white;",icon("linkedin",style="margin-right:4px;"),"LinkedIn"),
                      tags$a(href="https://orcid.org/0000-0002-5209-2029",target="_blank",class="profile-strip-btn",style="background:#A6CE39;color:white;",icon("id-badge",style="margin-right:4px;"),"ORCID"),
                      tags$a(href="https://github.com/sm-hasanulbari/demcaps-hta",target="_blank",class="profile-strip-btn",style="background:#21262D;color:#E6EDF3;border:1px solid #30363D;",icon("github",style="margin-right:4px;"),"GitHub")
             )
    ),
    
    # ── TAB NAVIGATION ────────────────────────
    tags$div(class="tab-nav-wrapper",
             tags$div(class="tab-row",
                      tags$button(id="htab_overview",    class="htab-btn active", onclick="switchTab('overview')",    "Dashboard"),
                      tags$button(id="htab_psa",         class="htab-btn",        onclick="switchTab('psa')",         "PSA"),
                      tags$button(id="htab_sensitivity", class="htab-btn",        onclick="switchTab('sensitivity')", "Sensitivity"),
                      tags$button(id="htab_scenarios",   class="htab-btn",        onclick="switchTab('scenarios')",   "Scenarios"),
                      tags$button(id="htab_evidence",    class="htab-btn",        onclick="switchTab('evidence')",    "Evidence",tags$span(class="badge-live","Live")),
                      tags$button(id="htab_ipd",         class="htab-btn",        onclick="switchTab('ipd')",         "IPD Calib.")
             ),
             tags$div(class="tab-row",
                      tags$button(id="htab_causal",      class="htab-btn", onclick="switchTab('causal')",      "Causal"),
                      tags$button(id="htab_advanced",    class="htab-btn", onclick="switchTab('advanced')",    "Adv. Causal"),
                      tags$button(id="htab_survival",    class="htab-btn", onclick="switchTab('survival')",    "Survival RWE"),
                      tags$button(id="htab_hta",         class="htab-btn", onclick="switchTab('hta')",         "Extended HTA"),
                      tags$button(id="htab_assumptions", class="htab-btn", onclick="switchTab('assumptions')", "Assumptions",tags$span(class="badge-red","32")),
                      tags$button(id="htab_refs",        class="htab-btn", onclick="switchTab('refs')",        "References")
             )
    ),
    
    tags$div(class="tab-content-area",
             
             # ==========================================================================
             # DASHBOARD
             # ==========================================================================
             tags$div(id="panel_overview", class="tab-panel",
                      
                      fluidRow(
                        tags$div(style="margin:0 15px 18px 15px;",
                                 tags$div(style="background:linear-gradient(135deg,#161B22 0%,#21262D 100%);border:1px solid #30363D;border-radius:8px;overflow:hidden;",
                                          fluidRow(
                                            column(7,
                                                   tags$div(style="padding:24px 28px;border-right:1px solid #21262D;",
                                                            tags$div(style="display:inline-block;background:rgba(26,188,156,0.12);border:1px solid #1ABC9C;border-radius:4px;padding:3px 10px;font-size:10px;font-weight:700;color:#1ABC9C;letter-spacing:1.5px;text-transform:uppercase;margin-bottom:12px;","Independent Research Project · 2026"),
                                                            tags$h1(style="color:#E6EDF3;font-weight:700;margin:0 0 8px 0;font-size:22px;line-height:1.3;",icon("brain",style="color:#1ABC9C;margin-right:8px;"),"DEM-CAPS Health-Economic Model"),
                                                            tags$p(style="color:#8B949E;font-size:13px;margin:0 0 14px 0;line-height:1.7;","A Markov state-transition model evaluating psychosocial and technology-based dementia support interventions against Standard of Care. 20-year societal perspective, Netherlands setting, EUR 2025 costs. Secondary data | GDPR compliant."),
                                                            tags$div(lapply(c("Markov","PSA","DSA","TMLE","NMA","EVSI","MCDA","IPD Cal","28 Methods"),function(b) tags$span(class="method-badge",b))),
                                                            tags$div(style="margin-top:14px;",
                                                                     tags$a(href="https://github.com/sm-hasanulbari/demcaps-hta",target="_blank",style="font-size:11px;color:#1ABC9C;text-decoration:none;margin-right:16px;",icon("code-branch",style="margin-right:4px;"),"GitHub Repository"),
                                                                     tags$span(style="font-size:11px;color:#566573;",icon("circle",style="color:#27AE60;font-size:8px;margin-right:4px;"),"Open source · CC BY 4.0"))
                                                   )
                                            ),
                                            column(5,
                                                   tags$div(style="padding:24px;background:linear-gradient(180deg,#0D1117 0%,#161B22 100%);",
                                                            tags$div(style="display:flex;align-items:center;margin-bottom:14px;",
                                                                     tags$div(style="width:52px;height:52px;border-radius:50%;background:linear-gradient(135deg,#1ABC9C 0%,#3498DB 100%);display:flex;align-items:center;justify-content:center;font-size:18px;font-weight:700;color:white;margin-right:14px;flex-shrink:0;border:2px solid #1ABC9C;","SH"),
                                                                     tags$div(
                                                                       tags$div(style="color:#E6EDF3;font-weight:700;font-size:15px;line-height:1.2;","Sm Hasan ul Bari"),
                                                                       tags$div(style="color:#1ABC9C;font-size:10px;margin-top:3px;","MBBS · MSc HE&DM · MSc ABiostat & Epi"),
                                                                       tags$div(style="color:#566573;font-size:10px;margin-top:2px;","Founder, Health Quotient")
                                                                     )
                                                            ),
                                                            tags$div(style="background:#21262D;border-radius:6px;padding:12px 14px;margin-bottom:10px;",
                                                                     tags$div(style="color:#8B949E;font-size:10px;font-weight:700;letter-spacing:1px;text-transform:uppercase;margin-bottom:8px;","Credentials"),
                                                                     tags$div(style="color:#C9D1D9;font-size:11px;line-height:1.9;",
                                                                              tags$div(icon("graduation-cap",style="color:#1ABC9C;margin-right:6px;"),"MSc Health Economics & Decision Modelling",tags$span(style="color:#566573;font-size:10px;"," Merit")),
                                                                              tags$div(icon("graduation-cap",style="color:#3498DB;margin-right:6px;"),"MSc Advanced Biostatistics & Epidemiology",tags$span(style="color:#566573;font-size:10px;"," Distinction")),
                                                                              tags$div(icon("award",style="color:#F39C12;margin-right:6px;"),"Erasmus Mundus Scholar"),
                                                                              tags$div(icon("building-columns",style="color:#8E44AD;margin-right:6px;"),"WHO Implementation Research Fellow"),
                                                                              tags$div(icon("briefcase",style="color:#E74C3C;margin-right:6px;"),"Health Economist · NICE · LSHTM")
                                                                     )
                                                            ),
                                                            tags$div(style="margin-bottom:12px;",
                                                                     tags$div(style="color:#8B949E;font-size:10px;font-weight:700;letter-spacing:1px;text-transform:uppercase;margin-bottom:6px;","Expertise"),
                                                                     tags$div(lapply(c("HTA","Pharmacoeconomics","Market Access","HEOR","Value-Based Pricing","Regulatory Affairs"),
                                                                                     function(t) tags$span(style="display:inline-block;background:rgba(52,152,219,0.12);border:1px solid #3498DB;color:#3498DB;border-radius:3px;padding:2px 7px;font-size:10px;font-weight:600;margin:2px;",t)))
                                                            ),
                                                            tags$div(style="display:flex;gap:6px;",
                                                                     tags$a(href="https://www.linkedin.com/in/hasanbari/",target="_blank",style="display:flex;align-items:center;justify-content:center;background:#0A66C2;color:white;border-radius:5px;padding:7px 10px;font-size:11px;font-weight:700;text-decoration:none;flex:1;",icon("linkedin",style="margin-right:5px;"),"LinkedIn"),
                                                                     tags$a(href="https://orcid.org/0000-0002-5209-2029",target="_blank",style="display:flex;align-items:center;justify-content:center;background:#A6CE39;color:white;border-radius:5px;padding:7px 10px;font-size:11px;font-weight:700;text-decoration:none;flex:1;",icon("id-badge",style="margin-right:5px;"),"ORCID"),
                                                                     tags$a(href="https://github.com/sm-hasanulbari/demcaps-hta",target="_blank",style="display:flex;align-items:center;justify-content:center;background:#21262D;color:#E6EDF3;border:1px solid #30363D;border-radius:5px;padding:7px 10px;font-size:11px;font-weight:700;text-decoration:none;flex:1;",icon("github",style="margin-right:5px;"),"GitHub")
                                                            )
                                                   )
                                            )
                                          )
                                 )
                        )
                      ),
                      
                      # ── HOW TO USE ────────────────────────────
                      fluidRow(
                        tags$div(style="margin:0 15px 18px 15px;",
                                 tags$div(style="background:#161B22;border:1px solid #30363D;border-left:4px solid #3498DB;border-radius:8px;overflow:hidden;",
                                          tags$div(
                                            style="padding:14px 20px;border-bottom:1px solid #21262D;display:flex;align-items:center;justify-content:space-between;cursor:pointer;",
                                            onclick="var c=document.getElementById('how_to_content');c.style.display=c.style.display==='none'?'block':'none';",
                                            tags$div(icon("circle-question",style="color:#3498DB;margin-right:10px;"),
                                                     tags$b(style="color:#E6EDF3;font-size:14px;","Quick Start Guide — How to Use This Model"),
                                                     tags$span(style="color:#8B949E;font-size:12px;margin-left:10px;","(click to expand/collapse)")),
                                            tags$span(style="color:#3498DB;font-size:11px;","New here? Start here")
                                          ),
                                          tags$div(id="how_to_content", style="display:none;padding:16px 20px;",
                                                   fluidRow(
                                                     column(6,
                                                            tags$div(style="color:#1ABC9C;font-size:10px;font-weight:700;letter-spacing:1px;text-transform:uppercase;margin-bottom:10px;","Essential Steps"),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","1"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title","Adjust Cohort & Parameters (Optional)"),
                                                                              tags$div(class="step-desc","Use the left sidebar sliders to configure your cohort proportions, intervention effect sizes (relative risks), and model settings. Defaults reflect published Netherlands dementia data.")
                                                                     )
                                                            ),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","2"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title",icon("play",style="color:#1ABC9C;font-size:10px;margin-right:4px;"),"Click 'Run Model'"),
                                                                              tags$div(class="step-desc","Runs the deterministic base-case Markov model (~1 second). Results appear in the Dashboard: ICER value boxes, base-case results table, and disease trajectory chart.")
                                                                     )
                                                            ),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","3"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title",icon("bolt",style="color:#F39C12;font-size:10px;margin-right:4px;"),"Click 'Run Full Analysis' for everything"),
                                                                              tags$div(class="step-desc","Runs base-case + PSA (500 Monte Carlo simulations). Populates the CE Plane and CEAC on the Dashboard. Then visit individual tabs and press each tab's own Run button for deeper analysis.")
                                                                     )
                                                            ),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","4"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title","Explore Each Analysis Tab"),
                                                                              tags$div(class="step-desc",
                                                                                       "Each tab has its own ",tags$b(style="color:#1ABC9C;","Run button"),". Press it to compute that module:",
                                                                                       tags$div(style="margin-top:6px;",
                                                                                                tags$span(class="step-tag","PSA")," Monte Carlo uncertainty",tags$br(),
                                                                                                tags$span(class="step-tag","Sensitivity")," Tornado / one-way DSA",tags$br(),
                                                                                                tags$span(class="step-tag","Scenarios")," 6 pre-specified scenarios",tags$br(),
                                                                                                tags$span(class="step-tag","Evidence")," Live API data pull",tags$br(),
                                                                                                tags$span(class="step-tag","IPD Calib.")," Calibrate transition probabilities"
                                                                                       )
                                                                              )
                                                                     )
                                                            )
                                                     ),
                                                     column(6,
                                                            tags$div(style="color:#1ABC9C;font-size:10px;font-weight:700;letter-spacing:1px;text-transform:uppercase;margin-bottom:10px;","Advanced Modules"),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","5"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title","Causal Inference Tabs"),
                                                                              tags$div(class="step-desc",
                                                                                       tags$span(class="step-tag","Causal")," PSM, IPW, DiD, G-computation, EVPPI.",tags$br(),
                                                                                       tags$span(class="step-tag","Adv. Causal")," TMLE, IV/2SLS, RDD, ITS, Bayesian Networks.",tags$br(),
                                                                                       tags$span(style="color:#566573;font-size:11px;","Both use synthetic IPD; ~30 seconds each.")
                                                                              )
                                                                     )
                                                            ),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","6"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title","Survival & Extended HTA"),
                                                                              tags$div(class="step-desc",
                                                                                       tags$span(class="step-tag","Survival RWE")," Parametric survival, NMA, competing risks.",tags$br(),
                                                                                       tags$span(class="step-tag","Extended HTA")," EVSI, Budget Impact, MCDA, Subgroup CEA, Equity.",tags$br(),
                                                                                       tags$span(style="color:#F39C12;font-size:11px;","Note: Extended HTA requires Run Model first.")
                                                                              )
                                                                     )
                                                            ),
                                                            tags$div(class="how-to-step",
                                                                     tags$div(class="step-num","7"),
                                                                     tags$div(class="step-body",
                                                                              tags$div(class="step-title","Assumptions & References"),
                                                                              tags$div(class="step-desc",
                                                                                       "The ",tags$b(style="color:#F39C12;","Assumptions")," tab lists 32 model assumptions with RAG ratings. Click any row to view full justification and export to Word.",tags$br(),
                                                                                       "The ",tags$b(style="color:#3498DB;","References")," tab lists the full 32-item bibliography."
                                                                              )
                                                                     )
                                                            ),
                                                            tags$div(style="margin-top:12px;padding:10px 14px;background:#21262D;border-radius:6px;border-left:3px solid #F39C12;",
                                                                     tags$div(style="color:#F39C12;font-size:11px;font-weight:700;margin-bottom:4px;",icon("triangle-exclamation",style="margin-right:4px;"),"Important Notes"),
                                                                     tags$ul(style="color:#8B949E;font-size:11px;margin:0;padding-left:16px;line-height:1.8;",
                                                                             tags$li("This model uses synthetic data — not real patient data"),
                                                                             tags$li("PSA default = 500 sims (~15 sec); increase slider for publication"),
                                                                             tags$li("All costs in EUR 2025; ZIN Netherlands discount rates"),
                                                                             tags$li("WTP thresholds shown: EUR 20k, 50k, 80k per QALY")
                                                                     )
                                                            )
                                                     )
                                                   )
                                          )
                                 )
                        )
                      ),
                      
                      fluidRow(
                        column(6,tags$div(class="diagram-box",
                                          tags$h4(icon("circle-nodes",style="margin-right:6px;"),"Markov State-Transition Model"),
                                          HTML('<svg viewBox="0 0 600 165" xmlns="http://www.w3.org/2000/svg" style="width:100%;"><rect width="600" height="165" fill="#161B22" rx="6"/><rect x="8" y="52" width="86" height="38" rx="5" fill="#1ABC9C" fill-opacity="0.18" stroke="#1ABC9C" stroke-width="1.5"/><text x="51" y="75" text-anchor="middle" fill="#1ABC9C" font-size="11" font-weight="700">MCI</text><rect x="120" y="52" width="86" height="38" rx="5" fill="#3498DB" fill-opacity="0.18" stroke="#3498DB" stroke-width="1.5"/><text x="163" y="75" text-anchor="middle" fill="#3498DB" font-size="11" font-weight="700">Mild AD</text><rect x="232" y="52" width="100" height="38" rx="5" fill="#F39C12" fill-opacity="0.18" stroke="#F39C12" stroke-width="1.5"/><text x="282" y="75" text-anchor="middle" fill="#F39C12" font-size="11" font-weight="700">Moderate AD</text><rect x="358" y="52" width="86" height="38" rx="5" fill="#E74C3C" fill-opacity="0.18" stroke="#E74C3C" stroke-width="1.5"/><text x="401" y="75" text-anchor="middle" fill="#E74C3C" font-size="11" font-weight="700">Severe AD</text><rect x="470" y="52" width="122" height="38" rx="5" fill="#717D7E" fill-opacity="0.28" stroke="#717D7E" stroke-width="1.5"/><text x="531" y="75" text-anchor="middle" fill="#717D7E" font-size="11" font-weight="700">Death</text><defs><marker id="a1" markerWidth="7" markerHeight="7" refX="5" refY="3" orient="auto"><path d="M0,0 L0,6 L7,3 z" fill="#1ABC9C"/></marker><marker id="a2" markerWidth="7" markerHeight="7" refX="5" refY="3" orient="auto"><path d="M0,0 L0,6 L7,3 z" fill="#717D7E"/></marker></defs><line x1="94" y1="71" x2="118" y2="71" stroke="#1ABC9C" stroke-width="1.5" marker-end="url(#a1)"/><line x1="206" y1="71" x2="230" y2="71" stroke="#3498DB" stroke-width="1.5" marker-end="url(#a1)"/><line x1="332" y1="71" x2="356" y2="71" stroke="#F39C12" stroke-width="1.5" marker-end="url(#a1)"/><line x1="444" y1="71" x2="468" y2="71" stroke="#E74C3C" stroke-width="1.5" marker-end="url(#a1)"/><line x1="51" y1="90" x2="51" y2="125" stroke="#717D7E" stroke-width="1" stroke-dasharray="3,2"/><line x1="51" y1="125" x2="516" y2="125" stroke="#717D7E" stroke-width="1" stroke-dasharray="3,2"/><line x1="516" y1="125" x2="516" y2="92" stroke="#717D7E" stroke-width="1" stroke-dasharray="3,2" marker-end="url(#a2)"/><path d="M 36,52 Q 16,25 66,52" fill="none" stroke="#1ABC9C" stroke-width="1" stroke-dasharray="2,2"/><path d="M 148,52 Q 128,25 178,52" fill="none" stroke="#3498DB" stroke-width="1" stroke-dasharray="2,2"/><path d="M 258,52 Q 238,25 306,52" fill="none" stroke="#F39C12" stroke-width="1" stroke-dasharray="2,2"/><path d="M 382,52 Q 362,25 418,52" fill="none" stroke="#E74C3C" stroke-width="1" stroke-dasharray="2,2"/><text x="300" y="150" text-anchor="middle" fill="#566573" font-size="10">Dashed = death | Self-loops = staying | Annual cycle</text></svg>')
                        )),
                        column(6,tags$div(class="diagram-box",
                                          tags$h4(icon("code-branch",style="margin-right:6px;"),"Decision Problem"),
                                          HTML('<svg viewBox="0 0 500 195" xmlns="http://www.w3.org/2000/svg" style="width:100%;"><rect width="500" height="195" fill="#161B22" rx="6"/><rect x="18" y="75" width="58" height="38" rx="4" fill="#1ABC9C" fill-opacity="0.18" stroke="#1ABC9C" stroke-width="2"/><text x="47" y="98" text-anchor="middle" fill="#1ABC9C" font-size="10" font-weight="700">Treat?</text><line x1="76" y1="85" x2="140" y2="45" stroke="#1ABC9C" stroke-width="1.5"/><text x="102" y="57" fill="#1ABC9C" font-size="10" font-weight="600">Yes</text><rect x="140" y="28" width="100" height="26" rx="4" fill="#1ABC9C" fill-opacity="0.12" stroke="#1ABC9C" stroke-width="1.5"/><text x="190" y="45" text-anchor="middle" fill="#1ABC9C" font-size="10" font-weight="600">Psychosocial</text><rect x="140" y="66" width="100" height="26" rx="4" fill="#3498DB" fill-opacity="0.12" stroke="#3498DB" stroke-width="1.5"/><text x="190" y="83" text-anchor="middle" fill="#3498DB" font-size="10" font-weight="600">Technology</text><rect x="140" y="104" width="100" height="26" rx="4" fill="#8E44AD" fill-opacity="0.12" stroke="#8E44AD" stroke-width="1.5"/><text x="190" y="121" text-anchor="middle" fill="#8E44AD" font-size="10" font-weight="600">Combination</text><line x1="76" y1="94" x2="140" y2="79" stroke="#3498DB" stroke-width="1"/><line x1="76" y1="103" x2="140" y2="117" stroke="#8E44AD" stroke-width="1"/><line x1="76" y1="111" x2="140" y2="160" stroke="#E74C3C" stroke-width="1.5"/><text x="102" y="154" fill="#E74C3C" font-size="10" font-weight="600">No</text><rect x="140" y="146" width="100" height="26" rx="4" fill="#E74C3C" fill-opacity="0.12" stroke="#E74C3C" stroke-width="1.5"/><text x="190" y="163" text-anchor="middle" fill="#E74C3C" font-size="10" font-weight="600">Standard of Care</text><rect x="298" y="26" width="110" height="155" rx="6" fill="#1ABC9C" fill-opacity="0.06" stroke="#1ABC9C" stroke-width="1"/><text x="353" y="50" text-anchor="middle" fill="#1ABC9C" font-size="11" font-weight="700">Outputs</text><text x="353" y="72" text-anchor="middle" fill="#8B949E" font-size="9">ICER (EUR/QALY)</text><text x="353" y="92" text-anchor="middle" fill="#8B949E" font-size="9">NMB @ WTP</text><text x="353" y="112" text-anchor="middle" fill="#8B949E" font-size="9">P(cost-effective)</text><text x="353" y="132" text-anchor="middle" fill="#8B949E" font-size="9">QALY gain</text><text x="353" y="152" text-anchor="middle" fill="#8B949E" font-size="9">Cost difference</text><line x1="240" y1="41" x2="296" y2="78" stroke="#1ABC9C" stroke-width="1"/><line x1="240" y1="79" x2="296" y2="95" stroke="#3498DB" stroke-width="1"/><line x1="240" y1="117" x2="296" y2="115" stroke="#8E44AD" stroke-width="1"/><line x1="240" y1="159" x2="296" y2="138" stroke="#E74C3C" stroke-width="1"/></svg>')
                        ))
                      ),
                      
                      tags$div(style="padding:4px 15px 8px 15px;color:#1ABC9C;font-size:11px;font-weight:700;letter-spacing:1.5px;text-transform:uppercase;",
                               icon("bolt",style="margin-right:6px;"),"Live Results — click Run Model to update"),
                      
                      fluidRow(
                        valueBoxOutput("vbox_icer_psych",width=3),
                        valueBoxOutput("vbox_icer_tech", width=3),
                        valueBoxOutput("vbox_icer_combo",width=3),
                        valueBoxOutput("vbox_nmb_psych", width=3)
                      ),
                      
                      fluidRow(box(width=12,title="Base-Case Results",status="primary",solidHeader=TRUE,
                                   DTOutput("tbl_basecase"))),
                      
                      fluidRow(
                        box(width=6,title="Cost-Effectiveness Plane",status="primary",solidHeader=TRUE,
                            plotlyOutput("plt_ce_plane_dash",height="360px")),
                        box(width=6,title="CEAC — Dutch WTP Thresholds",status="primary",solidHeader=TRUE,
                            plotlyOutput("plt_ceac_dash",height="360px"))
                      ),
                      
                      fluidRow(box(width=12,title="Disease State Trajectory",status="info",solidHeader=TRUE,
                                   fluidRow(
                                     column(3,selectInput("trace_arm","Strategy:",choices=c("SoC","Psychosocial","Technology","Combination"),selected="SoC")),
                                     column(9,tags$p(style="padding-top:25px;color:#8B949E;font-size:12px;","Proportion in each health state over the model horizon."))
                                   ),
                                   plotlyOutput("plt_trace",height="400px")))
             ),
             
             # ==========================================================================
             # PSA
             # ==========================================================================
             tags$div(id="panel_psa",class="tab-panel",
                      fluidRow(box(width=12,status="warning",
                                   fluidRow(
                                     column(8,tags$p(icon("triangle-exclamation"),tags$b(" PSA: "),"Monte Carlo simulations. Default 500 sims ~15 seconds.")),
                                     column(4,actionButton("run_psa_btn","Run PSA",class="btn-warning btn-lg btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(
                        box(width=6,title="Cost-Effectiveness Plane",status="primary",solidHeader=TRUE,
                            plotlyOutput("plt_ce_plane",height="420px")),
                        box(width=6,title="CEAC",status="primary",solidHeader=TRUE,
                            plotlyOutput("plt_ceac",height="420px"))
                      )
             ),
             
             # ==========================================================================
             # SENSITIVITY
             # ==========================================================================
             tags$div(id="panel_sensitivity",class="tab-panel",
                      fluidRow(box(width=12,
                                   fluidRow(
                                     column(8,tags$p("One-way DSA — each parameter varied between published low and high values.")),
                                     column(4,actionButton("run_dsa_btn","Run DSA",class="btn-info btn-lg btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(box(width=12,title="Tornado Diagram",status="primary",solidHeader=TRUE,
                                   plotlyOutput("plt_tornado",height="520px"))),
                      fluidRow(box(width=12,title="DSA Results",status="info",solidHeader=TRUE,
                                   DTOutput("tbl_dsa")))
             ),
             
             # ==========================================================================
             # SCENARIOS
             # ==========================================================================
             tags$div(id="panel_scenarios",class="tab-panel",
                      fluidRow(box(width=12,
                                   fluidRow(
                                     column(8,tags$div(
                                       tags$p(style="margin:0 0 6px 0;font-weight:600;","6 pre-specified scenarios:"),
                                       tags$ul(style="color:#8B949E;font-size:12px;margin:0;padding-left:18px;",
                                               tags$li(tags$b(style="color:#1ABC9C;","S1:")," Base-case (default settings)"),
                                               tags$li(tags$b(style="color:#1ABC9C;","S2:")," Early cohort (higher MCI proportion)"),
                                               tags$li(tags$b(style="color:#1ABC9C;","S3:")," Late cohort (higher severe AD proportion)"),
                                               tags$li(tags$b(style="color:#1ABC9C;","S4:")," No informal care costs (medical costs only)"),
                                               tags$li(tags$b(style="color:#1ABC9C;","S5:")," 10-year time horizon"),
                                               tags$li(tags$b(style="color:#1ABC9C;","S6:")," Technology scale-up (enhanced efficacy)")
                                       )
                                     )),
                                     column(4,tags$br(),actionButton("run_scenarios_btn","Run Scenarios",class="btn-info btn-lg btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(box(width=12,title="Scenario Analysis — 6 scenarios × 4 arms",status="primary",solidHeader=TRUE,
                                   DTOutput("tbl_scenarios")))
             ),
             
             # ==========================================================================
             # EVIDENCE
             # ==========================================================================
             tags$div(id="panel_evidence",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(8,tags$p(icon("database",style="color:#1ABC9C;"),tags$b(" Live APIs: "),"ClinicalTrials.gov, PubMed, WHO GHO, Eurostat.")),
                                     column(4,actionButton("run_evidence_btn","Refresh Evidence",class="btn-primary btn-block",icon=icon("rotate")),uiOutput("evidence_last_updated"))
                                   )
                      )),
                      fluidRow(
                        valueBoxOutput("vbox_n_trials",  width=3),
                        valueBoxOutput("vbox_n_rct",     width=3),
                        valueBoxOutput("vbox_n_enrolled",width=3),
                        valueBoxOutput("vbox_n_pubmed",  width=3)
                      ),
                      fluidRow(
                        tabBox(width=12,id="evidence_tabs",
                               tabPanel("Clinical Trials",icon=icon("flask"),
                                        fluidRow(
                                          column(4,selectInput("ev_category","Category:",choices=c("All","Psychosocial interventions","Technology interventions","Combination interventions","Caregiver support","MCI interventions"),selected="All")),
                                          column(4,selectInput("ev_status","Status:",choices=c("All","COMPLETED","ACTIVE_NOT_RECRUITING"),selected="All"))
                                        ),DTOutput("tbl_trials")),
                               tabPanel("Evidence Summary",icon=icon("chart-bar"),DTOutput("tbl_evidence_summary")),
                               tabPanel("PubMed",icon=icon("book-open"),
                                        fluidRow(
                                          column(6,tags$p(style="color:#8B949E;","Psychosocial"),DTOutput("tbl_pubmed_psych")),
                                          column(6,tags$p(style="color:#8B949E;","Technology"),DTOutput("tbl_pubmed_tech"))
                                        )),
                               tabPanel("WHO & Eurostat",icon=icon("globe"),
                                        fluidRow(
                                          box(width=6,title="WHO Prevalence",status="primary",solidHeader=TRUE,DTOutput("tbl_who")),
                                          box(width=6,title="Eurostat NL 65+",status="info",solidHeader=TRUE,DTOutput("tbl_eurostat"))
                                        ))
                        )
                      )
             ),
             
             # ==========================================================================
             # IPD CALIBRATION
             # ==========================================================================
             tags$div(id="panel_ipd",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(4,selectInput("ipd_method","Method:",choices=c("counts","multinomial"),selected="counts")),
                                     column(4,sliderInput("ipd_n_patients","Patients:",100,1000,500,100)),
                                     column(4,tags$br(),actionButton("run_ipd_btn","Run Calibration",class="btn-primary btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(
                        valueBoxOutput("vbox_ipd_patients",width=3),
                        valueBoxOutput("vbox_ipd_trans",   width=3),
                        valueBoxOutput("vbox_ipd_rmse",    width=3),
                        valueBoxOutput("vbox_ipd_mae",     width=3)
                      ),
                      fluidRow(
                        box(width=6,title="GOF: Observed vs Predicted",status="primary",solidHeader=TRUE,
                            plotOutput("plt_gof",height="520px")),
                        box(width=6,title="TP Matrix Comparison",status="info",solidHeader=TRUE,
                            plotOutput("plt_tp_compare",height="520px"))
                      ),
                      fluidRow(box(width=12,title="Calibrated Transition Matrix",status="info",solidHeader=TRUE,
                                   DTOutput("tbl_tp_cal")))
             ),
             
             # ==========================================================================
             # CAUSAL INFERENCE
             # ==========================================================================
             tags$div(id="panel_causal",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(8,tags$p(tags$b("Methods: "),"PSM, IPW, DiD, G-computation, EVPPI")),
                                     column(4,actionButton("run_causal_btn","Run Causal Analysis",class="btn-primary btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(box(width=12,title="Causal Estimates",status="primary",solidHeader=TRUE,DTOutput("tbl_causal"))),
                      # KEY FIX: taller boxes so legend fits inside
                      fluidRow(
                        box(width=6,title="Propensity Score Distribution",status="info",solidHeader=TRUE,
                            plotOutput("plt_ps_dist",height="460px")),
                        box(width=6,title="EVPPI — Research Priority",status="info",solidHeader=TRUE,
                            plotOutput("plt_evppi",height="460px"))
                      )
             ),
             
             # ==========================================================================
             # ADVANCED CAUSAL
             # ==========================================================================
             tags$div(id="panel_advanced",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(8,tags$p(tags$b("Methods: "),"TMLE, IV (2SLS), RDD, MSM, ITS, Bayesian Networks")),
                                     column(4,actionButton("run_advanced_btn","Run Advanced Causal",class="btn-primary btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(box(width=12,title="Advanced Causal Summary",status="primary",solidHeader=TRUE,DTOutput("tbl_advanced"))),
                      # KEY FIX: taller boxes so legend fits inside
                      fluidRow(
                        box(width=6,title="Interrupted Time Series",status="info",solidHeader=TRUE,
                            plotOutput("plt_its",height="480px")),
                        box(width=6,title="Regression Discontinuity",status="info",solidHeader=TRUE,
                            plotOutput("plt_rdd",height="480px"))
                      )
             ),
             
             # ==========================================================================
             # SURVIVAL & RWE
             # ==========================================================================
             tags$div(id="panel_survival",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(8,tags$p(tags$b("Methods: "),"Parametric survival, competing risks, multi-state, NMA, MTC, MICE")),
                                     column(4,actionButton("run_survival_btn","Run Survival & RWE",class="btn-primary btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(
                        tabBox(width=12,id="survival_tabs",
                               tabPanel("Survival Extrapolation",icon=icon("chart-line"),plotOutput("plt_survival",height="460px")),
                               tabPanel("Competing Risks",icon=icon("code-branch"),plotOutput("plt_cif",height="460px")),
                               tabPanel("NMA Forest Plot",icon=icon("sitemap"),plotOutput("plt_nma",height="460px")),
                               tabPanel("NMA League Table",icon=icon("table"),DTOutput("tbl_nma")),
                               tabPanel("Multi-State Rates",icon=icon("diagram-project"),DTOutput("tbl_multistate"))
                        )
                      )
             ),
             
             # ==========================================================================
             # EXTENDED HTA
             # ==========================================================================
             tags$div(id="panel_hta",class="tab-panel",
                      fluidRow(box(width=12,status="primary",
                                   fluidRow(
                                     column(8,tags$p(tags$b("Methods: "),"EVSI, Budget Impact, Equity, Subgroup CEA, Threshold, MCDA")),
                                     column(4,actionButton("run_hta_btn","Run Extended HTA",class="btn-primary btn-block",icon=icon("play")))
                                   )
                      )),
                      fluidRow(
                        tabBox(width=12,id="hta_tabs",
                               tabPanel("EVSI",icon=icon("magnifying-glass-chart"),plotOutput("plt_evsi",height="460px")),
                               tabPanel("Budget Impact",icon=icon("coins"),plotOutput("plt_bia",height="460px")),
                               tabPanel("MCDA",icon=icon("scale-balanced"),
                                        fluidRow(column(6,plotOutput("plt_mcda",height="460px")),column(6,DTOutput("tbl_mcda")))),
                               tabPanel("Threshold",icon=icon("crosshairs"),plotOutput("plt_threshold",height="460px")),
                               tabPanel("Subgroup CEA",icon=icon("users"),DTOutput("tbl_subgroup")),
                               tabPanel("Equity Weights",icon=icon("equals"),DTOutput("tbl_equity"))
                        )
                      )
             ),
             
             # ==========================================================================
             # ASSUMPTIONS
             # ==========================================================================
             tags$div(id="panel_assumptions",class="tab-panel",
                      fluidRow(
                        tags$div(style="background:linear-gradient(135deg,#161B22 0%,#21262D 100%);border:1px solid #30363D;border-left:4px solid #F39C12;border-radius:8px;padding:20px 25px;margin:0 15px 15px 15px;",
                                 fluidRow(
                                   column(7,
                                          tags$h3(style="color:#E6EDF3;margin:0 0 6px 0;",icon("clipboard-check",style="color:#F39C12;")," Model Assumptions Register"),
                                          tags$p(style="color:#8B949E;margin:0;font-size:13px;","32 assumptions — justification, evidence, uncertainty impact. NICE/EMA submission grade.")
                                   ),
                                   column(5,fluidRow(
                                     column(4,tags$div(style="text-align:center;background:#FADBD8;border-radius:6px;padding:10px;",tags$div(style="font-size:24px;font-weight:700;color:#C0392B;",uiOutput("n_high_inline")),tags$div(style="font-size:11px;color:#C0392B;font-weight:600;","HIGH"))),
                                     column(4,tags$div(style="text-align:center;background:#FDEBD0;border-radius:6px;padding:10px;",tags$div(style="font-size:24px;font-weight:700;color:#D35400;",uiOutput("n_mod_inline")),tags$div(style="font-size:11px;color:#D35400;font-weight:600;","MODERATE"))),
                                     column(4,tags$div(style="text-align:center;background:#D5F5E3;border-radius:6px;padding:10px;",tags$div(style="font-size:24px;font-weight:700;color:#1E8449;",uiOutput("n_low_inline")),tags$div(style="font-size:11px;color:#1E8449;font-weight:600;","LOW")))
                                   ))
                                 )
                        )
                      ),
                      fluidRow(box(width=12,status="warning",
                                   fluidRow(
                                     column(3,selectInput("assumption_filter","Filter by impact:",choices=c("All","High","Moderate","Low"),selected="All")),
                                     column(3,selectInput("assumption_cat","Filter by category:",choices=c("All","Structural","Transition Probabilities","Intervention Effects","Costs","Utilities","Model Settings"),selected="All")),
                                     column(3,tags$br(),actionButton("goto_dsa","View DSA",class="btn-info btn-block",icon=icon("tornado"))),
                                     column(3,tags$br(),actionButton("export_assumptions","Export to Word",class="btn-success btn-block",icon=icon("file-word")))
                                   )
                      )),
                      fluidRow(box(width=12,title="Assumptions Register",status="primary",solidHeader=TRUE,
                                   DTOutput("tbl_assumptions"))),
                      fluidRow(box(width=12,title="Selected Assumption — Detail",status="info",solidHeader=TRUE,
                                   uiOutput("assumption_detail")))
             ),
             
             # ==========================================================================
             # REFERENCES
             # ==========================================================================
             tags$div(id="panel_refs",class="tab-panel",
                      fluidRow(box(width=12,title="Bibliography (Vancouver)",status="primary",solidHeader=TRUE,
                                   tags$ol(style="color:#E6EDF3;line-height:2.4;font-size:13px;",
                                           tags$li("Winblad B, Palmer K, Kivipelto M, Jelic V, Fratiglioni L, Wahlund LO, et al. Mild cognitive impairment — beyond controversies, towards a consensus. J Intern Med. 2004;256(3):240-6."),
                                           tags$li("IPECAD Working Group. Guidelines for health-economic modelling in Alzheimer's disease. IPECAD; 2022 [cited 2026]."),
                                           tags$li("Zorginstituut Nederland. Kostenhandleiding. Diemen: ZIN; 2024."),
                                           tags$li("Hofman A, Rocca WA, Brayne C, Breteler MM, Clarke M, Cooper B, et al. The prevalence of dementia in Europe. Neuroepidemiology. 1991;10(5-6):286-95."),
                                           tags$li("Handels RL, Wolfs CA, Aalten P, Verhey FR, Severens JL. Diagnosing Alzheimer's disease: a systematic review of economic evaluations. Alzheimers Dement. 2017;13(6):627-38."),
                                           tags$li("Wimo A, Handels R, Black CM, Lamure M, Frolich L, Holms C, et al. Dementia resource utilisation: the GERAS observational study. J Alzheimers Dis. 2017;57(3):793-803."),
                                           tags$li("Olazaran J, Reisberg B, Clare L, Cruz I, Pena-Casanova J, Del Ser T, et al. Nonpharmacological therapies in Alzheimer's disease. Dement Geriatr Cogn Disord. 2010;30(2):161-78."),
                                           tags$li("Brodaty H, Arasaratnam C. Meta-analysis of nonpharmacological interventions for neuropsychiatric symptoms of dementia. Am J Psychiatry. 2012;169(9):946-53."),
                                           tags$li("Imbeault H, Bier N, Pigot H, Gagnon L, Marcotte N, Fulop T, et al. Electronic organiser and Alzheimer's disease. Neuropsychol Rehabil. 2014;24(3-4):510-36."),
                                           tags$li("Klimova B, Valis M, Kuca K. Cognitive decline in normal aging and its prevention. Curr Alzheimer Res. 2018;15(10):975-83."),
                                           tags$li("Pinquart M, Sorensen S. Helping caregivers of persons with dementia. Int Psychogeriatr. 2006;18(4):577-95."),
                                           tags$li("Krol M, Papenburg J, van Exel J. Does including informal care in economic evaluations matter? Pharmacoeconomics. 2015;33(2):123-35."),
                                           tags$li("Livingston G, Huntley J, Sommerlad A, Ames D, Ballard C, Banerjee S, et al. Dementia prevention, intervention, and care: 2020 Lancet Commission. Lancet. 2020;396(10248):413-46."),
                                           tags$li("Alzheimer Nederland. De maatschappelijke kosten van dementie in Nederland 2023. Amersfoort: Alzheimer Nederland; 2023."),
                                           tags$li("Briggs AH, Claxton K, Sculpher MJ. Decision modelling for health economic evaluation. Oxford: OUP; 2006."),
                                           tags$li("Drummond MF, Sculpher MJ, Claxton K, Stoddart GL, Torrance GW. Methods for the economic evaluation of health care programmes. 4th ed. Oxford: OUP; 2015."),
                                           tags$li("Kuntz KM, Weinstein MC. Modelling in economic evaluation. Oxford: OUP; 2001. p.141-71."),
                                           tags$li("NICE Decision Support Unit. Probabilistic sensitivity analysis. Sheffield: DSU; 2009."),
                                           tags$li("Hunink MG, Glasziou PP, Siegel JE, Weeks JC, Pliskin JS, Elstein AS, et al. Decision making in health and medicine. Cambridge: CUP; 2001."),
                                           tags$li("Fenwick E, Claxton K, Sculpher M. Cost-effectiveness acceptability curves. Health Econ. 2001;10(8):779-87."),
                                           tags$li("Lothgren M, Zethraeus N. Definition and calculation of cost-effectiveness acceptability curves. Health Econ. 2000;9(7):623-30."),
                                           tags$li("Hamby DM. A review of techniques for parameter sensitivity analysis. Environ Monit Assess. 1994;32(2):135-54."),
                                           tags$li("Saltelli A, Ratto M, Andres T, Campolongo F, Cariboni J, Gatelli D, et al. Global sensitivity analysis: the primer. Chichester: Wiley; 2008."),
                                           tags$li("Spackman DE, Veenstra DL, Garrison LP Jr. A net benefits approach to optimal disease simulation model states. Med Decis Making. 2013;33(5):678-91."),
                                           tags$li("Williams R. Generalized ordered logit models. Stata J. 2006;6(1):58-82."),
                                           tags$li("Agresti A. Categorical data analysis. 3rd ed. Hoboken: Wiley; 2013."),
                                           tags$li("Welton NJ, Ades AE. Estimation of Markov chain transition probabilities. Med Decis Making. 2005;25(6):633-45."),
                                           tags$li("Lunn DJ, Thomas A, Best N, Spiegelhalter D. WinBUGS. Stat Med. 2000;19(17-18):2343-60."),
                                           tags$li("Plummer M. JAGS: a program for analysis of Bayesian graphical models. Proceedings of DSC; 2003."),
                                           tags$li("van Buuren S, Groothuis-Oudshoorn K. mice: multivariate imputation in R. J Stat Softw. 2011;45(3):1-67."),
                                           tags$li("Sekhon JS. Multivariate and propensity score matching software. J Stat Softw. 2011;42(7):1-52."),
                                           tags$li("Austin PC. An introduction to propensity score methods. Multivariate Behav Res. 2011;46(3):399-424.")
                                   )
                      ))
             )
             
    ) # end tab-content-area
  ) # end dashboardBody
) # end dashboardPage