# =============================================================================
# DEM-CAPS Health-Economic Model
# File: model/api_clinicaltrials.R
# Description: ClinicalTrials.gov, PubMed, WHO and Eurostat API module
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# -----------------------------------------------------------------------------
# SECTION 1: CORE CLINICALTRIALS.GOV FUNCTION
# -----------------------------------------------------------------------------

empty_trials_df <- function() {
  data.frame(
    nct_id          = character(0),
    title           = character(0),
    status          = character(0),
    phase           = character(0),
    study_type      = character(0),
    enrollment      = numeric(0),
    start_date      = character(0),
    completion      = character(0),
    primary_outcome = character(0),
    stringsAsFactors = FALSE
  )
}

parse_trials <- function(studies) {
  n <- nrow(studies)
  
  safe_extract <- function(df, col, default = NA_character_) {
    if (!col %in% names(df)) return(rep(default, n))
    val <- df[[col]]
    if (is.list(val)) {
      val <- sapply(seq_len(n), function(i) {
        x <- val[[i]]
        if (is.null(x) || length(x) == 0) return(default)
        paste(unlist(x), collapse = ", ")
      })
    }
    if (length(val) != n) return(rep(default, n))
    val
  }
  
  data.frame(
    nct_id = safe_extract(studies,
                          "protocolSection.identificationModule.nctId"),
    title = safe_extract(studies,
                         "protocolSection.identificationModule.briefTitle"),
    status = safe_extract(studies,
                          "protocolSection.statusModule.overallStatus"),
    phase = safe_extract(studies,
                         "protocolSection.designModule.phases"),
    study_type = safe_extract(studies,
                              "protocolSection.designModule.studyType"),
    enrollment = suppressWarnings(as.numeric(safe_extract(studies,
                                                          "protocolSection.designModule.enrollmentInfo.count"))),
    start_date = safe_extract(studies,
                              "protocolSection.statusModule.startDateStruct.date"),
    completion = safe_extract(studies,
                              "protocolSection.statusModule.completionDateStruct.date"),
    primary_outcome = safe_extract(studies,
                                   "protocolSection.outcomesModule.primaryOutcomes"),
    stringsAsFactors = FALSE
  )
}

query_clinicaltrials <- function(
    condition    = "Alzheimer Disease",
    intervention = NULL,
    status       = c("COMPLETED", "ACTIVE_NOT_RECRUITING"),
    max_results  = 100) {
  
  base_url <- "https://clinicaltrials.gov/api/v2/studies"
  
  params <- list(
    "query.cond"           = condition,
    "filter.overallStatus" = paste(status, collapse = "|"),
    "pageSize"             = min(max_results, 100),
    "format"               = "json",
    "fields"               = paste(c(
      "NCTId", "BriefTitle", "OverallStatus",
      "StartDate", "CompletionDate", "EnrollmentCount",
      "Phase", "StudyType", "PrimaryOutcomeMeasure"
    ), collapse = "|")
  )
  
  if (!is.null(intervention)) {
    params[["query.intr"]] <- intervention
  }
  
  cat(sprintf("Querying: condition='%s'", condition))
  if (!is.null(intervention))
    cat(sprintf(", intervention='%s'", intervention))
  cat("\n")
  
  response <- tryCatch(
    httr::GET(base_url, query = params,
              httr::timeout(30),
              httr::user_agent("DEM-CAPS-HTA/1.0")),
    error = function(e) { message(e$message); NULL }
  )
  
  if (is.null(response) ||
      httr::status_code(response) != 200) {
    message("API call failed — returning empty.")
    return(empty_trials_df())
  }
  
  parsed <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    flatten = TRUE
  )
  
  if (is.null(parsed$studies) ||
      length(parsed$studies) == 0) {
    message("No results found.")
    return(empty_trials_df())
  }
  
  cat(sprintf("Retrieved %d trials.\n", nrow(parsed$studies)))
  parse_trials(parsed$studies)
}

# -----------------------------------------------------------------------------
# SECTION 2: DEMENTIA SEARCHES
# -----------------------------------------------------------------------------

run_demcaps_searches <- function() {
  
  cat("=== DEM-CAPS ClinicalTrials.gov Evidence Search ===\n\n")
  
  searches <- list(
    psychosocial = list(
      condition    = "Alzheimer Disease",
      intervention = "psychosocial OR cognitive stimulation OR reminiscence",
      label        = "Psychosocial interventions"
    ),
    technology = list(
      condition    = "Alzheimer Disease",
      intervention = "technology OR digital OR assistive OR app",
      label        = "Technology interventions"
    ),
    combination = list(
      condition    = "dementia",
      intervention = "multicomponent OR combination OR integrated",
      label        = "Combination interventions"
    ),
    caregiver = list(
      condition    = "dementia",
      intervention = "caregiver OR carer OR family",
      label        = "Caregiver support"
    ),
    mci = list(
      condition    = "Mild Cognitive Impairment",
      intervention = NULL,
      label        = "MCI interventions"
    )
  )
  
  results <- lapply(names(searches), function(s_name) {
    s  <- searches[[s_name]]
    cat(sprintf("Searching: %s\n", s$label))
    df <- query_clinicaltrials(
      condition    = s$condition,
      intervention = s$intervention,
      max_results  = 100
    )
    df$search_category <- s$label
    Sys.sleep(0.5)
    df
  })
  
  all_trials <- do.call(rbind, results)
  all_trials <- all_trials[!duplicated(all_trials$nct_id), ]
  cat(sprintf("\nTotal unique trials: %d\n", nrow(all_trials)))
  all_trials
}

# -----------------------------------------------------------------------------
# SECTION 3: EVIDENCE SUMMARY
# -----------------------------------------------------------------------------

summarise_trial_evidence <- function(trials_df) {
  
  if (nrow(trials_df) == 0) {
    message("No trial data.")
    return(NULL)
  }
  
  trials_df %>%
    group_by(search_category) %>%
    summarise(
      n_trials       = n(),
      n_rct          = sum(grepl("RANDOMIZED|INTERVENTIONAL",
                                 toupper(study_type)),
                           na.rm = TRUE),
      total_enrolled = sum(enrollment, na.rm = TRUE),
      completed      = sum(status == "COMPLETED", na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    mutate(
      current_rr = case_when(
        grepl("Psychosocial", search_category) ~ 0.82,
        grepl("Technology",   search_category) ~ 0.88,
        grepl("Combination",  search_category) ~ 0.82 * 0.88,
        TRUE ~ NA_real_
      ),
      evidence_strength = case_when(
        n_rct >= 10 & total_enrolled >= 1000 ~ "Strong",
        n_rct >= 5  & total_enrolled >= 500  ~ "Moderate",
        n_rct >= 2  & total_enrolled >= 100  ~ "Weak",
        TRUE ~ "Insufficient"
      )
    )
}

# -----------------------------------------------------------------------------
# SECTION 4: SAVE & LOAD CACHE
# -----------------------------------------------------------------------------

save_trial_data <- function(
    trials_df,
    path = "data/processed/clinicaltrials_cache.rds") {
  dir.create(dirname(path),
             showWarnings = FALSE, recursive = TRUE)
  saveRDS(list(
    data           = trials_df,
    retrieved_date = Sys.Date(),
    n_trials       = nrow(trials_df)
  ), path)
  cat(sprintf("Saved: %s (%d trials)\n", path, nrow(trials_df)))
}

load_trial_cache <- function(
    path = "data/processed/clinicaltrials_cache.rds") {
  if (!file.exists(path)) {
    message("No cache. Run run_demcaps_searches() first.")
    return(NULL)
  }
  cache    <- readRDS(path)
  age_days <- as.numeric(Sys.Date() - cache$retrieved_date)
  cat(sprintf("Cache loaded: %d trials, age %d days\n",
              cache$n_trials, age_days))
  if (age_days > 30)
    warning("Cache >30 days old. Consider refreshing.")
  cache$data
}

# -----------------------------------------------------------------------------
# SECTION 5: PUBMED API
# -----------------------------------------------------------------------------

query_pubmed <- function(
    search_term = "dementia psychosocial cost-effectiveness",
    max_results = 50) {
  
  cat(sprintf("PubMed: '%s'\n", search_term))
  
  search_resp <- tryCatch(
    httr::GET(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
      query = list(
        db      = "pubmed",
        term    = search_term,
        retmax  = max_results,
        retmode = "json",
        sort    = "relevance"
      ),
      httr::timeout(30)
    ),
    error = function(e) { message(e$message); NULL }
  )
  
  if (is.null(search_resp) ||
      httr::status_code(search_resp) != 200) {
    message("PubMed search failed.")
    return(NULL)
  }
  
  pmids <- jsonlite::fromJSON(
    httr::content(search_resp, as = "text",
                  encoding = "UTF-8"))$esearchresult$idlist
  
  if (length(pmids) == 0) {
    message("No PubMed results.")
    return(NULL)
  }
  
  cat(sprintf("Found %d articles.\n", length(pmids)))
  
  fetch_resp <- tryCatch(
    httr::GET(
      "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi",
      query = list(
        db      = "pubmed",
        id      = paste(pmids, collapse = ","),
        retmode = "json"
      ),
      httr::timeout(30)
    ),
    error = function(e) { message(e$message); NULL }
  )
  
  if (is.null(fetch_resp) ||
      httr::status_code(fetch_resp) != 200) return(NULL)
  
  articles <- jsonlite::fromJSON(
    httr::content(fetch_resp, as = "text",
                  encoding = "UTF-8"),
    flatten = TRUE)$result
  
  pmids_ret <- articles$uids
  
  results <- lapply(pmids_ret, function(pmid) {
    art <- articles[[pmid]]
    data.frame(
      pmid    = pmid,
      title   = if (!is.null(art$title))  art$title  else NA,
      journal = if (!is.null(art$source)) art$source else NA,
      year    = if (!is.null(art$pubdate))art$pubdate else NA,
      authors = if (!is.null(art$authors) &&
                    is.data.frame(art$authors) &&
                    nrow(art$authors) > 0)
        paste(art$authors$name[
          seq_len(min(3, nrow(art$authors)))],
          collapse = ", ")
      else NA,
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}

# -----------------------------------------------------------------------------
# SECTION 6: WHO API
# -----------------------------------------------------------------------------

query_who_dementia <- function() {
  
  cat("Querying WHO GHO...\n")
  
  resp <- tryCatch(
    httr::GET(
      "https://ghoapi.azureedge.net/api/NEUROLOGICAL_DEMENTIAPREVALENCE",
      httr::timeout(30)
    ),
    error = function(e) { message(e$message); NULL }
  )
  
  fallback <- data.frame(
    region     = c("Europe", "Netherlands", "Global", "High-income"),
    year       = c(2021, 2021, 2021, 2021),
    prevalence = c(7.7, 7.5, 5.8, 7.2),
    source     = "WHO 2021 (published estimate)",
    stringsAsFactors = FALSE
  )
  
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("WHO API unavailable — using published estimates.")
    return(fallback)
  }
  
  who_raw <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    flatten = TRUE)
  
  if (!is.null(who_raw$value) && nrow(who_raw$value) > 0) {
    df <- who_raw$value %>%
      select(any_of(c("SpatialDim", "TimeDim", "NumericValue"))) %>%
      filter(!is.na(NumericValue))
    cat(sprintf("WHO: %d records.\n", nrow(df)))
    return(df)
  }
  
  message("WHO parse failed — fallback.")
  fallback
}

# -----------------------------------------------------------------------------
# SECTION 7: EUROSTAT API
# -----------------------------------------------------------------------------

query_eurostat_population <- function() {
  
  cat("Querying Eurostat...\n")
  
  resp <- tryCatch(
    httr::GET(paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/",
      "statistics/1.0/data/demo_pjan",
      "?format=JSON&geo=NL&sex=T&age=Y_GE65&lang=EN"
    ), httr::timeout(30)),
    error = function(e) { message(e$message); NULL }
  )
  
  fallback <- data.frame(
    year       = 2020:2024,
    pop_65plus = c(3340000, 3390000, 3440000, 3490000, 3540000),
    country    = "Netherlands",
    source     = "CBS/Eurostat (published estimate)",
    stringsAsFactors = FALSE
  )
  
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("Eurostat unavailable — using fallback.")
    return(fallback)
  }
  
  raw <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    flatten = TRUE)
  
  if (!is.null(raw$value)) {
    years  <- names(raw$dimension$time$category$label)
    values <- unlist(raw$value)
    df <- data.frame(
      year       = as.integer(years),
      pop_65plus = as.numeric(values),
      country    = "Netherlands",
      source     = "Eurostat API",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(pop_65plus))
    cat(sprintf("Eurostat: %d years.\n", nrow(df)))
    return(df)
  }
  
  message("Eurostat parse failed — fallback.")
  fallback
}

# -----------------------------------------------------------------------------
# SECTION 8: PULL ALL EVIDENCE
# -----------------------------------------------------------------------------

pull_all_evidence <- function(use_cache = TRUE) {
  
  cat("=== DEM-CAPS Full Evidence Pull ===\n")
  cat(as.character(Sys.time()), "\n\n")
  
  cache_path <- "data/processed/clinicaltrials_cache.rds"
  
  trials <- if (use_cache && file.exists(cache_path)) {
    load_trial_cache(cache_path)
  } else {
    t <- run_demcaps_searches()
    save_trial_data(t, cache_path)
    t
  }
  
  Sys.sleep(0.3)
  pubmed_psych <- query_pubmed(
    "dementia psychosocial intervention cost-effectiveness", 30)
  
  Sys.sleep(0.3)
  pubmed_tech <- query_pubmed(
    "dementia technology digital health randomized trial", 30)
  
  who_data      <- query_who_dementia()
  eurostat_data <- query_eurostat_population()
  trial_summary <- summarise_trial_evidence(trials)
  
  cat("\n=== Pull Complete ===\n")
  cat(sprintf("Trials: %d | PubMed psych: %d | PubMed tech: %d\n",
              nrow(trials),
              if (!is.null(pubmed_psych)) nrow(pubmed_psych) else 0,
              if (!is.null(pubmed_tech))  nrow(pubmed_tech)  else 0
  ))
  
  invisible(list(
    trials         = trials,
    trial_summary  = trial_summary,
    pubmed_psych   = pubmed_psych,
    pubmed_tech    = pubmed_tech,
    who_prevalence = who_data,
    eurostat_pop   = eurostat_data,
    pull_date      = Sys.Date()
  ))
}