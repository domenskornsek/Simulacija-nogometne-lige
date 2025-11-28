# app.R
library(shiny)
library(DT)
library(glue)
library(dplyr)
library(ggplot2)

# -----------------------------
# POWER: uporablja uteži, ki si jih podal
# -----------------------------
calculate_power <- function(team, is_home = FALSE, event_mod = 0) {
  weights <- c(
    napad = 10,
    sredina = 20,
    obramba = 15,
    vratar = 5,
    domace = 15,
    trener = 10,
    utrujenost = 5,
    fitnes = 10,
    sodniki = 5,
    kvaliteta = 5
  )
  home_contrib <- ifelse(is_home, team$domace * weights["domace"], 0)
  base_power <-
    team$napad * weights["napad"] +
    team$sredina * weights["sredina"] +
    team$obramba * weights["obramba"] +
    team$vratar * weights["vratar"] +
    home_contrib +
    team$trener * weights["trener"] +
    (100 - team$utrujenost) * weights["utrujenost"] +
    team$fitnes * weights["fitnes"] +
    team$sodniki * weights["sodniki"] +
    team$kvaliteta * weights["kvaliteta"]
  final_power <- base_power - event_mod
  final_power <- max(1, final_power)
  return(final_power)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(".team-box { border:1px solid #e3e3e3; padding:10px; border-radius:6px; margin-bottom:8px; }
                                 .small-muted { font-size:12px; color:#777; }
                                 .match-card { border:1px solid #ddd; padding:8px; border-radius:6px; margin:6px 0; }
                                 details { margin-bottom:8px; }
                                 summary { font-weight:600; }
                                 "))),
  titlePanel("Soccer League Simulator + Monte Carlo analiza"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Create teams"),
                 textInput("team_name", "Team name", placeholder = "e.g. Red Rovers"),
                 sliderInput("napad",   "Moč napada (1-100)",   min = 1, max = 100, value = 50),
                 sliderInput("sredina", "Moč sredine (1-100)",  min = 1, max = 100, value = 50),
                 sliderInput("obramba", "Moč obrambe (1-100)", min = 1, max = 100, value = 50),
                 sliderInput("vratar",  "Moč vratarja (1-100)",min = 1, max = 100, value = 50),
                 sliderInput("domace",  "Prednost domačega igrišča (1-100)", min = 1, max = 100, value = 50),
                 sliderInput("trener",  "Sposobnost trenerja/taktika (1-100)", min = 1, max = 100, value = 50),
                 sliderInput("utrujenost","Utrujenost (1-100; več = bolj utrujeni)", min = 1, max = 100, value = 10),
                 sliderInput("fitnes",  "Fizična priprava (1-100)", min = 1, max = 100, value = 50),
                 sliderInput("sodniki", "Naklonjenost sodnikov (1-100; 50 neutralno)", min = 1, max = 100, value = 50),
                 sliderInput("kvaliteta","Individualna kvaliteta igralcev (1-100)", min = 1, max = 100, value = 50),
                 actionButton("add_team", "Add / Update Team", class = "btn-primary"),
                 actionButton("remove_last", "Remove Last Team", class = "btn-danger", style = "margin-left:6px;"),
                 hr(),
                 h4("League controls"),
                 numericInput("games_each_side", "Games per side (N) — each pair plays N home and N away matches", value = 1, min = 1, max = 5, step = 1),
                 checkboxInput("use_weather", "Enable random weather effects", value = TRUE),
                 actionButton("generate_schedule", "Generate Schedule & Reset Results", class = "btn-success"),
                 actionButton("simulate_next_round", "Simulate Next Round", class = "btn-primary"),
                 actionButton("simulate_all", "Simulate All Remaining Matches", class = "btn-primary", style = "margin-left:6px;"),
                 actionButton("reset_all", "Reset All", class = "btn-warning", style = "margin-left:6px;"),
                 hr(),
                 h4("Monte Carlo"),
                 numericInput("n_sim", "Število simulacij:", value = 1000, min = 1),
                 actionButton("simulate_montecarlo", "Zaženi Monte Carlo", class = "btn-primary"),
                 hr(),
                 h5("Notes"),
                 p(class="small-muted", "All data is in memory. For development try n_sim=200-2000; larger values give smoother estimates but take longer.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Ekipe",
                 br(),
                 uiOutput("teams_list"),
                 br(),
                 DTOutput("teams_dt")
        ),
        tabPanel("Liga",
                 br(),
                 actionButton("force_table_refresh", "Refresh Table", class = "btn-secondary"),
                 br(), br(),
                 DTOutput("league_table"),
                 br(),
                 h4("Match Results by Round"),
                 uiOutput("results_by_round_ui")
        ),
        tabPanel("Monte Carlo",
                 br(),
                 h4("Povprečja iz Monte Carlo"),
                 DTOutput("montecarlo_table_out"),
                 br(),
                 h5("Opomba: za podrobno analizo pojdi na zavihek 'Analiza'")
        ),
        tabPanel("Analiza",
                 br(),
                 fluidRow(
                   column(6, selectInput("mc_team", "Izberi ekipo:", choices = NULL)),
                   column(6, numericInput("mc_bins", "Število binov (hist):", value = 20, min = 5))
                 ),
                 hr(),
                 h4("Verjetnosti (izbrana ekipa)"),
                 tableOutput("mc_probabilities"),
                 hr(),
                 h4("Distribucija točk izbrane ekipe"),
                 plotOutput("mc_hist_points", height = "300px"),
                 hr(),
                 h4("Distribucija točk ekip, ki končajo na 1. mestu (winners)"),
                 plotOutput("mc_hist_winners", height = "350px"),
                 hr(),
                 h4("Standardni odkloni (točke, GF, GA)"),
                 tableOutput("mc_sd_table"),
                 hr(),
                 h4("Tabela verjetnosti zmage / top3 / izpadanja"),
                 DTOutput("mc_win_prob_table")
        )
      )
    )
  )
)

server <- function(input, output, session){
  vals <- reactiveValues(
    teams = list(),
    fixtures = NULL,
    results = NULL,
    montecarlo_table = NULL,
    events = NULL,
    mc_results = NULL  # long-format per-simulation results
  )
  
  ## --- Team management ---
  observeEvent(input$add_team, {
    name <- trimws(input$team_name)
    if(name == ""){
      showNotification("Team name cannot be blank", type = "error")
      return()
    }
    existing <- sapply(vals$teams, function(t) t$name)
    team_entry <- list(
      name = name,
      napad = as.numeric(input$napad),
      sredina = as.numeric(input$sredina),
      obramba = as.numeric(input$obramba),
      vratar = as.numeric(input$vratar),
      domace = as.numeric(input$domace),
      trener = as.numeric(input$trener),
      utrujenost = as.numeric(input$utrujenost),
      fitnes = as.numeric(input$fitnes),
      sodniki = as.numeric(input$sodniki),
      kvaliteta = as.numeric(input$kvaliteta),
      matches_played = 0
    )
    if(name %in% existing){
      idx <- which(existing == name)[1]
      vals$teams[[idx]] <- team_entry
      showNotification(glue("Updated team: {name}"), type = "message")
    } else {
      vals$teams[[length(vals$teams)+1]] <- team_entry
      showNotification(glue("Added team: {name}"), type = "message")
    }
    updateTextInput(session, "team_name", value = "")
  })
  
  observeEvent(input$remove_last, {
    if(length(vals$teams) == 0){ showNotification("No teams to remove", type = "warning"); return() }
    removed <- vals$teams[[length(vals$teams)]]$name
    vals$teams <- vals$teams[-length(vals$teams)]
    showNotification(glue("Removed team: {removed}"), type = "message")
  })
  
  output$teams_list <- renderUI({
    if(length(vals$teams) == 0) return(tags$p("No teams yet. Add some on the left."))
    lapply(seq_along(vals$teams), function(i){
      t <- vals$teams[[i]]
      div(class="team-box",
          strong(glue("{i}. {t$name}")), br(),
          span(class="small-muted", glue(
            "Napad: {t$napad} | Sredina: {t$sredina} | Obramba: {t$obramba} | Vratar: {t$vratar} | Domače: {t$domace} | Trener: {t$trener} | Utrujenost: {t$utrujenost} | Fitnes: {t$fitnes} | Sodniki: {t$sodniki} | Kvaliteta: {t$kvaliteta}"
          ))
      )
    })
  })
  
  output$teams_dt <- renderDT({
    if(length(vals$teams)==0) return(NULL)
    df <- do.call(rbind, lapply(vals$teams, function(t) data.frame(
      Name=t$name,
      Napad=t$napad, Sredina=t$sredina, Obramba=t$obramba, Vratar=t$vratar,
      Domace=t$domace, Trener=t$trener, Utrujenost=t$utrujenost, Fitnes=t$fitnes,
      Sodniki=t$sodniki, Kvaliteta=t$kvaliteta,
      stringsAsFactors=FALSE)))
    datatable(df, rownames=FALSE, options = list(dom='t'))
  })
  
  observeEvent(input$reset_all, {
    vals$teams <- list(); vals$fixtures <- NULL; vals$results <- NULL; vals$montecarlo_table <- NULL; vals$events <- NULL; vals$mc_results <- NULL
    updateSelectInput(session, "mc_team", choices = character(0))
    showNotification("League reset", type = "message")
  })
  
  ## --- Schedule generation (balanced round-robin) ---
  generate_round_robin <- function(team_names, games_each_side = 1){
    teams <- team_names
    n <- length(teams)
    if(n %% 2 == 1){
      teams <- c(teams, "BYE")
      n <- n + 1
    }
    rounds_base <- n - 1
    half <- n / 2
    
    schedule_list <- vector("list", rounds_base)
    current <- teams
    for(r in seq_len(rounds_base)){
      matches <- data.frame(home=character(), away=character(), stringsAsFactors = FALSE)
      for(i in seq_len(half)){
        h <- current[i]
        a <- current[n - i + 1]
        if(h != "BYE" && a != "BYE"){
          matches <- rbind(matches, data.frame(home = h, away = a, stringsAsFactors = FALSE))
        }
      }
      schedule_list[[r]] <- matches
      current <- c(current[1], current[n], current[2:(n-1)])
    }
    
    sets <- 2 * games_each_side
    full_matches <- data.frame(match_id=integer(), round=integer(), home=character(), away=character(), stringsAsFactors = FALSE)
    round_counter <- 1
    for(set_idx in seq_len(sets)){
      for(r in seq_len(rounds_base)){
        mat <- schedule_list[[r]]
        if(set_idx %% 2 == 0){
          mat <- data.frame(home = mat$away, away = mat$home, stringsAsFactors = FALSE)
        }
        mat$round <- round_counter
        mat$match_id <- NA_integer_
        full_matches <- rbind(full_matches, mat[, c('match_id','round','home','away')])
        round_counter <- round_counter + 1
      }
    }
    full_matches$match_id <- seq_len(nrow(full_matches))
    return(full_matches)
  }
  
  # --- Assign season events ---
  assign_events <- function(fixtures, teams_list){
    total_matches <- nrow(fixtures)
    rain_matches <- if(total_matches >= 2) sample(fixtures$match_id, size = min(2, total_matches)) else integer(0)
    
    team_scores <- sapply(teams_list, function(t) (t$napad + t$sredina + t$obramba + t$kvaliteta))
    team_names <- sapply(teams_list, function(t) t$name)
    ord <- order(-team_scores)
    top_k <- min(8, length(team_names))
    top8_names <- team_names[ord][seq_len(top_k)]
    
    reserve_matches <- list()
    bad_matches <- list()
    for(tn in top8_names){
      eligible <- fixtures$match_id[ (fixtures$home == tn) | (fixtures$away == tn) ]
      if(length(eligible) == 0){
        reserve_matches[[tn]] <- integer(0)
        bad_matches[[tn]] <- integer(0)
        next
      }
      rcount <- min(2, length(eligible))
      reserve_matches[[tn]] <- sample(eligible, size = rcount)
      bad_matches[[tn]] <- sample(eligible, size = 1)
    }
    return(list(rain_matches = rain_matches, reserve_matches = reserve_matches, bad_matches = bad_matches))
  }
  
  observeEvent(input$generate_schedule, {
    n <- length(vals$teams)
    if(n < 3){ showNotification("Please add at least 3 teams before generating schedule", type = "error"); return() }
    team_names <- sapply(vals$teams, function(x) x$name)
    fixtures <- generate_round_robin(team_names, games_each_side = as.integer(input$games_each_side))
    vals$fixtures <- fixtures
    vals$results <- data.frame(match_id = fixtures$match_id, home_goals = NA_integer_, away_goals = NA_integer_, weather = NA_character_, played = FALSE, stringsAsFactors = FALSE)
    for(i in seq_along(vals$teams)) vals$teams[[i]]$matches_played <- 0
    vals$events <- assign_events(fixtures, vals$teams)
    showNotification("Schedule generated and seasonal events assigned.", type = "message")
  })
  
  ## Generic simulate over an environment (list) so we can reuse for Monte Carlo
  simulate_matches_env <- function(env, rounds_to_simulate = NULL){
    fixtures <- env$fixtures
    results <- env$results
    teams <- env$teams
    events <- env$events
    use_weather_local <- env$use_weather
    
    find_team_idx <- function(tname) which(sapply(teams, function(t) t$name) == tname)
    
    to_sim_idx <- seq_len(nrow(fixtures))
    if(!is.null(rounds_to_simulate)){
      to_sim_idx <- which(fixtures$round %in% rounds_to_simulate)
    }
    
    for(i in to_sim_idx){
      mid <- fixtures$match_id[i]
      if(isTRUE(results$played[results$match_id==mid])) next
      
      home_name <- fixtures$home[i]; away_name <- fixtures$away[i]
      idx_h <- find_team_idx(home_name)
      idx_a <- find_team_idx(away_name)
      if(length(idx_h)==0 || length(idx_a)==0) next
      
      home_t <- teams[[idx_h]]
      away_t <- teams[[idx_a]]
      
      # compute event_mod for each team
      ev_mod_h <- 0; ev_mod_a <- 0
      if(!is.null(events) && length(events$rain_matches)>0 && mid %in% events$rain_matches){
        ev_mod_h <- ev_mod_h + 10
        ev_mod_a <- ev_mod_a + 10
      }
      if(!is.null(events) && !is.null(events$reserve_matches)){
        hm_res <- events$reserve_matches[[home_name]]
        aw_res <- events$reserve_matches[[away_name]]
        if(!is.null(hm_res) && mid %in% hm_res) ev_mod_h <- ev_mod_h + 15
        if(!is.null(aw_res) && mid %in% aw_res) ev_mod_a <- ev_mod_a + 15
      }
      if(!is.null(events) && !is.null(events$bad_matches)){
        hm_bad <- events$bad_matches[[home_name]]
        aw_bad <- events$bad_matches[[away_name]]
        if(!is.null(hm_bad) && mid %in% hm_bad) ev_mod_h <- ev_mod_h + 20
        if(!is.null(aw_bad) && mid %in% aw_bad) ev_mod_a <- ev_mod_a + 20
      }
      
      # compute base powers
      base_h <- calculate_power(home_t, is_home = TRUE, event_mod = 0)
      base_a <- calculate_power(away_t, is_home = FALSE, event_mod = 0)
      # matches played penalty
      base_h <- base_h - (home_t$matches_played * (home_t$utrujenost / 40))
      base_a <- base_a - (away_t$matches_played * (away_t$utrujenost / 40))
      # apply seasonal event mods
      home_power <- max(1, base_h - ev_mod_h)
      away_power <- max(1, base_a - ev_mod_a)
      
      # ref and morale adjustments
      ref_factor_home <- (as.numeric(home_t$sodniki) - 50) / 100
      ref_factor_away <- (as.numeric(away_t$sodniki) - 50) / 100
      morale_home <- as.numeric(home_t$fitnes) / 50
      morale_away <- as.numeric(away_t$fitnes) / 50
      
      share_h <- home_power / (home_power + away_power)
      share_a <- away_power / (home_power + away_power)
      lambda_home <- pmax(0.05, share_h * 3) * morale_home * (1 + ref_factor_home)
      lambda_away <- pmax(0.05, share_a * 3) * morale_away * (1 + ref_factor_away)
      
      weather_str <- "Clear"
      if(isTRUE(use_weather_local)){
        w <- sample(c("Clear","Rain","Wind","Storm"), 1, prob = c(0.6,0.2,0.15,0.05))
        weather_str <- w
        if(w == "Rain"){ lambda_home <- lambda_home * 0.9; lambda_away <- lambda_away * 0.9 }
        else if(w == "Wind"){ lambda_home <- lambda_home * 0.95; lambda_away <- lambda_away * 0.95 }
        else if(w == "Storm"){ lambda_home <- lambda_home * 0.7; lambda_away <- lambda_away * 0.7 }
      }
      
      home_goals <- rpois(1, lambda_home)
      away_goals <- rpois(1, lambda_away)
      
      results$home_goals[results$match_id==mid] <- home_goals
      results$away_goals[results$match_id==mid] <- away_goals
      results$weather[results$match_id==mid] <- weather_str
      results$played[results$match_id==mid] <- TRUE
      
      teams[[idx_h]]$matches_played <- teams[[idx_h]]$matches_played + 1
      teams[[idx_a]]$matches_played <- teams[[idx_a]]$matches_played + 1
    }
    
    env$results <- results
    env$teams <- teams
    return(env)
  }
  
  # thin wrapper that uses reactive vals (for main UI)
  simulate_matches <- function(rounds_to_simulate = NULL){
    req(vals$fixtures)
    env <- list(fixtures = vals$fixtures, results = vals$results, teams = vals$teams, events = vals$events, use_weather = input$use_weather)
    env2 <- simulate_matches_env(env, rounds_to_simulate = rounds_to_simulate)
    vals$results <- env2$results
    vals$teams <- env2$teams
  }
  
  observeEvent(input$simulate_next_round, {
    req(vals$fixtures)
    unplayed <- vals$results$match_id[!vals$results$played]
    if(length(unplayed)==0){ showNotification("All matches already simulated", type = "message"); return() }
    next_round <- min(vals$fixtures$round[vals$fixtures$match_id %in% unplayed])
    simulate_matches(rounds_to_simulate = next_round)
    showNotification(glue("Simulated Round {next_round}"), type = "message")
  })
  
  observeEvent(input$simulate_all, {
    req(vals$fixtures)
    unplayed <- vals$results$match_id[!vals$results$played]
    if(length(unplayed)==0){ showNotification("All matches already simulated", type = "message"); return() }
    rounds_left <- sort(unique(vals$fixtures$round[vals$fixtures$match_id %in% unplayed]))
    for(r in rounds_left) simulate_matches(rounds_to_simulate = r)
    showNotification("All remaining matches simulated", type = "message")
  })
  
  ## --- League table calculation (works from current vals) ---
  league_table_df <- reactive({
    req(vals$teams)
    teams <- sapply(vals$teams, function(x) x$name)
    tbl <- data.frame(Team = teams, Played = 0, W = 0, D = 0, L = 0, GF = 0, GA = 0, GD = 0, Points = 0, stringsAsFactors = FALSE)
    if(!is.null(vals$results) && nrow(vals$results)>0){
      res <- vals$results
      fixtures <- vals$fixtures
      for(i in seq_len(nrow(res))){
        if(!res$played[i]) next
        mid <- res$match_id[i]
        f <- fixtures[fixtures$match_id==mid,]
        h <- f$home; a <- f$away
        hg <- res$home_goals[i]; ag <- res$away_goals[i]
        tbl$Played[tbl$Team==h] <- tbl$Played[tbl$Team==h] + 1
        tbl$Played[tbl$Team==a] <- tbl$Played[tbl$Team==a] + 1
        tbl$GF[tbl$Team==h] <- tbl$GF[tbl$Team==h] + hg
        tbl$GA[tbl$Team==h] <- tbl$GA[tbl$Team==h] + ag
        tbl$GF[tbl$Team==a] <- tbl$GF[tbl$Team==a] + ag
        tbl$GA[tbl$Team==a] <- tbl$GA[tbl$Team==a] + hg
        if(hg > ag){
          tbl$W[tbl$Team==h] <- tbl$W[tbl$Team==h] + 1
          tbl$L[tbl$Team==a] <- tbl$L[tbl$Team==a] + 1
          tbl$Points[tbl$Team==h] <- tbl$Points[tbl$Team==h] + 3
        } else if(hg < ag){
          tbl$W[tbl$Team==a] <- tbl$W[tbl$Team==a] + 1
          tbl$L[tbl$Team==h] <- tbl$L[tbl$Team==h] + 1
          tbl$Points[tbl$Team==a] <- tbl$Points[tbl$Team==a] + 3
        } else {
          tbl$D[tbl$Team==h] <- tbl$D[tbl$Team==h] + 1
          tbl$D[tbl$Team==a] <- tbl$D[tbl$Team==a] + 1
          tbl$Points[tbl$Team==h] <- tbl$Points[tbl$Team==h] + 1
          tbl$Points[tbl$Team==a] <- tbl$Points[tbl$Team==a] + 1
        }
      }
    }
    tbl$GD <- tbl$GF - tbl$GA
    tbl <- tbl[order(-tbl$Points, -tbl$GD, -tbl$GF), ]
    tbl$Position <- seq_len(nrow(tbl))
    tbl
  })
  
  output$league_table <- renderDT({
    req(vals$teams)
    datatable(league_table_df(), rownames = FALSE, options = list(dom='t'))
  })
  
  ## --- Fixtures display ---
  output$fixtures_ui <- renderUI({
    req(vals$fixtures)
    f <- vals$fixtures
    results <- vals$results
    rounds <- sort(unique(f$round))
    lapply(rounds, function(r){
      matches <- f[f$round==r,]
      div(
        h4(glue("Round {r}")),
        lapply(seq_len(nrow(matches)), function(i){
          mid <- matches$match_id[i]
          res <- results[results$match_id==mid,]
          if(!res$played){
            div(class="match-card", glue("{matches$home[i]} vs {matches$away[i]} — not played"))
          } else {
            ev_notes <- character(0)
            if(!is.null(vals$events)){
              if(mid %in% vals$events$rain_matches) ev_notes <- c(ev_notes, "Rain")
              if(!is.null(vals$events$reserve_matches[[matches$home[i]]]) && mid %in% vals$events$reserve_matches[[matches$home[i]]]) ev_notes <- c(ev_notes, paste(matches$home[i],"reserve"))
              if(!is.null(vals$events$reserve_matches[[matches$away[i]]]) && mid %in% vals$events$reserve_matches[[matches$away[i]]]) ev_notes <- c(ev_notes, paste(matches$away[i],"reserve"))
              if(!is.null(vals$events$bad_matches[[matches$home[i]]]) && mid %in% vals$events$bad_matches[[matches$home[i]]]) ev_notes <- c(ev_notes, paste(matches$home[i],"bad"))
              if(!is.null(vals$events$bad_matches[[matches$away[i]]]) && mid %in% vals$events$bad_matches[[matches$away[i]]]) ev_notes <- c(ev_notes, paste(matches$away[i],"bad"))
            }
            notes <- if(length(ev_notes)>0) paste0(" [events: ", paste(ev_notes, collapse = ", "), "]") else ""
            div(class="match-card", glue("{matches$home[i]} {res$home_goals}-{res$away_goals} {matches$away[i]} ({res$weather}){notes}"))
          }
        })
      )
    })
  })
  
  ## --- Results by round ---
  output$results_by_round_ui <- renderUI({
    req(vals$fixtures, vals$results)
    res <- vals$results
    f <- vals$fixtures
    rounds <- sort(unique(f$round))
    lapply(rounds, function(r){
      matches <- f[f$round==r,]
      div(
        tags$details(tags$summary(glue("Round {r} Results")),
                     lapply(seq_len(nrow(matches)), function(i){
                       mid <- matches$match_id[i]
                       rs <- res[res$match_id==mid,]
                       if(!rs$played) glue("{matches$home[i]} vs {matches$away[i]} — not played yet") else glue("{matches$home[i]} {rs$home_goals}-{rs$away_goals} {matches$away[i]} ({rs$weather})")
                     })
        )
      )
    })
  })
  
  ## --- Monte Carlo helpers ---
  compute_league_table_from_env <- function(env){
    fixtures <- env$fixtures
    results <- env$results
    teams <- sapply(env$teams, function(t) t$name)
    tbl <- data.frame(Team = teams, Played = 0, W = 0, D = 0, L = 0, GF = 0, GA = 0, GD = 0, Points = 0, stringsAsFactors = FALSE)
    if(!is.null(results) && nrow(results)>0){
      for(i in seq_len(nrow(results))){
        if(!results$played[i]) next
        mid <- results$match_id[i]
        f <- fixtures[fixtures$match_id==mid,]
        h <- f$home; a <- f$away
        hg <- results$home_goals[i]; ag <- results$away_goals[i]
        tbl$Played[tbl$Team==h] <- tbl$Played[tbl$Team==h] + 1
        tbl$Played[tbl$Team==a] <- tbl$Played[tbl$Team==a] + 1
        tbl$GF[tbl$Team==h] <- tbl$GF[tbl$Team==h] + hg
        tbl$GA[tbl$Team==h] <- tbl$GA[tbl$Team==h] + ag
        tbl$GF[tbl$Team==a] <- tbl$GF[tbl$Team==a] + ag
        tbl$GA[tbl$Team==a] <- tbl$GA[tbl$Team==a] + hg
        if(hg > ag){
          tbl$W[tbl$Team==h] <- tbl$W[tbl$Team==h] + 1
          tbl$L[tbl$Team==a] <- tbl$L[tbl$Team==a] + 1
          tbl$Points[tbl$Team==h] <- tbl$Points[tbl$Team==h] + 3
        } else if(hg < ag){
          tbl$W[tbl$Team==a] <- tbl$W[tbl$Team==a] + 1
          tbl$L[tbl$Team==h] <- tbl$L[tbl$Team==h] + 1
          tbl$Points[tbl$Team==a] <- tbl$Points[tbl$Team==a] + 3
        } else {
          tbl$D[tbl$Team==h] <- tbl$D[tbl$Team==h] + 1
          tbl$D[tbl$Team==a] <- tbl$D[tbl$Team==a] + 1
          tbl$Points[tbl$Team==h] <- tbl$Points[tbl$Team==h] + 1
          tbl$Points[tbl$Team==a] <- tbl$Points[tbl$Team==a] + 1
        }
      }
    }
    tbl$GD <- tbl$GF - tbl$GA
    tbl <- tbl[order(-tbl$Points, -tbl$GD, -tbl$GF), ]
    tbl$Position <- seq_len(nrow(tbl))
    return(tbl)
  }
  
  observeEvent(input$simulate_montecarlo, {
    req(vals$fixtures, vals$teams)
    n_sim <- as.integer(input$n_sim)
    if (is.na(n_sim) || n_sim < 1) {
      showNotification("Vnesi pozitivno število simulacij", type = "error")
      return()
    }
    
    showNotification(glue("Začenjam Monte Carlo simulacijo ({n_sim} ponovitev)..."), type = "message", duration = NULL, id = "mc_notice")
    on.exit({ removeNotification(id = "mc_notice") }, add = TRUE)
    
    team_names <- sapply(vals$teams, function(x) x$name)
    n_teams <- length(team_names)
    
    sims_list <- vector("list", n_sim)
    
    fixtures_master <- vals$fixtures
    
    for (sim in seq_len(n_sim)) {
      # local env
      env <- list()
      env$fixtures <- fixtures_master
      env$results <- data.frame(match_id = fixtures_master$match_id, home_goals = NA_integer_, away_goals = NA_integer_, weather = NA_character_, played = FALSE, stringsAsFactors = FALSE)
      env$teams <- lapply(vals$teams, function(t){ t$matches_played <- 0; t })
      env$events <- assign_events(env$fixtures, env$teams)
      env$use_weather <- input$use_weather
      
      rounds_all <- sort(unique(env$fixtures$round))
      for(r in rounds_all){
        env <- simulate_matches_env(env, rounds_to_simulate = r)
      }
      
      tbl <- compute_league_table_from_env(env)
      # keep team-level results with sim index
      df_sim <- tbl[, c("Team","Points","GF","GA","GD","Position")]
      df_sim$sim <- sim
      sims_list[[sim]] <- df_sim
      
      if(sim %% 50 == 0) {
        showNotification(glue("Monte Carlo progress: {sim}/{n_sim}"), type = "message", duration = 1)
      }
    }
    
    mc_long <- do.call(rbind, sims_list)
    vals$mc_results <- mc_long
    
    # compute averages & aggregate table (same as before but from mc_long)
    montecarlo_table <- mc_long %>%
      group_by(Team) %>%
      summarise(
        MeanPoints = mean(Points),
        MeanGF = mean(GF),
        MeanGA = mean(GA),
        MeanGD = mean(GD),
        SDPoints = sd(Points),
        SDGF = sd(GF),
        SDGA = sd(GA),
        WinProb = mean(Position == 1),
        Top3Prob = mean(Position <= pmin(3, n_teams)),
        RelegProb = mean(Position >= pmax((n_teams-1),1))
      ) %>%
      ungroup() %>%
      arrange(-MeanPoints, -MeanGD, -MeanGF)
    
    # fix column names safe for DT (remove space)
    names(montecarlo_table) <- gsub(" ", "_", names(montecarlo_table))
    
    vals$montecarlo_table <- montecarlo_table
    
    # update team selector for analysis
    updateSelectInput(session, "mc_team", choices = team_names, selected = team_names[1])
    
    showNotification("Monte Carlo simulacija končana!", type = "message")
  })
  
  output$montecarlo_table_out <- renderDT({
    req(vals$montecarlo_table)
    datatable(vals$montecarlo_table, rownames = FALSE, options = list(pageLength = 50))
  })
  
  # update selectInput when teams change (so user can select team for analysis even before MC)
  observe({
    team_names <- sapply(vals$teams, function(x) x$name)
    # if mc_team already has a selection keep it, otherwise select first
    sel <- isolate(input$mc_team)
    if(is.null(sel) || !(sel %in% team_names)){
      newsel <- if(length(team_names)>0) team_names[1] else character(0)
    } else newsel <- sel
    updateSelectInput(session, "mc_team", choices = team_names, selected = newsel)
  })
  
  ## --- Analysis outputs ---
  output$mc_probabilities <- renderTable({
    req(vals$mc_results, input$mc_team)
    team <- input$mc_team
    df <- vals$mc_results %>% filter(Team == team)
    n_sim <- length(unique(vals$mc_results$sim))
    n_teams <- length(unique(vals$mc_results$Team))
    # probabilities
    prob_win <- mean(df$Position == 1)
    prob_top3 <- mean(df$Position <= pmin(3, n_teams))
    prob_releg <- mean(df$Position >= pmax(n_teams-1,1))
    mean_points <- mean(df$Points)
    sd_points <- sd(df$Points)
    mean_gf <- mean(df$GF)
    sd_gf <- sd(df$GF)
    mean_ga <- mean(df$GA)
    sd_ga <- sd(df$GA)
    tib <- data.frame(
      Metric = c("Simulacije (n)", "Verjetnost zmage (1.)", "Verjetnost top3", "Verjetnost izpada (zadnji 2)", "Povprečje točk", "SD točk", "Povprečje GF", "SD GF", "Povprečje GA", "SD GA"),
      Value = c(n_sim, round(prob_win,4), round(prob_top3,4), round(prob_releg,4), round(mean_points,2), round(sd_points,2), round(mean_gf,2), round(sd_gf,2), round(mean_ga,2), round(sd_ga,2)),
      stringsAsFactors = FALSE
    )
    tib
  }, sanitize.text.function = function(x) x)
  
  output$mc_hist_points <- renderPlot({
    req(vals$mc_results, input$mc_team)
    df <- vals$mc_results %>% filter(Team == input$mc_team)
    bins <- input$mc_bins
    ggplot(df, aes(x = Points)) +
      geom_histogram(bins = bins, boundary = -0.5, closed = "left", fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(title = glue("Distribucija točk: {input$mc_team}"), x = "Točke", y = "Frekvenca")
  })
  
  output$mc_hist_winners <- renderPlot({
    req(vals$mc_results)
    winners <- vals$mc_results %>% filter(Position == 1)
    ggplot(winners, aes(x = Points)) +
      geom_histogram(bins = 30, boundary = -0.5, closed = "left", fill = "darkgreen", color = "white") +
      theme_minimal() +
      labs(title = "Distribucija točk ekip, ki končajo na 1. mestu", x = "Točke (winner)", y = "Frekvenca")
  })
  
  output$mc_sd_table <- renderTable({
    req(vals$mc_results)
    sdtab <- vals$mc_results %>%
      group_by(Team) %>%
      summarise(
        MeanPoints = mean(Points),
        SDPoints = sd(Points),
        MeanGF = mean(GF),
        SDGF = sd(GF),
        MeanGA = mean(GA),
        SDGA = sd(GA)
      ) %>%
      arrange(-MeanPoints)
    # fix colnames
    names(sdtab) <- c("Team","MeanPoints","SDPoints","MeanGF","SDGF","MeanGA","SDGA")
    sdtab
  })
  
  output$mc_win_prob_table <- renderDT({
    req(vals$mc_results)
    n_teams <- length(unique(vals$mc_results$Team))
    probtab <- vals$mc_results %>%
      group_by(Team) %>%
      summarise(
        WinProb = mean(Position == 1),
        Top3Prob = mean(Position <= pmin(3, n_teams)),
        RelegProb = mean(Position >= pmax(n_teams-1, 1)),
        MeanPoints = mean(Points),
        SDPoints = sd(Points)
      ) %>%
      ungroup() %>%
      arrange(-WinProb, -Top3Prob, -MeanPoints)
    # round
    probtab <- probtab %>% mutate_at(vars(WinProb,Top3Prob,RelegProb), ~round(.,4)) %>%
      mutate_at(vars(MeanPoints,SDPoints), ~round(.,2))
    datatable(probtab, rownames = FALSE, options = list(pageLength = 50, dom = 't'))
  })
  
  # manual refresh button for league table (keeps reactives tidy)
  observeEvent(input$force_table_refresh, {
    output$league_table <- renderDT({
      req(vals$teams)
      datatable(league_table_df(), rownames = FALSE, options = list(dom='t'))
    })
    showNotification("Table refreshed", type = "message")
  })
  
}

shinyApp(ui, server)

