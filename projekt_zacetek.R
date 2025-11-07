# Soccer League Simulator - Shiny App z Monte Carlo simulacijo
# Shrani kot app.R in zaženi z: runApp('app.R') ali klikni "Run App" v RStudiu.

library(shiny)
library(DT)
library(glue)

ui <- fluidPage(
  tags$head(tags$style(HTML(".team-box { border:1px solid #e3e3e3; padding:10px; border-radius:6px; margin-bottom:8px; }
                                 .small-muted { font-size:12px; color:#777; }
                                 .match-card { border:1px solid #ddd; padding:8px; border-radius:6px; margin:6px 0; }
                                 details { margin-bottom:8px; }
                                 summary { font-weight:600; }
                                 "))),
  titlePanel("Soccer League Simulator"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Create teams"),
                 textInput("team_name", "Team name", placeholder = "e.g. Red Rovers"),
                 sliderInput("team_strength", "Team Strength (0-100)", min=0, max=100, value=50),
                 sliderInput("ref_bias", "Referee Bias (0-100, 50 neutral)", min=0, max=100, value=50),
                 sliderInput("fatigue", "Fatigue Factor (0-100)", min=0, max=100, value=10),
                 sliderInput("home_adv", "Home Field Advantage (0-100)", min=0, max=100, value=10),
                 sliderInput("morale", "Team Morale (0-100, 50 neutral)", min=0, max=100, value=50),
                 actionButton("add_team", "Add / Update Team", class = "btn-primary"),
                 actionButton("remove_last", "Remove Last Team", class = "btn-danger", style = "margin-left:6px;"),
                 hr(),
                 h4("League controls"),
                 numericInput("games_each_side", "Games per side (N) — each pair plays N home and N away matches (enter 1 = 1 home + 1 away)", value = 1, min = 1, max = 5, step = 1),
                 checkboxInput("use_weather", "Enable random weather effects", value = TRUE),
                 actionButton("generate_schedule", "Generate Schedule & Reset Results", class = "btn-success"),
                 actionButton("simulate_next_round", "Simulate Next Round", class = "btn-primary"),
                 actionButton("simulate_all", "Simulate All Remaining Matches", class = "btn-primary", style = "margin-left:6px;"),
                 actionButton("reset_all", "Reset All", class = "btn-warning", style = "margin-left:6px;"),
                 hr(),
                 h4("Monte Carlo simulacija"),
                 numericInput("n_sim", "Število simulacij:", value = 1000, min = 1),
                 actionButton("simulate_montecarlo", "Zaženi Monte Carlo", class = "btn-primary"),
                 hr(),
                 h5("Notes"),
                 p(class="small-muted", "This app holds all data in memory and resets when you refresh the page. Use 'Games per side' to control how many home/away meetings each pair has.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Teams", value = "teams",
                 br(),
                 fluidRow(column(12, uiOutput("teams_list"))),
                 br(),
                 DTOutput("teams_dt")
        ),
        tabPanel("Fixtures", value = "fixtures",
                 br(),
                 uiOutput("fixtures_ui")
        ),
        tabPanel("Results & Table", value = "table",
                 br(),
                 DTOutput("league_table"),
                 br(),
                 h4("Match Results by Round"),
                 uiOutput("results_by_round_ui")
        ),
        tabPanel("Monte Carlo Povprečja",
                 br(),
                 DTOutput("montecarlo_table_out"))
      )
    )
  )
)

server <- function(input, output, session){
  vals <- reactiveValues(
    teams = list(),
    fixtures = NULL,
    results = NULL,
    montecarlo_table = NULL
  )
  
  ## --- Team management ---
  observeEvent(input$add_team, {
    name <- trimws(input$team_name)
    if(name == ""){
      showNotification("Team name cannot be blank", type = "error")
      return()
    }
    existing <- sapply(vals$teams, function(t) t$name)
    team_entry <- list(name = name,
                       strength = as.numeric(input$team_strength),
                       ref_bias = as.numeric(input$ref_bias),
                       fatigue = as.numeric(input$fatigue),
                       home_adv = as.numeric(input$home_adv),
                       morale = as.numeric(input$morale),
                       matches_played = 0)
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
          span(class="small-muted", glue("Strength: {t$strength} | Ref bias: {t$ref_bias} | Fatigue: {t$fatigue} | Home adv: {t$home_adv} | Morale: {t$morale} | Matches played: {t$matches_played}"))
      )
    })
  })
  
  output$teams_dt <- renderDT({
    if(length(vals$teams)==0) return(NULL)
    df <- do.call(rbind, lapply(vals$teams, function(t) data.frame(Name=t$name, Strength=t$strength, RefBias=t$ref_bias, Fatigue=t$fatigue, HomeAdv=t$home_adv, Morale=t$morale, MatchesPlayed=t$matches_played, stringsAsFactors=FALSE)))
    datatable(df, rownames=FALSE, options = list(dom='t'))
  })
  
  observeEvent(input$reset_all, {
    vals$teams <- list(); vals$fixtures <- NULL; vals$results <- NULL; vals$montecarlo_table <- NULL
    showNotification("League reset", type = "message")
  })
  
  ## --- Schedule generation (balanced round-robin) ---
  generate_round_robin <- function(team_names, games_each_side = 1){
    teams <- team_names
    n <- length(teams)
    added_bye <- FALSE
    if(n %% 2 == 1){
      teams <- c(teams, "BYE")
      n <- n + 1
      added_bye <- TRUE
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
  
  observeEvent(input$generate_schedule, {
    n <- length(vals$teams)
    if(n < 3){ showNotification("Please add at least 3 teams before generating schedule", type = "error"); return() }
    team_names <- sapply(vals$teams, function(x) x$name)
    fixtures <- generate_round_robin(team_names, games_each_side = as.integer(input$games_each_side))
    vals$fixtures <- fixtures
    vals$results <- data.frame(match_id = fixtures$match_id, home_goals = NA_integer_, away_goals = NA_integer_, weather = NA_character_, played = FALSE, stringsAsFactors = FALSE)
    for(i in seq_along(vals$teams)) vals$teams[[i]]$matches_played <- 0
    showNotification("Schedule generated. Use the simulate buttons to play matches.", type = "message")
  })
  
  ## --- Simulation core ---
  simulate_matches <- function(rounds_to_simulate = NULL){
    req(vals$fixtures)
    fixtures <- vals$fixtures
    results <- vals$results
    
    teams_df <- do.call(rbind, lapply(vals$teams, function(t) data.frame(name=t$name,strength=t$strength,ref_bias=t$ref_bias,fatigue=t$fatigue,home_adv=t$home_adv,morale=t$morale,matches_played=t$matches_played, stringsAsFactors=FALSE)))
    
    to_sim_idx <- seq_len(nrow(fixtures))
    if(!is.null(rounds_to_simulate)){
      to_sim_idx <- which(fixtures$round %in% rounds_to_simulate)
    }
    
    for(i in to_sim_idx){
      mid <- fixtures$match_id[i]
      if(isTRUE(results$played[results$match_id==mid])) next
      
      home_name <- fixtures$home[i]; away_name <- fixtures$away[i]
      home_t <- teams_df[teams_df$name==home_name, ]
      away_t <- teams_df[teams_df$name==away_name, ]
      
      adjust_strength <- function(base, matches_played, fatigue){
        return(max(1, base - matches_played * (fatigue/40)))
      }
      home_eff <- adjust_strength(as.numeric(home_t$strength), as.numeric(home_t$matches_played), as.numeric(home_t$fatigue))
      away_eff <- adjust_strength(as.numeric(away_t$strength), as.numeric(away_t$matches_played), as.numeric(away_t$fatigue))
      
      ref_factor_home <- (as.numeric(home_t$ref_bias)-50)/100
      ref_factor_away <- (as.numeric(away_t$ref_bias)-50)/100
      
      base_lambda_home <- 0.5 + (home_eff/100) * 1.2 + (as.numeric(home_t$home_adv)/100) * 0.4
      base_lambda_away <- 0.5 + (away_eff/100) * 1.0
      
      morale_home <- as.numeric(home_t$morale)/50
      morale_away <- as.numeric(away_t$morale)/50
      
      base_lambda_home <- base_lambda_home * morale_home * (1 + ref_factor_home)
      base_lambda_away <- base_lambda_away * morale_away * (1 + ref_factor_away)
      
      weather_str <- "Clear"
      if(isTRUE(input$use_weather)){
        w <- sample(c("Clear","Rain","Wind","Storm"), 1, prob = c(0.6,0.2,0.15,0.05))
        weather_str <- w
        if(w == "Rain"){
          base_lambda_home <- base_lambda_home * 0.9
          base_lambda_away <- base_lambda_away * 0.9
        } else if(w == "Wind"){
          base_lambda_home <- base_lambda_home * 0.95
          base_lambda_away <- base_lambda_away * 0.95
        } else if(w == "Storm"){
          base_lambda_home <- base_lambda_home * 0.7
          base_lambda_away <- base_lambda_away * 0.7
        }
      }
      
      lambda_home <- max(0.05, base_lambda_home)
      lambda_away <- max(0.05, base_lambda_away)
      
      home_goals <- rpois(1, lambda_home)
      away_goals <- rpois(1, lambda_away)
      
      results$home_goals[results$match_id==mid] <- home_goals
      results$away_goals[results$match_id==mid] <- away_goals
      results$weather[results$match_id==mid] <- weather_str
      results$played[results$match_id==mid] <- TRUE
      
      for(j in seq_along(vals$teams)){
        if(vals$teams[[j]]$name == home_name || vals$teams[[j]]$name == away_name){
          vals$teams[[j]]$matches_played <- vals$teams[[j]]$matches_played + 1
        }
      }
      teams_df <- do.call(rbind, lapply(vals$teams, function(t) data.frame(name=t$name,strength=t$strength,ref_bias=t$ref_bias,fatigue=t$fatigue,home_adv=t$home_adv,morale=t$morale,matches_played=t$matches_played, stringsAsFactors=FALSE)))
    }
    vals$results <- results
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
  
  ## --- League table calculation ---
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
            div(class="match-card", glue("{matches$home[i]} {res$home_goals}-{res$away_goals} {matches$away[i]} ({res$weather})"))
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
  
  ## --- Monte Carlo simulacija ---
  observeEvent(input$simulate_montecarlo, {
    req(vals$fixtures)
    n_sim <- as.integer(input$n_sim)
    if (is.na(n_sim) || n_sim < 1) {
      showNotification("Vnesi pozitivno število simulacij", type = "error")
      return()
    }
    
    showNotification(glue("Začenjam Monte Carlo simulacijo ({n_sim} ponovitev)..."), type = "message")
    
    team_names <- sapply(vals$teams, function(x) x$name)
    aggregate_results <- data.frame(
      Team = team_names,
      Points = 0, GF = 0, GA = 0, GD = 0
    )
    
    for (sim in seq_len(n_sim)) {
      # reset lig
      fixtures <- vals$fixtures
      results <- data.frame(
        match_id = fixtures$match_id,
        home_goals = NA_integer_,
        away_goals = NA_integer_,
        weather = NA_character_,
        played = FALSE,
        stringsAsFactors = FALSE
      )
      vals_temp <- vals
      vals_temp$results <- results
      vals_temp$teams <- lapply(vals$teams, function(t){ t$matches_played <- 0; t })
      
      for(r in sort(unique(fixtures$round))){
        simulate_matches(rounds_to_simulate = r)
      }
      
      tbl <- league_table_df()
      
      for (team in team_names) {
        aggregate_results[aggregate_results$Team == team, "Points"] <-
          aggregate_results[aggregate_results$Team == team, "Points"] + tbl$Points[tbl$Team == team]
        aggregate_results[aggregate_results$Team == team, "GF"] <-
          aggregate_results[aggregate_results$Team == team, "GF"] + tbl$GF[tbl$Team == team]
        aggregate_results[aggregate_results$Team == team, "GA"] <-
          aggregate_results[aggregate_results$Team == team, "GA"] + tbl$GA[tbl$Team == team]
        aggregate_results[aggregate_results$Team == team, "GD"] <-
          aggregate_results[aggregate_results$Team == team, "GD"] + tbl$GD[tbl$Team == team]
      }
    }
    
    aggregate_results$Points <- aggregate_results$Points / n_sim
    aggregate_results$GF <- aggregate_results$GF / n_sim
    aggregate_results$GA <- aggregate_results$GA / n_sim
    aggregate_results$GD <- aggregate_results$GD / n_sim
    
    aggregate_results <- aggregate_results[order(-aggregate_results$Points, -aggregate_results$GD, -aggregate_results$GF), ]
    aggregate_results$Position <- seq_len(nrow(aggregate_results))
    
    vals$montecarlo_table <- aggregate_results
    showNotification("Monte Carlo simulacija končana!", type = "message")
  })
  
  output$montecarlo_table_out <- renderDT({
    req(vals$montecarlo_table)
    datatable(vals$montecarlo_table, rownames = FALSE, options = list(pageLength = 50, dom = 't'))
  })
}


shinyApp(ui, server)
