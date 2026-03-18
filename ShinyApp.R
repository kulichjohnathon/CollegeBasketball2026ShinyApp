library(shiny)
library(dplyr)
library(DT)
library(rvest)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggimage)
library(png)
library(plotly)
library(base64enc)
library(shinydashboard)

# CBB Advanced Stats
cbb_adv_url <- read_html("https://www.sports-reference.com/cbb/seasons/men/2026-advanced-school-stats.html")

tables_adv <- cbb_adv_url %>% html_nodes("table")

cbb_adv_data <- tables_adv[[1]] %>%
  html_table(fill = TRUE)

# Make the first row the headers
colnames(cbb_adv_data) <- cbb_adv_data[1, ]

# Get rid of unwanted rows
cbb_adv_data <- cbb_adv_data[-which(cbb_adv_data[,1]=="Rk"),]
cbb_adv_data <- cbb_adv_data[-which(cbb_adv_data[,1]==""),]

# Get rid of N/A columns
cbb_adv_clean <- 
  cbb_adv_data %>%
  select(
    School, SRS, SOS, Pace, ORtg, FTr, `3PAr`, `TS%`, `TRB%`, `AST%`, `STL%`, `BLK%`,
    `eFG%`, `TOV%`, `ORB%`, `FT/FGA`
  )

# Make all numeric
cols_to_convert_adv <- c("SRS", "SOS", "Pace", "ORtg", "FTr", "3PAr", "TS%", "TRB%", "AST%", "STL%", "BLK%",
                         "eFG%", "TOV%", "ORB%", "FT/FGA")
cbb_adv_clean[cols_to_convert_adv] <- lapply(cbb_adv_clean[cols_to_convert_adv], as.numeric)

# Add logo to cbb advanced
ncaa <- c("NCAA")

cbb_adv_clean <- cbb_adv_clean %>%
  filter(str_sub(School, -4, -1) %in% ncaa)

cbb_adv_clean$School <- substr(cbb_adv_clean$School, 1, nchar(cbb_adv_clean$School) - 5)

cbb_adv_clean$logo <- paste0(Path, cbb_per_game$School, ".png")

# Add rankings
cbb_adv_rank <- cbb_adv_clean
cbb_adv_rank$SRSRank <- rank(-cbb_adv_rank$SRS, ties.method = "first")
cbb_adv_rank$OffEffRank <- rank(-cbb_adv_rank$ORtg, ties.method = "first")
cbb_adv_rank$PaceRank <- rank(-cbb_adv_rank$Pace, ties.method = "first")

# CBB Advanced Opponent Stats
cbb_adv_opp_url <- read_html("https://www.sports-reference.com/cbb/seasons/men/2026-advanced-opponent-stats.html")

tables_adv_opp <- cbb_adv_opp_url %>% html_nodes("table")

cbb_adv_opp_data <- tables_adv_opp[[1]] %>%
  html_table(fill = TRUE)

# Make the first row the headers
colnames(cbb_adv_opp_data) <- cbb_adv_opp_data[1, ]

# Get rid of unwanted rows
cbb_adv_opp_data <- cbb_adv_opp_data[-which(cbb_adv_opp_data[,1]=="Rk"),]
cbb_adv_opp_data <- cbb_adv_opp_data[-which(cbb_adv_opp_data[,1]==""),]

# Get rid of N/A columns
cbb_adv_opp_clean <- 
  cbb_adv_opp_data %>%
  select(
    School, Pace, ORtg, FTr, `3PAr`, `TS%`, `TRB%`, `AST%`, `STL%`, `BLK%`, `eFG%`, `TOV%`, `ORB%`, `FT/FGA`
  )

# Make all numeric
cols_to_convert_adv_opp <- c("Pace", "ORtg", "FTr", "3PAr", "TS%", "TRB%", "AST%", "STL%", "BLK%", "eFG%", "TOV%", "ORB%", "FT/FGA")
cbb_adv_opp_clean[cols_to_convert_adv_opp] <- lapply(cbb_adv_opp_clean[cols_to_convert_adv_opp], as.numeric)

ncaa <- c("NCAA")

cbb_adv_opp_clean <- cbb_adv_opp_clean %>%
  filter(str_sub(School, -4, -1) %in% ncaa)

cbb_adv_opp_clean$School <- substr(cbb_adv_opp_clean$School, 1, nchar(cbb_adv_opp_clean$School) - 5)

# Rankings
cbb_opp_rank <- cbb_adv_opp_clean
cbb_opp_rank$DefRatingRank <- rank(cbb_opp_rank$ORtg, ties.method = "first")

# CBB Regular Stats
cbb_url <- read_html("https://www.sports-reference.com/cbb/seasons/men/2026-school-stats.html")

tables <- cbb_url %>% html_nodes("table")

cbb_data <- tables[[1]] %>%
  html_table(fill = TRUE)

# Make the first row the headers
colnames(cbb_data) <- cbb_data[1, ]

# Get rid of unwanted rows
cbb_data <- cbb_data[-which(cbb_data[,1]=="Rk"),]
cbb_data <- cbb_data[-which(cbb_data[,1]==""),]

names(cbb_data) <- make.names(names(cbb_data), unique = TRUE)

# Get rid of N/A columns
cbb_clean <- 
  cbb_data %>%
  select(
    Rk, School, G, W, L, `W.L.`, SRS, SOS, `Tm.`, `Opp.`, MP,
    FG, FGA, `FG.`, `X3P`, `X3PA`, `X3P.`,
    FT, FTA, `FT.`, ORB, TRB, AST,
    BLK, TOV, PF
  )

# Make all numeric
cols_to_convert <- c("Rk", "G", "W", "L", "W.L.", "SRS", "SOS", "Tm.",
                     "Opp.", "MP", "FG", "FGA", "FG.", "X3P", "X3PA", "X3P.",
                     "FT", "FTA", "FT.", "ORB", "TRB", "AST", "BLK", "TOV", "PF")
cbb_clean[cols_to_convert] <- lapply(cbb_clean[cols_to_convert], as.numeric)


# Figure out which teams average the most ppg
cbb_clean$PPG <- round(cbb_clean$Tm./cbb_clean$G, 1)

cbb_ppg <- cbb_clean %>%
  select(School, PPG)

cbb_ppg$Percentile <- percent_rank(cbb_ppg$PPG)

# Create a full per game stat sheet
cbb_clean$OppPPG <- round(cbb_clean$Opp./cbb_clean$G, 1)
cbb_clean$`FG/G` <- round(cbb_clean$FG/cbb_clean$G, 1)
cbb_clean$`FGA/G` <- round(cbb_clean$FGA/cbb_clean$G, 1)
cbb_clean$`3P/G` <- round(cbb_clean$X3P/cbb_clean$G, 1)
cbb_clean$`3PA/G` <- round(cbb_clean$X3PA/cbb_clean$G, 1)
cbb_clean$`FT/G` <- round(cbb_clean$FT/cbb_clean$G, 1)
cbb_clean$`FTA/G` <- round(cbb_clean$FTA/cbb_clean$G, 1)
cbb_clean$`ORB/G` <- round(cbb_clean$ORB/cbb_clean$G, 1)
cbb_clean$`TRB/G` <- round(cbb_clean$TRB/cbb_clean$G, 1)
cbb_clean$`AST/G` <- round(cbb_clean$AST/cbb_clean$G, 1)
cbb_clean$`BLK/G` <- round(cbb_clean$BLK/cbb_clean$G, 1)
cbb_clean$`TOV/G` <- round(cbb_clean$TOV/cbb_clean$G, 1)
cbb_clean$`PF/G` <- round(cbb_clean$PF/cbb_clean$G, 1)

cbb_per_game <- cbb_clean %>%
  select(School, SOS, PPG, OppPPG, `FG/G`, `FGA/G`, `FG.`, `3P/G`, `3PA/G`, `X3P.`, `FT/G`,
         `FTA/G`, `FT.`, `ORB/G`, `TRB/G`, `AST/G`, `BLK/G`, `TOV/G`, `PF/G`)

# Rename Columns
cbb_per_game <- rename(cbb_per_game, c('Points Per Game' = 'PPG',
                                       'Opponent Points Per Game' = 'OppPPG',
                                       'Field Goals Per Game' = 'FG/G',
                                       'Field Goal Attempts Per Game' = 'FGA/G',
                                       'Field Goal Percentage' = 'FG.',
                                       'Three Point Field Goals Per Game' = '3P/G',
                                       'Three Point Field Goal Attempts Per Game' = '3PA/G',
                                       'Three Point Percentage' = 'X3P.',
                                       'Free Throws Per Game' = 'FT/G',
                                       'Free Throw Attempts Per Game' = 'FTA/G',
                                       'Free Throw Percentage' = 'FT.',
                                       'Offensive Rebounds Per Game' = 'ORB/G',
                                       'Total Rebounds Per Game' = 'TRB/G',
                                       'Assists Per Game' = 'AST/G',
                                       'Blocks Per Game' = 'BLK/G',
                                       'Turnovers Per Game' = 'TOV/G',
                                       'Personal Fouls Per Game' = 'PF/G'))


# # Get MSU image
# # Path
# Path <- "C:\\Users\\johna\\Documents\\Sports Models\\College Basketball 2026\\College Basketball Logos\\"
# msu <- paste0(Path, "Michigan State.png")
# 
# msu_stats <- cbb_per_game %>%
#   filter(School == "Michigan State")
# 
# msu_stats$logo <- msu
# 
# ggplot(msu_stats, aes(x = `Points Per Game`, y = `Opponent Points Per Game`, image = logo)) +
#   geom_point() +
#   geom_image(size = 0.03)

# Add logo/scoring margin to dataframe
cbb_per_game$ScoringMargin <- cbb_per_game$`Points Per Game` - cbb_per_game$`Opponent Points Per Game`
cbb_per_game$logo <- paste0(Path, cbb_per_game$School, ".png")

ncaa <- c("NCAA")

cbb_per_game <- cbb_per_game %>%
  filter(str_sub(School, -4, -1) %in% ncaa)

cbb_per_game$School <- substr(cbb_per_game$School, 1, nchar(cbb_per_game$School) - 5)
cbb_per_game$logo <- paste0(Path, cbb_per_game$School, ".png")

cbb_per_game_rank <- cbb_per_game
cbb_per_game_rank$PPGRank <- rank(-cbb_per_game_rank$`Points Per Game`, ties.method = "first")
cbb_per_game_rank$PARank <- rank(cbb_per_game_rank$`Opponent Points Per Game`, ties.method = "first")
cbb_per_game_rank$FGRank <- rank(-cbb_per_game_rank$`Field Goal Percentage`, ties.method = "first")
cbb_per_game_rank$SOSRank <- rank(-cbb_per_game_rank$SOS, ties.method = "first")
cbb_per_game_rank$`3PRank` <- rank(-cbb_per_game_rank$`Three Point Percentage`, ties.method = "first")
cbb_per_game_rank$FTRank <- rank(-cbb_per_game_rank$`Free Throw Percentage`, ties.method = "first")
cbb_per_game_rank$ORBRank <- rank(-cbb_per_game_rank$`Offensive Rebounds Per Game`, ties.method = "first")
cbb_per_game_rank$TRBRank <- rank(-cbb_per_game_rank$`Total Rebounds Per Game`, ties.method = "first")
cbb_per_game_rank$ASTRank <- rank(-cbb_per_game_rank$`Assists Per Game`, ties.method = "first")


# Get Offensive Efficiency vs Defensive Efficiency
off_def_efficiency <- data_frame(School = cbb_adv_opp_clean$School, def_eff = cbb_adv_opp_clean$ORtg, off_eff = cbb_adv_clean$ORtg)
off_def_efficiency$logo <- paste0(Path, off_def_efficiency$School, ".png")

# UI
ui <- fluidPage(
  titlePanel("March Madness Stats"),
  br(),
    mainPanel(
      textOutput("Source"),
      tabsetPanel(
        tabPanel("Team Dashboard", br(), fluidRow(
          column(12, selectInput(inputId = "school", label = "Select School",
                             choices = sort(unique(cbb_per_game$School)),
                             selected = sort(unique(cbb_per_game$School))[1]))
        ),
                 infoBoxOutput("SRS_rank"), infoBoxOutput("off_rtg"), infoBoxOutput("def_rtg"),
                 infoBoxOutput("SOS_rank"), infoBoxOutput("pace_rank"), infoBoxOutput("ppg_rank"), 
                 infoBoxOutput("PA_rank"), infoBoxOutput("fg_rank"), infoBoxOutput("3p_rank"),
                 infoBoxOutput("FT_rank"), infoBoxOutput("ORB_rank"), infoBoxOutput("TRB_rank"),
                 infoBoxOutput("AST_rank")),
        tabPanel("Team Comparison",
                 br(),
                 fluidRow(
                   column(6, selectInput("comp_school1", "Select Team 1",
                                         choices = sort(unique(cbb_per_game$School)),
                                         selected = sort(unique(cbb_per_game$School))[1])),
                   column(6, selectInput("comp_school2", "Select Team 2",
                                         choices = sort(unique(cbb_per_game$School)),
                                         selected = sort(unique(cbb_per_game$School))[2]))
                 ),
                 br(),
                 DT::DTOutput("comparison_table")
        ),
        tabPanel("Scoring Margin vs Strength of Schedule", br(), plotlyOutput("sm_vs_sos", width = "100%", height = "90vh")),
        tabPanel("Pace vs SRS", br(), plotlyOutput("pace_vs_srs", width = "100%", height = "90vh")),
        tabPanel("Offensive Efficiency vs Defensive Efficiency", br(), plotlyOutput("oe_vs_de", width = "100%", height = "90vh"))
      )
    )
  )

server <- function(input, output, session) {
  
  output$Source <- renderText({
    "Source: Sports Reference College Basketball Stats"
  })
  
  output$SRS_rank <- renderInfoBox({
    srs_rank <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(SRSRank)
    
    srs_value <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(SRS)
    
    infoBox(
      title = "Simple Rating System Rank",
      value = paste0("#", srs_rank),
      subtitle = paste0(srs_value),
      icon = icon("basketball"),
      color = "orange"
    )
  }
  )
  
  output$off_rtg <- renderInfoBox({
    off_eff_rank <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(OffEffRank)
    
    off_eff_value <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(ORtg)
    
    infoBox(
      title = "Offense Rating Rank",
      value = paste0("#", off_eff_rank),
      subtitle = paste0(off_eff_value, " Offensive Rating"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$def_rtg <- renderInfoBox({
    def_rtg_rank <- cbb_opp_rank %>%
      filter(School == input$school) %>%
      select(DefRatingRank)
    
    def_rtg_value <- cbb_opp_rank %>%
      filter(School == input$school) %>%
      select(ORtg)
    
    infoBox(
      title = "Defensive Rating Rank",
      value = paste0("#", def_rtg_rank),
      subtitle = paste0(def_rtg_value, " Defensive Rating"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$SOS_rank <- renderInfoBox({
    sos_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(SOSRank)
    
    sos_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(SOS)
    
    infoBox(
      title = "Strength of Schedule Rank",
      value = paste0("#", sos_rank),
      subtitle = paste0(sos_value, " SOS"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$pace_rank <- renderInfoBox({
    pace_rank <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(PaceRank)
    
    pace_value <- cbb_adv_rank %>%
      filter(School == input$school) %>%
      select(Pace)
    
    infoBox(
      title = "Pace Rank",
      value = paste0("#", pace_rank),
      subtitle = paste0(pace_value, " Pace"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$ppg_rank <- renderInfoBox({
    ppg_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(PPGRank)
    
    ppg_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Points Per Game`)
    
    infoBox(
      title = "Points Per Game Rank",
      value = paste0("#", ppg_rank),
      subtitle = paste0(ppg_value, " PPG"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$PA_rank <- renderInfoBox({
    pa_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(PARank)
    
    pa_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Opponent Points Per Game`)
    
    infoBox(
      title = "Points Against Rank",
      value = paste0("#", pa_rank),
      subtitle = paste0(pa_value, " PA/G"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$fg_rank <- renderInfoBox({
    fg_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(FGRank)
    
    fg_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Field Goal Percentage`)
    
    infoBox(
      title = "Field Goal Rank",
      value = paste0("#", fg_rank),
      subtitle = paste0(fg_value, " FG%"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$`3p_rank` <- renderInfoBox({
    three_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`3PRank`)
    
    three_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Three Point Percentage`)
    
    infoBox(
      title = "Three Point Rank",
      value = paste0("#", three_rank),
      subtitle = paste0(three_value, " 3P%"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$FT_rank <- renderInfoBox({
    FT_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(FTRank)
    
    FT_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Free Throw Percentage`)
    
    infoBox(
      title = "Free Throw Rank",
      value = paste0("#", FT_rank),
      subtitle = paste0(FT_value, " FT%"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$ORB_rank <- renderInfoBox({
    ORB_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(ORBRank)
    
    ORB_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Offensive Rebounds Per Game`)
    
    infoBox(
      title = "Offensive Rebounds Rank",
      value = paste0("#", ORB_rank),
      subtitle = paste0(ORB_value, " ORB/G"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$TRB_rank <- renderInfoBox({
    TRB_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(TRBRank)
    
    TRB_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Total Rebounds Per Game`)
    
    infoBox(
      title = "Total Rebounds Rank",
      value = paste0("#", TRB_rank),
      subtitle = paste0(TRB_value, " TRB/G"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  output$AST_rank <- renderInfoBox({
    AST_rank <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(ASTRank)
    
    AST_value <- cbb_per_game_rank %>%
      filter(School == input$school) %>%
      select(`Assists Per Game`)
    
    infoBox(
      title = "Assists Rank",
      value = paste0("#", AST_rank),
      subtitle = paste0(AST_value, " AST/G"),
      icon = icon("basketball"),
      color = "orange"
    )
  })
  
  # Convert logos to base64 ONCE on app load (not inside render)
  logos_b64 <- sapply(cbb_per_game$logo, function(path) {
    paste0("data:image/png;base64,", base64encode(path))
  })
  
  logos_b64_adv <- sapply(cbb_adv_clean$logo, function(path) {
    paste0("data:image/png;base64,", base64encode(path))
  })
  
  logos_b64_eff <- sapply(off_def_efficiency$logo, function(path) {
    paste0("data:image/png;base64,", base64encode(path))
  })
  
  output$sm_vs_sos <- renderPlotly({
    sos_vs_sm <- plot_ly(
      data = cbb_per_game,
      x = cbb_per_game$SOS,
      y = cbb_per_game$ScoringMargin,
      type = "scatter",
      mode = "markers",
      marker = list(opacity = 0),
      text = ~paste0("<b>", School, "</b><br>SOS: ", cbb_per_game$SOS, "<br>Margin: ", cbb_per_game$ScoringMargin),
      hoverinfo = "text"
    )
    
    # Add each logo as a layout image
    logo_images <- lapply(seq_len(nrow(cbb_per_game)), function(i) {
      list(
        source = logos_b64[i],
        x = cbb_per_game$SOS[i],
        y = cbb_per_game$ScoringMargin[i],
        xref = "x",
        yref = "y",
        sizex = diff(range(cbb_per_game$SOS)) * 0.04,    # Adjust logo size
        sizey = diff(range(cbb_per_game$ScoringMargin)) * 0.08,
        xanchor = "center",
        yanchor = "middle",
        layer = "above"
      )
    })
    
    sos_vs_sm <- sos_vs_sm %>% layout(
      images = logo_images,
      xaxis = list(title = "SOS"),
      yaxis = list(title = "Scoring Margin"),
      shapes = list(
        # Vertical mean line
        list(type = "line", x0 = mean(cbb_per_game$SOS), x1 = mean(cbb_per_game$SOS),
             y0 = min(cbb_per_game$ScoringMargin), y1 = max(cbb_per_game$ScoringMargin),
             line = list(color = "red", dash = "dash", width = 2)),
        # Horizontal mean line
        list(type = "line", x0 = min(cbb_per_game$SOS), x1 = max(cbb_per_game$SOS),
             y0 = mean(cbb_per_game$ScoringMargin), y1 = mean(cbb_per_game$ScoringMargin),
             line = list(color = "red", dash = "dash", width = 2))
      ),
      paper_bgcolor = "white",
      plot_bgcolor = "white"
    )
    
    sos_vs_sm
  })
    
    output$oe_vs_de <- renderPlotly({
      oe_vs_de <- plot_ly(
        data = off_def_efficiency,
        x = off_def_efficiency$off_eff,
        y = off_def_efficiency$def_eff,
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0),
        text = ~paste0("<b>", School, "</b><br>Offensive Effiency: ", off_def_efficiency$off_eff, "<br>Defensive Efficiency: ", off_def_efficiency$def_eff),
        hoverinfo = "text"
      ) %>%
        layout(yaxis = list(autorange = "reversed"))
      
      # Add each logo as a layout image
      logo_images_eff <- lapply(seq_len(nrow(off_def_efficiency)), function(i) {
        list(
          source = logos_b64_eff[i],
          x = off_def_efficiency$off_eff[i],
          y = off_def_efficiency$def_eff[i],
          xref = "x",
          yref = "y",
          sizex = diff(range(off_def_efficiency$off_eff)) * 0.04,    # Adjust logo size
          sizey = diff(range(off_def_efficiency$def_eff)) * 0.08,
          xanchor = "center",
          yanchor = "middle",
          layer = "above"
        )
      })
        
        oe_vs_de <- oe_vs_de %>% layout(
          images = logo_images_eff,
          xaxis = list(title = "Offensive Efficiency"),
          yaxis = list(title = "Defensive Efficiency"),
          shapes = list(
            # Vertical mean line
            list(type = "line", x0 = mean(off_def_efficiency$off_eff), x1 = mean(off_def_efficiency$off_eff),
                 y0 = min(off_def_efficiency$def_eff), y1 = max(off_def_efficiency$def_eff),
                 line = list(color = "red", dash = "dash", width = 2)),
            # Horizontal mean line
            list(type = "line", x0 = min(off_def_efficiency$off_eff), x1 = max(off_def_efficiency$off_eff),
                 y0 = mean(off_def_efficiency$def_eff), y1 = mean(off_def_efficiency$def_eff),
                 line = list(color = "red", dash = "dash", width = 2))
          ),
          paper_bgcolor = "white",
          plot_bgcolor = "white"
        )
        
        oe_vs_de
        })
    
    output$pace_vs_srs <- renderPlotly({
      srspace <- plot_ly(
        data = cbb_adv_clean,
        x = cbb_adv_clean$Pace,
        y = cbb_adv_clean$SRS,
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0),
        text = ~paste0("<b>", School, "</b><br>Pace: ", cbb_adv_clean$Pace, "<br>SRS: ", cbb_adv_clean$SRS),
        hoverinfo = "text"
      )
      
      # Add each logo as a layout image
      logo_images_pace <- lapply(seq_len(nrow(cbb_adv_clean)), function(i) {
        list(
          source = logos_b64_adv[i],
          x = cbb_adv_clean$Pace[i],
          y = cbb_adv_clean$SRS[i],
          xref = "x",
          yref = "y",
          sizex = diff(range(cbb_adv_clean$Pace)) * 0.04,    # Adjust logo size
          sizey = diff(range(cbb_adv_clean$SRS)) * 0.08,
          xanchor = "center",
          yanchor = "middle",
          layer = "above"
        )
      })
      
      srspace <- srspace %>% layout(
        images = logo_images_pace,
        xaxis = list(title = "Pace"),
        yaxis = list(title = "SRS"),
        shapes = list(
          # Vertical mean line
          list(type = "line", x0 = mean(cbb_adv_clean$Pace), x1 = mean(cbb_adv_clean$Pace),
               y0 = min(cbb_adv_clean$SRS), y1 = max(cbb_adv_clean$SRS),
               line = list(color = "red", dash = "dash", width = 2)),
          # Horizontal mean line
          list(type = "line", x0 = min(cbb_adv_clean$Pace), x1 = max(cbb_adv_clean$Pace),
               y0 = mean(cbb_adv_clean$SRS), y1 = mean(cbb_adv_clean$SRS),
               line = list(color = "red", dash = "dash", width = 2))
        ),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      )
      
      srspace
    })
        
        output$comparison_table <- DT::renderDT({
          req(input$comp_school1, input$comp_school2)
          req(input$comp_school1 != input$comp_school2)
          
          s1 <- input$comp_school1
          s2 <- input$comp_school2
          
          get_val <- function(df, school, col) {
            val <- df %>% filter(School == school) %>% pull(!!sym(col))
            if (length(val) == 0) return(NA_real_)
            as.numeric(val)
          }
          
          stats <- tibble(
            Stat = c(
              # Info box stats
              "SRS", "Offensive Rating", "Defensive Rating",
              "Points Per Game", "Opponent Points Per Game", "Field Goal %",
              # Per game stats
              "3-Pointers Per Game", "3-Point Attempts Per Game", "3-Point %",
              "Free Throws Per Game", "Free Throw Attempts Per Game", "Free Throw %",
              "Offensive Rebounds Per Game", "Total Rebounds Per Game",
              "Assists Per Game", "Blocks Per Game", "Turnovers Per Game", "Personal Fouls Per Game",
              # Advanced stats
              "Pace", "True Shooting %", "Effective FG%", "Turnover %",
              "Offensive Rebound %", "Free Throw Rate", "3-Point Attempt Rate",
              "Total Rebound %", "Assist %", "Steal %", "Block %"
            ),
            lower_is_better = c(
              FALSE, FALSE, TRUE,
              FALSE, TRUE, FALSE,
              FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE,
              FALSE, FALSE,
              FALSE, FALSE, TRUE, TRUE,
              FALSE, FALSE, FALSE, TRUE,
              FALSE, FALSE, FALSE,
              FALSE, FALSE, FALSE, FALSE
            ),
            v1 = c(
              get_val(cbb_adv_rank,      s1, "SRS"),
              get_val(cbb_adv_rank,      s1, "ORtg"),
              get_val(cbb_opp_rank,      s1, "ORtg"),
              get_val(cbb_per_game_rank, s1, "Points Per Game"),
              get_val(cbb_per_game_rank, s1, "Opponent Points Per Game"),
              get_val(cbb_per_game_rank, s1, "Field Goal Percentage"),
              get_val(cbb_per_game,      s1, "Three Point Field Goals Per Game"),
              get_val(cbb_per_game,      s1, "Three Point Field Goal Attempts Per Game"),
              get_val(cbb_per_game,      s1, "Three Point Percentage"),
              get_val(cbb_per_game,      s1, "Free Throws Per Game"),
              get_val(cbb_per_game,      s1, "Free Throw Attempts Per Game"),
              get_val(cbb_per_game,      s1, "Free Throw Percentage"),
              get_val(cbb_per_game,      s1, "Offensive Rebounds Per Game"),
              get_val(cbb_per_game,      s1, "Total Rebounds Per Game"),
              get_val(cbb_per_game,      s1, "Assists Per Game"),
              get_val(cbb_per_game,      s1, "Blocks Per Game"),
              get_val(cbb_per_game,      s1, "Turnovers Per Game"),
              get_val(cbb_per_game,      s1, "Personal Fouls Per Game"),
              get_val(cbb_adv_rank,      s1, "Pace"),
              get_val(cbb_adv_rank,      s1, "TS%"),
              get_val(cbb_adv_rank,      s1, "eFG%"),
              get_val(cbb_adv_rank,      s1, "TOV%"),
              get_val(cbb_adv_rank,      s1, "ORB%"),
              get_val(cbb_adv_rank,      s1, "FTr"),
              get_val(cbb_adv_rank,      s1, "3PAr"),
              get_val(cbb_adv_rank,      s1, "TRB%"),
              get_val(cbb_adv_rank,      s1, "AST%"),
              get_val(cbb_adv_rank,      s1, "STL%"),
              get_val(cbb_adv_rank,      s1, "BLK%")
            ),
            v2 = c(
              get_val(cbb_adv_rank,      s2, "SRS"),
              get_val(cbb_adv_rank,      s2, "ORtg"),
              get_val(cbb_opp_rank,      s2, "ORtg"),
              get_val(cbb_per_game_rank, s2, "Points Per Game"),
              get_val(cbb_per_game_rank, s2, "Opponent Points Per Game"),
              get_val(cbb_per_game_rank, s2, "Field Goal Percentage"),
              get_val(cbb_per_game,      s2, "Three Point Field Goals Per Game"),
              get_val(cbb_per_game,      s2, "Three Point Field Goal Attempts Per Game"),
              get_val(cbb_per_game,      s2, "Three Point Percentage"),
              get_val(cbb_per_game,      s2, "Free Throws Per Game"),
              get_val(cbb_per_game,      s2, "Free Throw Attempts Per Game"),
              get_val(cbb_per_game,      s2, "Free Throw Percentage"),
              get_val(cbb_per_game,      s2, "Offensive Rebounds Per Game"),
              get_val(cbb_per_game,      s2, "Total Rebounds Per Game"),
              get_val(cbb_per_game,      s2, "Assists Per Game"),
              get_val(cbb_per_game,      s2, "Blocks Per Game"),
              get_val(cbb_per_game,      s2, "Turnovers Per Game"),
              get_val(cbb_per_game,      s2, "Personal Fouls Per Game"),
              get_val(cbb_adv_rank,      s2, "Pace"),
              get_val(cbb_adv_rank,      s2, "TS%"),
              get_val(cbb_adv_rank,      s2, "eFG%"),
              get_val(cbb_adv_rank,      s2, "TOV%"),
              get_val(cbb_adv_rank,      s2, "ORB%"),
              get_val(cbb_adv_rank,      s2, "FTr"),
              get_val(cbb_adv_rank,      s2, "3PAr"),
              get_val(cbb_adv_rank,      s2, "TRB%"),
              get_val(cbb_adv_rank,      s2, "AST%"),
              get_val(cbb_adv_rank,      s2, "STL%"),
              get_val(cbb_adv_rank,      s2, "BLK%")
            )
          ) %>%
            mutate(
              winner = case_when(
                is.na(v1) | is.na(v2)      ~ "tie",
                lower_is_better & v1 < v2  ~ "team1",
                lower_is_better & v2 < v1  ~ "team2",
                !lower_is_better & v1 > v2 ~ "team1",
                !lower_is_better & v2 > v1 ~ "team2",
                TRUE                       ~ "tie"
              )
            ) %>%
            mutate(
              Category = c(
                rep("Info Box", 6),
                rep("Per Game", 12),
                rep("Advanced", 11)
              ),
              !!s1 := round(v1, 2),
              !!s2 := round(v2, 2)
            ) %>%
            select(Category, Stat, !!s1, !!s2, winner)
          
          # Row indices for each winner (1-based for DT)
          team1_rows <- which(stats$winner == "team1")
          team2_rows <- which(stats$winner == "team2")
          tie_rows   <- which(stats$winner == "tie")
          
          DT::datatable(
            stats %>% select(-winner),
            rownames = FALSE,
            options = list(
              dom = "t",
              pageLength = 50,
              columnDefs = list(list(className = "dt-center", targets = c(2, 3)))
            )
          ) %>%
            formatStyle(
              columns = c("Stat", s1, s2),
              valueColumns = "Stat",                          # dummy anchor — we use rowIndex
              target = "row",
              backgroundColor = styleEqual(
                stats$Stat,
                dplyr::case_when(
                  stats$winner == "team1" ~ "#d4edda",        # green  — team 1 wins
                  stats$winner == "team2" ~ "#f8d7da",        # red    — team 1 loses
                  TRUE                   ~ "#ffffff"           # white  — tie
                )
              )
            )
    })
        
}
        



shinyApp(ui = ui, server = server)
