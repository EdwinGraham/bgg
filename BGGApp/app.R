#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(data.table)

load("bggData.rda") # or load("BGGApp/bggData.rda")

dtGames <- allData$dtGames
dtLinks <- allData$dtLinks
dtRanks <- allData$dtRanks
dateOfExtract <- allData$date
rm(allData)

# Add label
setorder(dtRanks, Id, subGroup)
dtRanks2 <- dtRanks[!is.na(rank), .(rankText = paste0(paste0("; ", subGroup, " ", formatC(rank, format="d", big.mark = ",")), collapse="")), keyby=Id]
setkey(dtGames, Id)
dtGames <- merge(dtGames, dtRanks2, all.x=TRUE)
dtGames[is.na(rankText), rankText := ""]

dtGames[, label:=paste0("<b>", title, "</b> (", year, ")",
                        "<br><b>Rank:</b> ",
                        ifelse(is.na(rank), "Expansion",
                               paste0("Overall ", formatC(rank, format="d", big.mark = ","), rankText)),
                        "<br><b>Players:</b> ",
                        ifelse(minPlayers==maxPlayers, minPlayers, paste0(minPlayers, "-", maxPlayers)),
                        "; recommended ", ifelse(minPlayersRecommended==maxPlayersRecommended, minPlayersRecommended, paste0(minPlayersRecommended,
                                                                                                                             "-", maxPlayersRecommended)),
                        "; best ", numPlayersBest,
                        "<br><b>Playing time:</b> ", ifelse(minPlayTime==maxPlayTime, minPlayTime, paste0(minPlayTime, "-", maxPlayTime)), " mins")]

num <- nrow(dtGames)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(paste0("Visualisation of ", formatC(num, format="d", big.mark = ","), " most rated BGG games and expansions"))
  ,
  tags$h5(paste0("Data extracted: ", dateOfExtract))
  ,
  
  # Line break
  tags$br()
  ,
  
  # Buttons
  actionButton(inputId="overall", label="Overall")
  ,
  actionButton(inputId="abstract", label="Abstract")
  ,
  actionButton(inputId="childrens", label="Children's")
  ,
  actionButton(inputId="custom", label="Customiseable")
  ,
  actionButton(inputId="family", label="Family")
  ,
  actionButton(inputId="party", label="Party")
  ,
  actionButton(inputId="strategy", label="Strategy")
  ,
  actionButton(inputId="thematic", label="Thematic")
  ,
  actionButton(inputId="warGames", label="War games")
  ,
  
  # Line break
  tags$br()
  ,
  
  # Line break
  tags$br()
  ,
  
  # Sidebar for weight range
  sliderInput(inputId="weightRange",
              label="Select weight range:",
              min = 1,
              max = 4,
              value = c(1, 4),
              step = 0.01,
              round = FALSE)
  ,
  
  # Show the plot
  plotlyOutput("plot")
  ,
  
  # Line break
  tags$br()
  ,
  
  # Click on a point
  uiOutput("click")
  ,
  
  fluidRow(
    
    column(6,
           # Click on a point
           uiOutput("click2")      
    ),
    
    column(3,
           # Click on a point
           uiOutput("click3")      
    ),
    
    column(3,
           # Click on a point
           uiOutput("click4")      
    )
  )
)

# Define server logic required
server <- function(input, output) {
  
  # Data
  rv <- reactiveValues(dt = copy(dtGames)[, .(Id, rating=geekRating, numRatings, label, avWeight)])
  
  # Update data when buttons are pressed
  observeEvent(input$overall, { rv$dt <- copy(dtGames)[, .(Id, rating=geekRating, numRatings, label, avWeight)] })
  observeEvent(input$abstract, {
    dtSubGroup <- dtRanks[subGroup=="Abstract", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$childrens, {
    dtSubGroup <- dtRanks[subGroup=="Children's", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$custom, {
    dtSubGroup <- dtRanks[subGroup=="Customisable", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$family, {
    dtSubGroup <- dtRanks[subGroup=="Family", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$party, {
    dtSubGroup <- dtRanks[subGroup=="Party", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$strategy, {
    dtSubGroup <- dtRanks[subGroup=="Strategy", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$thematic, {
    dtSubGroup <- dtRanks[subGroup=="Thematic", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  observeEvent(input$warGames, {
    dtSubGroup <- dtRanks[subGroup=="War games", .(rating), keyby=Id]
    rv$dt <- merge(dtGames[, .(Id, numRatings, label, avWeight)], dtSubGroup)
  })
  
  # Plot
  output$plot <- renderPlotly({
    plot_ly(rv$dt[avWeight>=input$weightRange[1] &
                    avWeight<=input$weightRange[2]],
            x = ~numRatings,
            y = ~rating,
            hoverinfo = "text",
            text = ~label,
            type = "scatter",
            mode = "markers",
            key = ~Id,
            marker=list(color = ~avWeight,
                        cmin=0,
                        cmax=4,
                        colorscale=list(list(0, "rgb(158, 1, 66)"),
                                        list(0.1, "rgb(213, 62, 79)"),
                                        list(0.2, "rgb(244, 109, 67)"),
                                        list(0.3, "rgb(253, 174, 97)"),
                                        list(0.4, "rgb(254, 224, 139)"),
                                        list(0.5, "rgb(255, 255, 191)"),
                                        list(0.6, "rgb(230, 245, 152)"),
                                        list(0.7, "rgb(171, 221, 164)"),
                                        list(0.8, "rgb(102, 194, 165)"),
                                        list(0.9, "rgb(50, 136, 189)"),
                                        list(1, "rgb(94, 79, 162)")),
                        showscale=TRUE,
                        reversescale=TRUE,
                        colorbar=list(
                          title='Weight'
                        ))) %>%
      layout(title = paste0("BGG most rated ", formatC(num, format="d", big.mark = ","), " games and expansions"),
             xaxis = list(title = "Number of ratings",
                          type = "log"),
             yaxis = list(title = "BGG Rating",
                          range = c(3, 9)))
    
  })
  
  # Click events
  output$click <- renderUI({
    d <- event_data("plotly_click")
    if(is.null(d)) cat("Click a point to display details (double-click to clear)") else{
      tags$a(href=paste0("https://boardgamegeek.com/boardgame/", d$key, "/"), "Click to view this game on BoardGameGeek.")
    }
  })
  
  output$click2 <- renderPrint({
    d <- event_data("plotly_click")
    if(!is.null(d)){
      cat(paste0("<br>", dtGames[Id==d$key, label]))
    }
  })
  
  output$click3 <- renderPrint({
    d <- event_data("plotly_click")
    if(!is.null(d)){
      cat(paste0("<br><b>Categories</b><br>", paste0(dtLinks[Id==d$key & type=="boardgamecategory", value], collapse="<br>")))
    }
  })
  
  output$click4 <- renderPrint({
    d <- event_data("plotly_click")
    if(!is.null(d)){
      cat(paste0("<br><b>Game Mechanics</b><br>", paste0(dtLinks[Id==d$key & type=="boardgamemechanic", value], collapse="<br>")))
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)