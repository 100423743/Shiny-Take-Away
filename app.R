#### SHINY TAKE-AWAY ASSIGNMENT - DATA TIDYING####
## IGNACIO MEDINA DE ANDRÉS ##

# LIBRARIES
library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(plotly)
library(magrittr)
library(DT)

## LOAD AND PRE-PROCESSING
df <- read.csv("https://query.data.world/s/457fikckeqdoemry75fqfhjoqwtrxv", 
               header=TRUE, stringsAsFactors=FALSE)
df <- df[,-c(1,3,4,8,11,14:20,22,24:30,43:92)]
df$club_position[df$club_position %in% c("GK")] <- "GOALKEEPER"
df$club_position[df$club_position %in% c("LCB","RCB","LB","RB","CB",
                                         "LWB","RWB")] <- "DEFENDER"
df$club_position[df$club_position %in% c("RW","LCM","CDM","LDM","CAM",
                                         "RAM","LW","LAM","CM","RM",
                                         "LM","RDM","RCM")] <- "MIDFIELDER"
df$club_position[df$club_position %in% c("LS","ST","RS","LF","RF",
                                         "CF")] <- "STRIKER"
df <- df%>%dplyr::filter(club_position=="GOALKEEPER"|club_position=="DEFENDER"|
                             club_position=="MIDFIELDER"|club_position=="STRIKER")
df <- df[complete.cases(df),]
df %<>% mutate_at(c("nationality", "club_team", "club_position"), as.factor)
df %<>% rename(Name=name, Age=age, Height=height_cm, Weight=weight_kgs, Nationality=nationality, 
               Overall=overall_rating, Price=value_euro, Wage=wage_euro, Team=club_team, Position=club_position,
               Crossing=crossing, Finishing=finishing, Heading=heading_accuracy, 
               ShortPassing=short_passing, Volleys=volleys, Dribbling=dribbling, 
               Curve=curve, Freekick=freekick_accuracy, LongPassing=long_passing,
               Control=ball_control, Acceleration=acceleration, Speed=sprint_speed)
  
list_choices_overall <- colnames(df[,c(7,22,14,11,12,13,16,20)])
list_choices_stats <- colnames(df[,c(3,4,6,8,11:22)])
list_choices_top <- colnames(df[,c(6,7,22,14,18,12)])
model_price <- lm(Price ~ Age+Wage+Speed+Control++Dribbling+ShortPassing+LongPassing,data=df)

## PANELS
dataPanel_1 <- tabPanel("Players Rating",
                        fluidPage(
                          sidebarLayout(sidebarPanel(
                            selectInput("select_overall", label = h3("Players Overall Rating by:"), 
                                        choices = list_choices_overall,
                                        selected = 1)
                          ), mainPanel(
                            h3("Overall Rating"),
                            plotOutput(outputId = "Overall_plot", click="point_click"),
                            h4("Click on a point to know the player:"),
                            tableOutput("table_point")
                          )
                          ))
                        
) # tabPanel

dataPanel_2 <- tabPanel("Players Stats",
                        fluidPage(
                          sidebarLayout(sidebarPanel(
                            selectInput("select_stats", label = h3("Players Stats by Position:"), 
                                        choices = list_choices_stats,
                                        selected = 1)
                          ), mainPanel(
                            h3("Player Stats"),
                            plotly::plotlyOutput(outputId = "Stats")
                          )
                          ))
                        
) # tabPanel

dataPanel_3 <- tabPanel("Price Calculator",
                        useShinyjs(),
                        sidebarLayout(position = "right",
                                      sidebarPanel(
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Age"),
                                          column(12,
                                                 sliderInput("n_age", label="", min = min(df$Age), max = max(df$Age), 
                                                             value =29, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Wage (€/year)"),
                                          column(12,
                                                 sliderInput("n_wage", label="", min = min(df$Wage), max = max(df$Wage), 
                                                             value = (max(df$Wage)+min(df$Wage))/2, step = 0.01)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Sprint Speed"),
                                          column(12,
                                                 sliderInput("n_speed", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Ball Control"),
                                          column(12,
                                                 sliderInput("n_control", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Dribbling Skills"),
                                          column(12,
                                                 sliderInput("n_dribbling", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Short Pass"),
                                          column(12,
                                                 sliderInput("n_pass", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 14px; margin-bottom: 0px;", "Long Pass"),
                                          column(12,
                                                 sliderInput("n_lpass", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Player Price Calculator", htmlOutput("Calculator")),
                                                    tabPanel("Model Summary", verbatimTextOutput("Summary"))
                                                    ) # tabsetPanel
                                        ) # mainPanel
                                      ) # sidebarLayout
) # tabPanel
                        

dataPanel_4 <- tabPanel("Top Players",
                        fluidPage(
                            mainPanel(
                              h3("Top Players Ranking"),
                              dataTableOutput(outputId="Top")
                            )
                          )
)

# UI
ui <- navbarPage("FIFA Football Players (Shiny Take-Away Assignment App)",
                 theme = shinytheme("flatly"),  
                 dataPanel_1,
                 dataPanel_2,
                 dataPanel_3,
                 dataPanel_4)
    
    

# SERVER
server <- function(input, output) {
  
  # 1
  output$Overall_plot <- renderPlot({
    ggplot(df, aes(x = .data[[input$select_overall]], y = Overall, color = Position)) + geom_point() + 
      labs(colour = "Position") + scale_x_continuous(labels = scales::comma)
  })
  
  output$table_point <- renderTable({
    nearPoints(df, coordinfo = input$select_overall, yvar = "Overall",
               input$point_click, maxpoints = 1)
  })
  # 2
  output$Stats <- plotly::renderPlotly({
    ggplot(df, aes(x = Position, y = .data[[input$select_stats]], fill = Position)) +
      geom_boxplot(alpha = 0.6) + labs(x = "Position", fill = "Position")
  })  
  
  # 3 
  output$Calculator <- renderText({
    
    price <- summary(model_price)$coef[1,1] + 
      summary(model_price)$coef[2,1]*input$n_age + summary(model_price)$coef[3,1]*input$n_wage + 
      summary(model_price)$coef[4,1]*input$n_speed + summary(model_price)$coef[5,1]*input$n_control +
      summary(model_price)$coef[6,1]*input$n_dribbling + summary(model_price)$coef[7,1]*input$n_pass +
      summary(model_price)$coef[8,1]*input$n_lpass
    
    paste("Hi there! In case you are the player transfer manager of a football club, you can now find out how much would it cost to sign a desired player in terms of his/her stats you are most interested in at the moment (tunning them on the right panel).", 
          "<br>","<br>", 
          "This final player price estimation has been computed from the FIFA (Fédération Internationale de Football Association) stats assigned for the 2019/20 season.",
          "<br>","<br>", 
          "So, after tunning these player stats, the estimated", "<b>", "signing price", "</b>", "for a player with the following characteristics:",
          "<br>",
          "<b>", input$n_age, "</b>", "years old,",
          "<br>",
          "earning", "<b>", input$n_wage, "</b>", "€ per year,",
          "<br>",
          "<b>", input$n_speed, "</b>", "of speed rating,",
          "<br>",
          "<b>", input$n_control, "</b>", "of ball control rating,",
          "<br>",
          "<b>", input$n_dribbling, "</b>", "of dribbling rating,",
          "<br>",
          "<b>", input$n_pass, "</b>", "of short pass rating,",
          "<br>",
          "and", "<b>", input$n_lpass, "</b>", "of finishing skills",
          "<br>",
          "would be around:", "<b>", round(price,2), "</b>", "€")
  })
  
  output$Summary <- renderPrint(summary(model_price))
  
  # 4
  
  output$Top <- renderDataTable({df[,c(1,2,10,9,5,6,7,22,14,18,12)]})
  
}  
# Run the application 
shinyApp(ui = ui, server = server)
