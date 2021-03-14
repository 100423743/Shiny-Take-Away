#### SHINY TAKE-AWAY ASSIGNMENT - DATA TIDYING####
## IGNACIO MEDINA DE ANDRÉS ##

# LIBRARIES
library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(plotly)
library(magrittr)

## LOAD AND PRE-PROCESSING
df <- read.csv("https://query.data.world/s/457fikckeqdoemry75fqfhjoqwtrxv", 
               header=TRUE, stringsAsFactors=FALSE)
df <- df[,-c(1,3,4,8,11,13:20,22,24:30,43:92)]
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
               Overall=overall_rating, Price=value_euro, Team=club_team, Position=club_position,
               Crossing=crossing, Finishing=finishing, Heading=heading_accuracy, 
               ShortPassing=short_passing, Volleys=volleys, Dribbling=dribbling, 
               Curve=curve, Freekick=freekick_accuracy, LongPassing=long_passing,
               Control=ball_control, Acceleration=acceleration, Speed=sprint_speed)
  
list_choices_overall <- colnames(df[,c(7,21,11,12,13,15,17)])
list_choices_stats <- colnames(df[,c(3,4,6,7,10:21)])
model_price <- lm(Price ~ Overall+Age+Speed+Control+
                    ShortPassing+Dribbling+Finishing+
                    Height+Weight+Freekick, data=df)

## PANELS
dataPanel_1 <- tabPanel("Players Rating",
                        fluidPage(
                          sidebarLayout(sidebarPanel(
                            selectInput("select_overall", label = h3("Players Overall Rating by:"), 
                                        choices = list_choices_overall,
                                        selected = 1)
                          ), mainPanel(
                            h3("Overall Rating"),
                            plotOutput(outputId = "Overall", click = "plot_click")
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
                            plotOutput(outputId = "Stats", click = "plot_click")
                          )
                          ))
                        
) # tabPanel

dataPanel_3 <- tabPanel("Price Calculator",
                        useShinyjs(),
                        sidebarLayout(position = "right",
                                      sidebarPanel(
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Age"),
                                          column(12,
                                                 sliderInput("n_age", label="", min = min(df$Age), max = max(df$Age), 
                                                             value = min(df$Age), step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Overall desired Rating"),
                                          column(12,
                                                 sliderInput("n_rating", label="", min = min(df$Overall), max = max(df$Overall), 
                                                             value = (max(df$Overall)+min(df$Overall))/2, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Height (cm)"),
                                          column(12,
                                                 sliderInput("n_height", label="", min = min(df$Height), max = max(df$Height), 
                                                             value = (max(df$Height)+min(df$Height))/2, step = 0.01)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Weight (kg)"),
                                          column(12,
                                                 sliderInput("n_weight", label="", min = min(df$Weight), max = max(df$Weight), 
                                                             value = (max(df$Weight)+min(df$Weight))/2, step = 0.1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Sprint Speed"),
                                          column(12,
                                                 sliderInput("n_speed", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Ball Control"),
                                          column(12,
                                                 sliderInput("n_control", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Short Pass"),
                                          column(12,
                                                 sliderInput("n_pass", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Freekick Accuracy"),
                                          column(12,
                                                 sliderInput("n_freekick", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Dribbling Skills"),
                                          column(12,
                                                 sliderInput("n_dribbling", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                        fluidRow(
                                          h3(style = "margin-left: 0px; margin-bottom: 0px;", "Finishing Skills"),
                                          column(12,
                                                 sliderInput("n_finish", label="", min = 0, max = 99, value = 50, step = 1)
                                          )
                                        ),
                                      ),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("Player Price Calculator", textOutput("Calculator")),
                                                    tabPanel("Model Summary", verbatimTextOutput("Summary"))
                                                    ) # tabsetPanel
                                        ) # mainPanel
                                      ) # sidebarLayout
) # tabPanel
                        

dataPanel_4 <- tabPanel("Top Players",
                        mainPanel(
                          h3("Top Players Table"),
                          tableOutput("Top")
                        )
                        
)
  
# UI
ui <- navbarPage("FIFA Football Players (Shiny Take-Away Assignment App)",
                 dataPanel_1,
                 dataPanel_2,
                 dataPanel_3,
                 dataPanel_4)
    
    

# SERVER
server <- function(input, output) {
  
  # 1
  output$Overall <- renderPlot({
    ggplot(df, aes(x = .data[[input$select_overall]], y = Overall, color = Position)) + geom_point() + 
      labs(colour = "Position") + scale_x_continuous(labels = scales::comma)
  })
  
  # 2
  output$Stats <- renderPlot({
    ggplot(df, aes(x = Position, y = .data[[input$select_stats]], fill = Position)) +
      geom_boxplot(alpha = 0.6) + labs(x = "Position", fill = "Position")
  })  
  
  # 3
  output$Calculator <- renderPrint({
    price <- summary(model_price)$coef[1,1] + summary(model_price)$coef[2,1]*input$n_rating +
      summary(model_price)$coef[3,1]*input$n_age + summary(model_price)$coef[4,1]*input$n_speed +
      summary(model_price)$coef[5,1]*input$n_control + summary(model_price)$coef[6,1]*input$n_pass +
      summary(model_price)$coef[7,1]*input$n_dribbling + summary(model_price)$coef[8,1]*input$n_finish +
      summary(model_price)$coef[9,1]*input$n_height + summary(model_price)$coef[10,1]*input$n_weight +
      summary(model_price)$coef[11,1]*input$n_freekick
    
    cat("You can find out how much would it cost a completely desired player by tunning its features on the right panel. \n\n")
    cat("After doing this, the estimated buying price for a player with the following characteristics: \n\n") 
    cat(input$n_age, "years old, \n\n")
    cat(input$n_rating, "of overall rating, \n\n")
    cat(input$n_height, "cm, \n\n")
    cat(input$n_weight, "kg, \n\n")
    cat(input$n_speed, "of speed, \n\n")
    cat(input$n_control, "of ball control, \n\n")
    cat(input$n_pass, "of short passing, \n\n")
    cat(input$n_freekick, "of freekick accuracy, \n\n")
    cat(input$n_dribbling, "of dribbling and \n\n")
    cat(input$n_finish, "of finishing skills, \n\n")
    cat("would be around:", price, "€")})
  
  output$Summary <- renderPrint(summary(model_price))
  

}  
# Run the application 
shinyApp(ui = ui, server = server)
