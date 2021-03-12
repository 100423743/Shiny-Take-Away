#### SHINY TAKE-AWAY ASSIGNMENT - DATA TIDYING####
## IGNACIO MEDINA DE ANDRÉS ##

# LIBRARIES
library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(plotly)
library(magrittr)

# PRE-PROCESSING
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

# Define UI for application that draws a histogram
ui <- 
    
    

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
