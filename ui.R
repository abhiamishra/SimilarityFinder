library(shiny)
library(tidyverse)
library("readxl")
library(reactable)

GMMData <- read_xlsx("newTry.xlsx") %>%
  filter(Min >= 1000) %>%
  na.omit()

UI = fluidPage(
  titlePanel("Finding Similar Players"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("player", "Select a Player", choices = GMMData$Player,
                     options = list(maxItems = 1, placeholder = 'Type Player Name')),
      sliderInput("ager", "Age:",
                  min = 16, max = 40,
                  value = c(18,29)),
      selectizeInput("league", "Select a League", choices = c("All","fr Ligue 1",
                                                              "de Bundesliga",
                                                              "eng Premier League",
                                                              "es La Liga",
                                                              "it Serie A"
                                                              ),
                     options = list(maxItems = 1, placeholder = 'Type League Name')),
      selectizeInput("nation", "Select a Nation", choices = c("All",GMMData$Nation),
                     options = list(maxItems = 1, placeholder = 'Type League Name')),
      sliderInput("cfm", "Clinical Finishing Score:",
                  min = -3, max = 3,
                  value = c(0,0.5), step = 0.5),
      actionButton("show", label = "Show"),

    ),
    
    mainPanel(
      reactableOutput("playerData")
    )
  )
)

