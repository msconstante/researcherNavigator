# %%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create a shiny app to display the wordclouds
library(shiny)
library(bslib)
library(DT)
library(shinyalert)
library(shinyalert)
library(rentrez)
library(XML)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(stm)
library(wordcloud2)
library(DT)
library(networkD3)
library(webshot)
library(htmlwidgets)
source("basicFunctions.R")
source("Request.R")


# Define UI
ui <- page_sidebar(
  # App title ----
  title ="Researcher Navigator - BETA",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar( 
    
    h2("Visualize Results"),
    width = 500,
    
    
    
    #selection input with filenames from folder ./output
    textInput("code", "Insert the code of the recent search to display (e.g. EGJLMLQF.2024.02.23.20.25.51.418766.SLWQEULU)",
              ""),
    
    # Make a text output with thename of the saearch
    textOutput("searchName"),
    actionButton("load", "Load Search"),
    
    
    
    #Add text and divider
    hr(),
    h2("New Search"),
    
    textInput("email", "email", ""),
    
    
    textInput("search", "Search Terms (e.g. 'mRNA vaccines')", ""),
    textInput("force", "Force Inclusion of these Authors (comma separated - e.g. 'Katalin Kariko, Drew Weissman')", ""),
    sliderInput("dates",
                "Date range",
                ticks = F,
                sep = " ",
                min = 1957,
                max = as.numeric(format(Sys.Date(), "%Y")),
                value = c(2014, as.numeric(format(Sys.Date(), "%Y"))),
                step = 1),
    actionButton("run", "Run Search"),
    
    h6("AGPL license"),
    h6(em("Send feedback to info@fuzzyowl.ca"))
    
    
  ),
  tabsetPanel(
    tabPanel("Researcher's Keywords", 
             #Add a select input for the authors
             #selectInput("author", "Select an author", choices = c("")),
             uiOutput("topAuthorSelector"),
             #Add two figures
             fluidRow(
               column(6, uiOutput("picture")),
               column(6, plotOutput("wordcloud"))

               
             )),
    tabPanel("Researcher's Network",
             forceNetworkOutput(outputId = "network")))
  
)



