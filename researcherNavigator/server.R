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
source("request.R")
options(shiny.sanitize.errors = FALSE)

# Define server logic
server <- function(input, output, session) {
  #options(shiny.sanitize.errors = FALSE)
  
  test <- FALSE
  global <- reactiveValues(response = FALSE)
  
  
  #Display search -----------
  observeEvent(input$code, {
    #message(input$file)
    fileResults <- file.path("output", input$code, "results.rds")
    try(listResults <- readRDS(fileResults))
    
    
    fileRequest <- file.path("output", input$code, "request.rds")
    try(request <- readRDS(fileRequest))
    
    output$searchName <- renderText({
      request$term
    })
    
    
    
    output$network <- renderForceNetwork(
      listResults[["net"]]
    )
    
    
    output$authors <- DT::renderDataTable({
      dt <- data.frame()
      for(i in 1:length(listResults[["imgs"]])){
        img <- paste("<img src='", listResults[['imgs']][[i]],"' width='250px' height='auto'>", sep="")
        cloud <- paste('<img src="/', code, '/', names(listResults[["imgs"]][i]),
                       '.jpg" width="250px" height="auto">', sep="")
        line <- c(names(listResults[["imgs"]][i]), img, cloud)
        dt <- rbind(dt, line)
      }
      colnames(dt) <- c("Author", "Picture", "Wordcloud")
      DT::datatable(dt, escape = FALSE) # HERE
    })
  })
  
  
  
  #RUN new search ---------
  
  observeEvent(input$run, {
    shinyalert(
      #https://daattali.com/shiny/shinyalert-demo/
      title = "Note:",
      callbackR = function(x){
        global$response <- x
        if(x){
          # MAIN REQUEST ---------------------------------------------
          Request(input)
        }
        #test <- x
      },
      text = paste("Your search is: '",
                   input$search,"'\n
                   For email: ", input$email, "\n\n
    A search can often take more than 1 hour\n
    An email will be sent when the search is completed with the code to access the results.\n
    THEREFORE PLEASE MAKE SURE THE EMAIL IS CORRECT", sep=""),
      size = "s", 
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
  })
  
  observeEvent(global$response, {
    if(global$response){
      shinyalert(
        #https://daattali.com/shiny/shinyalert-demo/
        title = "",
        callbackR = function(x){
          global$response <- x
          
          #test <- x
        },
        text = "Your search is running",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = FALSE,
        showCancelButton = TRUE,
        cancelButtonText = "Great",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
}

