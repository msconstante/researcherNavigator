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
library(wordcloud)
source("basicFunctions.R")
source("Request.R")
options(shiny.sanitize.errors = F)


# Define server logic
server <- function(input, output, session) {
  #options(shiny.sanitize.errors = FALSE)
  
  test <- FALSE
  global <- reactiveValues(response = FALSE)
  
  
  #Display search -----------
  observeEvent(input$load, {
    #message(input$file)
    code <- trimws(input$code)

    fileResults <- file.path("output", code, "results.rds")
    try(listResults <- readRDS(fileResults))
    
    
    fileRequest <- file.path("output", code, "request.rds")
    try(request <- readRDS(fileRequest))
    
    output$searchName <- renderText({
      request$term
    })
    
    
    
    output$network <- renderForceNetwork(
      listResults[["net"]]
    )
    

    # dt <- data.frame(matrix(nrow = length(listResults[["imgs"]]), ncol = 3))
    # colnames(dt) <- c("Author", "Picture", "Wordcloud")
    # for(i in 1:nrow(dt)){
    #   dt[i,1] <- names(listResults[["imgs"]])[i]
    #   dt[i,2] <- paste("<img src='", listResults[['imgs']][[i]],"' width='250px' height='auto'>", sep="")
    #   name <- names(listResults[["imgs"]][i])
    #   #dt[i,3] <- format_image(file.path(code, name))
    #   dt[i,3] <- format_image(paste(code,"/", name, ".jpg", sep=""))
    #   #dt[i,3] <- paste(code,"/", name, ".jpg", sep="")
    #   #dt[i,3] <- format_image("UHWBKDSXRJMFEDXVXWWJYWHXKOAZVHIUOEJTPOZUIVYZE/Alberto.Caminero.jpg")
    #   #dt[i,3] <- paste("<img src='", "./www/Alberto.Caminero.jpg","' width='250px' height='auto'>", sep="")
    #   #dt[i,3] <- paste('<img src="', code, '/', names(listResults[["imgs"]][i]),
    #   #                  '.jpg" width="250px" height="auto">', sep="")
    #   
    # }
    
    #Populate selectInput with names of authors
    output$topAuthorSelector <- renderUI({
      selectInput("author", "Select an author", choices = names(listResults[["figs"]]))
    })
    #Populate output$picture with the picture of the selected author from img src
    output$picture <- renderUI({
      entry <- which(names(listResults[["figs"]])==input$author)
      tags$img(src = listResults[["imgs"]][[entry]], width = "200px", height = "auto")
    })
    
    # output$wordcloud <- renderImage({
    #   req(input$author)
    #   #src <- paste("www/", input$author, ".jpg", sep="")
    #   src <- file.path("www/", code, "/", input$author, ".jpg", sep="")
    #   src <- "www/UHWBKDSXRJMFEDXVXWWJYWHXKOAZVHIUOEJTPOZUIVYZE/Alberto.Caminero.jpg"
    #   print(src)
    #   list(
    #     src = src,
    #     width = "400px", 
    #     height = "400px"
    #   )
    # }, deleteFile = FALSE)
    

    
    output$wordcloud <- renderPlot({
      req(input$author)
      wc <- listResults[["figs"]][[input$author]][[1]]
      wordcloud(words = wc$word, freq = wc$freq, min.freq = 0,           
                max.words=20, random.order=FALSE, rot.per=0.35,            
                colors=brewer.pal(8, "Dark2"))
    })
    
    
    
    
    
    
    
        
    # output$authors <- DT::renderDataTable({
    #   DT::datatable(dt, escape=F) # HERE
    # })
    
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

