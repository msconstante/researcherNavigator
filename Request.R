source("basicFunctions.R")

# shiny app server
Request <- function(input) {
  
  # Preapare Entrez Query --------------
  term <- input$search
  retmax <- input$retmax
  year <- paste(input$dates[1], ":", input$dates[2], sep="")
  force <- input$force
  email <- input$email

  
  
  #Request ------------------------------
  code  <- make.names(paste(randomCode(), Sys.time(),randomCode()))
  dirname <- file.path("output", code)
  if(dir.exists(dirname)){
    stop("Random directory name exists")
  } else {
    dir.create(dirname)
  }
  filename <- paste(dirname, "/request.rds", sep="")
  toSave <- list(code = code, email = email, dirname = dirname, term = term, 
                 retmax = retmax, year = year, force = force)
  saveRDS(toSave, file = filename)
  
  
  
}

