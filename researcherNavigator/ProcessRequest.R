source ("basicFunctions.R")
source ("auth.R")

ProcessRequest <- function(code){
  
  
  #Process ----------
  readRDS(file.path(getwd(), "output", code, "request.rds"))
  
  nAuthors <- 100
  
  #Extract forced authors ------
  
  force <- strsplit(force, ",")[[1]]
  for(i in 1:length(force)){
    force[i] <- trimws(force[i])
  }
  
  #Get PUBMED results ------
  search_results <- makeQuery(term, year, retmax)
  for(i in 1:length(force)){
    termp <- paste(term, "AND", force[i], sep=" ")
    search_results <- c(search_results, makeQuery(termp, year, retmax))
  }
  search_results <- unique(search_results)
  files <- fetchSearchResults(search_results)
  
  #Process PUBMED files --------
  df <- extractInfo(files)
  dataFrame <- df[["df"]]
  
  #Get authors to display --------
  freqAuthors <- data.frame(table(c(dataFrame[,"FirstAuthor"], dataFrame[, "LastAuthor"])))
  freqAuthors <- freqAuthors[order(freqAuthors[,"Freq"], decreasing = T),]
  
  forcedAuthors <- NULL
  for(i in 1:length(force)){
    forceEntry <- strsplit(force[i], " ")[[1]]
    forcedAuthorsFirst <- grep(forceEntry[1], freqAuthors[,"Var1"])
    forcedAuthorsLast <- grep(forceEntry[length(forceEntry)], freqAuthors[,"Var1"])  
    forcedAuthors <- c(forcedAuthors, intersect(forcedAuthorsFirst, forcedAuthorsLast))
  }
  
  toKeep <- c(1:(nAuthors-length(forcedAuthors)))
  toKeep <- toKeep[-forcedAuthors]
  toKeep <- c(forcedAuthors, toKeep)
  mainAuthors <- as.character(freqAuthors[toKeep, "Var1"])
  
  
  # Make workdclouds ------
  figs <- createWordClouds(df[["df"]], mainAuthors, nkeys=30)
  
  #Get faces and make wordcloud images-------
  imgs <- list()
  library(rvest)
  
  dirname <- file.path(getwd(), "www", code)
  if(dir.exists(dirname)){
    stop("Random directory name exists")
  } else {
    dir.create(dirname)
  }
  
  for (i in 1:length(figs)){
    #Make a google search and extract the first image
    sterm <- paste(term, names(figs[i]), "profile")
    sterm <- gsub(" ", "+", sterm)
    url <- paste("https://www.google.com/search?q=", sterm, "&tbm=isch", sep="")
    page <- read_html(url)
    img <- page %>% html_nodes("img") %>% html_attr("src")
    imgs <- c(imgs, list(img[[2]]))
    names(imgs)[i] <- make.names(names(figs[i]))
    
    saveWidget(figs[[i]], paste(dirname, "/tmp.html", sep=""), selfcontained = F)
    webshot(paste(dirname, "/tmp.html", sep=""), 
            paste(dirname,"/", names(imgs)[i], ".jpg", sep=""), 
            delay = 3, vwidth = 300, vheight=300) # changed to png. 
  }
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Network -----------------
  Authors <- as.character(freqAuthors[, "Var1"])
  
  net <- MakeNetwork(df[["df"]], Authors = Authors, 
                     mainAuthors = mainAuthors,
                     forcedAuthors = freqAuthors[forcedAuthors, "Var1"], 
                     collaborators = df[["collaborators"]])
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #Save Results -----------------
  dirname <- file.path(getwd(), "output", code)
  
  filename <- paste(dirname, "/results.rds", sep="")
  
  toSave <- list(ter = term, df = df, figs = figs, imgs = imgs, net = net)
  saveRDS(toSave, file = filename)
  
  #email  
  if(input$email != ""){
    sendEmail(input$email, dirname)
  }
}
