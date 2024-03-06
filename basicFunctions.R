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
library(mailR)
library(dplyr)


source ("auth.R")


format_image <- function(path) {
  sprintf('<img src="%s" height="250px" width="250px"/>', path)
}

randomCode <- function(n = 1, reps = 8) {
  a <- do.call(paste0, replicate(reps, sample(LETTERS, n, TRUE), FALSE))
  #paste0(a, sprintf("%04d", sample(99999999, n, TRUE)), sample(LETTERS, n, TRUE))
  paste0(a)
}

#Load a search term, year and the retmax variable and make an entrez search
#The search results are saved in a variable called search_results
makeQuery <- function(term, year, retmax){
  types<- c("Journal Article", "Letter")
  typesQuery <- paste(types, collapse="[pt] OR ")
  typesQuery <- paste(typesQuery, "[pt]", sep="")
  query <- paste(term, " AND (", typesQuery,")", sep="")
  query <- paste(query, "AND (", year, "[Publication Date])")
  search_results <- entrez_search(db = "pubmed", term = query, retmax=retmax, use_history = T)
  message("Querying: ", query)
  return(search_results[[1]])
}


fetchSearchResults <- function(search_results){
  if(!dir.exists("PMID")){
    dir.create("PMID")
  }
  files <- list()
  for(i in 1:length(search_results)){
    id <- search_results[i]
    file <- paste(getwd(),"/PMID/", id, ".rds", sep="")
    files <- c(files, file)
    if(!file.exists(file)){
      #message to console if fetching file
      message("Fetching file ", i, " of ", length(search_results))
      fetched <- entrez_fetch(db = "pubmed", id = id, rettype = "xml", retmode = "xml", parsed = F)
      #save file as xml
      saveRDS(fetched, file)
    }
  }
  return(unlist(files))
}

extractInfo <- function(files){
  #create empty data frame
  df <- data.frame(PMID = character(),
                   Title = character(),
                   Abstract = character(),
                   FirstAuthor = character(),
                   LastAuthor = character(),
                   stringsAsFactors = FALSE)
  collaborators <- list()
  
  for(i in 1:length(files)){
    #message to console if extracting info
    message("Extracting info from file ", i, " of ", length(files))
    #load file
    file <- files[i]
    fetched <- readRDS(file)
    
    #convert fetched to xml
    fetched <- xmlToList(fetched)
    
    #Skip if not in english
    if(is.null(names(fetched))){next}
    language <- fetched$PubmedArticle$MedlineCitation$Article$Language[[1]]
    if(is.null(language)){
      #issues <- c(issues, i)
      next
    }
    if(language != 'eng'){next}
    
    #extract info
    PMID <- fetched$PubmedArticle$MedlineCitation$PMID$text[[1]]
    Title <- fetched$PubmedArticle$MedlineCitation$Article$ArticleTitle[[1]]
    Abstract <- fetched$PubmedArticle$MedlineCitation$Article$Abstract$AbstractText[[1]]
    if(class(Abstract)!="character"){next} #TODO Find why there are fails
    
    
    #Extract first author full name and last author full name
    authors <- fetched$PubmedArticle$MedlineCitation$Article$AuthorList
    FirstAuthor <- try(paste(authors[[1]]$ForeName, authors[[1]]$LastName))
    if(length(FirstAuthor)==0){FirstAuthor <- authors[[1]]$CollectiveName}
    
    if(length(authors)>2){
      LastAuthor <- try(paste(authors[[length(authors)-1]]$ForeName, authors[[length(authors)-1]]$LastName))
    } else {LastAuthor <- FirstAuthor}
    if(length(LastAuthor)==0){LastAuthor <- authors[[length(authors)-1]]$CollectiveName}
    
    
    
    
    
    #add info to data frame
    dataFrame <- try(data.frame(PMID = PMID, #TODO Fix fails
                                Title = Title,
                                Abstract = Abstract,
                                FirstAuthor = iconv(FirstAuthor, to="ASCII//TRANSLIT"),
                                LastAuthor = iconv(LastAuthor, to="ASCII//TRANSLIT"),
                                stringsAsFactors = FALSE), silent = T)
    if(class(dataFrame) == "try-error"){next}
    collaborators <- c(collaborators, list(authors))
    names(collaborators)[length(collaborators)] <- PMID
    df <- rbind(df, dataFrame)
  }
  
  toReturn <- list(df, collaborators)
  names(toReturn) <- c("df", "collaborators")
  return(toReturn)
}


createWordClouds <- function(dataFrame, Authors = NULL, nkeys=20){
  
  
  
  corp <- corpus(data.frame(dataFrame), text_field = "Abstract")
  tokenCorp <- tokens(corp, remove_punct = T)
  tokenCorp <- tokens_remove(tokenCorp, stopwords('en'))
  tokenCorp <- tokens_tolower(tokenCorp)
  
  dtm <- dfm(tokenCorp)
  
  
  authorDF <- NULL
  
  #Check only top authors
  #Authors <- unique(c(docvars(dtm)$LastAuthor, docvars(dtm)$FirstAuthor))
  
  figs <- vector("list", length(Authors))
  names(figs) <- Authors
  for (author in 1:length(Authors)){
    message(paste("Running ", author, " out of ", length(Authors)))
    if(is.na(Authors[author])){next} #TODO find out why fails
    aafirst <- docvars(dtm)$FirstAuthor == Authors[author]
    aalast <- docvars(dtm)$LastAuthor == Authors[author]
    aa <- aafirst | aalast
    aa_dtm <- dtm[aa,]
    
    ts <- try(textstat_keyness(dtm, target=aa), silent=T)
    if(class(ts)[[1]]=="try-error"){next}
    wcData <- ts[1:nkeys,c("feature", "chi2")] #FINISH
    wcData$chi2 <- sqrt(wcData$chi2)
    
    figs[[Authors[author]]] <- wordcloud2(wcData, minRotation = pi/2, maxRotation = pi/2, shape='pentagon', size = 0.2, color="blue")
    words <- paste(wcData[1:nkeys,1], collapse=", ")
    authorDF <- rbind(authorDF, c(Authors[author], words))
  }
  
  return(figs)
}

#MakeNetwork --------------------------------------------------------------
MakeNetwork <- function(dataFrame, mainAuthors, forcedAuthors = NULL, Authors, collaborators){
  collabData <- data.frame(from=character(0), to=character(0))
  uniqueDataFrame <- dataFrame[which(dataFrame$LastAuthor %in% Authors), ]
  uniqueDataFrame <- uniqueDataFrame[which(!is.na(uniqueDataFrame$PMID)),]
  
  for(i in 1:nrow(uniqueDataFrame)){
    authors <- collaborators[[uniqueDataFrame[i, "PMID"]]]
    
    firstAuthor <- try(paste(authors[[1]]$ForeName, authors[[1]]$LastName))
    if(length(firstAuthor)==0){firstAuthor <- authors[[1]]$CollectiveName}
    
    if(length(authors)>2){
      lastAuthor <- try(paste(authors[[length(authors)-1]]$ForeName, authors[[length(authors)-1]]$LastName))
    } else {lastAuthor <- firstAuthor}
    if(length(lastAuthor)==0){lastAuthor <- authors[[length(authors)-1]]$CollectiveName}
    
    for(auth in 1:(length(authors)-1)){
      if(any(names(authors[[auth]]) == "CollectiveName")) {next}
      name <- paste(authors[[auth]]$ForeName, authors[[auth]]$LastName)
      if(!(name %in% Authors)){next}
      collabData <- rbind(collabData, c(lastAuthor, name))
    }
  }
  
  
  
  library(dplyr)
  
  # Remove duplicates and add duplicate count column
  collabData <- collabData %>%
    group_by_all() %>%
    mutate(duplicate_count = n()) %>%
    distinct()
  colnames(collabData) <- c("from", "to", "weight")
  
  collabData <- collabData[order(collabData$weight, decreasing = T),]
  collabData <- data.frame(collabData)
  
  toKeep <- union(which(collabData[,"from"] %in% mainAuthors), which(collabData[,"to"] %in% mainAuthors))
  networkData <- collabData[toKeep,]
  #networkData[1:length(force),"node"] <- "red"
  #networkData[1:length(force),"color"] <- "purple"
  
  
  
  toRemove <- NULL
  if(nrow(networkData) > 1000){ #TODO  cycle this if until networkData rows are belos 1000
    for(i in 1:nrow(networkData)){
      if((networkData[i,"weight"] < 2)){
        if(is.null(forcedAuthors)){next}
        if(networkData[i,"from"] %in% forcedAuthors){next}
        if(networkData[i,"to"] %in% forcedAuthors){next}
        toRemove <- c(toRemove, i)}
    }
  }
  if(!is.null(toRemove)){networkData <- networkData[-toRemove, ]}
  
  
  # #Remove 'to' researchers without 'from'
  # toRemove <- NULL
  # toResearchers <- unique(networkData[,"to"])
  # fromResearchers <- unique(networkData[,"from"]) 
  # discardResearchers <- toResearchers[which(!(toResearchers %in% fromResearchers))]
  # for(i in 1:nrow(networkData)){
  #   if(networkData[i, "to"] %in% fromResearchers || 
  #      networkData[i, "to"] %in% forcedAuthors){next}
  #   else {toRemove <- c(toRemove, i)}
  # }
  
  
  networkData$color <- "black"
  if(!is.null(forcedAuthors)){
    networkData[which(networkData[,"from"] %in% forcedAuthors), "color"] <- "red"
    networkData[which(networkData[,"to"] %in% forcedAuthors), "color"] <- "red"
  }
  networkData$node <- "#69b3a2"
  
  
  
  
  library(networkD3)
  library(htmlwidgets)
  
  p <- simpleNetwork(networkData[,c("from", "to")], height="100px", width="100px",        
                     Source = 1,                 # column number of source
                     Target = 2,                 # column number of target
                     linkDistance = 150,          # distance between node. Increase this value to have more space between nodes
                     charge = -150,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                     fontSize = 20,               # size of the node names
                     fontFamily = "serif",       # font og node names
                     #linkColour = "black",        # colour of edges, MUST be a common colour for the whole graph
                     linkColour = networkData$color,        # colour of edges, MUST be a common colour for the whole graph
                     nodeColour = networkData$node,     # colour of nodes, MUST be a common colour for the whole graph
                     opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                     zoom = T                    # Can you zoom on the figure?
  )
  
  
}


