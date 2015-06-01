##This script contains functions used to parse a line from
## a web server log.
# Load required libraries

library(rEMM)
#library(hash)


##bot.list <- c("")


# Load the Model
#stopifnot(file.exists("./data/emm.RData"))
#load(file="./data/emm.RData")

# Load the pages hash table
#stopifnot(file.exists("./data/page-hash.RData"))
#load(file="./data/page-hash.RData")

# Load the pages list
#stopifnot(file.exists("./data/page-list.RData"))
#load(file="./data/page-list.RData")


##origin.pattern <- "^\\S+(\\.\\S+)*\\.\\S+"
##path.pattern <- "GET\\s+[/\\w]+.\\w+"
##date.pattern <- ""

page.data <- matrix(0L,0,6)
colnames(page.data) <- c("4/27/2015", 
                         "4/28/2015", 
                         "5/2/2015", 
                         "5/3/2015", 
                         "5/11/2015", 
                         "5/12/2015")

page.list <- matrix(NA,0,4)
colnames(page.list) <- c("ID", "Date", "Domain", "SiteTime")

emm <- EMM(threshold=0.2, measure="eJaccard")

reco.sites <- function(last.site) {
  # Load the pages hash table
  #stopifnot(file.exists("./data/page-hash.RData"))
  #load(file="./data/page-hash.RData")
  
  # Load the pages list
  #stopifnot(file.exists("./data/page-list.RData"))
  #load(file="./data/page-list.RData")
  
    stopifnot(file.exists("data/MySiteVisits.csv")) 
    data <- read.csv("data/MySiteVisits.csv") 
    
    for(i in 1:nrow(data)) {
      ##print(sprintf("row[%d]",i))
        tuple <- c(i,data[i,])
        tuple$Domain <- tolower(tuple$Domain)
        date <- unlist(as.character(tuple$Date))
        path <- unlist(tuple$Domain)
        site.time <- tuple$SiteTime
        
        
          page.list <- rbind(page.list, tuple)
          
          if (is.na(page.data[as.character(path)])) {
            newPath <- matrix(0,1,6)
            rownames(newPath) <- path
            page.data <- rbind(page.data, newPath)
          }
        
        page.data[as.character(path), date] <- as.integer(site.time)
        
      }

    #emm <- EMM(threshold=0.2, measure="eJaccard")
    build(emm, page.data)
    
    # Get data point mapping to its cluster ID
    page.hash <- find_clusters(emm, page.data)
    
    prediction <- vector()
    nRow <- nrow(page.list)
    
    if (!is.null(last.site)) {
      id <- page.list[match(tolower(last.site),page.list[,3]),]$ID
      
      # last.site does not exist if id is null
      if (!is.null(id)) {
        last.request <- page.hash[id]
      }
      else {
        last.request <- current_state(emm)
      }
    }
    else {
      last.request <- current_state(emm)
    }
    
    
    if (! is.null(last.request)) {
      likelihoods <- predict(emm, current_state=last.request, 
                             probabilities=TRUE)
      top.five <- names(sort(x=likelihoods, decreasing=TRUE)[1:5])
      for (i in 1:5) {
        prediction[[i]] <- toString(page.list[as.numeric(top.five[i]),3])
      } 
    }
    
    return(prediction)
}
