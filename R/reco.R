##This script contains functions used to parse a line from
## a web server log.
# Load required libraries
#
# curl https://public.opencpu.org/ocpu/github/melghazali/sitereco/R/reco.sites/json -d 'last.site="live.com"'
#

reco.sites <- function(last.site="") {
    
    library(rEMM)
    
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
    
    data <- read.csv("http://melghazali.ocpu.io/sitereco/data/MySiteVisits/csv") 
    
    for(i in 1:nrow(data)) {
      
        str <- strsplit(as.vector(data[i,]), ",")
        tuple <- matrix(c(i, unlist(str)), ncol=4, nrow=1)
        tuple[3] <- tolower(tuple[3])
        date <- unlist(as.character(tuple[2]))
        path <- unlist(tuple[3])
        site.time <- as.integer(tuple[4])
        
        
        page.list <- rbind(page.list, tuple)
          
        if (is.na(page.data[as.character(path)])) {
            newPath <- matrix(0,1,6)
            rownames(newPath) <- path
            page.data <- rbind(page.data, newPath)
        }
        
        page.data[as.character(path), date] <- as.integer(site.time)            
        
      }

    # build the model
    build(emm, page.data)
    
    # Get data point mapping to its cluster ID
    page.hash <- find_clusters(emm, page.data)
    
    prediction <- vector()
    nRow <- nrow(page.list)
    
    if (!is.null(last.site)) {
      id <- as.integer(page.list[match(tolower(last.site),page.list[,3]),1])
      
      # last.site does not exist if id is NA
      if (!is.na(id)) {
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
      top.ten <- names(sort(x=likelihoods, decreasing=TRUE)[1:10])
      
      j = 1
      for (i in 1:10) {
          site = toString(page.list[as.numeric(top.ten[i]),3])
          if (site != last.site) {
              prediction[j] <- site
              j <- j + 1
          }
      } 
    }
    
    return(prediction)
}
