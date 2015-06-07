##This script contains functions used to parse a line from
## a web server log.
# Load required libraries
#
# curl https://public.opencpu.org/ocpu/github/melghazali/sitereco/R/reco.sites/json -d 'last.site="live.com"'
#

reco.sites <- function(last.site="") {
    
    library(rEMM)
    
    page.data <- matrix(0L,0,17) 
    colnames(page.data) <- c("4/27/2015", 
                             "4/28/2015", 
                             "5/2/2015", 
                             "5/3/2015", 
                             "5/11/2015", 
                             "5/12/2015",
                             "5/26/2015",
                             "5/27/2015", 
                             "5/28/2015",
                             "5/30/2015",
                             "5/31/2015",
                             "6/1/2015",
                             "6/2/2015",
                             "6/3/2015",
                             "6/4/2015",
                             "6/5/2015",
                             "6/6/2015")
    
    page.list <- matrix(0L,0,2)
    colnames(page.list) <- c("ID", "Domain")
    
    emm <- EMM(threshold=0.2, measure="eJaccard")
    
    data <- read.csv("http://melghazali.ocpu.io/sitereco/data/MySiteVisits2/csv") 
    #data <- read.csv("../data/MySiteVisits2.csv") 
    index <- 1L
    
    for(i in 1:nrow(data)) {
        
        str <- strsplit(as.vector(data[i,]), ",")
        tuple <- matrix(c(unlist(str)), ncol=3, nrow=1)
        tuple[2] <- tolower(tuple[2])
        date <- unlist(as.character(tuple[1]))
        path <- unlist(tuple[2])
        site.time <- as.integer(tuple[3])
        
        if (! path %in% page.list[,2]) {
            page.list <- rbind(page.list, c(index, path))
            index <- index + 1
        }
        
        if (! path %in% rownames(page.data)) {
            newPath <- matrix(0,1,17)
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
        id <- as.integer(page.list[match(tolower(last.site),page.list[,2]),1])
        
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
            site = toString(page.list[as.numeric(top.ten[i]),2])
            if (site != last.site) {
                prediction[j] <- site
                j <- j + 1
            }
        } 
    }
    
    return(prediction)
}
