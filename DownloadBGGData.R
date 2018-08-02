library(RCurl)
library(XML)
library(httr)
library(rvest)
library(data.table)

# Function to scrape high-level board game details from bgg main page
getInfo <- function(i){
  # Download a page
  flag <- 0
  while(flag==0){
    x <- try(download.file(paste0("https://boardgamegeek.com/browse/boardgame/page/", i, "?sort=numvoters&sortdir=desc"), 
                          destfile="page.html"), silent=TRUE)
    if (class(x)=="try-error") {
      Sys.sleep(5)
    } else flag <- 1 
  }
  
  # Read it into R
  page <- read_html("page.html")
  # Find the rows of the table
  rows <- html_nodes(page, css="tr[id=row_]")
  # Find rank
  rank <- sapply(rows, function(x){
    suppressWarnings(as.numeric(html_text(html_nodes(x, css="td[class=collection_rank]"))))
  })
  
  # Find Id
  collection_thumbnail <- sapply(rows, function(x) html_nodes(x, "td[class=collection_thumbnail]"))
  collection_thumbnail_a <- sapply(collection_thumbnail, function(x) html_nodes(x, "a"))
  Id <- gsub("/.*", "", gsub("/boardgame/|/boardgameexpansion/", "", sapply(collection_thumbnail_a, function(x) html_attr(x, "href"))))
  
  # Find Thumbnail
  #thumbnail <- sapply(collection_thumbnail_a, function(x) html_attr(html_nodes(x, "img"), "src"))

  # Find Title
  title <- sapply(1:length(rows), function(i) rows[i] %>% 
                    html_nodes(paste0("td[id=CEcell_objectname", i, "]")) %>%
                    html_nodes(paste0("div[id=results_objectname", i, "]")) %>%
                    html_nodes("a") %>%
                    html_text())
  # Find Year
  year <- gsub("\\(|\\)", "", sapply(1:length(rows), function(i) rows[i] %>% 
                                       html_nodes(paste0("td[id=CEcell_objectname", i, "]")) %>%
                                       html_nodes(paste0("div[id=results_objectname", i, "]")) %>%
                                       html_nodes("span") %>%
                                       html_text())) %>% as.numeric()
  # Find Ratings
  #ratings <- t(sapply(rows, function(x){
  #  suppressWarnings(sapply(html_text(html_nodes(x, css="td[class=collection_bggrating]")), as.numeric))
  #}))
  #colnames(ratings) <- c("GeekRating", "AvRating", "NumVotes")
  
  dt <- data.table(rank=rank
                   ,Id=Id
                   ,title=title
                   ,year=year
                   #,thumbnail=thumbnail
                   )
  file.remove("page.html")
  
  return(dt)
}

# Function to scrape more detailed info for an individual game
getDetails <- function(Id){
  flag <- 0
  while(flag==0){
    x <- try(download.file(paste0("http://www.boardgamegeek.com/xmlapi2/thing?id=", paste0(Id, collapse=","), "&stats=1"), 
                           destfile="page.html"), silent=TRUE)
    if (class(x)=="try-error") {
      Sys.sleep(5)
    } else flag <- 1 
  }
  
  # Read it into R
  page <- read_html("page.html")
  
  # Split into board games
  item <- html_nodes(page, "item")
  
  # Get Id, in case of any order change
  Id <- sapply(item, function(x) html_attr(x, "id"))
  
  # Get description
  #description <- sapply(item, function(x) as.character(html_nodes(x, css="description")))
  
  # Num players
  minPlayers <- sapply(item, function(x) as.numeric(html_attr(html_nodes(x, css="minplayers"), "value")))
  maxPlayers <- sapply(item, function(x) as.numeric(html_attr(html_nodes(x, css="maxplayers"), "value")))
  
  # Playing time
  minPlayTime <- sapply(item, function(x) as.numeric(html_attr(html_nodes(x, css="minplaytime"), "value")))
  maxPlayTime <- sapply(item, function(x) as.numeric(html_attr(html_nodes(x, css="maxplaytime"), "value")))
  
  # Min player age
  minAge <- sapply(item, function(x) as.numeric(html_attr(html_nodes(x, css="minage"), "value")))
  
  # Recommended number of players
  poll <- lapply(item, function(x) html_nodes(x, css="poll"))
  suggested_numplayers <- lapply(poll, function(x) x[which(sapply(x, function(y) html_attr(y, "name"))=="suggested_numplayers")])
  results <- lapply(suggested_numplayers, function(x) html_nodes(x, "results"))
  numplayers <- lapply(results, function(x) sapply(x, function(y) html_attr(y, "numplayers")))
  bestVotes <- lapply(results, function(x){
    tryCatch(sapply(x, function(y){
      result <- html_nodes(y, "result")
      best <- which(sapply(result, function(z) html_attr(z, "value"))=="Best")
      return(as.numeric(html_attr(result[[best]], "numvotes")))
    }), error=function(err) return(0))
  })
  recommendedVotes <- lapply(results, function(x){
    tryCatch(sapply(x, function(y){
      result <- html_nodes(y, "result")
      best <- which(sapply(result, function(z) html_attr(z, "value"))=="Recommended")
      return(as.numeric(html_attr(result[[best]], "numvotes")))
    }), error=function(err) return(0))
  })
  notRecommendedVotes <- lapply(results, function(x){
    tryCatch(sapply(x, function(y){
      result <- html_nodes(y, "result")
      best <- which(sapply(result, function(z) html_attr(z, "value"))=="Not Recommended")
      return(as.numeric(html_attr(result[[best]], "numvotes")))
    }), error=function(err) return(0))
  })
  netVotes1 <- sapply(seq(1, length(item)), function(i){
    bestVotes[[i]]+recommendedVotes[[i]]-notRecommendedVotes[[i]]
                     })
  netVotes2 <- sapply(seq(1, length(item)), function(i){
    2*bestVotes[[i]]+recommendedVotes[[i]]-notRecommendedVotes[[i]]
  })
  
  minPlayersRecommended <- suppressWarnings(sapply(1:length(numplayers), function(i) as.numeric(gsub("\\+", "", numplayers[[i]][min(which(netVotes1[[i]]>0))]))))
  maxPlayersRecommended <- suppressWarnings(sapply(1:length(numplayers), function(i) as.numeric(gsub("\\+", "", numplayers[[i]][max(which(netVotes1[[i]]>0))]))))
  numPlayersBest <- suppressWarnings(sapply(1:length(numplayers), function(i) as.numeric(gsub("\\+", "", numplayers[[i]][which.max(netVotes2[[i]])]))))
  
  # Categories and mechanics
  links <- lapply(item, function(x) html_nodes(x, css="link"))
  dtLinks <- rbindlist(lapply(seq(1, length(item)),
                             function(i) data.table(Id=Id[i],
                                                    type=sapply(links[[i]], function(x) html_attr(x, "type")),
                                                    value=sapply(links[[i]], function(x) html_attr(x, "value")))))
  dtLinks <- dtLinks[type %in% c("boardgamecategory", "boardgamemechanic")]
  #sapply(links, function(x) html_attr(x, "type"))
  #sapply(links, function(x) html_attr(x, "value"))
  
  # Ratings
  statistics <- lapply(item, function(x) html_nodes(x, css="statistics"))
  ratings <- lapply(statistics, function(x) html_nodes(x, css="ratings"))
  usersrated <- sapply(ratings, function(x) as.integer(html_attr(html_nodes(x, css="usersrated"), "value")))
  average <- sapply(ratings, function(x) as.numeric(html_attr(html_nodes(x, css="average"), "value")))
  bayesaverage <- sapply(ratings, function(x) as.numeric(html_attr(html_nodes(x, css="bayesaverage"), "value")))
  stddev <- sapply(ratings, function(x) as.numeric(html_attr(html_nodes(x, css="stddev"), "value")))
  numweights <- sapply(ratings, function(x) as.integer(html_attr(html_nodes(x, css="numweights"), "value")))
  averageweight <- sapply(ratings, function(x) as.numeric(html_attr(html_nodes(x, css="averageweight"), "value")))
  
  # Board game sub-groups and rankings
  ranks <- lapply(statistics, function(x) html_nodes(x, css="rank"))
  rankTypes <- lapply(ranks, function(x) sapply(x, function(y) html_attr(y, "type")))
  rankValues <- suppressWarnings(lapply(ranks, function(x) as.integer(sapply(x, function(y) html_attr(y, "value")))))
  rankRatings <- lapply(ranks, function(x) as.numeric(sapply(x, function(y) html_attr(y, "bayesaverage"))))
  rankNames <- lapply(ranks, function(x) sapply(x, function(y) html_attr(y, "name")))
  
  dtRanks <- rbindlist(lapply(seq(1, length(item)), function(i) data.table(Id=Id[i],
                                                                           type=rankTypes[[i]],
                                                                           subGroup=rankNames[[i]],
                                                                           rank=rankValues[[i]],
                                                                           rating=rankRatings[[i]])))
  dtRanks <- dtRanks[type=="family"]
  dtRanks[, type := NULL]
  
  dtRanks[, subGroup := ifelse(subGroup=="abstracts", "Abstract",
                               ifelse(subGroup=="cgs", "Customisable",
                                      ifelse(subGroup=="childrensgames", "Children's",
                                             ifelse(subGroup=="familygames", "Family",
                                                    ifelse(subGroup=="partygames", "Party",
                                                           ifelse(subGroup=="strategygames", "Strategy",
                                                                  ifelse(subGroup=="thematic", "Thematic", "War games")))))))]
  
  # Remove file
  file.remove("page.html")
  
  # Put together main table
  dtGames <- data.table(Id=Id,
                        #description=description,
                        minPlayers=minPlayers,
                        maxPlayers=maxPlayers,
                        minPlayersRecommended=pmax(minPlayersRecommended, minPlayers),
                        maxPlayersRecommended=pmin(maxPlayersRecommended, maxPlayers),
                        numPlayersBest=pmin(pmax(numPlayersBest, minPlayers), maxPlayers),
                        minPlayTime=minPlayTime,
                        maxPlayTime=maxPlayTime,
                        minAge=minAge,
                        numRatings=usersrated,
                        avRating=average,
                        geekRating=bayesaverage,
                        sdRating=stddev,
                        numWeights=numweights,
                        avWeight=averageweight)
  
  # Fix player count
  dtGames[, minPlayers := pmax(minPlayers, 1)]
  dtGames[, maxPlayers := pmax(minPlayers, maxPlayers)]
  
  # Fix player recommendations
  dtGames[is.na(minPlayersRecommended), numPlayersBest := NA]
  dtGames[, minPlayersRecommended := pmin(pmax(minPlayersRecommended, minPlayers), maxPlayers)]
  dtGames[, maxPlayersRecommended := pmin(pmax(maxPlayersRecommended, minPlayers), maxPlayers)]
  
  # Fix playing time
  dtGames[, minPlayTime := pmax(minPlayTime, 10)]
  dtGames[, maxPlayTime := pmax(minPlayTime, maxPlayTime)]
  
  return(list(dtGames=dtGames,
              dtLinks=dtLinks,
              dtRanks=dtRanks))
}

# Combine functions
getAllData <- function(i){
  dt <- getInfo(i)
  dtList <- getDetails(dt$Id)
  dtList$dtGames <- merge(dt, dtList$dtGames)
  return(dtList)
}


# Run
#n <- 783
n <- 100
allData <- vector(mode="list", length=n)
for(i in 1:n){
  allData[[i]] <- getAllData(i)
}

dtGames <- rbindlist(lapply(allData, function(x) x$dtGames))
dtLinks <- rbindlist(lapply(allData, function(x) x$dtLinks))
dtRanks <- rbindlist(lapply(allData, function(x) x$dtRanks))

allData <- list(dtGames=dtGames,
                dtLinks=dtLinks,
                dtRanks=dtRanks,
                date=Sys.time())

# Save
save(allData, file="BGGApp/bggData.rda")
