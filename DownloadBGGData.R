# Load packages -----------------------------------------------------------

library(RCurl)
library(XML)
library(httr)
library(rvest)
library(data.table)


# Scrape data from BGG browse page ----------------------------------------

# Function to scrape high-level board game details from bgg main page
getInfo <- function(i){
  # file name of page once downloaded
  pageLoc <- paste0("infoPage", i, ".html")
  
  # Download a page
  # This can fail so keep trying until it works
  flag <- 0
  while(flag == 0){
    # Download html page data
    x <- try(
      suppressWarnings(
        download.file(
          paste0(
            "https://boardgamegeek.com/browse/boardgame/page/",
            i,
            "?sort=title"
          ), 
          destfile = pageLoc)
      ),
      silent = TRUE
    )
    if (class(x) == "try-error") {
      # If download fails, sleep for 5 seconds and try again
      Sys.sleep(5)
    } else flag <- 1 
  }
  
  # Read it into R
  page <- read_html(pageLoc)
  
  # Find the rows of the table
  rows <- html_nodes(page, css = "tr[id=row_]")
  
  # Find rank and convert to number
  rank <- sapply(
    rows,
    function(x){
      suppressWarnings(
        html_nodes(x, css = "td[class=collection_rank]") %>%
          html_text() %>%
          as.integer()
      )
    }
  )
  
  # Find ratings and convert to number
  ratings <- lapply(
    rows,
    function(x){
      suppressWarnings(
        html_nodes(x, css = "td[class=collection_bggrating]") %>%
          html_text() %>%
          as.numeric()
      )
    }
  )
  
  # Find the game Id using the path of the thumbnail pic
  collection_thumbnail <- sapply(
    rows,
    function(x){
      html_nodes(x, "td[class=collection_thumbnail]")
    }
  )
  collection_thumbnail_a <- sapply(
    collection_thumbnail,
    function(x){
      html_nodes(x, "a")
    }
  )
  Id <- gsub(
    "/.*",
    "",
    gsub(
      "/boardgame/|/boardgameexpansion/",
      "",
      sapply(
        collection_thumbnail_a,
        function(x){
          html_attr(x, "href")
        }
      )
    )
  )  %>%
    as.integer()
  
  # Find thumbnail path
  thumbnail <- sapply(
    collection_thumbnail_a,
    function(x){
      gsub(
        "https://cf.geekdo-images.com/micro/img/",
        "",
        html_attr(html_nodes(x, "img"), "src")
      )
    }
  )
  
  # Find title
  title <- sapply(
    seq_along(rows),
    function(i){
      rows[i] %>% 
        html_nodes(paste0("td[id=CEcell_objectname", i, "]")) %>%
        html_nodes(paste0("div[id=results_objectname", i, "]")) %>%
        html_nodes("a") %>%
        html_text()
    }
  )
  
  # Find year
  year <- suppressWarnings(
    gsub(
      "\\(|\\)",
      "",
      sapply(
        seq_along(rows),
        function(i){
          rows[i] %>% 
            html_nodes(paste0("td[id=CEcell_objectname", i, "]")) %>%
            html_nodes(paste0("div[id=results_objectname", i, "]")) %>%
            html_nodes("span") %>%
            html_text()
        }
      )
    ) %>% as.numeric()
  )
  
  # Combine into a data.table
  dt <- data.table(
    Id = Id,
    title = title,
    year = year,
    rank = rank,
    geekRating = sapply(ratings, function(x) x[1]),
    avgRating = sapply(ratings, function(x) x[2]),
    numVoters = sapply(ratings, function(x) as.integer(x[3])),
    thumbnail = thumbnail
  )
  
  # Remove file
  file.remove(pageLoc)
  
  # Return data.table
  return(dt)
}

# Extraction data
extractionDate = Sys.time()

# Number of pages
numPages <- 1143 # 1143 pages of board game info on bgg!!

# Get pages and put together where geekRating exists
dtGames <- rbindlist(
  lapply(
    seq_len(numPages),
    function(i){
      dt <- getInfo(i)[! is.na(geekRating)]
      cat(paste0(i, " of ", numPages, "\n"))
      return(dt)
    }
  )
)

# Ensure no duplicated games
dtGames <- dtGames[! duplicated(Id)]

# Set Id as the key
setkey(dtGames, Id)

# Save to disk
save(
  dtGames,
  file = "BGGApp/dtGames.rda"
)

# Clear environment
Id <- dtGames[, Id]
rm(numPages, getInfo, dtGames)
gc()


# Use API to get extra details --------------------------------------------

# Function to get more detailed info for an individual game using the API
getDetails <- function(i, Id){
  # file name of page once downloaded
  pageLoc <- paste0("detailsPage", i, ".html")
  
  # Get the Ids required
  Ids <- na.exclude(Id[seq(100 * (i - 1) + 1, 100 * i)])
  
  # Download a page associated with supplied game Ids
  # This can fail so keep trying until it works
  flag <- 0
  while(flag == 0){
    x <- try(
      suppressWarnings(
        download.file(
          paste0(
            "http://www.boardgamegeek.com/xmlapi2/thing?id=",
            paste0(Ids, collapse = ","),
            "&stats=1&ratingcomments=1"
          ), 
          destfile = pageLoc)
      ),
      silent = TRUE
    )
    if (class(x) == "try-error"){
      # If download fails, sleep for 5 seconds and try again
      Sys.sleep(5)
    } else flag <- 1 
  }
  
  # Read it into R
  page <- read_html(pageLoc)
  
  # Split into board games
  item <- html_nodes(page, "item")
  
  # Get Ids, in case of any order change
  Ids <- sapply(
    item,
    function(x){
      html_attr(x, "id") %>%
        as.integer()
    }
  )
  
  # Get type
  IdType <- sapply(
    item,
    function(x){
      html_attr(x, "type")
    }
  )
  IdType <- factor(IdType, levels = c("boardgame", "boardgameexpansion"))
  
  # Get description
  description <- sapply(
    item,
    function(x){
      as.character(
        html_node(x, css = "description")
      )
    }
  )
  description <- substr(description, 14, nchar(description) - 14)
  
  # Function to get numeric values from the "item" list
  numericItemValue <- function(x){
    sapply(
      item,
      function(y){
        html_nodes(y, css = x) %>%
          html_attr("value") %>%
          as.numeric()
      }
    )
  }
  
  # Num players
  minPlayers <- numericItemValue("minplayers")
  maxPlayers <- numericItemValue("maxplayers")
  
  # Playing time
  minPlayTime <- numericItemValue("minplaytime")
  maxPlayTime <- numericItemValue("maxplaytime")
  
  # Min player age
  minAge <- numericItemValue("minage")
  
  # For recommended number of players, get "poll" info into list
  poll <- lapply(
    item,
    function(x){
      html_nodes(x, css = "poll")
    }
  )
  
  # Find the suggested number of players and put into another list
  suggested_numplayers <- lapply(
    poll,
    function(x){
      x[which(
        sapply(
          x,
          function(y){
            html_attr(y, "name")
          }
        ) == "suggested_numplayers"
      )]
    }
  )
  
  # Get the results of suggested number of players poll into list
  results <- lapply(
    suggested_numplayers,
    function(x){
      html_nodes(x, "results")
    }
  )
  
  # Get the number of players from the results into a list
  numplayers <- lapply(
    results,
    function(x){
      sapply(
        x,
        function(y){
          html_attr(y, "numplayers")
        }
      )
    }
  )
  
  # Create function to get vote counts for best / recommended / not recommended
  getVoteCounts <- function(voteType){
    lapply(
      results,
      function(x){
        tryCatch(
          sapply(
            x,
            function(y){
              result <- html_nodes(y, "result")
              selection <- which(
                sapply(
                  result,
                  function(z){
                    html_attr(z, "value")
                  }
                ) == voteType
              )
              return(
                html_attr(result[[selection]], "numvotes") %>% as.numeric()
              )
            }
          ),
          error = function(err) return(0)
        )
      }
    )
  }
  
  # Get number of votes for best / recommended / not recommended
  bestVotes <- getVoteCounts("Best")
  recommendedVotes <- getVoteCounts("Recommended")
  notRecommendedVotes <- getVoteCounts("Not Recommended")
  
  # Put player count votes into a table
  dtPlayerCountPoll <- data.table(
    Id = sapply(
      seq_along(Ids),
      function(i){
        rep(Ids[i], length(numplayers[[i]]))
      }
    ) %>% unlist(),
    numplayers = numplayers %>% unlist(),
    bestVotes = bestVotes %>% unlist(),
    recommendedVotes = recommendedVotes %>% unlist(),
    notRecommendedVotes = notRecommendedVotes %>% unlist()
  )
  
  # Categories and mechanics
  links <- lapply(
    item,
    function(x){
      html_nodes(x, css = "link")
    }
  )
  
  # Put categories into table
  dtLinks <- rbindlist(
    lapply(
      seq_along(item),
      function(i){
        data.table(
          Id = Ids[i],
          type = sapply(links[[i]], function(x) html_attr(x, "type")),
          value = sapply(links[[i]], function(x) html_attr(x, "value")),
          ParentId = sapply(links[[i]], function(x) html_attr(x, "id"))
        )
      }
    )
  )
  
  # Put expansion / parent (base game) relationships into new table
  dtExpansionLinks <- dtLinks[type == "boardgameexpansion", .(ExpansionId = Id, ParentId)]
  IdExpansions <- Ids[which(IdType == "boardgameexpansion")]
  dtExpansionLinks <- dtExpansionLinks[ExpansionId %in% IdExpansions]
  
  # Filter the links table to just categories and mechanics
  dtLinks <- dtLinks[type %in% c("boardgamecategory", "boardgamemechanic"), .(Id, type, value)]
  dtLinks[, type := factor(type, levels = c("boardgamecategory", "boardgamemechanic"))]
  
  # Pull out statistics (ratings and rankings) for each game
  statistics <- lapply(
    item,
    function(x){
      html_nodes(x, css = "statistics")
    }
  )
  
  # Pull out rating information
  ratings <- lapply(
    statistics,
    function(x){
      html_nodes(x, css = "ratings")
    }
  )
  
  # function for finding different rating types
  getRatings <- function(rating){
    sapply(
      ratings,
      function(x){
        html_nodes(x, css = rating) %>%
          html_attr("value")
      }
    )
  }
  
  # Get ratings details using getRatings function
  usersrated <- getRatings("usersrated") %>% as.integer()
  average <- getRatings("average") %>% as.numeric()
  bayesaverage <- getRatings("bayesaverage") %>% as.numeric()
  stddev <- getRatings("stddev") %>% as.numeric()
  numweights <- getRatings("numweights") %>% as.integer()
  averageweight <- getRatings("averageweight") %>% as.numeric()
  
  # Board game sub-groups and rankings
  ranks <- lapply(
    statistics,
    function(x){
      html_nodes(x, css = "rank")
    }
  )
  
  # Find the type of each ranking
  rankTypes <- lapply(
    ranks,
    function(x){
      sapply(
        x,
        function(y){
          html_attr(y, "type")
        }
      )
    }
  )
  
  # Find the actual rank for each type
  rankValues <- suppressWarnings(
    lapply(
      ranks,
      function(x){
        sapply(
          x,
          function(y){
            html_attr(y, "value")
          }
        ) %>% as.integer()
      }
    ) 
  )
  
  # Find the rating used for determining rank
  rankRatings <- suppressWarnings(
    lapply(
      ranks,
      function(x){
        sapply(
          x,
          function(y){
            html_attr(y, "bayesaverage")
          }
        ) %>% as.numeric()
      }
    )
  )
  
  # Find the name for each rank
  rankNames <- lapply(
    ranks,
    function(x){
      sapply(
        x,
        function(y){
          html_attr(y, "name")
        }
      )
    }
  )
  
  # Put together in a table
  dtRanks <- rbindlist(
    lapply(
      seq_along(item),
      function(i){
        data.table(
          Id = Ids[i],
          type = rankTypes[[i]],
          subGroup = rankNames[[i]],
          rank = rankValues[[i]],
          rating = rankRatings[[i]]
        )[type == "family"]
      }
    )
  )
  dtRanks[, type := NULL]
  
  # Fix subgroup
  dtRanks[
    ,
    subGroup := factor(
      subGroup,
      levels = c(
        "abstracts",
        "cgs",
        "childrensgames",
        "familygames",
        "partygames",
        "strategygames",
        "thematic",
        "wargames"
      )
    )
  ]
  levels(dtRanks$subGroup) <- c(
    "Abstract",
    "Customisable",
    "Children's",
    "Family",
    "Party",
    "Strategy",
    "Thematic",
    "War games"
  )
  
  
  # Rating comments
  comments <- lapply(
    item,
    function(x){
      html_nodes(x, css = "comments")
    }
  )
  
  # Total number of ratings comments
  numComments <- sapply(
    comments,
    function(x){
      num <- html_attr(x, "totalitems") %>%
        as.integer()
      if (length(num) == 0){
        num <- NA
      }
      return(num)
    }
  )
  
  # Usernames
  commentUser <- lapply(
    comments,
    function(x){
      html_nodes(x, css = "comment") %>%
        html_attr("username")
    }
  )
  
  # Actual ratings
  commentValue <- lapply(
    comments,
    function(x){
      html_nodes(x, css = "comment") %>%
        html_attr("rating") %>%
        as.numeric()
    }
  )
  
  # Put together in a table
  dtRatings <- rbindlist(
    lapply(
      seq_along(item),
      function(i){
        data.table(
          Id = rep(Ids[i], length(commentUser[[i]])),
          user = commentUser[[i]],
          rating = commentValue[[i]]
        )
      }
    )
  )
  
  # Put together main table
  dtGames <- data.table(
    Id = Ids,
    type = IdType,
    description = description,
    minPlayers = minPlayers,
    maxPlayers = maxPlayers,
    minPlayTime = minPlayTime,
    maxPlayTime = maxPlayTime,
    minAge = minAge,
    numRatings = usersrated,
    numRatingComments = numComments,
    avRating = average,
    geekRating = bayesaverage,
    sdRating = stddev,
    numWeights = numweights,
    avWeight = averageweight
  )
  
  # Fix playing time
  dtGames[, minPlayTime := pmax(minPlayTime, 10)]
  dtGames[, maxPlayTime := pmax(minPlayTime, maxPlayTime)]
  
  # Remove file
  file.remove(pageLoc)
  
  # Return the tables for the games we are interested in
  return(
    list(
      dtGames = dtGames,
      dtPlayerCountPoll = dtPlayerCountPoll,
      dtLinks = dtLinks,
      dtRanks = dtRanks,
      dtRatings = dtRatings,
      dtExpansionLinks = dtExpansionLinks
    )
  )
}

# Get more detailed info on each game using the API
lsDetails <- lapply(
  seq_len(ceiling(length(Id) / 100)),
  function(i){
    ls <- getDetails(i, Id)
    cat(paste0(i, " of ", ceiling(length(Id) / 100), "\n"))
    return(ls)
  }
)

# Function to get the tables out of the list
extractTableFromList <- function(ls, x){
  rbindlist(
    lapply(
      ls,
      function(y){
        y[[x]]
      }
    )
  )
}

# Get all the object out of the list
dtGamesDetails <- extractTableFromList(lsDetails, "dtGames")
dtPlayerCountPoll <- extractTableFromList(lsDetails, "dtPlayerCountPoll")
dtLinks <- extractTableFromList(lsDetails, "dtLinks")
dtRanks <- extractTableFromList(lsDetails, "dtRanks")
dtRatings <- extractTableFromList(lsDetails, "dtRatings")
dtExpansionLinks <- extractTableFromList(lsDetails, "dtExpansionLinks")
rm(lsDetails)

# Save to disk
save(
  dtGamesDetails,
  dtPlayerCountPoll,
  dtLinks,
  dtRanks,
  dtExpansionLinks,
  extractionDate,
  file = "BGGApp/bggData.rda"
)
save(
  dtRatings,
  file = "BGGApp/bggRatingsData1.rda"
)

# Keep num ratings comments for next step
dtNumRatingsComments <- dtGamesDetails[, .(Id, numRatingComments)]

# Clear environment
rm(
  dtGamesDetails,
  dtPlayerCountPoll,
  dtLinks,
  dtRanks,
  dtExpansionLinks,
  dtRatings,
  Id,
  extractTableFromList,
  getDetails,
  extractionDate
)
gc()


# Extract the rest of the ratings comments --------------------------------

# Function to get extra pages of ratings comments
getRatingsComments <- function(Id, page){
  # file name of page once downloaded
  pageLoc <- paste0("ratingsComments", Id[1], "_", page, ".html")
  
  # Download a page associated with supplied game Ids
  # This can fail so keep trying until it works
  flag <- 0
  while(flag == 0){
    x <- try(
      suppressWarnings(
        download.file(
          paste0(
            "http://www.boardgamegeek.com/xmlapi2/thing?id=",
            paste0(Id, collapse = ","),
            "&ratingcomments=1&page=",
            page
          ), 
          destfile = pageLoc)
      ),
      silent = TRUE
    )
    if (class(x) == "try-error"){
      # If download fails, sleep for 5 seconds and try again
      Sys.sleep(5)
    } else flag <- 1 
  }
  
  # Read it into R
  page <- read_html(pageLoc)
  
  # Split into board games
  item <- html_nodes(page, "item")
  
  # Rating comments
  comments <- lapply(
    item,
    function(x){
      html_nodes(x, css = "comments")
    }
  )
  
  # User
  commentUser <- lapply(
    comments,
    function(x){
      html_nodes(x, css = "comment") %>%
        html_attr("username")
    }
  )
  
  # Value
  commentValue <- lapply(
    comments,
    function(x){
      html_nodes(x, css = "comment") %>%
        html_attr("rating") %>%
        as.numeric()
    }
  )
  
  # Put together into table
  dtRatings <- rbindlist(
    lapply(
      seq_along(item),
      function(i){
        data.table(
          Id = rep(Id[i], length(commentUser[[i]])),
          user = commentUser[[i]],
          rating = commentValue[[i]]
        )
      }
    )
  )
  
  # Remove file
  file.remove(pageLoc)
  
  # Return table
  return(dtRatings)
}

# Number of pages of comments per game
dtNumRatingsComments[, numRatingPages := ceiling(numRatingComments / 100)]

# Maximum number of pages of ratings comments (total passes through outer loop)
n <- dtNumRatingsComments[, max(numRatingPages)] - 1

# Total number of calls to API for each run through outer loop
m <- sapply(
  seq_len(n),
  function(i){
    ceiling(dtNumRatingsComments[numRatingPages > i, .N/100])
  }
)

# Get ratings comments and save to disk
invisible(
  lapply(
    seq_len(n),
    function(i){
      # Put together as a table
      dtRatings <- rbindlist(
        lapply(
          seq_len(m[i]),
          function(j){
            Id <- dtNumRatingsComments[numRatingPages > i][seq(1 + 100 * (j - 1), min(100 * j, .N)), Id]
            dt <- getRatingsComments(Id, i + 1)
            cat(paste0(j + sum(m[seq_len(i - 1)]), " out of ", sum(m), "\n"))
            return(dt)
          }
        )
      )
      
      # Save to disk
      save(
        dtRatings,
        file = paste0("BGGApp/bggRatingsData", i + 1,".rda")
      )
      
      # Return nothing (already saved to disk)
      return(NULL)
    }
  )
)

# Clear up environment
rm(n, m, dtNumRatingsComments, getRatingsComments)

# Get vector of ratings comments filenames
ratingsFiles <- grep("bggRatingsData" ,list.files("./BGGApp"), value = TRUE)

# Put together into a single table
dtRatingsComments <- rbindlist(
  lapply(
    ratingsFiles,
    function(x){
      load(paste0("./BGGApp/", x))
      return(dtRatings)
    }
  )
)

# Save to disk
save(
  dtRatingsComments,
  file = "BGGApp/dtRatingsComments.rda"
)

# Delete temp files
# for (x in ratingsFiles){
#   file.remove(paste0("./BGGApp/", x))
# }