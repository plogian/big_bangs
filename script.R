library(jsonlite)
library(dplyr)
library(XML)

omdb_api_key <- "omdb api key here"

#initiate empty dataframe
omdb_list <- data.frame(Title= character(), Year= integer(), Rated = character(), Released=character(), 
                        Runtime = character(), Genre = character(), Director = character(), Writer= character(),
                        Actors = character(), Plot=character(), Language=character(), Country=character(), Awards=character(), 
                        Poster= character(), imdbRating = character(), RottenTomatoes=character(), Metascore=character(),
                        imdbRating2 = character(),  imdbVotes=character(), imdbID=character(), DVD=character(),
                        BoxOffice= character(), Production=character(), Website=character(), Response= character(), 
                        Season=character(), Episode=character(), totalSeasons=character(), metacritic=character(), stringsAsFactors = FALSE)

rownames <- c("Title", "Year", "Rated", "Released", "Runtime", "Genre", "Director", "Writer", "Actors",
              "Plot", "Language", "Country", "Awards", "Poster", "imdbRating", "RottenTomatoes", "Metascore",
              "imdbRating2", "imdbVotes", "imdbID", "DVD", "BoxOffice", "Production", "Website", "Response", 
              "Season", "Episode", "totalSeasons", "Metacritic")

collect_links <- function (url) {
  html <- readLines(url)
  id_location <- grep("data-tconst", html)
  ids <- html[id_location]
  ids <- gsub("[^0-9]", "", ids)
  return(ids)
}


#list of ids
b <- c()

for(i in 1:11) {
  season_url <- paste0("http://www.imdb.com/title/tt0898266/episodes?season=", i)
  ids <- collect_links(season_url)
  b <- c(b, ids)
}


for(i in 1:length(b)) {
  url <- paste0("http://omdbapi.com/?i=tt", b[i], "&plot=Full&r=json&apikey=", omdb_api_key)
  print(url)
  movie_data <- fromJSON(url)
  row <-
  for(j in c(1:15, 17, 19:28)) {
    if(length(movie_data[rownames[j]][[1]])>0 & isTRUE(movie_data[rownames[j]][[1]] != "N/A")) {
      omdb_list[row, j] <- as.character(movie_data[rownames[j]][[1]])
    } else {
      omdb_list[row, j] <- "NA"
    }
  }
  
  for(j in c(16)) {
    if(length(which(movie_data$Ratings$Source=="Rotten Tomatoes"))>0) {
      omdb_list[row, j] <- as.numeric(sub("%", "", movie_data$Ratings[which(movie_data$Ratings$Source=="Rotten Tomatoes"), 2]))
    } else {
      omdb_list[row, j] <- "NA"
    }
  }
  for(j in c(29)) {
    if(length(which(movie_data$Ratings$Source=="Metacritic"))>0) {
      omdb_list[row, j] <- as.numeric(sub("/100", "", movie_data$Ratings[which(movie_data$Ratings$Source=="Metacritic"), 2]))
    } else {
      omdb_list[row, j] <- "NA"
    }
  }
  
  for(j in c(18)) {
    if(length(which(movie_data$Ratings$Source=="Internet Movie Database"))>0) {
      omdb_list[row, j] <- as.numeric(sub("/10", "", movie_data$Ratings[which(movie_data$Ratings$Source=="Internet Movie Database"), 2]))
    } else {
      omdb_list[row, j] <- "NA"
    } 
  }
}

write.csv(omdb_list, "big_bangs.csv")