# This script is used to create:
# 1) Movie search matrix - search.csv
# 2) User profiles - users.csv
# from our initial datasets (movies.csv & ratings.csv)

library(magrittr)
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

# Data Load ----

links <- read.csv("data/links.csv") # not used currently
movies <- read.csv("data/movies.csv", stringsAsFactors = FALSE)
ratings <- read.csv("data/ratings.csv")
tags <- read.csv("data/tags.csv") # not used currently

# Movie Search Matrix ----

genres <- as.data.frame(movies$genres, stringsAsFactors = FALSE)
genres <- as.data.frame(tstrsplit(genres[,1], '[|]',
                                type.convert = TRUE),
                        stringsAsFactors = FALSE)
colnames(genres) <- c(1:10)

genres_list <- as.array(unique(unlist(genres)))
genres_list <- genres_list[-c(19:21)] # remove entries that are not genres


## create matrix of genres

number_movies <- nrow(movies)
number_genres <- nrow(genres_list)

genre_matrix <- matrix(0, number_movies + 1, number_genres) # create empty matrix to populate with genre encoding
genre_matrix[1,] <- genres_list
colnames(genre_matrix) <- genres_list

## populate matrix
 for (i in 1:nrow(genres)){
   for (c in 1:ncol(genres)){
    genmat_col = which(genre_matrix[1,] == genres[i, c])
    genre_matrix[i+1, genmat_col] <- 1
   }
 }

genre_matrix <- as.data.frame(genre_matrix[-1,], stringsAsFactors = FALSE)
## change character entries to integers
for (c in 1:ncol(genre_matrix)){
  genre_matrix[,c] <- as.integer(genre_matrix[,c])
}

## Create matrix to search for movies by genre

## separate title and year column

title <- as.data.frame(movies$title, stringsAsFactors = FALSE)
colnames(title) <- "title"
title_year_split <- title %>% tidyr::separate(col = title, into = c("title", "year"), sep = -6)
title_year_split$year <- gsub("\\(|\\)", "", title_year_split$year) # remove brackets

search_matrix <- cbind(movies[,1], title_year_split, genre_matrix)
colnames(search_matrix) <- c('movieId', 'title', "year", genres_list)

## write to csv

write.csv(search_matrix, "data/search.csv")

# User Profiles ----

binary_ratings <- ratings

# map ratings to binary - SIMPLIFICATION FOR INITIAL IMPLEMENTATION - REVISIT LATER
for (i in 1:nrow(binary_ratings)){
  if (binary_ratings[i, 3] > 3){
    binary_ratings[i, 3] <- 1
  } else {
  binary_ratings[i, 3] <- -1
  }
} # This is slow - come back and use apply functions if time allows

# test <- function(i){
#   if (binary_ratings[i, 3] > 3){
#   binary_ratings[i, 3] <- 1
#   } else {
#   binary_ratings[i, 3] <- -1
#   }
# }
# 
# lapply(binary_ratings, test)

# put into correct format (not tidy)
binary_ratings <- dcast(binary_ratings, movieId~userId, value.var = "rating", na.rm=FALSE)
binary_ratings[is.na(binary_ratings)] <- 0  

# rows are movie ids, so remove movieId column

binary_ratings <- binary_ratings[,-1]

      
