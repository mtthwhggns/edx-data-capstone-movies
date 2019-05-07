#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## Now the original work begins - let's create a function to calculate the RMSE

calculate_rmse <- function(predictions, actual_values){
  sqrt(mean((actual_values - predictions) ^ 2))
}

## Start with the overall mean rating

mean_rating <- mean(edx$rating)

## Now let's create the data frame of movie effects

movie_effects <- edx %>% 
                   group_by(movieId) %>% 
                   summarize(movie_effect = mean(rating - mean_rating),
                                                 ratings_for_movie = n())

## And the user effects that are able to take account of the movie effects

user_effects <- edx %>% 
  group_by(userId) %>% 
  left_join(movie_effects, by = "movieId") %>%
  summarize(user_effect = mean(rating - movie_effect - mean_rating), 
            ratings_by_user = n())

## And finally let's create the predictions and print the RMSE

predictions <- validation %>% 
  left_join(movie_effects, by = "movieId") %>%
  left_join(user_effects, by = "userId") %>%
  mutate(combined_effect = user_effect + movie_effect) %>%
  mutate(prediction = mean_rating + combined_effect) %>%
  .$prediction

# And let's use the new predictions to calculate am RMSE value
rmse <- calculate_rmse(predictions, validation$rating)

# And, finally, print the calculated RMSE value.
print(rmse)
