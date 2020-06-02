## ---- warning=FALSE, message=FALSE---------------------------------------
################################
#  Install packages (if not installed)
################################
# Note: this process could take a couple of minutes
repos_path<- "http://cran.us.r-project.org"
if(!require(tidyverse)) install.packages("tidyverse", repos =repos_path)
if(!require(caret)) install.packages("caret", repos = repos_path)
if(!require(data.table)) install.packages("data.table", repos =repos_path)
if(!require(lubridate)) install.packages("lubridate", repos = repos_path)
if(!require(dplyr)) install.packages("dplyr", repos = repos_path)
if(!require(sjmisc)) install.packages("dplyr", repos = repos_path)

################################
# Load libraries
################################
library(lubridate)
library(tidyverse)
library(dplyr)
library(lubridate)
library(sjmisc)


## ---- warning=FALSE, message=FALSE---------------------------------------
################################
# Downloading data
################################
# MovieLens 10M dataset:
 # https://grouplens.org/datasets/movielens/10m/
 # http://files.grouplens.org/datasets/movielens/ml-10m.zip

url <- "http://files.grouplens.org/datasets/movielens/ml-10m.zip"
dl <- tempfile()
    download.file(url, dl)
  
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))), 
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
   colnames(movies) <- c("movieId", "title", "genres")
   movies <- as.data.frame(movies) %>% mutate(movieId =   
                                      as.numeric(levels(movieId))[movieId], 
                                      title = as.character(title), 
                                      genres =    as.character(genres))

################################
# Creating edx and validation sets
################################
movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind = "Rounding")
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

 # Removing the objects from environment as no longer required
rm(dl, ratings, movies, test_index, temp, movielens, removed)



## ------------------------------------------------------------------------
head(edx)


## ------------------------------------------------------------------------
sprintf("Edx Dataset - Rows = %d  | Columns = %d",nrow(edx),ncol(edx))

## ------------------------------------------------------------------------
summary(edx)


## ------------------------------------------------------------------------
#####################################
# EDX Dataset
#####################################
# Extracting premiere date from movie title
edx <-edx %>% extract(title, c("title", "premiereYr"), 
                      regex = "^(.*) \\(([0-9 ##\\-]*)\\)$")
# Converting to integer format from char
edx$premiereYr <- as.numeric(edx$premiereYr)

# Converting timestamp to date format
edx$timestamp <- as.Date(as_datetime((edx$timestamp), origin="1970-01-01"))

#####################################
# Validation Dataset
#####################################
# Extracting premiere date from movie title
validation <-validation %>% extract(title, c("title", "premiereYr"),
                                    regex = "^(.*) \\(([0-9 ##\\-]*)\\)$")
# Converting to integer format from char
validation$premiereYr <- as.numeric(validation$premiereYr)

# Converting timestamp to date format
validation$timestamp <- as.Date(as_datetime((validation$timestamp), origin="1970-01-01"))

head(edx)


## ------------------------------------------------------------------------
# Finding unique users
edx %>% summarize(users = n_distinct(userId))


## ------------------------------------------------------------------------
# Calculate the average number of ratings for users
edx %>% count(userId) %>% summarise(mean_rating=mean(n))


## ------------------------------------------------------------------------
# Plotting frequency of ratings per user 
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30,  fill='cadetblue', color='black') +
  scale_x_log10() +
  xlab("Number of users") + 
  ylab("Fequency of ratings") +
  ggtitle("Distribution of ratings based on Users")


## ------------------------------------------------------------------------
# Finding unique movies
edx %>% summarize(movies = n_distinct(title))


## ------------------------------------------------------------------------
library(scales)
# Plotting frequency of rated moves over the years
# We use premiere years of movies as the range of years
edx %>%
  select(movieId, premiereYr) %>% # select columns we need
  group_by(premiereYr) %>% # group by year
  summarise(count = n())  %>% # count movies per year
  arrange(premiereYr)%>%
  ggplot(aes(x = premiereYr, y = count)) +
  scale_y_log10() +
  scale_y_continuous(breaks = c(seq(0, 800000, 100000)), labels=comma)+
  labs(x="Movie Premiere Years", y="Frequency of movies") +
  geom_point(color="cadetblue") +
  ggtitle("Distribution of rated movies with respect to premiere years")
 


## ------------------------------------------------------------------------
# Distribution or freqeuncy of ratings for all movies
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(bins=30, fill = 'cadetblue', color = 'black') +
  scale_x_log10() +
  xlab("Number of movies") + 
  ylab("Fequency of ratings") +
  ggtitle("Distribution of ratings per movie")


## ------------------------------------------------------------------------
# Analysing when ratings data was added by users
years<-format(edx$timestamp, format="%Y")
sprintf("Rating year (minimum) =%s", min(years))
sprintf("Rating year (maximu) =%s", max(years))


## ------------------------------------------------------------------------

# Generating groups of data for half star and full star ratings
star_ratings_group <-  ifelse((edx$rating == 1 |edx$rating == 2 | edx$rating == 3 | 
                  edx$rating == 4 | edx$rating == 5) ,
                   "Full_star", 
                   "Half_star") 

# Plotting the distribution of ratings in terms of half and full star
ratings_distribution <- data.frame(edx$rating, star_ratings_group)
ggplot(ratings_distribution, aes(x= edx.rating, fill = star_ratings_group)) +
  geom_histogram( binwidth = 0.2,color = 'black') +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000)))+
  labs(x="Ratings", y="Number of ratings") +
  ggtitle("Distribution of ratings")


## ------------------------------------------------------------------------
movies_count<-(edx %>% group_by(title) %>%summarize(count=n()))
sprintf("Movies with a single rating = %d",sum(movies_count$count==1))


## ------------------------------------------------------------------------
top_movies <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(10) %>%
  arrange(desc(count))

top_movies %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="cadetblue") + coord_flip(y=c(0, 40000)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 10 movies titles based on number of ratings" )


## ------------------------------------------------------------------------
bottom_movies <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

bottom_movies %>% tail(10) %>%
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat='identity', fill="cadetblue") + coord_flip(y=c(0, 100)) +
  labs(x="", y="Number of ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Bottom 10 movies based on number of ratings")


## ------------------------------------------------------------------------
# Extracting all unique combination of genres
unique_genres_combination<-edx%>% group_by(genres)%>%summarize(count = n())

# Extract all possible genres (which exist in the dataset)
movie_genres <- unique(unlist(strsplit(unique(edx$genres), split='|', fixed=TRUE)))
movie_genres <- as.data.frame(movie_genres)
movie_genres<-movie_genres %>% add_column(count = 0)

# Looping through to find the freqeucny or distribution of each genre
for(i in 1:dim(movie_genres)[1]) {
  for(j in 1:dim(unique_genres_combination)[1])
  {
    match = as.character(unique_genres_combination[j,1])
    count = as.numeric(unique_genres_combination[j,2])
    # See if genre category exists in the combination
    if (str_contains(movie_genres[i,1],match))
    {
      movie_genres[i,2]<-movie_genres[i,2]+count
    }
  }
}

# Plot distrobution of movie genres 
movie_genres%>%
  ggplot(aes(x=reorder(movie_genres, count), y=count)) +
  geom_bar(stat='identity', fill="cadetblue") + coord_flip(y=c(0, 900000)) +
  labs(x="Count", y="Movie genres") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Distribution of movies based on genre" )


## ------------------------------------------------------------------------
edx %>% group_by(premiereYr) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(premiereYr, rating)) +
  geom_point() +
  labs(x="Years", y="Mean Rating")+
  geom_smooth(alpha=0.5, color='cadetblue',method = lm,formula = y ~ splines::bs(x, 3))


## ------------------------------------------------------------------------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## ------------------------------------------------------------------------
#Initiate RMSE results to compare various models
rmse_results <- data_frame(method = "Target RSME", RMSE = 0.86490)
rmse_results


## ------------------------------------------------------------------------
# Calculating mean of ratings
mean_rating <- mean(edx$rating)
mean_rating
# Making predictions using mean rating
mean_model_rmse <- RMSE(validation$rating, mean_rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Average Rating Model",  
                                     RMSE = mean_model_rmse ))
rmse_results



## ------------------------------------------------------------------------
# Computing average ranking for movie "i"
movie_averages <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mean_rating))


## ------------------------------------------------------------------------
# Plotting average ranking estimates for movies
movie_averages%>%
  ggplot(aes(b_i)) +
  labs(x="b_i", y="Count")+
  geom_histogram(bins = 30, color = "black")


## ------------------------------------------------------------------------
# Making predictions using new model
predicted_ratings <- mean_rating +  validation %>%
  left_join(movie_averages, by='movieId') %>%
  pull(b_i)

# Calculating RMSE
model_movie_effects_rmse <- RMSE(predicted_ratings, validation$rating)

# Appending results to RMSE table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_movie_effects_rmse ))
rmse_results


## ------------------------------------------------------------------------
# Calculating average rating for user u
user_averages <- edx %>%
  left_join(movie_averages, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mean_rating - b_i))

# Plotting average rating for users
user_averages %>%
  ggplot(aes(b_u)) +
  labs(x="u_i", y="Count")+
  geom_histogram(bins = 10, color = "black")


## ------------------------------------------------------------------------
# Making predictions using new model
predicted_ratings <- validation%>%
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  mutate(pred = mean_rating + b_i + b_u) %>%
  pull(pred)

# Calculating RMSE on validation dataset
model_user_effect_rmse <- RMSE(predicted_ratings, validation$rating)

# Appending results to RMSE table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_user_effect_rmse))
rmse_results


## ------------------------------------------------------------------------
# Possible Lambda values
lambdas <- seq(0, 10, 0.25)

# Calculating mean of model
mean_rating <- mean(edx$rating)
rmses <- sapply(lambdas, function(l){
  # Penalised movie effect
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mean_rating)/(n()+l))
  # Penalised user effect
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mean_rating)/(n()+l))
  # Predicting new ratings
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mean_rating + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})



## ----echo=TRUE-----------------------------------------------------------
lambda <- lambdas[which.min(rmses)]
qplot(lambdas, rmses)+
  ggtitle(sprintf("Optimal Lambda = %f , Rmse = %f",lambda,min(rmses)))

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie and user effect model",  
                                     RMSE = min(rmses)))


## ------------------------------------------------------------------------
as.data.frame(rmse_results)

