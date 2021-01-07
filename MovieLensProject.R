install.packages("xfun")
install.packages("knitr")
remotes::update_packages()

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################


# Note: this process could take a couple of minutes


install.packages("rmarkdown")


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
install.packages('plyr', repos = "http://cran.us.r-project.org")

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(caret)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#######END OF GIVEN CODE#######################


# Check for any #NA Values
anyNA(edx)

#######################################
#EXPLORING THE DATA
#######################################

head(edx)
# the release year needs to be separated from the movie title
# timestamp format needs to be converted , this represents the rating date
# the genres are in the same column need to be separated by the pipe “|”
# The same movie entry might belong to more than one genre. 
# Every distinct rating by a user is on a different row.
# userId, movieId, timestamp (date&time) are: quantitative - Discrete unique numbers.
# Title and genres are: qualitative and not unique.

unique(edx$rating)
# 10 different rate scores 
# a rate is give by one user for one movie.

summary(edx)

#the rate mean 3.512 


edx %>% summarize(n_users = n_distinct(userId) , n_movies = n_distinct(movieId))
# distinct number of users = 69878 and distinct number of movies = 10677.

head(validation)
#The validation set is the same format & contains the same columns as our training set and therefore we will perform the same data transformation on both training and test datasets

##############################
# TRANSFORMATION OF THE DATA
##############################

# String Extract to extract the release year from title and store in a separate field
edx <- edx %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))

validation <- validation %>% mutate(releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))

# Change the format of the rate timestamp to date.
edx <- edx %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01",  tz = "GMT"))
#get the rate year
edx$timestamp <- format(edx$timestamp, "%Y")

#same for validation set
validation <- validation %>% mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01", tz = "GMT"))
validation$timestamp <- format(validation$timestamp, "%Y")


#Age of movie
edx <- edx %>% mutate(movie_age = 2020 - releaseyear)

validation <- validation %>% mutate(movie_age = 2020 - releaseyear)

head(edx)
head(validation)

###########################
# VISUALISING THE DATA
##########################

# Genre Analysis
# Creation of a new dataframe with useful measures to identify outliers and further analyse the data 
edx_genre_measures  <- edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% 
  summarize(Ratings_perGenre_Sum = n(), Ratings_perGenre_Avg = mean(rating), 
            Movies_perGenre_Sum = n_distinct(movieId), Users_perGenre_Sum = n_distinct(userId))
edx_genre_measures[order(-edx_genre_measures$Movies_perGenre_Sum), ]

#Genre Dimension 

edx_genre_measures %>% ggplot( aes(x = Ratings_perGenre_Sum,y=reorder(genres, Ratings_perGenre_Sum ) ))+
     geom_bar(aes(fill =genres),stat = "identity")+ 
    xlab("Sum of Ratings")+  ylab("Genres") +
     labs(title = " No. of Rating for per Genre")+
     scale_x_continuous(labels = scales::comma)+
     theme(axis.text.x  = element_text(angle= 90, vjust = 50 ))+
     theme_light()

# Observations
# 19 distinct genres
# Drama is the most rated genre = 3 910 127
# Comedy & Action – 2nd & 3rd highest
# IMAX least amount of ratings – could be due to the time of this dataset - IMAX was still new 
# There are also movies that have no genres
# Genre may be used as a good predictor but will look at genre over the release year

# Outlier -  One movie with no genre with only 7 users who rated this is considered an outlier 
# If using Genre as a predictor removing this row may yield better results.
edx_genre_measures <- subset(edx_genre_measures, genres != "(no genres listed)")
edx_genre_measures

# Some genres have very low sums of ratings – Will check the correlation between sums of rating and the rating mean

edx_genre_measures  %>%
    ggplot(aes(Ratings_perGenre_Sum, Ratings_perGenre_Avg)) +
     geom_point() +
     theme_hc() + 
  geom_smooth() +
  scale_x_continuous(labels = scales::comma)+
     ggtitle("Ratings_perGenre_Sum Year vs. Avg Rating")

# We can clearing see in this graph that the lower the Sum of Ratings the higher the Average Rate.
# Outlier -  These values can be treated as outliers as there is a slight bias due to not enough people rating the movie.

#Create same metrix for the validation set.

validation_genre_measures <- validation %>% separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>% summarize(Ratings_perGenre_Sum = n(), Ratings_perGenre_Avg = mean(rating), 
                                 Movies_perGenre_Sum = n_distinct(movieId), Users_perGenre_Sum = n_distinct(userId))

validation_genre_measures[order(-validation_genre_measures$Movies_perGenre_Sum), ]

#/////////////////////////////////////
#Rating Distribution
 edx %>% 
     ggplot(aes(rating)) + 
     geom_histogram(binwidth=0.2 , color="black", fill="light blue") + 
    labs(title = "Rating Distribution", 
          x = "Ratings Scale", y = "Sum Of Rating") 

#Movie Dimension
 #number of rating per movies 
 edx %>% count(movieId) %>% ggplot(aes(n))+
   geom_histogram(binwidth=0.2 ,color = "black" , fill= "light blue")+
   scale_x_log10()+
   ggtitle("No. of Ratings Per Movie")+
   theme_gray()
 
 # Some movies get rated more than others, indicating some movies may be more popular than others.
# I will add this to the training set and test set – Number of ratings per movie
 
edx <- edx %>% group_by(movieId) %>% mutate(Users_perMovie  = n())
validation <- validation %>% group_by(movieId) %>% mutate(Users_perMovie = n())
 
# Add the average rating per movie for each row
edx <- edx %>% group_by(movieId) %>% mutate(Avg_rating_by_movie = round(mean(rating)*2)/2 )
validation <- validation %>% group_by(movieId) %>% mutate(Avg_rating_by_movie = round(mean(rating)*2)/2 )
 
#User Dimension
#number of rating per user   

# We could penalized users with low number of reviews.
# I will add this measure to the training set and test set - Number of Ratings per user

edx <- edx %>% group_by(userId) %>% mutate(Movies_perUser = n())
validation <- validation %>% group_by(userId) %>% mutate(Movies_perUser = n())

edx %>% group_by(number_movies_byUser) %>%
     summarize(avg_rating = mean(rating)) %>%
     ggplot(aes(number_movies_byUser, avg_rating)) +
     geom_point() +
     theme_hc() + 
     geom_smooth() +
     ggtitle("No. movies per User vs. Rating")

# As mentioned earlier - We can see the lower the number of users per movie the higher the rating . 
# There is a slight bias due to not enough people rating the movie
  
#Release Year / Age of Movie Analysis

#No. of movies per year
edx %>%   group_by(releaseyear) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% ggplot(aes(releaseyear,count)) + 
  geom_line(color="blue")+ 
  labs(title = " No. of Movies by release year")+
  theme(axis.text.x  = element_text(angle= 90, vjust = 50 ))+
  theme_light()


# View release year vs rating
edx %>% group_by(releaseyear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(releaseyear, rating)) +
  geom_point() +
  theme_hc() + 
  geom_smooth() +
  ggtitle("Release Year vs. Rating")
# Older "classics" get higher ratings. This could allow us to penalize a movie based on release year
# by a calculated weight.

avg_rating_per_age <- edx %>% group_by(movie_age) %>% summarize(avg_rating_by_age = mean(rating))
avg_rating_per_age

install.packages("ggpubr")
library("ggpubr")
ggscatter(avg_rating_per_age, x = "movie_age", y = "avg_rating_by_age", 
          add = "reg.line", conf.int = TRUE, 
          color = "avg_rating_by_age",
          title = "Age of Movie vs Avg Rating - Correlation",
          cor.coef = TRUE, cor.method = "pearson")


edx <- subset(edx, select = -c(releaseyear) )
validation <- subset(validation, select = -c(releaseyear) )

#Time Rated Dimension
avg_rating_per_timestamp_year <- edx %>% group_by(timestamp) %>% summarize(avg_rating = mean(rating))
avg_rating_per_timestamp_year


ggscatter(avg_rating_per_timestamp_year, x = "timestamp", y = "avg_rating", 
          add = "reg.line", conf.int = TRUE, 
          title = "Year of Rating vs Avg Rating - Correlation",
          cor.coef = TRUE, cor.method = "pearson")

# We observe that the oldest rating was in 1995 given the highest rating – this is also an outlier.
# There is a slight downward trend with the remaining of the movies , 
# showing that the older the rating the higher the avg rating.


############################################################
# REMOVING OUTLIERS MENSIONED ABOVE
############################################################

#remove entries with no genres
edx_clean <- subset(edx, genres != "(no genres listed)")

#remove entries with timestamp year =1995
edx_clean <- subset(edx_clean, timestamp != "1995")




############################################################
# Model Building and Training
############################################################


#Partition the edx data 
library(caret)
set.seed(1)
test_index <- createDataPartition(y = edx_clean$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx_clean[-test_index,]
test_set <- edx_clean[test_index,]
#to make sure we don't include users and movies in the test set that do not appear in 
#the training set, we remove these entries using the semi_join function:
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


train_set[1,2,4,7,8,9] = scale(train_set[1,2,4,7,8,9])
test_set[1,2,4,7,8,9] = scale(test_set[1,2,4,7,8,9])

######################
## RMSE
######################

# The rmse() function is available in Metrics package in R is used to calculate root mean square error
# however I will define the RMSE function with the following:

RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))  }

############ MODEL 1 ############ 
## Get Ave Rate across all movies
Pred_1 <- mean(train_set$rating)
Pred_1

## Average Rate across all movies (3.512) is used as a baseline
Rmse_1 <- RMSE(test_set$rating,Pred_1)
Rmse_1


############ MODEL 2 ############ 
#Multi Linear Regression 1

MLRregressor = lm(formula = rating ~ movieId + userId + movie_age + Users_perMovie + Avg_rating_per_movie + Movies_perUser ,
                   data = train_set)
summary(MLRregressor)

# Using Multi Linear Regression we can see from the P value and 3 stars next to the values 
# that all dimension used for the Regressor are highly statisitcally significant 

Pred_2 = predict(MLRregressor, newdata = test_set)
Pred_2
Rmse_2 <- RMSE( test_set$rating,Pred_2)
Rmse_2





#==============================================================================================================

######### MODEL 2 ######## 
mu <- mean(train_set$rating)
movie_mean <- train_set %>% 
    group_by(movieId) %>%
summarize(b_i = mean(rating - Pred_1))  
movie_mean

Pred_2 <- test_set %>%
  left_join(movie_mean, by = "movieId") %>%
  mutate(pred = Pred_1 + b_i) %>%
  pull(b_i)
 Rmse_2 <- rmse(test_set$rating, Pred_2)
 Rmse_2


 


######### MODEL 3 ######## 
#Ploynomial Regression 
PLoyRegressData = train_set %>% select(1,2,7,8,9,10,3)

PLoyRegressData$movieId2 = PLoyRegressData$movieId^2
PLoyRegressData$movieId3 = PLoyRegressData$movieId^3
PLoyRegressData$movieId4 = PLoyRegressData$movieId^4
model <- lm(rating ~ poly(movieId, 5, raw = TRUE),  
            data = train_set)

predictions <- model %>% predict(test_set) 
# Model performance 
modelPerfomance = data.frame( 
  RMSE = RMSE(predictions, test_set$rating), 
  R2 = R2(predictions, test_set$rating) 
) 


model2 <- lm(rating ~ movieId + I(movieId^2) + userId + releaseyear, data = train_set) 
model2
predictions <- model2 %>% predict(test_set) 
RMSE(predictions, test_set$rating)


ggplot(train_set, aes(movieId, rating) ) + geom_point() +  
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE)) 

pred_6 = predict(poly_reg, newdata = test_set)

rmse(test_set$rating, pred_6 )

######### MODEL 3 ######## 
# K-Nearest Neighbors (K-NN)

# In this example we are going to use only 2 dimensions to predict the ratings
KNN_dataset = edx_clean %>% select(1,2,7,8,9,10,3)

# Splitting the dataset into the Training set and Test set
KNN_test_index <- createDataPartition(y = KNN_dataset$rating, times = 1, p = 0.2, list = FALSE)
KNN_train_set <- KNN_dataset[-test_index,]
KNN_test_set <- KNN_dataset[test_index,]
#to make sure we don’t include users and movies in the test set that do not appear in 
#the training set, we remove these entries using the semi_join function:
KNN_test_set <- KNN_test_set %>% 
  semi_join(KNN_train_set, by = "movieId") %>%
  semi_join(KNN_train_set, by = "userId")

# Feature Scaling
KNN_train_set[-7] = scale(KNN_train_set[-7])
KNN_test_set[-7] = scale(KNN_test_set[-7])

# Fitting K-NN to the Training set and Predicting the Test set results
train = KNN_train_set[, -7]
cl=KNN_train_set[, 7]
length(cl)
length(train)
library(class)
Pred_3 = knn(train = KNN_train_set[, -7, drop = FALSE],
             test = KNN_test_set[, -7, drop = FALSE],
             cl = KNN_train_set$rating,
             k = 5,
             prob = TRUE)


##### MODEL 4 #####

# Matrix Factorization 

install.packages("recosystem")
library(recosystem)
set.seed(123)

train_set_fac <- train_set %>% select(movieId, userId, rating, movie_age)
test_set_fac <- test_set %>% select(movieId, userId, rating, movie_age)

train_set_fac <- as.matrix(train_set_fac)
test_set_fac <- as.matrix(test_set_fac)

write.table(train_set_fac, file = "edx_train_set.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)

write.table(test_set_fac, file = "edx_test_set.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)

set.seed(1)
MF_train_dataset <- data_file("edx_train_set.txt")
MF_test_dataset <- data_file("edx_test_set.txt")

# Create a model object (a Reference Class object in R) by calling Reco().
recommender <- Reco()

# Call the $tune() method to select best tuning parameters along a set of candidate values.
opts = recommender$tune(MF_train_dataset, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2), 
        costp_l1 = 0, costq_l1 = 0, nthread = 1, niter = 10))
opts

# Train the model by calling the $train() method. 
# A number of parameters can be set inside the function, possibly coming from the result of $tune().

recommender$train(MF_train_dataset, opts = c(opts$min, nthread = 1, niter = 20))

## Use the $predict() method to compute predicted values.
## Write predictions to file
pred_file <- tempfile()
recommender$predict(MF_test_dataset, out_file(pred_file))
# prediction output generated at C:\Users\SHANNA~1\AppData\Local\Temp\RtmpMZ9KMa\file32f47b093ee9

print(scan(pred_file, n = 20))


edx_test_ratings <- read.table("edx_test_set.txt", header = FALSE, sep = " ")$V3


pred_ratings <- scan(pred_file)

# will calculate RMSE
Rmse_4 <- RMSE(edx_test_ratings, pred_ratings)
Rmse_4


#Final Test on validation Set
validation_fac <- validation %>% select(movieId, userId, rating, movie_age)
validation_fac <- as.matrix(validation_fac)

write.table(validation_fac, file = "validation_set.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)

MF_validation_dataset <- data_file("validation_set.txt")
recommender <- Reco()

recommender$train(MF_train_dataset, opts = c(opts$min, nthread = 1, niter = 20))

pred_file <- tempfile()
recommender$predict(MF_validation_dataset, out_file(pred_file))
real_ratings <- read.table("validation_set.txt", header = FALSE, sep = " ")$V3
pred_ratings <- scan(pred_file)
Final_Rmse <- RMSE(real_ratings, pred_ratings)
Final_Rmse
