## ----setup, include=FALSE, warning=FALSE---------------------

knitr::opts_chunk$set(echo = FALSE, fig.align = 'center')


## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE----
# Install all needed libraries if it is not present
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools")
if(!require(kassambara/easyGgplot2)) install_github("kassambara/easyGgplot2")
require(plotly)
require(rpart)
require(lattice)
require(ggthemes)
require(GGally)
require(knitr)
require(tidyr)
require(wordcloud)
require(kableExtra)
require(RColorBrewer)
require(dplyr)
require(ggplot2)
require(lubridate)
require(h2o)
require(stringr)
require(formatR)
require(recosystem)
require(knitr)
require(scales)



## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE----
# Loading all needed libraries
library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(ggplot2)
library(caret)
library(easyGgplot2)
library(devtools)
library(plotly)
library(rpart)
library(lattice)
library(ggthemes)
library(GGally)
library(knitr)
library(tidyr)
library(wordcloud)
library(kableExtra)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(lubridate)
library(h2o)
library(stringr)
library(formatR)
library(recosystem)
library(knitr)
library(scales)




## ----echo=FALSE, message=FALSE, warning=FALSE----------------
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# MovieLens 10M dataset:
#https://grouplens.org/datasets/movielens/10m/
#http://files.grouplens.org/datasets/movielens/ml-10m.zip

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)

# if using R 3.5 or earlier, use `set.seed(1)` instead
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



# overview of the EDX dataset
## ------------------------------------------------------------
str(edx)


## ------------------------------------------------------------
#summary of the edx data set
summary(edx)


## ------------------------------------------------------------
#summary of the valudation dataset
summary(validation)


## ------------------------------------------------------------
#Table with movies and average ratings
edx_movies_metrics <- edx %>% separate_rows(genres, sep = "\\|") %>% group_by(genres) %>% summarize(Ratings_perGenre_Sum = n(), Ratings_perGenre_Mean = mean(rating), Movies_perGenre_Sum = n_distinct(movieId), Users_perGenre_Sum = n_distinct(userId))
edx_movies_metrics



## ------------------------------------------------------------
edx_movies_metrics$Ratings_perGenre_Mean <- round(edx_movies_metrics$Ratings_perGenre_Mean, digits = 2)
edx_movies_metrics[order(-edx_movies_metrics$Ratings_perGenre_Sum), ]



## ----echo=FALSE, fig.height=5, fig.width=8, message=FALSE, warning=FALSE----
ggplot(data = edx_movies_metrics, aes(x = reorder(genres, -Ratings_perGenre_Sum), y = Ratings_perGenre_Sum, colour = "red", fill = genres, label = Ratings_perGenre_Sum)) + geom_col() + geom_text(aes(label = comma(Ratings_perGenre_Sum)), angle = 50, color = "white", size = 2, check_overlap = T, position = position_stack((vjust = 0.5))) + 
  xlab("Movies") + ylab("Million ratings") + ggtitle("Rating per genre") + 
  scale_y_continuous(labels = scales::comma) + theme_solarized(light = FALSE) + 
  theme(axis.text.x = element_text(angle = 80))


## ----fig.height=3, fig.width=5, message=FALSE, warning=FALSE----
#Add mean line to the histogram plot.
ggplot2.histogram(data=edx$rating, xName='Rating',
                  fill="white", color="blue",
                  addMeanLine=TRUE, meanLineColor="red",
                  meanLineType="dashed", meanLineSize=2)


## ----echo=FALSE----------------------------------------------
#pie chart of sample versus population
df <- data.frame(group = c("Training set", "Test set"),
  value = c(9000061, 999999))

bp <- ggplot(df, aes(x="", y=value/10, fill=group))+
geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()


## ------------------------------------------------------------
# calculating the overall average rating on the training dataset
mu <- mean(edx$rating)

# predicting every unknown ratings with mu and calculate the RMSE
RMSE(validation$rating, mu)




## ------------------------------------------------------------
# Method 1

# add average ranking term, b_i
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predict all unknown ratings with mu and b_i
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# RMSE m1
RMSE(validation$rating, predicted_ratings)



## ---- echo=FALSE, fig.height=3, fig.cap = "Here is shown the frequency of xxx"----
# plotting M1
qplot(b_i, data = b_i, bins = 25, color = I("black")) 



## ----message=FALSE, warning=FALSE----------------------------
# compute user bias 
b_u <- edx %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# predict new ratings with movie and user bias
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
# Calucate m2
RMSE(predicted_ratings, validation$rating)



## ----echo=FALSE, message=FALSE, warning=FALSE----------------

# determine best lambda from a sequence
lambdas <- seq(from=0, to=10, by=0.25)
# output RMSE of each lambda, repeat earlier steps (with regularization)
rmses <- sapply(lambdas, function(l){
  # calculate average rating across training data
  mu <- mean(edx$rating)
  # compute regularized movie bias term
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  # compute regularize user bias term
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  # compute predictions on validation set based on these above terms
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  # output RMSE of these predictions
  return(RMSE(predicted_ratings, validation$rating))
})



## ----echo=FALSE, fig.cap="Shows the RMSEs of each $lambda$ - seq(from=0, to=10, by=0.25)"----
# M3 calculation
qplot(lambdas, rmses)



## ------------------------------------------------------------
# print minimum RMSE 
min(rmses)



## ----message=FALSE, warning=FALSE----------------------------
# M4 calculation
lam <- lambdas[which.min(rmses)]

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lam))
# compute regularize user bias term
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lam))
# compute predictions on validation set based on these above terms
predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
# output RMSE of these predictions
RMSE(predicted_ratings, validation$rating)


## ----echo=FALSE, message=FALSE, warning=FALSE----------------
#Plotting the development in REMSES
x <- c(1.06120, 0.94391, 0.86535, 0.86481)
df <- data.frame(method=c("Method_1", "Method_2", "Method_3", "Method_4"), rms=c(1.06120, 0.94391, 0.86535, 0.86481))

ggplot(data=df, aes(x=method, y=rms, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point() +
  geom_line(linetype="dotted", color="red", size=2)+
  geom_point(color="blue", size=3) + ggtitle("Incremental RMSE")


