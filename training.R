library(tidyverse)
library(randomForest)
source("data_processing.R")
library(caret)
# load data
pokemon <- read_csv("data/pokemon.csv")
combats <- read_csv("data/combats.csv")
# training data
data <- AB_advantage_tibble(combats = combats, pokemon = pokemon)
# train on 80% of data
train <- data %>% sample_frac(.8)
test <- anti_join(data, train)
# train a random forest model with 500 trees
# pas forcément à run à chaque fois
(model <- randomForest(winner~., data = train, ntree = 500, na.action = na.omit))
# test 
# on predit sans la classe
pred <- predict(model, test[,1:10])
# matrice de confusion
conf <- confusionMatrix(data = pred, reference = test$winner)
# save the model to avoid repeting everythin all the time
saveRDS(model, file =  "models/randomForest500.rda")
# model2 <- readRDS("models/randomForest500.rda")