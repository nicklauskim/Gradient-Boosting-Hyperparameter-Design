library(data.table)
library(CHNOSZ)
library(xgboost)
library(lhs)
library(LHD)
library(MaxPro)
library(lightgbm)
library(methods)
library(MLmetrics)


# Make sure the R session's working directory is set to the right place
setwd('~/Documents/STATS 201A/Final Project')


# Load data
df <- fread('./Data/gsenergies/lambda.csv', drop = 1)
Y <- df$Eat


set.seed(101)
inTrain <- sample(1:dim(df)[1], size = floor(0.7*dim(df)[1]), replace = FALSE)
train.Y <- Y[inTrain]; test.Y <- Y[-inTrain]
train.X <- df[inTrain,]; test.X <- df[-inTrain,] 


dtrain.X <- xgb.DMatrix(as.matrix(train.X), label = train.Y/scl)
dtest.X <- xgb.DMatrix(as.matrix(test.X), label = test.Y/scl)


# Generate Latin hypercube designs
X <- randomLHS(n = 10, k = 4)
A <- geneticLHS(10, 4, pop = 100, gen = 5, pMut = 0.1)
B <- maximinLHS(10, 4, method = "build", dup = 5)
D <- maximinLHS(10, 4, method = "iterative", optimize.on = "result", eps = 0.01, maxIter = 300)
E <- improvedLHS(10, 4, dup = 5)
G <- optimumLHS(10, 4, maxSweeps = 10, eps = 0.01)

data.frame(method = c("random","genetic","maximin","maximin","improved","optimum"),
           mean_dist = c(mean(dist(X)), mean(dist(A)), mean(dist(B)),
                         mean(dist(D)), mean(dist(E)), mean(dist(G))),
           min_dist = c(min(dist(X)), min(dist(A)), min(dist(B)),
                        min(dist(D)), min(dist(E)), min(dist(G))))



# Their model
# Watchlist
watchlist <- list(train=dtrain.X, test=dtest.X)

# Parameters
param <- list(booster="gbtree",
              eval_metric="rmse",
              eta=0.0156,
              colsample_bytree = 0.4,
              max_depth = 16,
              min_child_weight = 10,
              gamma = 0.0,
              lambda = 1.0,
              subsample = 0.8)

# Test xgboost
xgb.model <- xgb.train(data=dtrain.X, params = param, watchlist=watchlist, nround = 600)


# Prediction
pred <- predict(xgb.model, newdata = dtest.X)*scl

# RMSE they got
sqrt(mean((pred - test.Y)^2)) 




# Our model using LHD to optimize hps

# Iterate to train multiple models
preds <- matrix(0, nrow = nrow(B), ncol = 5)
results <- matrix(0, nrow = nrow(B), ncol = 5)
mse <- c()
design <- B

for (i in 1:nrow(design)){
  
  # Set parameters for model training
  train_params <- list(
    learning_rate = 0.05 * B[i, 1] + 0.05,
    max_depth = floor(10 * B[i, 2]) + 15,
    min_data_in_leaf = floor(10 * B[i, 3]),
    feature_fraction = 0.5 * B[i, 4] + 0.25,
    objective = "regression",
    nthread = 2L
  )
  
  # Train model with cross-validation
  sse <- 0
  num_folds <- 5
  for (j in 1:num_folds){  
    # Split data into training and validation sets
    # We have 21263 rows altogether
    n <- 14000    # Roughly 2/3 as in the paper
    train_rows <- sample(nrow(data), n)
    train <- data[train_rows, ]
    test <- data[-train_rows, ]
    
    
    
    dtrain.X
    
    
    # Train model
    bst <- lightgbm(
      data = as.matrix(subset(train, select = -c(critical_temp))),
      params = train_params,
      label = as.matrix(train$critical_temp),
      nrounds = 750L
    )
    
    pred <- predict(bst, as.matrix(subset(test, select = -c(critical_temp))))
    sse <- sse + MSE(pred, test$critical_temp)
  }  
  
  mse[i] <- sse / num_folds
  
  preds[i, ] <- c(train_params$learning_rate, 
                  train_params$max_depth, 
                  train_params$min_data_in_leaf, 
                  train_params$feature_fraction,
                  mse[i])
  
  results[i, ] <- c(B[i, 1], B[i, 2], B[i, 3], B[i, 4], mse[i])
}

colnames(preds) <- c('learning_rate', 'max_depth', 'min_data_in_leaf', 'feature_fraction', 'mse')
colnames(results) <- c('learning_rate', 'max_depth', 'min_data_in_leaf', 'feature_fraction', 'mse')


# Save results to file
write.csv(x = preds, file = "../Results/sc_preds.csv")
write.csv(x = results, file = "../Results/sc_results.csv")










