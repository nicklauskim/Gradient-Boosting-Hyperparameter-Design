library(data.table)
library(DiceDesign)
library(DiceEval)
library(DiceKriging)
library(DiceOptim)
library(DiceView)


# Try EGO.nsteps function

model <- km(formula = ~x + I(x^2), design = data.frame(x = inputs),
            response = output, covtype = "matern5_2", coef.trend = trend,
            coef.cov = theta, coef.var = sigma^2)


# Make sure the R session's working directory is set to the right place
setwd('~/Documents/STATS 201A/Final Project')
path = './Results/sc_results.csv'
DT = fread(path)
str(DT)

# pull out the columns we need
new_design = DT[ , .(learning_rate, max_depth, min_data_in_leaf, feature_fraction)]

# pull out the results column
new_results = DT$mse


 
# Use only linear models for the mean function 
# Added noise var because of the sampling in the algorithm 
model_2 = km(formula = ~ .,
             design = new_design[1:10, ], 
             response = new_results[1:10],
             noise.var = rep(0.01, 10))

# Make the plot window bigger
DiceView::sectionview(model_2, center = c(0.2, 0.5, 0.5, 0.5))




