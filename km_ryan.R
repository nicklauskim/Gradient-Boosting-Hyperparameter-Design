library(data.table)
library(DiceKriging)
library(DiceOptim)
library(DiceView)

path = './Results/results_7.csv'
DT = fread(path)
str(DT)

# pull out the columns we need
new_results = DT[ , .(learn_rate, max_depths,
                      min_node,col_samples)]

# pull out the results column
new_res = DT$results

# only feed results 1-10
# 11-20 were suggested by EGO.nsteps 
# use only linear models for the mean function 
# added noise var because of the sampling 
# in the algorithm 
model_2 = km(formula = ~ . ,
             design = new_results[1:10,], 
             response = new_res[1:10],
             noise.var = rep(0.01, 10))

# need to make the plot window huge
DiceView::sectionview(model_2, center = c(0.2,0.5,0.5,0.5) )


