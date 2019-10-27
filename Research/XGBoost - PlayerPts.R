#XGBoost - Player Points

Train <- sparseMatrix(c(i = Points_Train$TOI, 
                        Points_Train$A, 
                        Points_Train$S, 
                        Points_Train$Cumulative_Goals
)
)

Resp <- Points_Train$G

Data <- list(Train, Resp)

xgboost::xgboost(data = Data[[1]], 
                 label = Data[[2]], 
                 max.depth = 2, 
                 eta = 1, 
                 nrounds = 5,
                 nthread = 2,
                 objective = "count:poisson")
