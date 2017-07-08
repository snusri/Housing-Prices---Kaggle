library(ggplot2)
library(dplyr)
library(Amelia)
library(varhandle)
library(caret)
library(moments)
library(mice)
library(corrplot)
library(randomForest)
library(gridExtra)
library(xgboost)
library(glmnet)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train_row <- nrow(train)
test_row <- nrow(test)

comb_data <- rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))

NA_sum <- sort(sapply(comb_data, function(x) sum(is.na(x))), decreasing = TRUE)
print(NA_sum)                   
 
mice_mod <- mice(comb_data[, colnames(comb_data) %in% c('LotFrontage',
                                                          'Alley','BsmtQual',' BsmtCond',
                                                          'BsmtExposure','BsmtFinType1',
                                                          'BsmtFinType2', 'Electrical',
                                                          'FireplaceQu','GarageType', 
                                                          'GarageYrBlt','GarageFinish', 
                                                          'PoolQC','Fence', 'MiscFeature', 
                                                          'PoolQC','Fence', 'MasVnrType',
                                                          'BsmtCond', 'GarageQual',
                                                          'GarageCond', "MasVnrArea", "MSZoning",
                                                          "Utilities", "BsmtFullBath",
                                                          "MSZoning", 'BsmtHalfBath', 'Functional',
                                                          'Exterior1st', 'Exterior2nd', 'BsmtFinSF1',
                                                          'BsmtFinSF2', 'BsmtUnfSf', 'TotalBsmtSF',
                                                          'Electrical', 'KitchenQual', 'GarageCars',
                                                          'GarageArea', 'SaleType',
                                                        'BsmtUnfSF')], method = "pmm")

complete_pmm <- complete(mice_mod)

sim_col <- match(colnames(complete_pmm), colnames(comb_data))

comb_data <- comb_data[,-sim_col]
comb_data <- cbind(comb_data, complete_pmm)

numeric <- sapply(comb_data, is.numeric)

numeric_dat <- comb_data[,numeric==TRUE]
dat_numeric




hotencode <- function(column, dum.vars, dataframe) {
        for(col in column){
                dataframe[col] <- as.numeric(dum.vars[comb_data[,col]])
        }
        return(dataframe)
}

qual_column <- c('ExterQual', 'ExterCond', 'GarageQual', 
               'GarageCond', 'FireplaceQu', 'KitchenQual', 
               'HeatingQC', 'BsmtQual')

qual_list <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

numeric_dat <- hotencode(qual_column, qual_list, numeric_dat)


bsmt_list <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

numeric_dat <-hotencode(c("BsmtExposure"), bsmt_list, numeric_dat)

bsmtFin_list <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2,
                   'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)

numeric_dat <- hotencode(c('BsmtFinType1','BsmtFinType2'), bsmtFin_list, numeric_dat)

functional_list <- c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4,
                     'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)

numeric_dat['Functional'] <- as.numeric(functional_list[comb_data$Functional])


garageFin_list <- c('None' = 0,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)

numeric_dat['GarageFinish'] <- as.numeric(garageFin_list[comb_data$GarageFinish])

fence_list <- c('None' = 0, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)

numeric_dat['Fence'] <- as.numeric(fence_list[comb_data$Fence])


numeric_cor <- cbind(numeric_dat[1:1460,], train['SalePrice'])


corr_df <- cor(numeric_cor)
diag(corr_df) <- 0

highcor <- as.matrix(sort(corr_df[ ,'SalePrice'], decreasing = TRUE))

corr.idx <- names(which(apply(highcor, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(corr_df[corr.idx,corr.idx]), type = 'upper', method='square', 
         addCoef.col = 'black', tl.cex = .8,cl.cex = .8, number.cex=.6)


#########################

numeric_dat['RegularLotShape'] <- (comb_data$LotShape == 'Reg') * 1
numeric_dat['LandLeveled'] <- (comb_data$LandContour == 'Lvl') * 1
numeric_dat['LandSlopeGentle'] <- (comb_data$LandSlope == 'Gtl') * 1
numeric_dat['ElectricalSB'] <- (comb_data$Electrical == 'SBrkr') * 1
numeric_dat['GarageDetchd'] <- (comb_data$GarageType == 'Detchd') * 1
numeric_dat['HasPavedDrive'] <- (comb_data$PavedDrive == 'Y') * 1
numeric_dat['HasWoodDeck'] <- (comb_data$WoodDeckSF > 0) * 1
numeric_dat['Has2ndFlr'] <- (comb_data$X2ndFlrSF > 0) * 1
numeric_dat['HasMasVnr'] <- (comb_data$MasVnrArea > 0) * 1
numeric_dat['HasShed'] <- (comb_data$MiscFeature == 'Shed') * 1



## New column when the house was not remodeled in the same year as built

numeric_dat['Remodeled'] <- (comb_data$YearBuilt != comb_data$YearRemodAdd) * 1

numeric_dat['RecentRemodel'] <- (comb_data$YearRemodAdd >= comb_data$YrSold) * 1

#New colun for house sold in the same year it was built

numeric_dat['NewHouse'] <- (numeric_dat$YearBuilt == numeric_dat$YrSold) * 1



feature_column<- c('X2ndFlrSF', 'MasVnrArea', 
                 'WoodDeckSF', 'OpenPorchSF', 
                 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')


for (col in feature_column){
        numeric_dat[str_c('Has',col)] <- (comb_data[,col] ==0) * 1
}



heating.list <- c('Po' = 0, 'Fa' = 1, 'TA' = 2, 'Gd' = 3, 'Ex' = 4)

numeric_dat['HeatingScale'] <- as.numeric(heating.list[comb_data$HeatingQC])



train_dat <- cbind(train_dat, train['SalePrice'])


dummy <- dummyVars(" ~ .",data=comb_data[,numeric==FALSE])

df.categoric <- data.frame(predict(dummy,newdata=comb_data[,numeric==FALSE]))

df <- cbind(numeric_dat, df.categoric)

nzv <- nearZeroVar(df, saveMetrics = TRUE)

col_drp <- rownames(nzv)[nzv$nzv == TRUE]

new.df <- df[,!names(df) %in% col_drp] 




train_dat <- new.df[1:1460, ]

test_dat <- new.df[1461:nrow(new.df),]

boxplot(train_dat$GrLivArea, main = "GrLivArea")

boxplot(train_dat$GarageArea, main = "GarageArea")

condition1 <- which(train_dat$GarageArea < 1200 & train_dat$GrLivArea < 4000)

train_dat <- train_dat[1:nrow(train_dat) %in% condition1, ]


y_true <- train$SalePrice[which(1:nrow(train) %in% condition1)]


true_dist <- qplot(true_y, geom = "density") + geom_histogram(aes(y=..density..),fill = "Blue", alpha = .5, bins = 75)+
        geom_line(aes(y=..density..), color='green', lwd = 1, stat = 'density') +
        stat_function(fun = dnorm, colour = 'red', lwd = 1, args = 
                              list(mean(y_true), sd(y_true))) +
        labs(x = 'SalePrice') + ggtitle("Distribution of Sale Price") + 
        annotate('text', label = paste('skewness =', signif(skewness(y_true),3)),
                 x=6e+05,y=7.5e-06)

y_train <-log(y_true+1)


train_dist <- qplot(y_train, geom = "density") + geom_histogram(aes(y=..density..),fill = "Blue", alpha = .5, bins = 75)+
        geom_line(aes(y=..density..), color='green', lwd = 1, stat = 'density') +
        stat_function(fun = dnorm, colour = 'red', lwd = 1, args = 
                              list(mean(log.sale), sd(log.sale))) +
        labs(x = 'log(SalePrice + 1)') + ggtitle("Distribution of Log(Sale Price + 1)") + 
        annotate('text', label = paste('skewness =', signif(skewness(y_train),3)),
                 x=13,y=1.2)




grid.arrange(true_dist, train_dist, ncol = 2 )





#######################

x <- train_dat %>% data.matrix()

lamdas <- 10^seq(3, -2, by = -.1)

fit <- glmnet(x, y_train, alpha = 0, lambda = lamdas)




glm.cv.ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lamdas)
glm.cv.lasso <- cv.glmnet(x, y_train, alpha = 1, lambda = lamdas)
glm.cv.net <- cv.glmnet(x, y_train, alpha = 0.01, lambda = lamdas)

par(mfrow=c(1,3))

plot(glm.cv.ridge, main = "Ridge")
plot(glm.cv.lasso, main = "Lasso")
plot(glm.cv.net, main = "Elastic Net")


penalty.ridge <- glm.cv.ridge$lambda.min
penalty.lasso <- glm.cv.lasso$lambda.min
penalty.net <- glm.cv.net$lambda.min

fit <- cv_fit$glmnet.fit
summary(fit)


pred.ridge <- predict(glm.cv.ridge, s = penalty.ridge, newx = data.matrix(test_dat))
ridge.SalePrice <- as.data.frame(exp(pred.ridge)-1)
pred.lasso <- predict(glm.cv.lasso, s = penalty.lasso, newx = data.matrix(test_dat))
lasso.SalePrice <- as.data.frame(exp(pred.lasso)-1)
pred.net <- predict(glm.cv.net, s = penalty.net, newx = data.matrix(test_dat))
net.SalePrice <- as.data.frame(exp(pred.net)-1)

pred.xgb <- predict(bst, as.matrix(test_dat))

xgb.SalePrice <- as.data.frame(exp(pred.lasso)-1)

model_rf <- randomForest(y_train ~ ., data = train_dat)

pred.rf <- predict(model_rf, test_dat)

rf.SalePrice <- as.data.frame(exp(pred.net)-1)

SalePrice <- (xgb.SalePrice) 

submission <- cbind(test['Id'], SalePrice$`1`)


colnames(submission) <- c("Id", "SalePrice")

write.csv(submission, file = "submission.csv", row.names = F)



#############################################

dtrain <- xgb.DMatrix(as.matrix(train_dat), label = y_train)
dtest <- xgb.DMatrix(as.matrix(test_dat))


cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 4, 
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = 750,
                        eta = c(0.01,0.005,0.001),
                        max_depth = c(4,6,8),
                        colsample_bytree=c(0,1,10),
                        min_child_weight = 2,
                        subsample=c(0,0.2,0.4,0.6),
                        gamma=0.01)

set.seed(53)
xgb_tune <- train(as.matrix(train_dat),
                  y_train, method="xgbTree", 
                  trControl=cv.ctrl, 
                  tuneGrid=xgb.grid, 
                  verbose=T, metric="RMSE", nthread =3)

xgb_params <- list(
        booster = 'gbtree',
        objective = 'reg:linear',
        colsample_bytree=1,
        eta=0.005,
        max_depth=4,
        min_child_weight=3,
        alpha=0.3,
        lambda=0.4,
        gamma=0.01, # less overfit
        subsample=0.6,
        seed=5,
        silent=TRUE)

xgb.cv(xgb_params, train_dat, nrounds = 5000, nfold = 4, early_stopping_rounds = 500)

bst <- xgb.train(xgb_params,dtrain, nrounds = 10000, early_stopping_rounds = 300, watchlist = list(train=dtrain))

model.names <- dimnames(dtrain)[[2]]

importance_matrix <- xgb.importance(model.names, model = bst)

xgb.plot.importance(importance_matrix[1:10])

rmse_eval <- function(y.true, y.pred) {
        mse_eval <- sum((y.true - exp(y.pred)-1)^2) / length(y.true)
        return(sqrt(mse_eval))
}

y_pred.xgb <- predict(bst, as.matrix(test_dat))

rmse_eval(y.true, y_pred.xgb)

#############################
