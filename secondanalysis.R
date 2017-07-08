library(ggplot2)
library(Amelia)
library(varhandle)
library(caret)
library(moments)
library(mice)
library(corrplot)
library(randomForest)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


skewness(train$SalePrice)
skewness(train$)

target <- log(train$SalePrice)
skewness(target)

train <- cbind(train, target)

nums <- sapply(train, is.numeric)

num_train <- train[,nums==TRUE]
cat_train <- train[, nums==FALSE]

M <- abs(cor(num_train[, -38]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

condition1 <- train[,c("GarageArea")] < 1200
newTrain <- train[condition1,]

null_values <- sapply(newTrain, function(x) sum(is.na(x)))

save1train <- train

train <- newTrain

train$encodeStreet <- factor(ifelse(train$Street != "Grvl", 1, 0))
test$encodeStreet <- factor(ifelse(test$Street != "Grvl", 1, 0))

train$enc_condition <- factor(ifelse(train$SaleCondition == 'Partial',0,1))

test$enc_condition <- factor(ifelse(test$SaleCondition == 'Partial',0,1))

tr <- train

for(i in 1:ncol(tr)){
        tr[is.na(tr[,i]), i] <- mean(tr[,i], na.rm=TRUE)
        }

tr <- train


mice_mod <- mice_mod_rf <- mice(tr[,colnames(tr) %in% c('LotFrontage',
                                                                    'Alley','BsmtQual',' BsmtCond',
                                                                    'BsmtExposure','BsmtFinType1',
                                                                    'BsmtFinType2', 'Electrical',
                                                                    'FireplaceQu','GarageType', 
                                                                    'GarageYrBlt','GarageFinish', 
                                                                    'PoolQC','Fence', 'MiscFeature', 
                                                                    'PoolQC','Fence', 'MasVnrType',
                                                                    'BsmtCond', 'GarageQual',
                                                                    'GarageCond', "MasVnrArea")],
                                method = "pmm")

complete_pmm <- complete(mice_mod)

n <- match(colnames(complete_rf), colnames(tr))

tr <- tr[,-n]

ncol(complete_pmm)

tr <- cbind(tr,complete_pmm)

which(colnames(tr) == colnames(train))

tr <- cbind(tr,target)

which(colnames(tr) == c("SalePrice"))

tr <- tr[,-c(62,1)]

save2train <- tr

train <- tr

y <- test

y <- test[,-1]

#########################

inTrain <- createDataPartition(train$target, p = 0.5, list = FALSE)

train_in <- train[inTrain,]
test_in <- train[-inTrain,]


linear_model <- train(target ~ . , data = train_in, method = "lm")

pred <- predict(linear_model, test_in)


rf_mod <- randomForest(target ~ ., data = train_in)
pred_rf <- predict(rf_mod, test_in)



#Ensembling

combDF <- data.frame(pred_boost, pred_rf, target = test_in$target)
comb_mod <- train(target ~ ., method = "gam", data = combDF)
pred_comb <- predict(comb_mod, test_in)
r.squared_comb <- summary(lm(target ~ pred_comb, data = test_in))$r.squared
r.squared_comb
RMSE(test_in$target, pred_comb)


final <- predict(comb_mod, newdata = test)


