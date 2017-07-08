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


hist((train$SalePrice))
skewness(train$SalePrice)

nums <- sapply(train, is.numeric)
train.num <- train[,nums]

M <-abs(cor(train.num[,-38]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

anova <- aov(SalePrice~., data =train.num)
summary(anova)

agg.qual <- aggregate(train$SalePrice, 
                      by=list(factor(train$OverallQual)), 
                      function(x) mean(x, na.rm = T))
colnames(agg.qual) <- c("Overall.Quality", "Mean.Sale.Price")

ggplot(data=agg.qual, aes(x=factor(Overall.Quality), y=Mean.Sale.Price)) +
        geom_col() + ggtitle("Mean Sale Price Aggregated by Mean") + 
        xlab("Overall Quality") + ylab("Mean Sale Price")

ggplot(data=train, aes(x=GrLivArea, y=SalePrice)) + 
        geom_point()+geom_smooth(method ="lm") + 
        ggtitle("Sale Price vs Ground Living Area") +
        xlab("Ground Living Area") + ylab("Sale Price")

ggplot(data=train, aes(x=GarageArea, y=SalePrice)) + 
        geom_point() + 
        ggtitle("SalePrice vs Garage Area") + 
        xlab("Garage Area") + ylab("Sale Price") + 
        geom_smooth(method ="lm")

#Removing outliers
condition1 <- train[,c("GarageArea")] < 1200
newTrain <- train[condition1,]


missmap(newTrain, col = c("black", "White"), 
        main = "Missing Data", rank.order = T,
        y.labels = F, y.at = F, legend = F, tsvar = T)

null.values <- sapply(newTrain, function(x) sum(is.na(x)))

train.nonnum <- newTrain[,nums == FALSE]

test$enc_street <- factor(ifelse(test$Street!= 'Grvl', 1, 0))

agg.cond <- aggregate(newTrain$SalePrice, 
                      by = list(factor(newTrain$SaleCondition)), 
                      function(x) mean(x, na.rm = T))

colnames(agg.cond) <- c("Overall.condition", "Mean.Sale.Price")

ggplot(data=agg.cond, aes(x=factor(Overall.condition), y=Mean.Sale.Price)) + 
        geom_col() + ggtitle("Mean Sale Price Aggregated by Mean") + 
        xlab("Overall Condition") + ylab("Mean Sale Price")

newTrain$enc_condition <- factor(ifelse(newTrain$SaleCondition == 'Partial',0,1))

test$enc_condition <- factor(ifelse(test$SaleCondition == 'Partial',0,1))

mice_mod_rf <- mice(newTrain[,colnames(newTrain) %in% c('LotFrontage',
                                                        'Alley','BsmtQual',' BsmtCond',
                                                        'BsmtExposure','BsmtFinType1',
                                                        'BsmtFinType2', 'Electrical',
                                                        'FireplaceQu','GarageType', 
                                                        'GarageYrBlt','GarageFinish', 
                                                        'PoolQC','Fence', 'MiscFeature', 
                                                        'PoolQC','Fence', 'MasVnrType',
                                                        'BsmtCond', 'GarageQual',
                                                        'GarageCond', "MasVnrArea")],
                    method = "rf")

complete_rf <- complete(mice_mod_rf)

complete <- cbind(newTrain$Id, complete_rf)
mat <- match(colnames(complete_rf), colnames(newTrain))

newtesttrain <- newTrain[,-mat]

newtesttrain <- cbind(newtesttrain, complete_rf)

sapply(newtesttrain, function(x) sum(is.na(x)))

train <- newtesttrain



inTrain <- createDataPartition(train$SalePrice, p = 0.7, list = FALSE)

train_in <- train[inTrain,]
test_in <- train[-inTrain,]


linear_model <- train(SalePrice ~ . , data = train_in, method = "lm")

finMod <- modFit$finalModel

plot(linear_model$finalModel$residuals, pch = 19)

pred <- predict(linear_model, test_in)
ggplot(data = test_in, aes(SalePrice, pred)) + geom_point() + geom_smooth(method = 'lm')
r.squared <- summary(lm(SalePrice ~ pred, data = test_in))$r.squared
RMSE(pred, test_in$SalePrice)

rf_mod <- randomForest(SalePrice ~ ., data = train_in, method = "rf")
pred_rf <- predict(rf_mod, test_in)
ggplot(data = test_in, aes(SalePrice, pred_rf)) + geom_point() + geom_smooth(method = 'lm')
r.squared_rf <- summary(lm(SalePrice ~ pred_rf, data = test_in))$r.squared
r.squared_rf
RMSE(test_in$SalePrice, pred_rf)



r.squared_boost <- summary(lm(SalePrice ~ pred_boost, data = test_in))$r.squared
r.squared_boost
RMSE(test_in$SalePrice, pred_boost)


#Ensembling

combDF <- data.frame(pred, pred_rf, SalePrice = test_in$SalePrice)
comb_mod <- train(SalePrice ~ ., method = "gam", data = combDF)
pred_comb <- predict(comb_mod, test_in$SalePrice)
ggplot(data = test_in, aes(SalePrice, pred_comb)) + geom_point() + geom_smooth(method = 'lm')
r.squared_comb <- summary(lm(SalePrice ~ pred_comb, data = test_in))$r.squared
r.squared_comb
RMSE(test_in$SalePrice, pred_comb)


final <- predict(comb_mod, newdata = test)

