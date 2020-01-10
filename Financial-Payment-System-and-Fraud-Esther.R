if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
#importing data
banksim_data <- read.csv("./banksim1/bs140513_032310.csv")

# Github link to access dataset https://github.com/estherlab/Synthetic-Financial-Data-for-Fraud-Detection.git 

#exploring data
summary(banksim_data)
head(banksim_data)
dim(banksim_data)

#Preprocessing
banksim_data$amount <- scale(banksim_data$amount) #standardizing
Standardized_Data <- banksim_data[,-c(1)] #eliminating steps column
NewData_banksim <- Standardized_Data [,-c(1,4,6)] #eliminating zipcodeOri, zipMerchant, and Customer column
head(NewData_banksim)

#Splitting data 80-20
set.seed(1)
test_index <- createDataPartition(y = NewData_banksim$fraud, times = 1, p = 0.2, list = FALSE)
train_data <- NewData_banksim[-test_index,]
test_data <- NewData_banksim[test_index,]

#First model: Decision Tree Model
if(!require(tree)) install.packages("tree", repos = "http://cran.us.r-project.org")
require(tree)
library(rpart)
library(rpart.plot)
tree.banksim <- rpart(fraud~.,train_data, method='class') #model
rpart.plot(tree.banksim) #plotting
summary(tree.banksim) #looking at summary : discovering variable importance
tree.pred <- predict(tree.banksim, test_data, type="class") #predicting on test model
confusionMatrix(tree.pred, as.factor(test_data$fraud)) #measuring performance

#Second model: Logistic Regression Model
fit_glm <- glm(fraud ~ merchant + amount + category, train_data, family = "binomial") #model
p_hat_glm <- predict(fit_glm, test_data, type = "response") 
y_hat_glm <- ifelse(p_hat_glm > 0.5, 1, 0) %>% factor() #prediction
y_hat_glm 
confusionMatrix(data = y_hat_glm, reference = as.factor(test_data$fraud)) #measuring performance

