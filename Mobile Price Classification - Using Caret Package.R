# ===== MOBILE PRICE CLASSIFICATION =====

# READ DATA
data = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/5th Semester/Quantitative Methods/Final Work/Mobile Price Classification/train.csv', header = TRUE, sep = ',')
#View(data)
str(data)
# TARGET VARIABLE 'PRICE RANGE'
table(data$price_range)
data$price_range = factor(data$price_range, levels = c('0','1','2','3'),
                          labels = c('Very Cheap','Cheap','Expensive','Very Expensive'))
price_range = data$price_range
round(prop.table(table(data$price_range))*100, digits = 1)
data = data[-21]
# HANDLE CATEGORICAL VARIABLES
colnames(data)[colnames(data)=='blue'] = 'blue.1'
data$blue.0 = NA
data$blue.0 = data$blue.1 - 1
data$blue.0 = abs(data$blue.0)

colnames(data)[colnames(data)=='dual_sim'] = 'dual_sim.1'
data$dual_sim.0 = NA
data$dual_sim.0 = data$dual_sim.1 - 1
data$dual_sim.0 = abs(data$dual_sim.0)

colnames(data)[colnames(data)=='four_g'] = 'four_g.1'
data$four_g.0 = NA
data$four_g.0 = data$four_g.1 - 1
data$four_g.0 = abs(data$four_g.0)

colnames(data)[colnames(data)=='three_g'] = 'three_g.1'
data$three_g.0 = NA
data$three_g.0 = data$three_g.1 - 1
data$three_g.0 = abs(data$three_g.0)

colnames(data)[colnames(data)=='touch_screen'] = 'touch_screen.1'
data$touch_screen.0 = NA
data$touch_screen.0 = data$touch_screen.1 - 1
data$touch_screen.0 = abs(data$touch_screen.0)

colnames(data)[colnames(data)=='wifi'] = 'wifi.1'
data$wifi.0 = NA
data$wifi.0 = data$wifi.1 - 1
data$wifi.0 = abs(data$wifi.0)
head(data)

data = cbind(data, price_range)

# ========== USING CARET AND E1071 ==========
library(caret)
library(e1071)
str(data)
# PARTITIONING THE DATA INTO TRAINING AND VALIDATION DATA
set.seed(100)
index = createDataPartition(data$price_range, p = 0.7, list = FALSE)
train = data[index,]
validation = data[-index,]
# EXPLORE DATA
dim(train)
dim(validation)
head(train)
head(validation)
# SETTING LEVELS FOR BOTH TRAINING AND VALIDATION DATA
levels(train$price_range) = make.names(levels(factor(train$price_range)))
levels(validation$price_range) = make.names(levels(factor(validation$price_range)))
# SETTING UP TRAIN CONTROLS
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE)
                 
knn_fit = train(price_range~.,
               data = train,
               method = 'knn',
               preProcess = c('center','scale'),
               trControl = x,
               tuneLength = 10)
knn_fit
plot(knn_fit)
# MAKING PREDICTION
valid_pred = predict(knn_fit, validation)
#STORING MODEL PERFORMANCE SCORES
confusionMatrix(valid_pred, validation$price_range)
