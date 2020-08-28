# ===== MOBILE PRICE CLASSIFICATION =====

# READ DATA
data = read.csv('D:/Audhi Aprilliant/IPB Works/Statistics Department/5th Semester/Quantitative Methods/Final Work/Mobile Price Classification/train.csv', header = TRUE, sep = ',')
View(data)
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
data = cbind(data,price_range)
View(data)

# TRANSFORMATION - NORMALIZING NUMERIC DATA
normalize = function(x) {
  normal = (x-min(x))/(max(x)-min(x))
  return(normal)
}

data_train_n = as.data.frame(lapply(data[1:26], normalize))
View(data_train_n)

# ========== SIMPLE CLASSIFICATION ==========
# DATA PREPARATION - CREATING TRAINING AND TEST DATASETS
library(caret)
set.seed(100)
index = createDataPartition(data$price_range, p = 0.7, list = FALSE)
data_train = data_train_n[index,]
data_test = data_train_n[-index,]
data_train_labels = data[index,27]
data_test_labels = data[-index,27]

# TRAINING A MODEL ON THE DATA
library(class)
data_test_pred = knn(train = data_train, test = data_test, cl = data_train_labels, k = sqrt(length(data_train)))

# EVALUATING MODEL PERFORMANCE
library(gmodels)
CrossTable(x = data_test_labels, y = data_test_pred, prop.chisq = FALSE)

# VISUALIZATION OUR KNN MODEL
plot.df = data.frame(data_test, data_test_pred)
