# Load library yang diperlukan
library(caret)
library(dplyr)
library(ROCR)
library(readxl)

# Definisikan working directory
setwd("D:/Kerjaan/Project Tugas Analisis Bisnis Predictive Analysis")

# Load dataset
data <- read_excel('fraud.xlsx')

# Melihat struktur dataset
str(data)

# Memilih variabel yang akan digunakan
data_selected <- data %>% select(category, amt, city, city_pop, is_fraud) # 'Class' adalah variabel target

# Konversi variabel kategorik menjadi faktor termasuk variabel target (is_fraud)
data_selected$is_fraud <- as.factor(data_selected$is_fraud)
data_selected$category <- as.factor(data_selected$category)
data_selected$city <- as.factor(data_selected$city)

# Membagi dataset menjadi training (70%) dan testing (30%) secara stratified
set.seed(123)
trainIndex <- createDataPartition(data_selected$is_fraud, p = 0.7, list = FALSE)
trainData <- data_selected[trainIndex, ]
testData <- data_selected[-trainIndex, ]

# Membuat model regresi logistik
model <- glm(is_fraud ~ ., data = trainData, family = binomial)

# Melihat ringkasan model
summary(model)

# Prediksi pada data testing
predictions <- predict(model, testData, type = 'response')

# Menentukan threshold 0.5 untuk klasifikasi
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = testData$is_fraud)
print(conf_matrix)

# Evaluasi model
tp <- conf_matrix[2,2] # Predicted fraud yang benar-benar fraud
tn <- conf_matrix[1,1] # Predicted non-fraud yang benar-benar non-fraud
fp <- conf_matrix[2,1] # Predicted fraud yang ternyata bukan fraud
fn <- conf_matrix[1,2] # Predicted non-fraud yang ternyata fraud

cat("Predicted as fraud & actually fraud:", tp, "\n")
cat("Predicted as fraud & actually not fraud:", fp, "\n")
cat("Predicted as not fraud & actually not fraud:", tn, "\n")
cat("Predicted as not fraud & actually fraud:", fn, "\n")
